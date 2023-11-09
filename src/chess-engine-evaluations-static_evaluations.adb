--
--  AdaChess - Smart Chess Engine
--
--  Copyright (C) 2013-2023 - Alessandro Iavicoli
--  Email: adachess@gmail.com - Web Page: https://github.com/adachess/AdaChess
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Text_IO;

with Chess;
with Chess.Io;
with Chess.Io.Fen;

package body Chess.Engine.Evaluations.Static_Evaluations is

   ----------------------------------
   -- Initialize_Evaluation_Engine --
   ----------------------------------

   procedure Initialize_Evaluation_Engine is
   begin
      Score_Random.Reset (Score_Seed);
      Fill_Knight_Distance_Table;
   end Initialize_Evaluation_Engine;
   
   
   -----------------------------
   -- Is_White_Pawn_Protected --
   -----------------------------
   
   function Is_White_Pawn_Protected (Chessboard : in Chessboard_Type; Square : in Square_Type) return Boolean
   is
      F : constant Coordinate_Type := File (Square);
   begin
      if F /= File_A and then Chessboard.Square (Square + South_West) = White_Pawn then
         return True;
      end if;
      if F /= File_H and then Chessboard.Square (Square + South_East) = White_Pawn then
         return True;
      end if;
      return False;
   end Is_White_Pawn_Protected;
   
   -----------------------------
   -- Is_Black_Pawn_Protected --
   -----------------------------
   
   function Is_Black_Pawn_Protected (Chessboard : in Chessboard_Type; Square : in Square_Type) return Boolean
   is
      F : constant Coordinate_Type := File (Square);
   begin
      if F /= File_A and then Chessboard.Square (Square + North_West) = Black_Pawn then
         return True;
      end if;
      if F /= File_H and then Chessboard.Square (Square + North_East) = Black_Pawn then
         return True;
      end if;
      return False;
   end Is_Black_Pawn_Protected;
   
   
   --------------
   -- Evaluate --
   --------------
   
   function Evaluate (Chessboard : in Chessboard_Type) return Evaluation_Type is
   
   -- This function analyze the Chessboard statically and will return a value
   -- representing the final evaluation score. The score is given as a number
   -- in the range [-Mate, +Mate]. Negative values indicate that the opponent
   -- opponent has a better position, values close to zero (or zero itself)
   -- is a balanced game and positive values indicate that the current side
   -- has better position. The evaluation detect bonuses and penalties for
   -- each side and then subtract them from the Side-to-move perspective.
   --
   -- AdaChess uses an array to represent the Chessboard, therefore this
   -- evaluation function requires two stepts. The first step is needed to
   -- collect all the necessary information and the second steps will perform
   -- the evaluation based on those information.
   --
   -- The parameters taken into account are related to the material balance,
   -- the mobility (reprensenting an estimation of the piece activity, a way
   -- to evaluate the positional play from a static-perspective), the pawn
   -- structure, the king safety, the rooks on (semi)open files and on the
   -- 7th (respectively for black: 2nd) rank. Some swapecific patterns are also
   -- evaluated like trapped bishops, blocked rook (encourage castling) and
   -- so on.
   -- Furthermore, AdaChess recognize some common pattern to detect draws by
   -- insufficient material or specific endgames and uses this information
   -- to "tune" the evaluation properly.
   -- The score given to each parameter is a constant value set into the
   -- specification file.
   --
   -- Each parameter is evaluated and scored from the opening/middle-game and
   -- the endgame perspective and the final evaluation interpolate the values
   -- according to the current phase of the game. The phase is calculated
   -- by taking into account the material on the board.
   
      -------------------
      -- Tapered_Score --
      -------------------
   
      Game_Phase : Phased_Score_Type;
      -- Phased score represent a value to tell us whether we are playing in the
      -- opening phase or in the endgame. Lower value refers to the endgame, while
      -- higher values represent the opening
   
      function Tapered_Score (Score : in Tapered_Score_Type) return Score_Type
        with
          Inline => True;
      -- Calculate the average score according to the game phase. The
      -- implementation indicates that the higher is the Game_Phase the more
      -- are the pieces value. Therefore, a low value of Game_Phase means
      -- that we are going into the End_Game.
      --
      -- Arguments
      --    Score : The score obtained for Opening and End_Game
      -- Returns
      --    The tapered score compute according to the game phase
      -- Aspects
      --    Inline : The funcion is inlined
   
      function Tapered_Score (Score : in Tapered_Score_Type) return Score_Type is
      begin
         pragma Assert (Game_Phase'Valid, "Unexpected Game Phase value:" & Score_Type'Image (Game_Phase));
         return ((Score (Opening) * Game_Phase) + (Score (End_Game) * (100 - Game_Phase))) / 100;
      end Tapered_Score;
   
   
      -- Pawn evaluation data --
      White_Pawn_Counter   : Pawn_Counter_Type := 0;
      White_Pawn_Structure : Pawn_Structure_Type;
      White_Pawn_Ranks : Pawn_Rank_Coordinate_Type renames White_Pawn_Structure.Ranks;
      White_Pawn_Files : Pawn_File_Amount_Type renames White_Pawn_Structure.Files;
      White_Passed_Pawn : Pawn_Rank_Coordinate_Type;
      
      Black_Pawn_Counter : Pawn_Counter_Type := 0;
      Black_Pawn_Structure : Pawn_Structure_Type;
      Black_Pawn_Ranks : Pawn_Rank_Coordinate_Type renames Black_Pawn_Structure.Ranks;
      Black_Pawn_Files : Pawn_File_Amount_Type renames Black_Pawn_Structure.Files;
      Black_Passed_Pawn : Pawn_Rank_Coordinate_Type;
      
      
      -- Material evaluation data
      White_Piece_Location : Piece_Location_Type;
      Black_Piece_Location : Piece_Location_Type;
   
      Material       : Material_Type;
   
      Endgame_Pattern : Endgame_Pattern_Type := No_Pattern;
      Insufficient_Material : Boolean := False;
   
   
      -----------------------
      -- Attacks_King_Zone --
      -----------------------
   
      function Attacks_King_Zone (Side : in Color_Type) return Score_Type is
         Opponent_Side : constant Color_Type := not Side;
   
         King_Position : constant Square_Type := (if Side = White then Chessboard.White_King_Position else Chessboard.Black_King_Position);
         King_Zone     : King_Zone_Type;
   
         --  Defenders     : Attack_Collection_Type;
         Attackers     : Attack_Collection_Type;
         Attacker      : Piece_Type;
   
         Piece         : Piece_Type;
   
         --  Undefended    : Boolean := False;
         --  Safe_Contact_Check : Boolean := False;
   
         Nb_Of_Attacker     : Natural := 0;
         Value_Of_Attack    : Score_Type := 0;
   
         Origin        : Square_Type;
         Attack_Origin      : Attack_Origin_Table := (others => 1);
   
      begin
         
         for I in King_Offsets'Range loop
            King_Zone (I) := King_Position + King_Offsets (I);
         end loop;
   
         for Square of King_Zone loop
            Piece := Chessboard.Square (Square);
   
            ------------------------------
            -- Attacks to the king zone --
            ------------------------------
   
            if Piece /= Frame then
   
               Attackers := Attacking_Piece
                 (Chessboard => Chessboard, Side => Opponent_Side, Square => Square, Only_One => False);
   
               for I in 1 .. Attackers.Number_Of_Attackers loop
                  Attacker := Attackers.Attacker (I).Piece;
                  Origin := Attackers.Attacker (I).Origin;
   
                  if Attacker not in Pawn_Type | King_Type then -- and then not Attack_Origin (Origin) then
                     Value_Of_Attack := Value_Of_Attack + King_Attack_Value (Attacker) / Attack_Origin (Origin);
                     if Attack_Origin (Origin) = 1 then
                        Nb_Of_Attacker := Nb_Of_Attacker + 1;
                     end if;
                     Attack_Origin (Origin) := Attack_Origin (Origin) + 1;
                  end if;
   
               end loop;
   
               ------------------------
               -- Safe Contact Check --
               ------------------------
   
               --  Safe_Contact_Check := (if Side = White then Piece in Black_Queen | Black_Rook else Piece in White_Queen | White_Rook);
               --  
               --  if Safe_Contact_Check then
               --     Defenders := Attacking_Square
               --       (Chessboard => Chessboard, Side => Side, Square => Square, Only_One => False);
               --     Undefended := Defenders.Number_Of_Attackers = 1; -- Only King defending the square
               --     if Undefended then
               --        Value_Of_Attack := Value_Of_Attack + Safe_Contact_Check_Attack (Piece);
               --     end if;
               --  end if;
   
            end if;
         end loop;
   
         return Value_Of_Attack * Attack_Weight (Nb_Of_Attacker) / Attacking_King_Factor;
   
      end Attacks_King_Zone;
      
      
      ---------------------
      -- Pawn is Hanging --
      ---------------------
      
      function Pawn_Is_Hanging (Square : in Square_Type) return Direction_Type is
         Pin : Pin_Type := No_Pin;
      
         King_Position : Square_Type := 0;
         Target        : Square_Type;
         Attacker : Piece_Type;
      
         Direction : Direction_Type;
         Pin_Direction : Direction_Type;
      
         Piece         : constant Piece_Type := Chessboard.Square (Square);
         Hanging_Side : Color_Type;
      
      begin
    
         pragma Assert (Piece in Pawn_Type,
            "Pawn_is_Hanging called with an input square containing a non-pawn piece (or empy/frame)");
      
         Pin_Direction := No_Direction;
      
         -- This routine verifies if a piece is hanging, which in this case
         -- means it is under absolute pin and cannot be moved.
         -- The goal of this routine is to improve the mobility evaluation
      
         if Piece = White_Pawn then
            Hanging_Side := White;
            King_Position := Chessboard.White_King_Position;
         elsif Piece = Black_Pawn then
            Hanging_Side := Black;
            King_Position := Chessboard.Black_King_Position;
         end if;
      
         pragma Assert (King_Position /= 0, "Pawn_is_Hanging called for a non-pawn piece!");
                        
         Direction := Find_Sliding_Direction
           (Origin      => Square,
            Destination => King_Position);
      
         -- Investigate if the current pawn is hanging. There must be no pieces,
         -- neither friendly nor enemy, between the pawn and the king. There
         -- must be also an attacker along the same direction.
      
         if Direction in South | North | East | West | South_East | South_West | North_East | North_West then
      
            Target := Square;
            loop
               Target := Target - Direction;
               exit when Chessboard.Square (Target) /= Empty;
            end loop;
      
            if Target /= King_Position then
               return No_Direction;
            end if;
      
            -- The king is behind this pawn!
            
            Target := Square;
            loop
               Target := Target + Direction;
               Attacker := Chessboard.Square (Target);
               exit when Attacker /= Empty;
            end loop;
      
            if Hanging_Side = White then
               if Direction in South | North | East | West and then Attacker in Black_Rook | Black_Queen then
                  Pin_Direction := -Direction;
               elsif Direction in South_East | South_West | North_East | North_West and then Attacker in Black_Bishop | Black_Queen then
                  Pin_Direction := -Direction;
               end if;
            elsif Hanging_Side = Black then
               if Direction in South | North | East | West and then Attacker in White_Rook | White_Queen then
                  Pin_Direction := -Direction;
               elsif Direction in South_East | South_West | North_East | North_West and then Attacker in White_Bishop | White_Queen then
                  Pin_Direction := -Direction;
               end if;
            end if;
      
         end if;
      
         return Pin_Direction;
      
      end Pawn_Is_Hanging;
   
      
      ----------------------
      -- Piece_is_Hanging --
      ----------------------
      
      function Piece_Is_Hanging (Square : in Square_Type) return Direction_Type is
         Pin : Pin_Type := No_Pin;
      
         King_Position : Square_Type := 0;
         Target        : Square_Type;
      
         Direction : Direction_Type;
         Pin_Direction : Direction_Type;
      
         Piece         : constant Piece_Type := Chessboard.Square (Square);
         Hanging_Side : Color_Type;
      
      begin
         
         Pin_Direction := No_Direction;
      
         -- This routine verifies if a piece is hanging, which in this case
         -- means it is under absolute pin and cannot be moved.
         -- The goal of this routine is to improve the mobility evaluation
      
         if Piece in White_Piece_Type then
            Hanging_Side := White;
            King_Position := Chessboard.White_King_Position;
         elsif Piece in Black_Piece_Type then
            Hanging_Side := Black;
            King_Position := Chessboard.Black_King_Position;
         end if;
      
         pragma Assert (King_Position /= 0, "Invalid Hanging piece situation found!");
      
         Direction := Find_Sliding_Direction
           (Origin      => Square,
            Destination => King_Position);
      
         -- If the pin is under a possible hanging direction, investigate what
         -- is the situation on the board along this direction
      
         if Direction in South | North | East | West | South_East | South_West | North_East | North_West then
      
            -- Are there any friendly pieces between "me" and my king?
            Target := Square;
            loop
               Target := Target - Direction;
               exit when Chessboard.Square (Target) /= Empty;
            end loop;
      
            if Target /= King_Position then
               return No_Direction;
            end if;
      
            -- The king is right behind this piece. We now want to search for
            -- attackers in the opposite direction to detect if this piece is
            -- hanging
      
            Target := Square;
            loop
               Target := Target + Direction;
               exit when Chessboard.Square (Target) /= Empty;
            end loop;
      
            if Hanging_Side = White then
               if Direction in South | North | East | West and then Chessboard.Square (Target) in Black_Rook | Black_Queen then
                  Pin_Direction := -Direction;
               elsif Direction in South_East | South_West | North_East | North_West and then Chessboard.Square (Target) in Black_Bishop | Black_Queen then
                  Pin_Direction := -Direction;
               end if;
            elsif Hanging_Side = Black then
               if Direction in South | North | East | West and then Chessboard.Square (Target) in White_Rook | White_Queen then
                  Pin_Direction := -Direction;
               elsif Direction in South_East | South_West | North_East | North_West and then Chessboard.Square (Target) in White_Bishop | White_Queen then
                  Pin_Direction := -Direction;
               end if;
            end if;
      
         end if;
      
         return Pin_Direction;
      
      end Piece_is_Hanging;
      
   
      -------------------------
      -- Collect Information --
      -------------------------
   
      procedure Collect_Information is
         R                    : Coordinate_Type;
         F                    : Coordinate_Type;
         Piece_Location_Index : Piece_Counter_Type;
         Square_Color         : Color_Type;
         White_Pieces         : Pieces_List_Type renames Chessboard.Pieces_List (White_Pieces_Range);
         Black_Pieces         : Pieces_List_Type renames Chessboard.Pieces_List (Black_Pieces_Range);
         Piece                : Piece_Type;
      begin
   
         White_Pawn_Ranks := (others => Rank_8);
         White_Passed_Pawn := (others => Rank_1);
   
         Black_Pawn_Ranks := (others => Rank_1);
         Black_Passed_Pawn := (others => Rank_8);
   
         White_Bishop_Pair := (others => False);
         Black_Bishop_Pair := (others => False);
   
         Piece_Location_Index := 0;
   
         for Square of White_Pieces loop
            exit when Square = 0;
   
            Piece := Chessboard.Square (Square);
            Material (Piece) := Material (Piece) + 1;
   
            case Piece is
               when White_Pawn =>
                  -- A White Pawn is found on the Chessboard. We can collect
                  -- many useful information hier that will be used later in the
                  -- pawn structure evaluation to detect strenghts and weakness
                  -- of the entire pawn structure for the White
                  R := Rank (Square);
                  F := File (Square);
                  White_Pawn_Counter := White_Pawn_Counter + 1;
                  White_Pawn_Structure.Location (White_Pawn_Counter) := Square;
                  if White_Pawn_Ranks (F) > R then
                     White_Pawn_Ranks (F) := R;
                  end if;
                  White_Pawn_Files (F) :=
                    White_Pawn_Files (F) + 1;
               when others =>
                  Piece_Location_Index := Piece_Location_Index + 1;
                  White_Piece_Location (Piece_Location_Index) := Square;
                  if Piece = White_Bishop then
                     Square_Color := Color_Board (Square);
                     White_Bishop_Pair (Square_Color) := True;
                  end if;
            end case;
   
         end loop;
   
         Piece_Location_Index := 0;
   
         for Square of Black_Pieces loop
            exit when Square = 0;
            Piece := Chessboard.Square (Square);
            Material (Piece) := Material (Piece) + 1;
   
            case Piece is
               when Black_Pawn =>
                  -- A Black Pawn is found on the Chessboard. We can collect
                  -- many useful information hier that will be used later in the
                  -- pawn structure evaluation to detect strenghts and weakness
                  -- of the entire pawn structure for the Black
                  R := Rank (Square);
                  F := File (Square);
                  Black_Pawn_Counter := Black_Pawn_Counter + 1;
                  Black_Pawn_Structure.Location (Black_Pawn_Counter) := Square;
                  if Black_Pawn_Ranks (F) < R then
                     Black_Pawn_Ranks (F) := R;
                  end if;
                  Black_Pawn_Files (F) :=
                    Black_Pawn_Files (F) + 1;
               when others =>
                  Piece_Location_Index := Piece_Location_Index + 1;
                  Black_Piece_Location (Piece_Location_Index) := Square;
                  if Piece = Black_Bishop then
                     Square_Color := Color_Board (Square);
                     Black_Bishop_Pair (Square_Color) := True;
                  end if;
            end case;
   
         end loop;
                  
      end Collect_Information;
   
   
   
      ----------------------------
      -- Detect_Endgame_Pattern --
      ----------------------------
   
      function Detect_Endgame_Pattern return Endgame_Pattern_Type is
   
         White_Total : constant Piece_Counter_Type := Chessboard.White_Pieces_Counter - 1; -- Don't consider the King
         Black_Total : constant Piece_Counter_Type := Chessboard.Black_Pieces_Counter - 1; -- Don't consider the King
   
         White_Pawns          : constant Piece_Counter_Type := Material (White_Pawn);
         White_Knights        : constant Piece_Counter_Type := Material (White_Knight);
         White_Bishops        : constant Piece_Counter_Type := Material (White_Bishop);
         White_Rooks          : constant Piece_Counter_Type := Material (White_Rook);
         White_Queens         : constant Piece_Counter_Type := Material (White_Queen);
         Black_Pawns          : constant Piece_Counter_Type := Material (Black_Pawn);
         Black_Knights        : constant Piece_Counter_Type := Material (Black_Knight);
         Black_Bishops        : constant Piece_Counter_Type := Material (Black_Bishop);
         Black_Rooks          : constant Piece_Counter_Type := Material (Black_Rook);
         Black_Queens         : constant Piece_Counter_Type := Material (Black_Queen);
   
         White_Major          : constant Piece_Counter_Type := White_Rooks + White_Queens * 2;
         White_Minor          : constant Piece_Counter_Type := White_Knights + White_Bishops;
         Black_Major          : constant Piece_Counter_Type := Black_Rooks + Black_Queens * 2;
         Black_Minor          : constant Piece_Counter_Type := Black_Knights + Black_Bishops;
   
         White_Non_Pawns      : constant Piece_Counter_Type := White_Major * 2 + White_Minor;
         Black_Non_Pawns      : constant Piece_Counter_Type := Black_Major * 2 + Black_Minor;
   
         Pattern : Endgame_Pattern_Type := No_Pattern;
   
      begin
   
         if White_Total = 0 and then Black_Total = 0 then
            Pattern := KK;
   
         elsif White_Total = 1 and then Black_Total = 0 then
            -- Case 1: Lonely Black King agains white
            Pattern := (if White_Knights = 1 then KN_K elsif White_Bishops = 1 then KB_K
                        elsif White_Rooks = 1 then KR_K elsif White_Queens = 1 then KQ_K
                        elsif White_Pawns = 1 then KP_K else No_Pattern);
         elsif White_Total = 0 and then Black_Total = 1 then
            Pattern := (if Black_Knights = 1 then K_KN elsif Black_Bishops = 1 then K_KB
                        elsif Black_Rooks = 1 then K_KR elsif Black_Queens = 1 then K_KQ
                        elsif Black_Pawns = 1 then K_KP else No_Pattern);
   
         elsif White_Total = 1 and then Black_Total = 1 then
            if White_Queens = 1 then
               Pattern := (if Black_Queens = 1 then KQ_KQ elsif Black_Pawns = 1 then KQ_KP else No_Pattern);
            elsif Black_Queens = 1 then
               Pattern := (if White_Pawns = 1 then KP_KQ else No_Pattern);
   
            elsif White_Rooks = 1 then
               Pattern := (if Black_Rooks = 1 then KR_KR elsif Black_Pawns = 1 then KR_KP else No_Pattern);
            elsif Black_Rooks = 1 then
               Pattern := (if White_Pawns = 1 then KP_KR else No_Pattern);
   
            elsif White_Bishops = 1 then
               Pattern := (if Black_Bishops = 1 then KB_KB elsif Black_Pawns = 1 then KB_KP else No_Pattern);
            elsif Black_Bishops = 1 then
               Pattern := (if White_Pawns = 1 then KP_KB else No_Pattern);
   
            elsif White_Knights = 1 then
               Pattern := (if Black_Knights = 1 then KN_KN elsif Black_Pawns = 1 then KN_KP else No_Pattern);
            elsif Black_Knights = 1 then
               Pattern := (if White_Pawns = 1 then KP_KN else No_Pattern);
               
            else
               Pattern := KP_KP;
   
            end if;
   
         elsif White_Total = 2 and then White_Pawns = 1 and then Black_Total = 0 then
            Pattern := (if White_Bishops = 1 then KBP_K elsif White_Knights = 1 then KNP_K else No_Pattern);
         elsif White_Total = 0 and then Black_Total = 2 and then Black_Pawns = 1 then
            Pattern := (if Black_Bishops = 1 then K_KBP elsif White_Knights = 1 then K_KNP else No_Pattern);
   
         elsif White_Total = 2 and then Black_Total = 1 and then White_Pawns = 1 then
            Pattern := (if White_Rooks = 1 and Black_Rooks = 1 then KRP_KR elsif White_Bishops = 1 and Black_Bishops = 1 then KBP_KB else No_Pattern);
         elsif White_Total = 1 and then Black_Total = 2 and then Black_Pawns = 1 then
            Pattern := (if Black_Rooks = 1 and White_Rooks = 1 then KR_KRP elsif Black_Bishops = 1 and White_Bishops = 1 then KB_KBP else No_Pattern);
   
         elsif White_Non_Pawns = 0 and then Black_Non_Pawns = 0 then
            Pattern := KP_KP;
            
         end if;
   
   
         ------------------------------------------
         -- Detect draw by insufficient material --
         ------------------------------------------
   
         if White_Total <= 2 and then White_Minor = White_Total and then Black_Total <= 2 and then Black_Minor = Black_Total then
            if White_Bishops = 0 and then Black_Bishops = 0 then
               Insufficient_Material := True;
            end if;
         end if;
   
         if Pattern in KK | KB_K | KN_K | K_KB | K_KN then
            Insufficient_Material := True;
            --  elsif Multiplier (White) = 0 and then Multiplier (Black) = 0 then
            --     Insufficient_Material := True;
         end if;
   
         return Pattern;
   
      end Detect_Endgame_Pattern;
   
   
   
      -------------------------
      -- Material Evaluation --
      -------------------------
   
      function Evaluate_White_Material return Score_Type is
         Score          : Tapered_Score_Type;
         Pawn_On_Center : Natural := 0;
      begin
         Score := Zero
           + Material (White_Pawn)   * Pawn_Score
           + Material (White_Knight) * Knight_Score
           + Material (White_Bishop) * Bishop_Score
           + Material (White_Rook)   * Rook_Score
           + Material (White_Queen)  * Queen_Score;
   
         if White_Bishop_Pair (White) and then White_Bishop_Pair (Black) then
            Score := Score + Bishop_Pair_Bonus;
   
            -- Add further bonus if the center of the board is free from pawns
            if Chessboard.Square (D3) in Pawn_Type then
               Pawn_On_Center := Pawn_On_Center + 1;
            end if;
            if Chessboard.Square (D4) in Pawn_Type then
               Pawn_On_Center := Pawn_On_Center + 1;
            end if;
            if Chessboard.Square (E3) in Pawn_Type then
               Pawn_On_Center := Pawn_On_Center + 1;
            end if;
            if Chessboard.Square (E4) in Pawn_Type then
               Pawn_On_Center := Pawn_On_Center + 1;
            end if;
            if Pawn_On_Center <= 1 then
               Score := Score + Bishop_Pair_Free_Center;
            end if;
   
         end if;
   
         return Tapered_Score (Score);
      end Evaluate_White_Material;
   
      function Evaluate_Black_Material return Score_Type is
         Score          : Tapered_Score_Type;
         Pawn_On_Center : Natural := 0;
      begin
         Score := Zero
           + Material (Black_Pawn)   * Pawn_Score
           + Material (Black_Knight) * Knight_Score
           + Material (Black_Bishop) * Bishop_Score
           + Material (Black_Rook)   * Rook_Score
           + Material (Black_Queen)  * Queen_Score;
   
         if Black_Bishop_Pair (White) and then Black_Bishop_Pair (Black) then
            Score := Score + Bishop_Pair_Bonus;
   
            -- Add further bonus if the center of the board is free from pawns
            if Chessboard.Square (D6) in Pawn_Type then
               Pawn_On_Center := Pawn_On_Center + 1;
            end if;
            if Chessboard.Square (D5) in Pawn_Type then
               Pawn_On_Center := Pawn_On_Center + 1;
            end if;
            if Chessboard.Square (E6) in Pawn_Type then
               Pawn_On_Center := Pawn_On_Center + 1;
            end if;
            if Chessboard.Square (E5) in Pawn_Type then
               Pawn_On_Center := Pawn_On_Center + 1;
            end if;
            if Pawn_On_Center <= 1 then
               Score := Score + Bishop_Pair_Free_Center;
            end if;
         end if;
   
         return Tapered_Score (Score);
      end Evaluate_Black_Material;
   
   
      -------------------------------
      -- Pawn Structure Evaluation --
      -------------------------------
   
      -- See: https://en.wikipedia.org/wiki/Pawn_structure
      -- See: https://en.wikipedia.org/wiki/Doubled_pawns
      -- See: https://en.wikipedia.org/wiki/Isolated_pawn
      -- See: https://en.wikipedia.org/wiki/Connected_pawns
      -- See: https://en.wikipedia.org/wiki/Passed_pawn (protected passed)
      function Evaluate_White_Pawn_Structure return Score_Type is
         Score : Tapered_Score_Type := Zero;
   
         function Evaluate_Passed_Pawn (Square : Square_Type; R, F : Coordinate_Type; Connected : Boolean) return Tapered_Score_Type is
            pragma Unreferenced (F);
            
            Passed_Score     : Tapered_Score_Type := Zero;
            King_Position    : Square_Type renames Chessboard.White_King_Position;
            Opponent_King    : Square_Type renames Chessboard.Black_King_Position;
            Unstoppable      : Boolean := False;
            Promotion_Square : constant Square_Type := White_Promotion_Square (Square);
            
            function King_Is_On_Square return Boolean is
            begin
               return Race (Chessboard      => Chessboard,
                            Piece           => Black_King,
                            Square          => Opponent_King,
                            Opponent        => White_Pawn,
                            Opponent_Square => Square,
                            Target          => Promotion_Square) in Equals | Closer | Winner;
            end King_Is_On_Square;
   
         begin
            pragma Assert (R in Rank_2 .. Rank_7);
   
            for Phase in Game_Phase_Type loop
               Passed_Score (Phase) := White_Passed_Pawn_Bonus (Phase, R);
            end loop;
   
            if Connected then
               Passed_Score := Passed_Score + Connected_Pawn_Bonus * R;
            end if;
   
            -- King Proximity --
            Passed_Score (End_Game) := Passed_Score (End_Game)
              - Distance (Chessboard => Chessboard,
                          Piece      => White_King,
                          Square     => King_Position,
                          Target     => Promotion_Square) * King_Proximity_Factor;
   
            Passed_Score (End_Game) := Passed_Score (End_Game)
              + Distance (Chessboard => Chessboard,
                          Piece      => Black_King,
                          Square     => Opponent_King,
                          Target     => Promotion_Square) * King_Proximity_Factor;
   
            -- Evaluate data specific to some endgame patterns --
   
            if Endgame_Pattern in K_KP | KP_KP then
               Unstoppable := not King_Is_On_Square;
            end if;
   
            if Unstoppable then
               Passed_Score (End_Game) := Passed_Score (End_Game) + Unstoppable_Passer;
            end if;
   
            return Passed_Score;
         end Evaluate_Passed_Pawn;
   
         function Evaluate_Candidate_Passed_Pawn (R : Coordinate_Type; Connected : Boolean) return Tapered_Score_Type is
            Candidate_Score : Tapered_Score_Type := Zero;
         begin
   
            Candidate_Score (End_Game) := White_Candidate_Passed_Pawn_Bonus (End_Game, R);
   
            if Connected then
               Candidate_Score := Candidate_Score + Connected_Pawn_Bonus;
            end if;
   
            return Candidate_Score;
         end Evaluate_Candidate_Passed_Pawn;
 
   
   
         Pawn_Structure : Pawn_Structure_Type renames White_Pawn_Structure;
   
         function Pawn_Is_Backward (Square : in Square_Type; F, R : Coordinate_Type; Supported : Boolean; Weak : out Boolean) return Boolean is
            Blocked : Boolean := False;
         begin
            
            -- Conditions to consider a pawn Backward (a weakness) are:
            -- 1) Other friendly pawns are already advanced
            -- 2) It is not supported
            -- 3) It can't advance freely without being captured
            
            Weak := False;
            
            if not Supported then
               Weak := (F = File_A or else R < Pawn_Structure.Ranks (F - 1) or else Pawn_Structure.Files (F - 1) = 0)
                 and then (F = File_H or else R < Pawn_Structure.Ranks (F + 1) or else Pawn_Structure.Files (F + 1) = 0);
               
               Blocked := Chessboard.Square (Square + North_North_West) = Black_Pawn
                 or else Chessboard.Square (Square + North_North_East) = Black_Pawn;
            end if;
            
            return Weak and then Blocked;
         end Pawn_Is_Backward;
         
         --  function Square_Cannot_Be_Attacked_By_Opponent_Pawns (Square : in Square_Type) return Boolean is
         --     F : constant Coordinate_Type := File (Square);
         --     R : constant Coordinate_Type := Rank (Square);
         --  begin
         --     return (F = File_A or else R >= Black_Pawn_Structure.Ranks (F - 1) or else Black_Pawn_Structure.Files (F - 1) = 0)
         --       and then (F = File_H or else R >= Black_Pawn_Structure.Ranks (F + 1) or else Black_Pawn_Structure.Files (F + 1) = 0);
         --  end Square_Cannot_Be_Attacked_By_Opponent_Pawns;
   
   
         F     : Coordinate_Type;
         R     : Coordinate_Type;
   
         Doubled   : Boolean;
         Isolated  : Boolean;
         Isolani   : Boolean; -- Isolated queen pawn (on D file)
         Weak      : Boolean;
         Backward  : Boolean;
         Connected : Boolean;
         Supported : Boolean;
         Exposed   : Boolean; -- On_Semi_Open_File and not connected
         Passed    : Boolean;
         Candidate : Boolean;
         Outpost   : Boolean;
   
         On_Semi_Open_File : Boolean;
   
         Current_File_Has_Pawn : Boolean;
         Left_File_Has_Pawn : Boolean;
         Islands : Pawn_Islands_Counter := 0;
         
         Hanging : Boolean;
   
         -- An Outpost is a square which is protected by a friendly pawn and
         -- cannot be attacked by enemy pawns. Outposts are good squares for
         -- placing our own pieces, especially knights. According to Nimzowitsch
         -- an outpost is a square on a half-open file on the opponent half of
         -- the board, defended by own pawn
   
         Outpost_Score   : Tapered_Score_Type := Zero;
         Candidate_Score : Tapered_Score_Type := Zero;
         Passed_Score    : Tapered_Score_Type := Zero;
   
         Stop  : Square_Type;
   
      begin
   
         for Square of Pawn_Structure.Location loop
            exit when Square = Not_Present;
   
            Stop := Square + North;
   
            F := File (Square);
            R := Rank (Square);
   
            Doubled   := False;
            Isolated  := False;
            Isolani   := False;
            Weak      := False;
            Backward  := False;
            Exposed   := False; -- On Semi open files and not protected
            Connected := False;
            Supported := False;
            Passed    := False;
            Candidate := False;
            Outpost   := False;
   
            On_Semi_Open_File := False;
            
            Outpost_Score := Zero;
            
            Hanging := Pawn_Is_Hanging (Square) /= No_Direction;
            
            -- Supported
   
            if F /= File_A and then Chessboard.Square (Square + South_West) = White_Pawn then
               Supported := True;
            elsif F /= File_H and then Chessboard.Square (Square + South_East) = White_Pawn then
               Supported := True;
            end if;
   
            -- Isolated --
   
            if (F = File_A or else Pawn_Structure.Files (F - 1) = 0)
              and then (F = File_H or else Pawn_Structure.Files (F + 1) = 0)
            then
               if F = File_D then
                  Isolani := True;
               else
                  Isolated := True;
               end if;
            else
               -- Backward --
               Backward := Pawn_Is_Backward (Square, F, R, Supported, Weak);
               White_Pawn_Structure.Backwards (F) := Backward;
            end if;
            
            -- Connected --
            
            if Supported
              or else (F /= File_A and then Chessboard.Square (Square + West) = White_Pawn)
              or else (F /= File_H and then Chessboard.Square (Square + East) = White_Pawn)
            then
               Connected := True;
            end if;
            
            -- Doubled --
   
            if not Supported and then Pawn_Structure.Files (F) > 1 then
               Doubled := True;
            end if;
   
            -- On_Semi_Open_File --
   
            --  if F in File_D | File_E then
               On_Semi_Open_File := White_Pawn_Files (F) = 0;
            --  end if;
   
            -- Exposed --
   
            if not Supported then
               Exposed := Hanging or else On_Semi_Open_File;
            end if;
   
            -- Passed pawn --
   
            if (F = File_A or else Black_Pawn_Files (F - 1) = 0 or else Black_Pawn_Ranks (F - 1) <= R)
              and then (Black_Pawn_Files (F) = 0 or else Black_Pawn_Ranks (F) < R)
              and then (F = File_H or else Black_Pawn_Files (F + 1) = 0 or else Black_Pawn_Ranks (F + 1) <= R)
            then
               Passed := True;
               Passed_Score := Evaluate_Passed_Pawn (Square => Square, R => R, F => F, Connected => Connected);
               White_Passed_Pawn (F) := R; -- If doubled, this info is inaccuarate
            end if;
   
            -- Candidate --
   
            if not Passed then
               if Black_Pawn_Files (F) = 0 or else Black_Pawn_Ranks (F) < R then
                  if (F = File_A or else Black_Pawn_Files (F - 1) <= White_Pawn_Files (F - 1))
                    and then (F = File_H or else Black_Pawn_Files (F + 1) <= White_Pawn_Files (F + 1)) then
                     Candidate := True;
                     Candidate_Score := Evaluate_Candidate_Passed_Pawn (R, Connected);
                  end if;
               end if;
            end if;
   
            -- Outposts --
            
            if Backward then
               Outpost := Chessboard.Square (Stop) in Black_Non_Pawn;
               if Outpost then
                  Outpost_Score := Outpost_Occupancy_Penalty (Chessboard.Square (Stop));
               end if;
            end if;
            
            --  if F /= File_A and then Square_Cannot_Be_Attacked_By_Opponent_Pawns (Stop + West) then
            --     if Chessboard.Square (Stop + West) in White_Piece_Type then
            --        Outpost := True;
            --        Outpost_Score := Outpost_Square_Occupancy (Chessboard.Square (Stop + West));
            --        --  Ada.Text_IO.Put_Line ("Outpost available on " & Pc_Sqr (Stop + West));
            --     end if;
            --  end if;
            --  
            --  if F /= File_H and then Square_Cannot_Be_Attacked_By_Opponent_Pawns (Stop + East) then
            --     if Chessboard.Square (Stop + East) in White_Piece_Type then
            --        Outpost := True;
            --        Outpost_Score := Outpost_Score + Outpost_Square_Occupancy (Chessboard.Square (Stop + East));
            --        --  Ada.Text_IO.Put_Line ("Outpost available on " & Pc_Sqr (Stop + East));
            --     end if;
            --  end if;
            
   
            --  if Doubled then
            --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is DOUBLED");
            --  end if;
            --  if isolated or Isolani then
            --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is ISOLATED");
            --  end if;
            --  if Backward then
            --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is BACKWARD");
            --  end if;
            --  --  if Connected then
            --  --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is CONNECTED");
            --  --  end if;
            --  --  if Supported then
            --  --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is SUPPORTED");
            --  --  end if;
            --  if Exposed then
            --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is EXPOSED");
            --  end if;
            --  if Passed then
            --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is PASSED");
            --  end if;
            --  if Candidate then
            --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is CANDIDATE PASSER");
            --  end if;
            --  if On_Semi_Open_File then
            --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is  On_Semi_Open_File");
            --  end if;
            --  if Outpost then
            --     Ada.Text_IO.Put_Line ("Square " & Pc_Sqr (Stop) & " is an outpost");
            --  end if;
            
   
            Score := Score
              + White_Pawn_Position_Table (Square)
              + (if Doubled           then Doubled_Pawn_Penalty      else Zero)
              + (if Isolated          then Isolated_Pawn_Penalty     else Zero)
              + (if Isolani           then Isolani_Pawn_Penalty      else Zero)
              + (if Weak              then Weak_Pawn_Penalty         else Zero)
              + (if Backward          then Backward_Pawn_Penalty     else Zero)
              --  + (if On_Semi_Open_File then On_Semi_Open_File_Penalty else Zero)
              + (if Exposed           then Exposed_Pawn_Penalty      else Zero)
              --  + (if Connected         then Connected_Pawn_Bonus      else Zero)
              + (if Outpost           then Outpost_Score             else Zero)
              + (if Passed            then Passed_Score              else Zero)
              + (if Candidate         then Candidate_Score           else Zero)
              + (if Hanging           then Hanging_Pawn_Penalty      else Zero);
   
         end loop;
   
         
         --  Ada.Text_IO.Put_Line ("White Weak square count is " & Natural'Image (Count_White_Weak_Squares));
         -- Pawn Islands --
   
         Islands := 0;
   
         Left_File_Has_Pawn := False;
         for I in File_A .. File_H loop
            Current_File_Has_Pawn := White_Pawn_Files (I) > 0;
            if Left_File_Has_Pawn and then not Current_File_Has_Pawn then
               Islands := Islands + 1;
            end if;
            Left_File_Has_Pawn := Current_File_Has_Pawn;
         end loop;
   
         if White_Pawn_Files (File_H) > 0 then
            Islands := Islands + 1;
         end if;
   
         if Islands > 1 then
            Score := Score + Pawn_Island_Penalty_Factor * Islands;
         end if;
         
         --  Score (Opening) := Score (Opening) + Count_White_Weak_Squares * Weak_Square_Penalty;
   
         return Tapered_Score (Score);
      end Evaluate_White_Pawn_Structure;
   
   
   
      function Evaluate_Black_Pawn_Structure return Score_Type is
         Score : Tapered_Score_Type := Zero;
   
         function Evaluate_Passed_Pawn (Square : Square_Type; R, F : Coordinate_Type; Connected : Boolean) return Tapered_Score_Type is
            pragma Unreferenced (F);
            
            Passed_Score     : Tapered_Score_Type := Zero;
            King_Position    : Square_Type renames Chessboard.Black_King_Position;
            Opponent_King    : Square_Type renames Chessboard.White_King_Position;
            Unstoppable      : Boolean := False;
            Promotion_Square : constant Square_Type := Black_Promotion_Square (Square);
            
            function King_Is_On_Square return Boolean is
            begin
               return Race (Chessboard      => Chessboard,
                            Piece           => White_King,
                            Square          => Opponent_King,
                            Opponent        => Black_Pawn,
                            Opponent_Square => Square,
                            Target          => Promotion_Square) in Equals | Closer | Winner;
            end King_Is_On_Square;
   
         begin
            pragma Assert (R in Rank_2 .. Rank_7);
   
            for Phase in Game_Phase_Type loop
               Passed_Score (Phase) := Black_Passed_Pawn_Bonus (Phase, R);
            end loop;
   
            if Connected then
               Passed_Score := Passed_Score + Connected_Pawn_Bonus * (9 - R);
            end if;
   
            -- King Proximity --
            Passed_Score (End_Game) := Passed_Score (End_Game)
              - Distance (Chessboard => Chessboard,
                          Piece      => Black_King,
                          Square     => King_Position,
                          Target     => Promotion_Square) * King_Proximity_Factor;
   
            Passed_Score (End_Game) := Passed_Score (End_Game)
              + Distance (Chessboard => Chessboard,
                          Piece      => White_King,
                          Square     => Opponent_King,
                          Target     => Promotion_Square) * King_Proximity_Factor;
   
            -- Evaluate data specific to some endgame patterns --
   
            if Endgame_Pattern in KP_K | KP_KP then
               Unstoppable := not King_Is_On_Square;
            end if;
               
   
            if Unstoppable then
               Passed_Score (End_Game) := Passed_Score (End_Game) + Unstoppable_Passer;
            end if;
   
            return Passed_Score;
         end Evaluate_Passed_Pawn;
   
         function Evaluate_Candidate_Passed_Pawn (R : Coordinate_Type; Connected : Boolean) return Tapered_Score_Type is
            Candidate_Score : Tapered_Score_Type := Zero;
         begin
   
            Candidate_Score (End_Game) := Black_Candidate_Passed_Pawn_Bonus (End_Game, R);
   
            if Connected then
               Candidate_Score := Candidate_Score + Connected_Pawn_Bonus;
            end if;
   
            return Candidate_Score;
         end Evaluate_Candidate_Passed_Pawn;
   
   
   
         Pawn_Structure : Pawn_Structure_Type renames Black_Pawn_Structure;
         
         function Pawn_Is_Backward (Square : in Square_Type; F, R : Coordinate_Type; Supported : Boolean; Weak : out boolean) return Boolean is
            Blocked : Boolean := False;
         begin
            
            -- Conditions to consider a pawn Backward (a weakness) are:
            -- 1) Other friendly pawns are already advanced
            -- 2) It is not supported
            -- 3) It can't advance freely without being captured
            
            Weak := False;
            
            if not Supported then            
               Weak := (F = File_A or else R > Pawn_Structure.Ranks (F - 1) or else Pawn_Structure.Files (F - 1) = 0)
                 and then (F = File_H or else R > Pawn_Structure.Ranks (F + 1) or else Pawn_Structure.Files (F + 1) = 0);
                  
               Blocked := Chessboard.Square (Square + South_South_West) = White_Pawn 
                 or else Chessboard.Square (Square + South_South_East) = White_Pawn;
            end if;
            
            return Weak and then Blocked;
         end Pawn_Is_Backward;
         
         --  function Square_Cannot_Be_Attacked_By_Opponent_Pawns (Square : in Square_Type) return Boolean is
         --     F : constant Coordinate_Type := File (Square);
         --     R : constant Coordinate_Type := Rank (Square);
         --  begin
         --     return (F = File_A or else R <= White_Pawn_Structure.Ranks (F - 1) or else White_Pawn_Structure.Files (F - 1) = 0)
         --       and then (F = File_H or else R <= White_Pawn_Structure.Ranks (F + 1) or else White_Pawn_Structure.Files (F + 1) = 0);
         --  end Square_Cannot_Be_Attacked_By_Opponent_Pawns;
   
   
         F     : Coordinate_Type;
         R     : Coordinate_Type;
   
         Doubled   : Boolean;
         Isolated  : Boolean;
         Isolani   : Boolean; -- Isolated queen pawn (on D file)
         Weak      : Boolean;
         Backward  : Boolean;
         Connected : Boolean;
         Supported : Boolean;
         Exposed   : Boolean; -- On_Semi_Open_File and not doubled
         Passed    : Boolean;
         Candidate : Boolean;
         Outpost   : Boolean;
   
         On_Semi_Open_File : Boolean;
   
         -- Outposts are square in front of a weak pawn. A weak pawn is a pawn
         -- that is either isolated or bacward. Exposed pawns are a weakness in
         -- the pawn structure, but they don't represent an outpost for opponent
         -- pieces since it does not have a strategical value
   
         Current_File_Has_Pawn : Boolean;
         Left_File_Has_Pawn : Boolean;
         Islands : Pawn_Islands_Counter := 0;
         
         Hanging : Boolean;
   
         Outpost_Score   : Tapered_Score_Type := Zero;
         Candidate_Score : Tapered_Score_Type := Zero;
         Passed_Score    : Tapered_Score_Type := Zero;
   
         Stop  : Square_Type;
   
      begin
         for Square of Pawn_Structure.Location loop
            exit when Square = Not_Present;
   
            Stop := Square + South;
   
            F := File (Square);
            R := Rank (Square);
   
            Doubled   := False;
            Isolated  := False;
            Isolani   := False;
            Weak      := False;
            Backward  := False;
            Exposed   := False;
            Connected := False;
            Supported := False;
            Passed    := False;
            Candidate := False;
            Outpost   := False;
   
            On_Semi_Open_File := False;
            
            Outpost_Score := Zero;
            
            Hanging := Pawn_Is_Hanging (Square) /= No_Direction;
            
            -- Supported --
   
            if F /= File_A and then Chessboard.Square (Square + North_West) = Black_Pawn then
               Supported := True;
            elsif F /= File_H and then Chessboard.Square (Square + North_East) = Black_Pawn then
               Supported := True;
            end if;
            
            -- Isolated --
   
            if (F = File_A or else Pawn_Structure.Files (F - 1) = 0)
              and then (F = File_H or else Pawn_Structure.Files (F + 1) = 0)
            then
               if F = File_D then
                  Isolani := True;
               else
                  Isolated := True;
               end if;
            else
               -- Backward --
               Backward := Pawn_Is_Backward (Square, F, R, Supported, Weak);
               Black_Pawn_Structure.Backwards (F) := Backward;
            end if;
   
            -- Connected --
            
            if Supported
              or else (F /= File_A and then Chessboard.Square (Square + West) = Black_Pawn)
              or else( F /= File_H and then Chessboard.Square (Square + East) = Black_Pawn)
            then
               Connected := True;
            end if;
                  
            -- Doubled --
   
            if not Supported and then Pawn_Structure.Files (F) > 1 then
               Doubled := True;
            end if;
   
            -- On_Semi_Open_File --
   
            --  if F in File_D | File_E then
               On_Semi_Open_File := Black_Pawn_Files (F) = 0;
            --  end if;
   
            -- Exposed --
   
            if not Supported then
               Exposed := Hanging or else On_Semi_Open_File; 
            end if;
   
            -- Passed --
   
            if (F = File_A or else White_Pawn_Files (F - 1) = 0 or else White_Pawn_Ranks (F - 1) >= R)
              and then (White_Pawn_Files (F) = 0 or else White_Pawn_Ranks (F) > R)
              and then (F = File_H or else White_Pawn_Files (F + 1) = 0 or else White_Pawn_Ranks (F + 1) >= R)
            then
               Passed := True;
               Passed_Score := Evaluate_Passed_Pawn (Square => Square, R => R, F => F, Connected => Connected);
               Black_Passed_Pawn (F) := R; -- If doubled, this info is inaccuarate
            end if;
   
            -- Candidate --
   
            if not Passed then
               if White_Pawn_Files (F) = 0 or else White_Pawn_Ranks (F) > R then
                  if (F = File_A or else White_Pawn_Files (F - 1) <= Black_Pawn_Files (F - 1))
                    and then (F = File_H or else White_Pawn_Files (F + 1) <= Black_Pawn_Files (F + 1)) then
                     Candidate := True;
                     Candidate_Score := Evaluate_Candidate_Passed_Pawn (R, Connected);
                  end if;
               end if;
            end if;
   
            -- Outposts --

            if Backward then
               Outpost := Chessboard.Square (Stop) in White_Non_Pawn;
               if Outpost then
                  Outpost_Score := Outpost_Occupancy_Penalty (Chessboard.Square (Stop));
               end if;
            end if;
            
            
            --  if F /= File_A and then Square_Cannot_Be_Attacked_By_Opponent_Pawns (Stop + West) then
            --     if Chessboard.Square (Stop + West) in Black_Piece_Type then
            --        Outpost := True;
            --        Outpost_Score := Outpost_Square_Occupancy (Chessboard.Square (Stop + West));
            --        --  Ada.Text_IO.Put_Line ("Outpost available on " & Pc_Sqr (Stop + West));
            --     end if;
            --  end if;
            --  
            --  if F /= File_H and then Square_Cannot_Be_Attacked_By_Opponent_Pawns (Stop + East) then
            --     if Chessboard.Square (Stop + East) in Black_Piece_Type then
            --        Outpost := True;
            --        Outpost_Score := Outpost_Score + Outpost_Square_Occupancy (Chessboard.Square (Stop + East));
            --        --  Ada.Text_IO.Put_Line ("Outpost available on " & Pc_Sqr (Stop + East));
            --     end if;
            --  end if;
            
            
            
   
            --  if Doubled then
            --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is DOUBLED");
            --  end if;
            --  if isolated or Isolani then
            --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is ISOLATED");
            --  end if;
            --  if Backward then
            --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is BACKWARD");
            --  end if;
            --  --  if Connected then
            --  --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is CONNECTED");
            --  --  end if;
            --  --  if Supported then
            --  --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is SUPPORTED");
            --  --  end if;
            --  if Exposed then
            --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is EXPOSED");
            --  end if;
            --  if Passed then
            --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is PASSED");
            --  end if;
            --  if Candidate then
            --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is CANDIDATE PASSER");
            --  end if;
            --  if On_Semi_Open_File then
            --     Ada.Text_IO.Put_Line ("Pawn on " & Pc_Sqr (Square) & " is On_Semi_Open_File");
            --  end if;
            --  if Outpost then
            --     Ada.Text_IO.Put_Line ("Square " & Pc_Sqr (Stop) & " is Outpost");
            --  end if;
            
            Score := Score
              + Black_Pawn_Position_Table (Square)
              + (if Doubled           then Doubled_Pawn_Penalty      else Zero)
              + (if Isolated          then Isolated_Pawn_Penalty     else Zero)
              + (if Isolani           then Isolani_Pawn_Penalty      else Zero)
              + (if Weak              then Weak_Pawn_Penalty         else Zero)
              + (if Backward          then Backward_Pawn_Penalty     else Zero)
              --  + (if On_Semi_Open_File then On_Semi_Open_File_Penalty else Zero)
              + (if Exposed           then Exposed_Pawn_Penalty      else Zero)
              --  + (if Connected         then Connected_Pawn_Bonus      else Zero)
              + (if Outpost           then Outpost_Score             else Zero)
              + (if Passed            then Passed_Score              else Zero)
              + (if Candidate         then Candidate_Score           else Zero)
              + (if Hanging           then Hanging_Pawn_Penalty      else Zero);
   
         end loop;
         
         
         --  Ada.Text_IO.Put_Line ("Black Weak square count is " & Natural'Image (Count_Black_Weak_Squares));
   
         -- Pawn Islands --
   
         Islands := 0;
   
         Left_File_Has_Pawn := False;
         for I in File_A .. File_H loop
            Current_File_Has_Pawn := Black_Pawn_Files (I) > 0;
            if Left_File_Has_Pawn and then not Current_File_Has_Pawn then
               Islands := Islands + 1;
            end if;
            Left_File_Has_Pawn := Current_File_Has_Pawn;
         end loop;
   
         if Black_Pawn_Files (File_H) > 0 then
            Islands := Islands + 1;
         end if;
   
         if Islands > 1 then
            Score := Score + Pawn_Island_Penalty_Factor * Islands;
         end if;
         
         --  Score (Opening) := Score (Opening) + Count_Black_Weak_Squares * Weak_Square_Penalty;
   
         return Tapered_Score (Score);
      end Evaluate_Black_Pawn_Structure;
   
   
      -------------------------
      -- Mobility Evaluation --
      -------------------------
   
      function Evaluate_White_Mobility return Score_Type is
         Score : Tapered_Score_Type := Zero;
         Piece : Piece_Type;
   
         Mobility : Score_Type := 0;
         Target   : Square_Type;
         Blocker  : Piece_Type;
         
         Rook_File : Coordinate_Type;
         Rook_Rank : Coordinate_Type;
   
         King_Position : Square_Type renames Chessboard.White_King_Position;
         Opponent_King : Square_Type renames Chessboard.Black_King_Position;
   
         Opponent_King_File : constant Coordinate_Type := File (Opponent_King);
         Opponent_King_Rank : constant Coordinate_Type := Rank (Opponent_King);
         File_Delta : Natural;
         
         Hanging_Direction : Direction_Type;
         
      begin
         
         for Square of White_Piece_Location loop
            exit when Square = Not_Present;
            
            Piece := Chessboard.Square (Square);
            
            
            case Piece is
               when White_Pawn | White_King => null;
                  -- Pawns and Kings are evaluated separately
   
               when White_Knight =>
                  
                  ------------------
                  -- White Knight --
                  ------------------
                  
                  Hanging_Direction := Piece_Is_Hanging (Square);
                  
                  Mobility := -Knight_Mobility_Penalty;
                  for Offset of Knight_Offsets loop
                     if Hanging_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        Blocker := Chessboard.Square (Target);
                        Mobility := Mobility + White_Mobility_Unit (Blocker);
                     end if;
                  end loop;
                  
                  for Phase in Game_Phase_Type loop
                     Score (Phase) := Score (Phase) + Mobility * Knight_Mobility_Bonus (Phase);
                  end loop;
               
                  Score (Opening) := Score (Opening) + Knight_Position_Table (Square);
               
               when White_Bishop =>
                  
                  ------------------
                  -- White Bishop --
                  ------------------
                  
                  Hanging_Direction := Piece_Is_Hanging (Square);
                  
                  Mobility := -Bishop_Mobility_Penalty;
                  for Offset of Bishop_Offsets loop
                     if Hanging_Direction in No_Direction | Offset | -Offset then
                        Target  := Square + Offset;
                        Blocker := Chessboard.Square (Target);
                        while Blocker in Empty | White_Queen loop -- X-Ray mobility through friendly Queen
                           Mobility := Mobility + White_Mobility_Unit (Blocker);
                           Target  := Target + Offset;
                           Blocker := Chessboard.Square (Target);
                        end loop;
                        Mobility := Mobility + White_Mobility_Unit (Blocker);
                     end if;
                  end loop;
               
                  for Phase in Game_Phase_Type loop
                     Score (Phase) := Score (Phase) + Mobility * Bishop_Mobility_Bonus (Phase);
                  end loop;
               
                  --  Score (Opening) := Score (Opening) + White_Bishop_Position_Table (Square);
               
               when White_Rook =>
                  
                  ----------------
                  -- White Rook --
                  ----------------
                  
                  Hanging_Direction := Piece_Is_Hanging (Square);
                  
                  Mobility := -Rook_Mobility_Penalty;
                  for Offset of Rook_Offsets loop
                     if Hanging_Direction in No_Direction | Offset | -Offset then
                        Target  := Square + Offset;
                        Blocker := Chessboard.Square (Target);
                        while Blocker in Empty | White_Rook loop -- X-Ray mobility through friendly Rook
                           Mobility := Mobility + White_Mobility_Unit (Blocker);
                           Target  := Target + Offset;
                           Blocker := Chessboard.Square (Target);
                        end loop;
                        Mobility := Mobility + White_Mobility_Unit (Blocker);
                     end if;
                  end loop;
               
                  for Phase in Game_Phase_Type loop
                     Score (Phase) := Score (Phase) + Mobility * Rook_Mobility_Bonus (Phase);
                  end loop;
               
                  -- Rook on (semi)open file --
               
                  Rook_File := File (Square);
                  Rook_Rank := Rank (Square);
               
                  if White_Pawn_Files (Rook_File) = 0 then
                     if Black_Pawn_Files (Rook_File) = 0 then
                        Score (Opening) := Score (Opening) + Rook_On_Open_File;
                     else
                        Score (Opening) := Score (Opening) + Rook_On_Semi_Open_File;
                        if Black_Pawn_Structure.Backwards (Rook_File) then
                           Score (Opening) := Score (Opening) + Rook_On_Semi_Open_With_Backward_Pawn;
                        end if;
                     end if;
                  end if;
               
                  -- Rook blocking opponent King --
               
                  if Endgame_Pattern = KR_K then
                     File_Delta := abs (Opponent_King_File - Rook_File);
                     if File_Delta <= 1 then
                        Score (Opening) := Score (Opening) + Rook_On_Semi_King_File;
                        if File_Delta = 0 then
                           Score (Opening) := Score (Opening) + Rook_On_King_File;
                        end if;
                     end if;
                  end if;
               
                  -- Rook on 7th rank --
               
                  if Rook_Rank = Rank_7 and then Opponent_King_Rank = Rank_8 then
                     for Phase in Game_Phase_Type loop
                        Score (Phase) := Score (Phase) + Rook_On_7th_Rank (Phase);
                     end loop;
                  end if;
               
                  -- Tarrash Rule --
               
                  if White_Passed_Pawn (Rook_File) > Rank_1 then
                     if Rook_Rank < White_Passed_Pawn (Rook_File) then
                        Score (End_Game) := Score (End_Game) + Tarrash_Rule;
                     end if;
                  end if;
                  if Black_Passed_Pawn (Rook_File) < Rank_8 then
                     if Rook_Rank < Black_Passed_Pawn (Rook_File) then
                        Score (End_Game) := Score (End_Game) + Tarrash_Rule;
                     end if;
                  end if;
                  
                  --  Score (Opening) := Score (Opening) + White_Rook_Position_Table (Square);
   
               when White_Queen =>
                  
                  -----------------
                  -- White Queen --
                  -----------------
                  
                  Hanging_Direction := Piece_Is_Hanging (Square);
                  
                  Mobility := -Queen_Mobility_Penalty;
                  for Offset of Queen_Offsets loop
                     if Hanging_Direction in No_Direction | Offset | -Offset then
                        Target  := Square + Offset;
                        Blocker := Chessboard.Square (Target);
                        while Blocker in Empty | White_Bishop loop -- X-Ray through friendly Bishop
                           Mobility := Mobility + White_Mobility_Unit (Blocker);
                           Target  := Target + Offset;
                           Blocker := Chessboard.Square (Target);
                        end loop;
                        Mobility := Mobility + White_Mobility_Unit (Blocker);
                     end if;
                  end loop;
               
                  for Phase in Game_Phase_Type loop
                     Score (Phase) := Score (Phase) + Mobility * Queen_Mobility_Bonus (Phase);
                  end loop;
               
                  --  Score (Opening) := Score (Opening) + White_Queen_Position_Table (Square);
               
                  if Rank (Square) = Rank_7 and then Opponent_King_Rank = Rank_8 then
                     for Phase in Game_Phase_Type loop
                        Score (Phase) := Score (Phase) + Queen_On_7th_Rank (Phase);
                     end loop;
                  end if;
  
               when others => null;
            end case;
   
         end loop;
         
         ----------------
         -- Fianchetto --
         ----------------
         
         if Chessboard.Square (G2) = White_Bishop then
            Score (Opening) := Score (Opening) + Fianchetto;
         end if;
         
         if Chessboard.Square (B2) = White_Bishop then
            Score (Opening) := Score (Opening) + Fianchetto;
         end if;
   
   
         --------------------
         -- Trapped Bishop --
         --------------------
   
         if Chessboard.Square (A7) = White_Bishop and then Chessboard.Square (B6) = Black_Pawn then
            for Phase in Game_Phase_Type loop
               Score (Phase) := Score (Phase) - Trapped_Bishop;
            end loop;
         elsif Chessboard.Square (B8) = White_Bishop and then Chessboard.Square (C7) = Black_Pawn then
            for Phase in Game_Phase_Type loop
               Score (Phase) := Score (Phase) - Trapped_Bishop;
            end loop;
         end if;
         
         if Chessboard.Square (H7) = White_Bishop and then Chessboard.Square (G6) = Black_Pawn then
            for Phase in Game_Phase_Type loop
               Score (Phase) := Score (Phase) - Trapped_Bishop;
            end loop;
         elsif Chessboard.Square (G8) = White_Bishop and then Chessboard.Square (F7) = Black_Pawn then
            for Phase in Game_Phase_Type loop
               Score (Phase) := Score (Phase) - Trapped_Bishop;
            end loop;
         end if;
         
         if Chessboard.Square (A6) = White_Bishop and then Chessboard.Square (B5) = Black_Pawn then
            for Phase in Game_Phase_Type loop
               Score (Phase) := Score (Phase) - Trapped_Bishop / 2;
            end loop;
         elsif Chessboard.Square (H6) = White_Bishop and then Chessboard.Square (G5) = Black_Pawn then
            for Phase in Game_Phase_Type loop
               Score (Phase) := Score (Phase) - Trapped_Bishop / 2;
            end loop;
         end if;
   
         --------------------
         -- Blocked bishop --
         --------------------
   
         if Chessboard.Square (C1) = White_Bishop and then Chessboard.Square (D2) = White_Pawn and then Chessboard.Square (D3) /= Empty then
            Score (Opening) := Score (Opening) - Blocked_Bishop;
         end if;
         
         if Chessboard.Square (F1) = White_Bishop and then Chessboard.Square (E2) = White_Pawn and then Chessboard.Square (E3) /= Empty then
            Score (Opening) := Score (Opening) - Blocked_Bishop;
         end if;
   
         ------------------
         -- Blocked Rook --
         ------------------
   
         if King_Position = C1 or else King_Position = B1 then
            if Chessboard.Square (A1) = White_Rook or else Chessboard.Square (A2) = White_Rook or else Chessboard.Square (B1) = White_Rook then
               Score (Opening) := Score (Opening) - Blocked_Rook;
            end if;
         end if;
         
         if King_Position = F1 or else King_Position = G1 then
            if Chessboard.Square (H1) = White_Rook or else Chessboard.Square (H2) = White_Rook or else Chessboard.Square (G1) = White_Rook then
               Score (Opening) := Score (Opening) - Blocked_Rook;
            end if;
         end if;
   
         ------------------------
         -- Exposing the Queen --
         ------------------------
   
         if Chessboard.Square (D1) /= White_Queen then
            if Chessboard.Square (B1) = White_Knight then
               Score (Opening) := Score (Opening) - Exposing_Queen;
            end if;
            if Chessboard.Square (C1) = White_Bishop then
               Score (Opening) := Score (Opening) - Exposing_Queen;
            end if;
            if Chessboard.Square (F1) = White_Bishop then
               Score (Opening) := Score (Opening) - Exposing_Queen;
            end if;
            if Chessboard.Square (G1) = White_Knight then
               Score (Opening) := Score (Opening) - Exposing_Queen;
            end if;
         end if;
   
         return Tapered_Score (Score);
      end Evaluate_White_Mobility;
   
   
      function Evaluate_Black_Mobility return Score_Type is
         Score : Tapered_Score_Type := Zero;
         Piece : Piece_Type;
   
         Mobility : Score_Type := 0;
         Target   : Square_Type;
         Blocker  : Piece_Type;
         
         Rook_File : Coordinate_Type;
         Rook_Rank : Coordinate_Type;
   
         King_Position : Square_Type renames Chessboard.Black_King_Position;
         Opponent_King : Square_Type renames Chessboard.White_King_Position;
   
         Opponent_King_File : constant Coordinate_Type := File (Opponent_King);
         Opponent_King_Rank : constant Coordinate_Type := Rank (Opponent_King);
         File_Delta : Natural;
         
         Hanging_Direction : Direction_Type;
         
      begin
         
         for Square of Black_Piece_Location loop
            exit when Square = Not_Present;
            
            Piece := Chessboard.Square (Square);
            
            case Piece is
               when Black_Pawn | Black_King => null;
                  -- Pawns and Kings are evaluated separately
   
               when Black_Knight =>
                  
                  ------------------
                  -- Black Knight --
                  ------------------
                  
                  Hanging_Direction := Piece_Is_Hanging (Square);
                  
                  Mobility := -Knight_Mobility_Penalty;
                  for Offset of Knight_Offsets loop
                     if Hanging_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        Blocker := Chessboard.Square (Target);
                        Mobility := Mobility + Black_Mobility_Unit (Blocker);
                     end if;
                  end loop;
               
                  for Phase in Game_Phase_Type loop
                     Score (Phase) := Score (Phase) + Mobility * Knight_Mobility_Bonus (Phase);
                  end loop;
               
                  Score (Opening) := Score (Opening) + Knight_Position_Table (Square);
               
               when Black_Bishop =>
                  
                  ------------------
                  -- Black Bishop --
                  ------------------
                  
                  Hanging_Direction := Piece_Is_Hanging (Square);
                  
                  Mobility := -Bishop_Mobility_Penalty;
                  for Offset of Bishop_Offsets loop
                     if Hanging_Direction in No_Direction | Offset | -Offset then
                        Target  := Square + Offset;
                        Blocker := Chessboard.Square (Target);
                        while Blocker in Empty | Black_Queen loop -- X-Ray mobility through friendly Queen
                           Mobility := Mobility + Black_Mobility_Unit (Blocker);
                           Target  := Target + Offset;
                           Blocker := Chessboard.Square (Target);
                        end loop;
                        Mobility := Mobility + Black_Mobility_Unit (Blocker);
                     end if;
                  end loop;
               
                  for Phase in Game_Phase_Type loop
                     Score (Phase) := Score (Phase) + Mobility * Bishop_Mobility_Bonus (Phase);
                  end loop;
               
                  --  Score (Opening) := Score (Opening) + Black_Bishop_Position_Table (Square);
               
               when Black_Rook =>
                  
                  ------------------
                  -- Black Rook --
                  ------------------
                  
                  Hanging_Direction := Piece_Is_Hanging (Square);
                  
                  Mobility := -Rook_Mobility_Penalty;
                  for Offset of Rook_Offsets loop
                     if Hanging_Direction in No_Direction | Offset | -Offset then
                     Target  := Square + Offset;
                     Blocker := Chessboard.Square (Target);
                     while Blocker in Empty | Black_Rook loop -- X-Ray mobility through friendly Rook
                        Mobility := Mobility + Black_Mobility_Unit (Blocker);
                        Target  := Target + Offset;
                        Blocker := Chessboard.Square (Target);
                     end loop;
                     Mobility := Mobility + Black_Mobility_Unit (Blocker);
                  end if;
                  end loop;
            
                  for Phase in Game_Phase_Type loop
                     Score (Phase) := Score (Phase) + Mobility * Rook_Mobility_Bonus (Phase);
                  end loop;
               
                  -- Rook on (semi)open file --
               
                  Rook_File := File (Square);
                  Rook_Rank := Rank (Square);
               
                  if Black_Pawn_Files (Rook_File) = 0 then
                     if White_Pawn_Files (Rook_File) = 0 then
                        Score (Opening) := Score (Opening) + Rook_On_Open_File;
                     else
                        Score (Opening) := Score (Opening) + Rook_On_Semi_Open_File;
                        if White_Pawn_Structure.Backwards (Rook_File) then
                           Score (Opening) := Score (Opening) + Rook_On_Semi_Open_With_Backward_Pawn;
                        end if;
                     end if;
                  end if;
               
                  -- Rook blocking opponent king --
               
                  if Endgame_Pattern = K_KR then
                     File_Delta := abs (Opponent_King_File - Rook_File);
                     if File_Delta <= 1 then
                        Score (Opening) := Score (Opening) + Rook_On_Semi_King_File;
                        if File_Delta = 0 then
                           Score (Opening) := Score (Opening) + Rook_On_King_File;
                        end if;
                     end if;
                  end if;
               
                  -- Rook on 2nd rank --
               
                  if Rook_Rank = Rank_2 and then Opponent_King_Rank = Rank_1 then
                     for Phase in Game_Phase_Type loop
                        Score (Phase) := Score (Phase) + Rook_On_2nd_Rank (Phase);
                     end loop;
                  end if;
               
                  -- Tarrash Rule --
               
                  if Black_Passed_Pawn (Rook_File) < Rank_8 then
                     if Rook_Rank > Black_Passed_Pawn (Rook_File) then
                        Score (End_Game) := Score (End_Game) + Tarrash_Rule;
                     end if;
                  end if;
                  if White_Passed_Pawn (Rook_File) > Rank_1 then
                     if Rook_Rank > White_Passed_Pawn (Rook_File) then
                        Score (End_Game) := Score (End_Game) + Tarrash_Rule;
                     end if;
                  end if;
                
                  --  Score (Opening) := Score (Opening) + Black_Rook_Position_Table (Square);
   
               when Black_Queen =>
                  
                  -----------------
                  -- Black Queen --
                  -----------------
                  
                  Hanging_Direction := Piece_Is_Hanging (Square);
                  
                  Mobility := -Queen_Mobility_Penalty;
                  for Offset of Queen_Offsets loop
                     if Hanging_Direction in No_Direction | Offset | -Offset then
                        Target  := Square + Offset;
                        Blocker := Chessboard.Square (Target);
                        while Blocker in Empty | Black_Bishop loop -- X-Ray through friendly Bishop
                           Mobility := Mobility + Black_Mobility_Unit (Blocker);
                           Target  := Target + Offset;
                           Blocker := Chessboard.Square (Target);
                        end loop;
                        Mobility := Mobility + Black_Mobility_Unit (Blocker);
                     end if;
                  end loop;
               
                  for Phase in Game_Phase_Type loop
                     Score (Phase) := Score (Phase) + Mobility * Queen_Mobility_Bonus (Phase);
                  end loop;
               
                  --  Score (Opening) := Score (Opening) + Black_Queen_Position_Table (Square);
               
                  if Rank (Square) = Rank_2 and then Opponent_King_Rank = Rank_1 then
                     for Phase in Game_Phase_Type loop
                        Score (Phase) := Score (Phase) + Queen_On_2nd_Rank (Phase);
                     end loop;
                  end if;
   
               when others => null;
            end case;
   
         end loop;
         
         ----------------
         -- Fianchetto --
         ----------------
         
         if Chessboard.Square (G7) = Black_Bishop then
            Score (Opening) := Score (Opening) + Fianchetto;
         end if;
         
         if Chessboard.Square (B7) = Black_Bishop then
            Score (Opening) := Score (Opening) + Fianchetto;
         end if;
   
   
         --------------------
         -- Trapped Bishop --
         --------------------
   
         if Chessboard.Square (A2) = Black_Bishop and then Chessboard.Square (B3) = White_Pawn then
            for Phase in Game_Phase_Type loop
               Score (Phase) := Score (Phase) - Trapped_Bishop;
            end loop;
         elsif Chessboard.Square (B1) = Black_Bishop and then Chessboard.Square (C2) = White_Pawn then
            for Phase in Game_Phase_Type loop
               Score (Phase) := Score (Phase) - Trapped_Bishop;
            end loop;
         end if;
         
         if Chessboard.Square (H2) = Black_Bishop and then Chessboard.Square (G3) = White_Pawn then
            for Phase in Game_Phase_Type loop
               Score (Phase) := Score (Phase) - Trapped_Bishop;
            end loop;
         elsif Chessboard.Square (G1) = Black_Bishop and then Chessboard.Square (F2) = White_Pawn then
            for Phase in Game_Phase_Type loop
               Score (Phase) := Score (Phase) - Trapped_Bishop;
            end loop;
         end if;
         
         if Chessboard.Square (A3) = Black_Bishop and then Chessboard.Square (B4) = White_Pawn then
            for Phase in Game_Phase_Type loop
               Score (Phase) := Score (Phase) - Trapped_Bishop / 2;
            end loop;
         elsif Chessboard.Square (H3) = Black_Bishop and then Chessboard.Square (G4) = White_Pawn then
            for Phase in Game_Phase_Type loop
               Score (Phase) := Score (Phase) - Trapped_Bishop / 2;
            end loop;
         end if;
   
         --------------------
         -- Blocked bishop --
         --------------------
   
         if Chessboard.Square (C8) = Black_Bishop and then Chessboard.Square (D7) = Black_Pawn and then Chessboard.Square (D6) /= Empty then
            Score (Opening) := Score (Opening) - Blocked_Bishop;
         end if;
         
         if Chessboard.Square (F8) = Black_Bishop and then Chessboard.Square (E7) = Black_Pawn and then Chessboard.Square (E6) /= Empty then
            Score (Opening) := Score (Opening) - Blocked_Bishop;
         end if;
   
         ------------------
         -- Blocked Rook --
         ------------------
   
         if King_Position = C8 or else King_Position = B8 then
            if Chessboard.Square (A8) = Black_Rook or else Chessboard.Square (A7) = Black_Rook or else Chessboard.Square (B8) = Black_Rook then
               Score (Opening) := Score (Opening) - Blocked_Rook;
            end if;
         end if;
         
         if King_Position = F8 or else King_Position = G8 then
            if Chessboard.Square (H8) = Black_Rook or else Chessboard.Square (H7) = Black_Rook or else Chessboard.Square (G8) = Black_Rook then
               Score (Opening) := Score (Opening) - Blocked_Rook;
            end if;
         end if;
   
         ------------------------
         -- Exposing the Queen --
         ------------------------
   
         if Chessboard.Square (D8) /= Black_Queen then
            if Chessboard.Square (B8) = Black_Knight then
               Score (Opening) := Score (Opening) - Exposing_Queen;
            end if;
            if Chessboard.Square (C8) = Black_Bishop then
               Score (Opening) := Score (Opening) - Exposing_Queen;
            end if;
            if Chessboard.Square (F8) = Black_Bishop then
               Score (Opening) := Score (Opening) - Exposing_Queen;
            end if;
            if Chessboard.Square (G8) = Black_Knight then
               Score (Opening) := Score (Opening) - Exposing_Queen;
            end if;
         end if;
   
   
         return Tapered_Score (Score);
      end Evaluate_Black_Mobility;
   
   
   
      ----------------------------
      -- King Safety evaluation --
      ----------------------------
   
      function Evaluate_White_King_Safety return Score_Type is
         Score : Tapered_Score_Type := Zero;
   
         function Shelter (Square : Square_Type) return Score_Type is
            Stop     : constant Square_Type := Square + North;
            Telestop : constant Square_Type := Stop + North;
         begin
            if Chessboard.Square (Square) = White_Pawn then
               return Pawn_Shelter;
            elsif Chessboard.Square (Stop) = White_Pawn then
               return Pawn_Shelter_Penalty;
            elsif Chessboard.Square (Telestop) = White_Pawn then
               return Pawn_Shelter_Defuse;
            end if;
            -- If the pawn is not present anymore, we don't provide any negative
            -- score because there will be an open file near or in front of the
            -- king.
            return 0;
         end Shelter;
   
   
         function Storm (F : Coordinate_Type) return Score_Type is
            R : constant Coordinate_Type := Black_Pawn_Ranks (F);
         begin
            case R is
               when Rank_2 | Rank_3 =>
                  return Pawn_Storm_Severe_Threat;
               when Rank_4 =>
                  return Pawn_Storm_Threat;
               when Rank_5 =>
                  return Pawn_Storm_Alert;
               when others =>
                  return 0;
            end case;
         end Storm;
   
   
         King_Position : Square_Type renames Chessboard.White_King_Position;
         F     : constant Coordinate_Type := File (King_Position);
         
         Profilactic : Boolean := False;
   
      begin
   
         Score (Opening) := White_King_Position_Table (King_Position);
   
         -- Pawn Shelter --
   
         if F in File_A | File_B | File_C then
            Score (Opening) := Score (Opening)
              + Shelter (A2) + Shelter (B2) + Shelter (C2);
         elsif F in File_F | File_G | File_H then
            Score (Opening) := Score (Opening)
              + Shelter (F2) + Shelter (G2) + Shelter (H2);
         --  elsif F in File_D | File_E then
         --     Score (Opening) := Score (Opening) - Castle_Not_Performed;
         end if;
   
         -- Open file near king --
   
         if F /= File_A and then White_Pawn_Files (F - 1) = 0 then
            Score (Opening) := Score (Opening) - Open_File_Near_King;
         end if;
         if White_Pawn_Files (F) = 0 then
            Score (Opening) := Score (Opening) - Open_File_In_Front_Of_King;
         end if;
         if F /= File_H and then White_Pawn_Files (F + 1) = 0 then
            Score (Opening) := Score (Opening) - Open_File_Near_King;
         end if;
   
         -- Semi-Open file near king --
   
         if F /= File_A and then Black_Pawn_Files (F - 1) = 0 then
            Score (Opening) := Score (Opening) - Semi_Open_File_Near_King;
         end if;
         if Black_Pawn_Files (F) = 0 then
            Score (Opening) := Score (Opening) - Semi_Open_File_In_Front_Of_King;
         end if;
         if F /= File_H and then Black_Pawn_Files (F + 1) = 0 then
            Score (Opening) := Score (Opening) - Semi_Open_File_Near_King;
         end if;
   
         -- Pawn Storm --
   
         if F in File_A | File_B | File_C then
            Score (Opening) := Score (Opening)
              - Storm (File_A) - Storm (File_B) - Storm (File_C);
         elsif F in File_F | File_G | File_H then
            Score (Opening) := Score (Opening)
              - Storm (File_F) - Storm (File_G) - Storm (File_H);
         --  elsif F in File_D | File_E then
         --     Score (Opening) := Score (Opening)
         --       - Storm (File_C) - Storm (File_D) - Storm (File_E) - Storm (File_F);
         end if;
         
         ----------------------
         -- Profilactic move --
         ----------------------
         
         for Square of Black_Piece_Location loop
            exit when Square = Not_Present;
            if Chessboard.Square(Square) not in Black_Pawn | Black_King then
               Profilactic := Find_Sliding_Direction
                 (Origin      => Square,
                  Destination => King_Position) /= No_Direction;
            end if;
            exit when Profilactic;
         end loop;
         
         if Profilactic then
            Score (Opening) := Score (Opening) - Profilactic_Move_Penalty;
         end if;
   
         -----------------------------
         -- Attacking the King Zone --
         -----------------------------
   
         -- See: https://www.chessprogramming.org/King_Safety
         Score (Opening) := Score (Opening)
           - Attacks_King_Zone (Side => White);
   
         -------------
         -- Endgame --
         -------------
   
         Score (End_Game) := King_End_Game_Position_Table (King_Position);
   
         return Tapered_Score (Score);
      end Evaluate_White_King_Safety;
   
   
      function Evaluate_Black_King_Safety return Score_Type is
         Score : Tapered_Score_Type := Zero;
   
         function Shelter (Square : Square_Type) return Score_Type is
            Stop     : constant Square_Type := Square + South;
            Telestop : constant Square_Type := Stop + South;
         begin
            if Chessboard.Square (Square) = Black_Pawn then
               return Pawn_Shelter;
            elsif Chessboard.Square (Stop) = Black_Pawn then
               return Pawn_Shelter_Penalty;
            elsif Chessboard.Square (Telestop) = Black_Pawn then
               return Pawn_Shelter_Defuse;
            end if;
            -- If the pawn is not present anymore, we don't provide any negative
            -- score because there will be an open file near or in front of the
            -- king.
            return 0;
         end Shelter;
   
   
         function Storm (F : Coordinate_Type) return Score_Type is
            R : constant Coordinate_Type := White_Pawn_Ranks (F);
         begin
            case R is
               when Rank_7 | Rank_6 =>
                  return Pawn_Storm_Severe_Threat;
               when Rank_5 =>
                  return Pawn_Storm_Threat;
               when Rank_4 =>
                  return Pawn_Storm_Alert;
               when others =>
                  return 0;
            end case;
         end Storm;
   
   
         King_Position : Square_Type renames Chessboard.Black_King_Position;
         F     : constant Coordinate_Type := File (King_Position);
   
         Profilactic : Boolean := False;
         
      begin
   
         Score (Opening) := Black_King_Position_Table (King_Position);
   
         -- Pawn Shelter --
   
         if F in File_A | File_B | File_C then
            Score (Opening) := Score (Opening)
              + Shelter (A7) + Shelter (B7) + Shelter (C7);
         elsif F in File_F | File_G | File_H then
            Score (Opening) := Score (Opening)
              + Shelter (F7) + Shelter (G7) + Shelter (H7);
         --  elsif F in File_D | File_E then
         --     Score (Opening) := Score (Opening) - Castle_Not_Performed;
         end if;
   
         -- Open file near king --
   
         if F /= File_A and then Black_Pawn_Files (F - 1) = 0 then
            Score (Opening) := Score (Opening) - Open_File_Near_King;
         end if;
         if Black_Pawn_Files (F) = 0 then
            Score (Opening) := Score (Opening) - Open_File_In_Front_Of_King;
         end if;
         if F /= File_H and then Black_Pawn_Files (F + 1) = 0 then
            Score (Opening) := Score (Opening) - Open_File_Near_King;
         end if;
   
         -- Semi-Open file near king --
   
         if F /= File_A and then White_Pawn_Files (F - 1) = 0 then
            Score (Opening) := Score (Opening) - Semi_Open_File_Near_King;
         end if;
         if White_Pawn_Files (F) = 0 then
            Score (Opening) := Score (Opening) - Semi_Open_File_In_Front_Of_King;
         end if;
         if F /= File_H and then White_Pawn_Files (F + 1) = 0 then
            Score (Opening) := Score (Opening) - Semi_Open_File_Near_King;
         end if;
   
         -- Pawn Storm --
   
         if F in File_A | File_B | File_C then
            Score (Opening) := Score (Opening)
              - Storm (File_A) - Storm (File_B) - Storm (File_C);
         elsif F in File_F | File_G | File_H then
            Score (Opening) := Score (Opening)
              - Storm (File_F) - Storm (File_G) - Storm (File_H);
         --  elsif F in File_D | File_E then
         --     Score (Opening) := Score (Opening)
         --       - Storm (File_C) - Storm (File_D) - Storm (File_E) - Storm (File_F);
         end if;
         
         ----------------------
         -- Profilactic move --
         ----------------------
         
         for Square of White_Piece_Location loop
            exit when Square = Not_Present;
            if Chessboard.Square (Square) not in White_Pawn | White_King then
               Profilactic := Find_Sliding_Direction
                 (Origin      => Square,
                  Destination => King_Position) /= No_Direction;
            end if;
            exit when Profilactic;
         end loop;
         
         if Profilactic then
            Score (Opening) := Score (Opening) - Profilactic_Move_Penalty;
         end if;
   
         -----------------------------
         -- Attacking the King Zone --
         -----------------------------
   
         -- See: https://www.chessprogramming.org/King_Safety
         Score (Opening) := Score (Opening)
           - Attacks_King_Zone (Side => Black);
   
         -------------
         -- Endgame --
         -------------
   
         Score (End_Game) := King_End_Game_Position_Table (King_Position);
   
         return Tapered_Score (Score);
      end Evaluate_Black_King_Safety;
   
   
      White_Material        : Score_Type := 0;
      Black_Material        : Score_Type := 0;
      White_Pawns_Structure : Score_Type := 0;
      Black_Pawns_Structure : Score_Type := 0;
      White_Mobility        : Score_Type := 0;
      Black_Mobility        : Score_Type := 0;
      White_King_Safety     : Score_Type := 0;
      Black_King_Safety     : Score_Type := 0;
   
      White_Score  : Score_Type := 0;
      Black_Score  : Score_Type := 0;
   
   
      Bias : Random_Score_Type := 0; -- used in Random mode only
      Final_Score  : Evaluation_Type;
   
   begin
   
      Collect_Information;
   
      Endgame_Pattern := Detect_Endgame_Pattern;
   
      if Insufficient_Material then
         Final_Score := Evaluation_Type'
           (Score      => 0,
            Game_Phase => Draw_By_Insufficient_Material);
         return Final_Score;
      end if;
   
   
      -- Calculate game phase
      Game_Phase_Block : declare
         Pawn_Phase   : Score_Type := 0;
         Knight_Phase : Score_Type := 0;
         Bishop_Phase : Score_Type := 0;
         Rook_Phase   : Score_Type := 0;
         Queen_Phase  : Score_Type := 0;
         Phase        : Score_Type := 0;
      begin
         Pawn_Phase   := 0 * (Material (White_Pawn)   + Material (Black_Pawn));
         Knight_Phase := 4 * (Material (White_Knight) + Material (Black_Knight));
         Bishop_Phase := 4 * (Material (White_Bishop) + Material (Black_Bishop));
         Rook_Phase   := 9 * (Material (White_Rook)   + Material (Black_Rook));
         Queen_Phase  := 16 * (Material (White_Queen) + Material (Black_Queen));
   
         Phase := 0
           + Pawn_Phase
           + Knight_Phase
           + Bishop_Phase
           + Rook_Phase
           + Queen_Phase;
   
         if Phase > 100 then
            Phase := 100;
         end if;
   
         Game_Phase := Phase;
      end Game_Phase_Block;
   
   
      White_Material := Evaluate_White_Material;
      Black_Material := Evaluate_Black_Material;
   
      White_Pawns_Structure := Evaluate_White_Pawn_Structure;
      Black_Pawns_Structure := Evaluate_Black_Pawn_Structure;
   
      White_Mobility := Evaluate_White_Mobility;
      Black_Mobility := Evaluate_Black_Mobility;
   
      White_King_Safety := Evaluate_White_King_Safety;
      Black_King_Safety := Evaluate_Black_King_Safety;
      
      --  Ada.Text_IO.Put_Line ("White Material : " & Score_Type'Image (White_Material));
      --  Ada.Text_IO.Put_Line ("Black Material : " & Score_Type'Image (Black_Material));
      --  Ada.Text_IO.Put_Line ("Diff. Material : " & Score_Type'Image (White_Material - Black_Material));
      --  
      --  Ada.Text_IO.Put_Line ("White Pawn Structure : " & Score_Type'Image (White_Pawns_Structure));
      --  Ada.Text_IO.Put_Line ("Black Pawn Structure : " & Score_Type'Image (Black_Pawns_Structure));
      --  Ada.Text_IO.Put_Line ("Diff. Pawn Structure : " & Score_Type'Image (White_Pawns_Structure - Black_Pawns_Structure));
      --  
      --  Ada.Text_IO.Put_Line ("White Mobility : " & Score_Type'Image (White_Mobility));
      --  Ada.Text_IO.Put_Line ("Black Mobility : " & Score_Type'Image (Black_Mobility));
      --  Ada.Text_IO.Put_Line ("Diff. Mobility : " & Score_Type'Image (White_Mobility - Black_Mobility));
      --  
      --  Ada.Text_IO.Put_Line ("White Safety : " & Score_Type'Image (White_King_Safety));
      --  Ada.Text_IO.Put_Line ("Black Safety : " & Score_Type'Image (Black_King_Safety));
      --  Ada.Text_IO.Put_Line ("Diff. Safety : " & Score_Type'Image (White_King_Safety - Black_King_Safety));
   
   
      White_Score := 0
        + White_Material
        + White_Pawns_Structure
        + White_Mobility
        + White_King_Safety;
   
      Black_Score := 0
        + Black_Material
        + Black_Pawns_Structure
        + Black_Mobility
        + Black_King_Safety;
      
      if Chessboard.Side_To_Move = White then
         White_Score := White_Score + Tempo_Bonus;
      else
         Black_Score := Black_Score + Tempo_Bonus;
      end if;
   
   
      -- Add a bias to the score if the random mode is activated. Note that the
      -- bias is given only with the purpose of vary the style between one game
      -- and another, especially when no opening book is used.
      -- The bias is also affected by a tapered evaluation to reduce the impact
      -- in endgames and, in general, the more the game advance the less we want
      -- the bias to influence the evaluation.
      if Random_Mode then
         Bias := Tapered_Score
           (Score =>
              (Opening => Score_Random.Random (Score_Seed), End_Game => 0));
      end if;
   
      --  White_Score := White_Score / Endgame_Score_Adapter (White);
      --  Black_Score := Black_Score / Endgame_Score_Adapter (Black);
   
      if Chessboard.Side_To_Move = White then
         Final_Score := Evaluation_Type'
           (Score      => White_Score - Black_Score + Bias,
            Game_Phase => In_Progress);
      else
         Final_Score := Evaluation_Type'
           (Score      => Black_Score - White_Score + Bias,
            Game_Phase => In_Progress);
      end if;
   
   
      return Final_Score;
   
   end Evaluate;
   
   
   
   --------------------------------
   -- Fill_Knight_Distance_Table --
   --------------------------------

   procedure Fill_Knight_Distance_Table is

      type Visited_Status is (Not_Visited, In_Visit, Visit_Complete)
        with
          Size => 2;
      
      type Visited_Square_Type is
        array (Board_Type'Range, Board_Type'Range) of Visited_Status;
      
      Visited : Visited_Square_Type := (others => (others => Not_Visited));

      -- Dijkstra --
      function Dijkstra (From, To : in Square_Type) return Distance_Type is
         Target            : Square_Type   := 0;
         Shortest_Distance : Distance_Type := Unreachable;
         D                 : Distance_Type := No_Distance;
      begin

         Visited (From, From)               := Visit_Complete;
         Knight_Distance_Table (From, From) := No_Distance;

         if From = To then
            return No_Distance;
         elsif Visited (From, To) = Visit_Complete then
            return Knight_Distance_Table (From, To);
         elsif Visited (To, From) = Visit_Complete then
            return Knight_Distance_Table (To, From);
         end if;

         -- Set up neighborhood first: all adjacent squares - in knight moves
         -- meaning - can be reached in a single move. This will make the
         -- search faster, because distances are very short in
         for Offset of Knight_Offsets loop
            Target := From + Offset;
            if not Square_Is_Frame (Target) then
               Visited (From, Target)               := Visit_Complete;
               Knight_Distance_Table (From, Target) := 1;
            end if;
         end loop;

         Visited (From, To) := In_Visit;

         for Offset of Knight_Offsets loop
            Target := From + Offset;
            if not Square_Is_Frame (Target) then
               case Visited (Target, To) is
                  when Visit_Complete =>
                     D := 1 + Knight_Distance_Table (Target, To);
                  when In_Visit =>
                     D := Unreachable - 1; -- Skip this path, already searching
                  when Not_Visited =>
                     D := 1 + Dijkstra (Target, To);
                     Visited (Target, To) := Visit_Complete;
               end case;
               if D < Shortest_Distance then
                  Shortest_Distance := D;
               end if;
            end if;
         end loop;

         Knight_Distance_Table (From, To) := Shortest_Distance;

         return Shortest_Distance;

      end Dijkstra;

   begin

      Knight_Distance_Table := (others => (others => Unreachable));

      for Square in Board_Type'Range loop
         if not Square_Is_Frame (Square) then
            for Target in Board_Type'Range loop
               if not Square_Is_Frame (Target) then
                  Visited := (others => (others => Not_Visited));
                  Knight_Distance_Table (Square, Target) :=
                    Dijkstra (Square, Target);
               end if;
            end loop;
         end if;
      end loop;

   end Fill_Knight_Distance_Table;
   
   
   
   --------------
   -- Distance --
   --------------

   function Distance
     (Chessboard     : in Chessboard_Type; Piece : in Piece_Type;
      Square, Target : in Square_Type) return Distance_Type
   is

      Side_To_Move : Color_Type renames Chessboard.Side_To_Move;
      Gain_Tempo   : Boolean := False;

   begin

      case Side_To_Move is
         when White =>
            Gain_Tempo := Piece in Black_Piece_Type;
         when Black =>
            Gain_Tempo := Piece in White_Piece_Type;
      end case;

      return Raw_Distance (Piece, Square, Target)
        + (if Gain_Tempo then 1 else 0);
   end Distance;
   

   ------------------
   -- Raw_Distance --
   ------------------

   function Raw_Distance
     (Piece : in Piece_Type; Square, Target : in Square_Type)
      return Distance_Type
   is

      function Max (V1, V2 : in Coordinate_Type) return Coordinate_Type is
        (if V1 > V2 then V1 else V2);

      X1 : constant Coordinate_Type := Rank (Square);
      Y1 : constant Coordinate_Type := File (Square);
      X2 : constant Coordinate_Type := Rank (Target);
      Y2 : constant Coordinate_Type := File (Target);

   begin

      pragma Assert
        (not Square_Is_Frame (Square),
         "Raw Distance calculation from FRAME is not allowed");
      
      pragma Assert
        (not Square_Is_Frame (Target),
         "Raw Distance calculation targeting FRAME is not allowed");

      pragma Assert
        (Piece not in Frame | Empty,
         "Raw Distance called with Frame | Empty piece not allowed!");

      case Piece is
         when King_Type =>
            return Distance_Type (Max ((abs (X1 - X2)), (abs (Y1 - Y2))));

         when White_Pawn =>
            return
              (if Y1 = Y2 and then Square > Target then Distance_Type (X2 - X1)
               else Unreachable);

         when Black_Pawn =>
            return
              (if Y1 = Y2 and then Square < Target then Distance_Type (X1 - X2)
               else Unreachable);

         when Bishop_Type =>
            if Color_Board (Square) = Color_Board (Target) then
               if Diagonal (Square) = Diagonal (Target)
                 or else Anti_Diagonal (Square) = Anti_Diagonal (Target)
               then
                  return 1;
               else
                  return 2;
               end if;
            else
               return Unreachable;
            end if;

         when Rook_Type =>
            if X1 = X2 or else Y1 = Y2 then
               return 1;
            else
               return 2;
            end if;

         when Queen_Type =>
            if X1 = X2 or else Y1 = Y2
              or else Diagonal (Square) = Diagonal (Target)
              or else Anti_Diagonal (Square) = Anti_Diagonal (Target)
            then
               return 1;
            else
               return 2;
            end if;

         when Knight_Type =>
            return Knight_Distance_Table (Square, Target);

         when others =>
            null;
      end case;

      raise Invalid_Distance_Calculation;
   end Raw_Distance;

   
   ----------
   -- Race --
   ----------

   function Race
     (Chessboard      : in Chessboard_Type; Piece : in Piece_Type;
      Square          : in Square_Type; Opponent : in Piece_Type;
      Opponent_Square : in Square_Type; Target : in Square_Type)
      return Distance_Match_Type
   is
      D1 : constant Distance_Type :=
        Distance (Chessboard, Piece, Square, Target);
      D2 : constant Distance_Type :=
        Distance (Chessboard, Opponent, Opponent_Square, Target);

      subtype Reachable is Distance_Type range 0 .. 8;
      -- If a square is reachable, then the maximun number of steps is 8

   begin

      case D1 is
         -- Piece can't reach target square? Then look if opponent can do!
         when Unreachable =>
            if D2 = Unreachable then
               return Not_Reachable;
            else
               return Loser;
            end if;

         when Reachable =>
            -- Opponent has no way? Then it's a sure win!
            if D2 = Unreachable then
               return Winner;
            end if;

            -- Otherwise.. the winner is the faster!
            if D1 = D2 then
               return Equals;
            elsif D1 < D2 then
               return Closer;
            else
               return Farther;
            end if;
      end case;

   end Race;
   
  
end Chess.Engine.Evaluations.Static_Evaluations;
