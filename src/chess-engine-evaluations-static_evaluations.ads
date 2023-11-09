--
--  AdaChess - Smart Chess Engine
--
--  Copyright (C) 2013-2022 - Alessandro Iavicoli
--  Email: adachess@gmail.com - Web Page: https://www.adachess.com
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


with Chess;
with Chess.Distances; use Chess.Distances;


package Chess.Engine.Evaluations.Static_Evaluations is

   -----------------
   -- Evaluations --
   -----------------

   procedure Initialize_Evaluation_Engine;
   -- Call this procedure once at startup to prepare the evaluation engine.
   -- Every operation that can be pre-calculated 
   

   function Evaluate (Chessboard : in Chessboard_Type) return Evaluation_Type;
   -- Statically evaluate the current chessboard. The evaluation takes care of
   -- all the main factors for both sides: material balance, pawn structure and
   -- passed pawns, piece mobility, king safety, rook on (semi)open file.
   -- Some pattern hard to be detected via search are also taken into account,
   -- as trapped rook/bishops and specific endgames
   --
   -- Arguments
   --    Chessboard : The current chess state
   -- Returns
   --    The evaluation given from the side-to-move perspective
   
   
private
   
   function Is_White_Pawn_Protected (Chessboard : in Chessboard_Type; Square : in Square_Type) return Boolean
     with
       Inline => True; 
   
   function Is_Black_Pawn_Protected (Chessboard : in Chessboard_Type; Square : in Square_Type) return Boolean
     with
       Inline => True; 
   
  
   ---------------
   -- Material  --
   ---------------
   
   Pawn_Score   : constant Tapered_Score_Type := (Opening => 100, End_Game => 105);
   Knight_Score : constant Tapered_Score_Type := (Opening => 325, End_Game => 325);
   Bishop_Score : constant Tapered_Score_Type := (Opening => 330, End_Game => 330);
   Rook_Score   : constant Tapered_Score_Type := (Opening => 550, End_Game => 550);
   Queen_Score  : constant Tapered_Score_Type := (Opening => 990, End_Game => 990);

   type Piece_Location_Type is array (Piece_Counter_Type range 1 .. 16) of Square_Type
     with 
       Default_Component_Value => 0;
   
   type Material_Type is array (Piece_Type'Range) of Piece_Counter_Type
     with 
       Default_Component_Value => 0;
   
   --------------
   -- Patterns --
   --------------
   
   type Endgame_Pattern_Type is
     (KK,    -- King vs King
      
      -- Symmetric sicuation
      KP_KP,  -- King + Pawn(s) vs King + Pawn(s)
      KQ_KQ,  -- King + Queen vs King + Queen
      KR_KR,  -- King + Rook vs King + Rook
      KB_KB,  -- King + Bishop vs King + Bishop
      KN_KN,  -- King + Knight vs King + Knight
      
      -- White persepctive
      KQ_K,   -- King + Queen vs King
      KR_K,   -- King + Rook vs King
      KB_K,   -- King + Bishop vs King
      KN_K,   -- King + Knight vs King
      KP_K,   -- King + Pawn vs King 
      KQ_KP,  -- King + Queen vs King + Pawn
      KR_KP,  -- King + Rook vs King + Pawn
      KB_KP,  -- King + Bishop vs King + Pawn
      KN_KP,  -- King + Knight vs King + Pawn
      KBP_K,  -- King + Bishop + Pawn vs King
      KNP_K,  -- King + Knight + Pawn vs King
      KRP_KR, -- King + Rook + Pawn vs King + Rook
      KBP_KB, -- King + Bishop + Pawn vs King + Bishop
      
      -- Black perspective
      K_KR,   -- King vs King + Knight
      K_KQ,   -- King vs King + Knight
      K_KB,   -- King vs King + Knight
      K_KN,   -- King vs King + Knight
      K_KP,   -- King vs King + Pawn 
      KP_KQ,  -- King + Pawn vs King + Queen
      KP_KR,  -- King + Pawn vs King + Rook
      KP_KB,  -- King + Pawn vs King + Bishop
      KP_KN,  -- King + Pawn vs King + Knight
      K_KBP,  -- King vs King + Bishop + Pawn
      K_KNP,  -- King vs King + Knight + Pawn
      KR_KRP, -- King + Rook vs King + Rook + Pawn
      KB_KBP, -- King + Bishop vs King + Bishop + Pawn
      
      No_Pattern); -- No pattern of particular interest found
   -- Detect specific endgame pattern where the strategy to play is known.

   
   subtype Endgame_Score_Adapter_Factor is Score_Type range 1 .. 16;
   No_Adapter_Factor   : constant Endgame_Score_Adapter_Factor := Endgame_Score_Adapter_Factor'First;
   Full_Adapter_Factor : constant Endgame_Score_Adapter_Factor := Endgame_Score_Adapter_Factor'Last;
   
   type Endgame_Score_Adapter_Type is array (Color_Type'Range) of Endgame_Score_Adapter_Factor
     with
       Default_Component_Value => No_Adapter_Factor;
   
   
   --------------------
   -- Pawn Structure --
   --------------------
   
   subtype Pawn_Counter_Type is Piece_Counter_Type range 0 .. 8;
   -- Count the number of pawn of a given side are in the board
   
   type Pawn_Location_Type is array (Pawn_Counter_Type range 1 .. 8) of Square_Type
     with 
       Default_Component_Value => 0;

   Not_Present : constant Square_Type := 0;
   
   subtype Board_Coordinate_Type is Coordinate_Type range File_A .. File_H;
   
   type Pawn_Rank_Coordinate_Type is array (Board_Coordinate_Type) of Coordinate_Type;
   -- Track the rank of pawns for each file in the chessboard. If there's no
   -- pawn on a file, the rank shall be Rank_8 for white pawns and Rank_1 for
   -- blacks. If there are multiple pawns on a file (doubled, tripled..), the 
   -- the farther to the promotion shall be kept.   
   
   type Pawn_File_Amount_Type is array (Board_Coordinate_Type) of Pawn_Counter_Type
     with 
       Default_Component_Value => 0;
   -- Count how many pawn are on a specific File
   
   type Pawn_Flag_Type is array (Board_Coordinate_Type) of Boolean
       with
       Default_Component_Value => False;
   
   type Pawn_Structure_Type is
      record
         Location    : Pawn_Location_Type;
         Ranks       : Pawn_Rank_Coordinate_Type;
         Files       : Pawn_File_Amount_Type;
         Backwards   : Pawn_Flag_Type;
      end record;
   
   subtype Pawn_Islands_Counter is Natural range 0 .. 4;
   
   Doubled_Pawn_Penalty       : constant Tapered_Score_Type := (Opening => -10, End_Game => -10);
   Isolated_Pawn_Penalty      : constant Tapered_Score_Type := (Opening =>  -8, End_Game => -12);
   Isolani_Pawn_Penalty       : constant Tapered_Score_Type := (Opening => -10, End_Game => -12);
   Weak_Pawn_Penalty          : constant Tapered_Score_Type := (Opening =>  -6, End_Game => -10);
   Backward_Pawn_Penalty      : constant Tapered_Score_Type := (Opening =>  -4, End_Game =>  -2);
   On_Semi_Open_File_Penalty  : constant Tapered_Score_Type := (Opening =>  -0, End_Game =>  -2);
   Exposed_Pawn_Penalty       : constant Tapered_Score_Type := (Opening =>  -4, End_Game =>  -7);
   Connected_Pawn_Bonus       : constant Tapered_Score_Type := (Opening =>   0, End_Game =>  10);
   Pawn_Island_Penalty_Factor : constant Tapered_Score_Type := (Opening =>  -5, End_Game => -10);
   Hanging_Pawn_Penalty       : constant Tapered_Score_Type := (Opening =>  -5, End_Game => -10);
   
   --  Pawn_Bishop_Square_Control : constant Tapered_Score_Type := (Opening => 2, End_Game => -2);
   
   type Passed_Pawn_Score_Type is array (Game_Phase_Type'Range, Coordinate_Type range Rank_2 .. Rank_7) of Score_Type
     with 
       Default_Component_Value => 0;
   
   White_Candidate_Passed_Pawn_Bonus : constant Passed_Pawn_Score_Type :=
     (Opening =>
        (Rank_2 =>  0,
         Rank_3 =>  0,
         Rank_4 =>  0,
         Rank_5 =>  0,
         Rank_6 =>  0,
         Rank_7 =>  0),
      End_Game =>
        (Rank_2 =>  0,
         Rank_3 =>  5,
         Rank_4 => 10,
         Rank_5 => 15,
         Rank_6 => 30,
         Rank_7 =>  0));
      
   Black_Candidate_Passed_Pawn_Bonus : constant Passed_Pawn_Score_Type :=
     (Opening =>
        (Rank_2 =>  0,
         Rank_3 =>  0,
         Rank_4 =>  0,
         Rank_5 =>  0,
         Rank_6 =>  0,
         Rank_7 =>  0),
      End_Game =>
        (Rank_2 =>  0,
         Rank_3 => 30,
         Rank_4 => 15,
         Rank_5 => 10,
         Rank_6 =>  5,
         Rank_7 =>  0));

   White_Passed_Pawn_Bonus : constant Passed_Pawn_Score_Type :=
     (Opening  =>
        (Rank_2 =>  10,
         Rank_3 =>  20,
         Rank_4 =>  30,
         Rank_5 =>  40,
         Rank_6 =>  50,
         Rank_7 => 100),
      End_Game =>
        (Rank_2 =>  20,
         Rank_3 =>  40,
         Rank_4 =>  60,
         Rank_5 =>  80,
         Rank_6 => 100,
         Rank_7 => 150));
     
     Black_Passed_Pawn_Bonus : constant Passed_Pawn_Score_Type :=
       (Opening  =>
          (Rank_2 => 100,
           Rank_3 =>  50,
           Rank_4 =>  40,
           Rank_5 =>  30,
           Rank_6 =>  20,
           Rank_7 =>  10),
        End_Game =>
          (Rank_2 => 150,
           Rank_3 => 100,
           Rank_4 =>  80,
           Rank_5 =>  60,
           Rank_6 =>  40,
           Rank_7 =>  20));

   Unstoppable_Passer : constant Score_Type := 500;
   
   
   type Pawn_Promotion_Table_Type is array (Board_Type'Range) of Square_Type;

   White_Promotion_Square : constant Pawn_Promotion_Table_Type :=
     (0,  0,  0,  0,  0,  0,  0,  0,  0, 0,
      0,  0,  0,  0,  0,  0,  0,  0,  0, 0,
      0,  0,  0,  0,  0,  0,  0,  0,  0, 0,
      0, A8, B8, C8, D8, E8, F8, G8, H8, 0,
      0, A8, B8, C8, D8, E8, F8, G8, H8, 0,
      0, A8, B8, C8, D8, E8, F8, G8, H8, 0,
      0, A8, B8, C8, D8, E8, F8, G8, H8, 0,
      0, A8, B8, C8, D8, E8, F8, G8, H8, 0,
      0, A8, B8, C8, D8, E8, F8, G8, H8, 0,
      0, 0,  0,  0,  0,  0,  0,   0,  0, 0,
      0, 0,  0,  0,  0,  0,  0,   0,  0, 0,
      0, 0,  0,  0,  0,  0,  0,   0,  0, 0);

   Black_Promotion_Square : constant Pawn_Promotion_Table_Type :=
     (0,  0,  0,  0,  0,  0,  0,  0,  0, 0,
      0,  0,  0,  0,  0,  0,  0,  0,  0, 0,
      0,  0,  0,  0,  0,  0,  0,  0,  0, 0,
      0, A1, B1, C1, D1, E1, F1, G1, H1, 0,
      0, A1, B1, C1, D1, E1, F1, G1, H1, 0,
      0, A1, B1, C1, D1, E1, F1, G1, H1, 0,
      0, A1, B1, C1, D1, E1, F1, G1, H1, 0,
      0, A1, B1, C1, D1, E1, F1, G1, H1, 0,
      0, A1, B1, C1, D1, E1, F1, G1, H1, 0,
      0, 0,  0,  0,  0,  0,  0,   0,  0, 0,
      0, 0,  0,  0,  0,  0,  0,   0,  0, 0,
      0, 0,  0,  0,  0,  0,  0,   0,  0, 0);
   
   King_Proximity_Factor            : constant Score_Type := 5;
   
   type Occupancy_Score_Type is array (Piece_Type'Range) of Tapered_Score_Type;
   
   --  Weak_Square_Occupancy : constant Occupancy_Score_Type :=
   --    (Frame        => (0, 0),
   --     Empty        => (0, 0),
   --     White_Pawn   => (0, 0),
   --     White_Knight => (Opening => 0, End_Game => 0),
   --     White_Bishop => (Opening => 0, End_Game => 0),
   --     White_Rook   => (Opening => 0, End_Game => 0),
   --     White_Queen  => (Opening => 0, End_Game => 0),
   --     White_King   => (0, 0),
   --     Black_Pawn   => (0, 0),
   --     Black_Knight => (Opening => 0, End_Game => 0),
   --     Black_Bishop => (Opening => 0, End_Game => 0),
   --     Black_Rook   => (Opening => 0, End_Game => 0),
   --     Black_Queen  => (Opening => 0, End_Game => 0),
   --     Black_King   => (0, 0));
   
   Weak_Square_Occupancy  : constant Occupancy_Score_Type :=
     (Frame        => (0, 0),
      Empty        => (0, 0),
      White_Pawn   => (0, 0),
      White_Knight => (Opening =>  5, End_Game => 5),
      White_Bishop => (Opening =>  5, End_Game => 5),
      White_Rook   => (Opening =>  0, End_Game => 5),
      White_Queen  => (Opening =>  0, End_Game => 5),
      White_King   => (0, 0),
      Black_Pawn   => (0, 0),
      Black_Knight => (Opening =>  5, End_Game => 5),
      Black_Bishop => (Opening =>  5, End_Game => 5),
      Black_Rook   => (Opening =>  0, End_Game => 5),
      Black_Queen  => (Opening =>  0, End_Game => 5),
      Black_King   => (0, 0));
   
   Outpost_Occupancy_Penalty     : constant Occupancy_Score_Type :=
     (Frame => (0, 0),
      Empty => (0, 0),
      White_Pawn => (0, 0),
      White_Knight => (Opening => -15, End_Game => -15),
      White_Bishop => (Opening => -10, End_Game => -10),
      White_Rook   => (Opening =>  -5, End_Game => -10),
      White_Queen  => (Opening =>   0, End_Game =>  -5),
      White_King   => (0, 0),
      Black_Pawn   => (0, 0),
      Black_Knight => (Opening => -15, End_Game => -15),
      Black_Bishop => (Opening => -10, End_Game => -10),
      Black_Rook   => (Opening =>  -5, End_Game => -10),
      Black_Queen  => (Opening =>   0, End_Game =>  -5),
      Black_King   => (0, 0));
      
   
   ------------------------
   -- Piece Square Table --
   ------------------------

   type Piece_Square_Table_Type is array (Board_Type'Range) of Score_Type;

   White_Pawn_Position_Table : constant Piece_Square_Table_Type :=
   -- F:   A    B    C    D    E    F    G    H
     (0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0, -- Rank 8
      0,  10,  10,  10,  10,  10,  10,  10,  10,   0, -- Rank 7
      0,   5,   5,   5,  10,  10,   5,   5,   5,   0, -- Rank 6
      0,   3,   4,   4,   5,   5,   4,   4,   3,   0, -- Rank 5
      0,   0,   0,   0,   2,   2,   0,   0,   0,   0, -- Rank 4
      0,   2,  -2,  -2,   0,   0,  -2,  -2,   2,   0, -- Rank 3
      0,   2,   5,   5,  -5,  -5,   5,   5,   2,   0, -- Rank 2
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0, -- Rank 1
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0);
   
   Black_Pawn_Position_Table : constant Piece_Square_Table_Type :=
   -- F:   A    B    C    D    E    F    G    H
     (0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0, -- Rank 8
      0,   2,   5,   5,  -5,  -5,   5,   5,   2,   0, -- Rank 7
      0,   2,  -2,  -2,   0,   0,  -2,  -2,   2,   0, -- Rank 6
      0,   0,   0,   0,   2,   2,   0,   0,   0,   0, -- Rank 5
      0,   3,   4,   4,   5,   5,   4,   4,   3,   0, -- Rank 4
      0,   5,   5,   5,  10,  10,   5,   5,   5,   0, -- Rank 3
      0,  10,  10,  10,  10,  10,  10,  10,  10,   0, -- Rank 2
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0, -- Rank 1
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0);
   

   Knight_Position_Table : constant Piece_Square_Table_Type :=
   -- F:   A    B    C    D    E    F    G    H
     (0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0, -30, -20, -10, -10, -10, -10, -20, -30,   0, -- Rank 8
      0, -20, -10,   0,   5,   5,   0, -10, -20,   0, -- Rank 7
      0, -20,  -5,   5,  10,  10,   5,  -5, -20,   0, -- Rank 6
      0, -20,   0,  10,  15,  15,  10,   0, -20,   0, -- Rank 5
      0, -20,   0,  10,  15,  15,  10,   0, -20,   0, -- Rank 4
      0, -20,  -5,   5,  10,  10,   5,  -5, -20,   0, -- Rank 3
      0, -20, -10,   0,   5,   5,   0, -10, -20,   0, -- Rank 2
      0, -30, -20, -10, -10, -10, -10, -20, -30,   0, -- Rank 1
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0);

   White_Bishop_Position_Table : constant Piece_Square_Table_Type :=
     (0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0, -20, -10, -10, -10, -10, -10, -10, -20,   0, -- Rank 8
      0, -10,   0,   0,   0,   0,   0,   0, -10,   0, -- Rank 7
      0, -10,   0,   5,  10,  10,   5,   0, -10,   0, -- Rank 6
      0, -10,   5,   5,  10,  10,   5,   5, -10,   0, -- Rank 5
      0, -10,   0,  10,  10,  10,  10,   0, -10,   0, -- Rank 4
      0, -10,  10,  10,  10,  10,  10,  10, -10,   0, -- Rank 3
      0, -10,   5,   0,   0,   0,   0,   5, -10,   0, -- Rank 2
      0, -20, -10, -10, -10, -10, -10, -10, -20,   0, -- Rank 1
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0);
   -- F:   A    B    C    D    E    F    G    H

   Black_Bishop_Position_Table : constant Piece_Square_Table_Type :=
     (0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0, -20, -10, -10, -10, -10, -10, -10, -20,   0, -- Rank 8
      0, -10,   5,   0,   0,   0,   0,   5, -10,   0, -- Rank 7
      0, -10,  10,  10,  10,  10,  10,  10, -10,   0, -- Rank 6
      0, -10,   0,  10,  10,  10,  10,   0, -10,   0, -- Rank 5
      0, -10,   5,   5,  10,  10,   5,   5, -10,   0, -- Rank 4
      0, -10,   0,   5,  10,  10,   5,   0, -10,   0, -- Rank 3
      0, -10,   0,   0,   0,   0,   0,   0, -10,   0, -- Rank 2
      0, -20, -10, -10, -10, -10, -10, -10, -20,   0, -- Rank 1
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0);
   -- F:   A    B    C    D    E    F    G    H

   White_Rook_Position_Table : constant Piece_Square_Table_Type :=
     (0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0, -- Rank 8
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0, -- Rank 7
      0,  -5,   0,   0,   0,   0,   0,   0,  -5,   0, -- Rank 6
      0,  -5,   0,   0,   0,   0,   0,   0,  -5,   0, -- Rank 5
      0,  -5,   0,   0,   0,   0,   0,   0,  -5,   0, -- Rank 4
      0,  -5,   0,   0,   0,   0,   0,   0,  -5,   0, -- Rank 3
      0,  -5,   0,   0,   0,   0,   0,   0,  -5,   0, -- Rank 2
      0,   0,   0,   0,   5,   5,   0,   0,   0,   0, -- Rank 1
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0);
   -- F:   A    B    C    D    E    F    G    H

   Black_Rook_Position_Table : constant Piece_Square_Table_Type :=
     (0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   5,   5,   0,   0,   0,   0, -- Rank 8
      0,  -5,   0,   0,   0,   0,   0,   0,  -5,   0, -- Rank 7
      0,  -5,   0,   0,   0,   0,   0,   0,  -5,   0, -- Rank 6
      0,  -5,   0,   0,   0,   0,   0,   0,  -5,   0, -- Rank 5
      0,  -5,   0,   0,   0,   0,   0,   0,  -5,   0, -- Rank 4
      0,  -5,   0,   0,   0,   0,   0,   0,  -5,   0, -- Rank 3
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0, -- Rank 2
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0, -- Rank 1
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0);
   -- F:   A    B    C    D    E    F    G    H

   White_Queen_Position_Table : constant Piece_Square_Table_Type :=
     (0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0, -20, -10, -10,  -5,  -5, -10, -10, -20,   0, -- Rank 8
      0, -10,   0,   0,   0,   0,   0,   0, -10,   0, -- Rank 7
      0, -10,   0,   5,   5,   5,   5,   0, -10,   0, -- Rank 6
      0,  -5,   0,   5,   5,   5,   5,   0,  -5,   0, -- Rank 5
      0,   0,   0,   5,   5,   5,   5,   0,  -5,   0, -- Rank 4
      0, -10,   5,   5,   5,   5,   5,   0, -10,   0, -- Rank 3
      0, -10,   0,   5,   0,   0,   0,   0, -10,   0, -- Rank 2
      0, -20, -10, -10,  -5,  -5, -10, -10, -20,   0, -- Rank 1
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0);
   -- F:   A    B    C    D    E    F    G    H

   Black_Queen_Position_Table : constant Piece_Square_Table_Type :=
     (0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0, -20, -10, -10,  -5,  -5, -10, -10, -20,   0, -- Rank 8
      0, -10,   0,   5,   0,   0,   0,   0, -10,   0, -- Rank 7
      0, -10,   5,   5,   5,   5,   5,   0, -10,   0, -- Rank 6
      0,   0,   0,   5,   5,   5,   5,   0,  -5,   0, -- Rank 5
      0,  -5,   0,   5,   5,   5,   5,   0,  -5,   0, -- Rank 4
      0, -10,   0,   5,   5,   5,   5,   0, -10,   0, -- Rank 3
      0, -10,   0,   0,   0,   0,   0,   0, -10,   0, -- Rank 2
      0, -20, -10, -10,  -5,  -5, -10, -10, -20,   0, -- Rank 1
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0);
   -- F:   A    B    C    D    E    F    G    H


   ----------------------------------
   -- Center control and occupancy --
   ----------------------------------

   Occupancy_Of_The_Centre : constant Piece_Square_Table_Type :=
     (0,   0,   0,   0,   0,   0,   0,   0,   0, 0,
      0,   0,   0,   0,   0,   0,   0,   0,   0, 0,
      0, -20, -10, -10, -10, -10, -10, -10, -20, 0,
      0, -10,  -5,   0,   0,   0,   0,  -5, -10, 0,
      0, -10,   0,   5,   5,   5,   5,   0, -10, 0,
      0, -10,   0,   5,  10,  10,   5,   0, -10, 0,
      0, -10,   0,   5,  10,  10,   5,   0, -10, 0,
      0, -10,   0,   5,   5,   5,   5,   0, -10, 0,
      0, -10,  -5,   0,   0,   0,   5,  -5, -10, 0,
      0, -20, -10, -10, -10, -10, -10, -10, -20, 0,
      0,   0,   0,   0,   0,   0,   0,   0,   0, 0,
      0,   0,   0,   0,   0,   0,   0,   0,   0, 0);
   
   
   --------------
   -- Mobility --
   --------------
   
   --  subtype Mobility_Range_Type        is Natural             range 0 .. 27;
   --  subtype Knight_Mobility_Range_Type is Mobility_Range_Type range 0 ..  8;
   --  subtype Bishop_Mobility_Range_Type is Mobility_Range_Type range 0 .. 13;
   --  subtype Rook_Mobility_Range_Type   is Mobility_Range_Type range 0 .. 14;
   --  subtype Queen_Mobility_Range_Tyoe  is Mobility_Range_Type range 0 .. 27;
   --  
   --  type Knight_Mobility_Score_Type is array (Knight_Mobility_Range_Type) of Score_Type;
   --  type Bishop_Mobility_Score_Type is array (Bishop_Mobility_Range_Type) of Score_Type;
   --  type Rook_Mobility_Score_Type   is array (Rook_Mobility_Range_Type) of Score_Type;
   --  type Queen_Mobility_Score_Type  is array (Queen_Mobility_Range_Tyoe) of Score_Type;
   --  
   --  Knight_Mobility             : constant Knight_Mobility_Score_Type :=
   --    (-10, -8, -4, 0, 4, 8, 12, 14, 16);
   --  
   --  Bishop_Mobility             : constant Bishop_Mobility_Score_Type :=
   --    (-15, -10, -5, 0, 6, 12, 18, 24, 28, 30, 32, 34, 35, 36);
   --  
   --  Rook_Mobility               : constant Rook_Mobility_Score_Type :=
   --    (-10, -6, -4, -2, 0, 2, 4, 6, 8, 10, 12, 14, 16, 17, 18);
   --  
   --  Queen_Mobility              : constant Queen_Mobility_Score_Type :=
   --    (-12, -10, -8, -4, -2, -1, 0, 1, 2, 4, 6, 8, 10, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27);
   
   Knight_Mobility_Penalty      : constant Score_Type := 4;
   Knight_Mobility_Bonus        : constant Tapered_Score_Type := (Opening => 4, End_Game => 4);
   Bishop_Mobility_Penalty      : constant Score_Type := 6;
   Bishop_Mobility_Bonus        : constant Tapered_Score_Type := (Opening => 5, End_Game => 5);
   Rook_Mobility_Penalty        : constant Score_Type := 8;
   Rook_Mobility_Bonus          : constant Tapered_Score_Type := (Opening => 2, End_Game => 4);
   Queen_Mobility_Penalty       : constant Score_Type := 12;
   Queen_Mobility_Bonus         : constant Tapered_Score_Type := (Opening => 1, End_Game => 2);
   
   Mobility_Factor             : constant Score_Type := 1;
   Attack_Factor               : constant Score_Type := 1;
   Defence_Factor              : constant Score_Type := 0;
   
   
   type Mobility_Unit_Type is array (Piece_Type'Range) of Score_Type;
   -- A Mobility unit represent the score given to a piece if
   -- when this piece target on a specific square.

   White_Mobility_Unit          : constant Mobility_Unit_Type :=
     (Frame        => 0,
      Empty        => Mobility_Factor,
      White_Pawn   => Defence_Factor,
      White_Knight => Defence_Factor,
      White_Bishop => Defence_Factor,
      White_Rook   => Defence_Factor,
      White_Queen  => Defence_Factor,
      White_King   => Defence_Factor,
      Black_Pawn   => Attack_Factor,
      Black_Knight => Attack_Factor,
      Black_Bishop => Attack_Factor,
      Black_Rook   => Attack_Factor,
      Black_Queen  => Attack_Factor,
      Black_King   => Attack_Factor);

   Black_Mobility_Unit          : constant Mobility_Unit_Type :=
     (Frame        => 0,
      Empty        => Mobility_Factor,
      White_Pawn   => Attack_Factor,
      White_Knight => Attack_Factor,
      White_Bishop => Attack_Factor,
      White_Rook   => Attack_Factor,
      White_Queen  => Attack_Factor,
      White_King   => Attack_Factor,
      Black_Pawn   => Defence_Factor,
      Black_Knight => Defence_Factor,
      Black_Bishop => Defence_Factor,
      Black_Rook   => Defence_Factor,
      Black_Queen  => Defence_Factor,
      Black_King   => Defence_Factor);
   
   ------------------------------------
   -- Rook and Bishop specific score --
   ------------------------------------
   
   Rook_On_Semi_Open_With_Backward_Pawn : constant Score_Type := 5;
   Rook_On_Semi_Open_File       : constant Score_Type := 10;
   Rook_On_Open_File            : constant Score_Type := 20;
   Rook_On_King_File            : constant Score_Type := 5;
   Rook_On_Semi_King_File       : constant Score_Type := 2;
   Rook_On_7th_Rank             : constant Tapered_Score_Type := (Opening => 0, End_Game => 20);
   Rook_On_2nd_Rank             : Tapered_Score_Type renames Rook_On_7th_Rank;
   Rook_Connected               : constant Tapered_Score_Type := (Opening => 5, End_Game => 22);
   Rook_Opposition              : constant Score_Type := 10;
   
   Tarrash_Rule                 : constant Score_Type := 15;
   -- The Tarrash rule is a general principle that applies to endgames. The idea
   -- is that rooks should be placed behind any passed pawn. Behind friendly
   -- passed pawn to support their advance and behind opponent to block their
   -- advance
   
   Queen_On_7th_Rank            : constant Tapered_Score_Type := (Opening => 0, End_Game => 15);
   Queen_On_2nd_Rank            : Tapered_Score_Type renames Queen_On_7th_Rank;
   
   Blocked_Rook                 : constant Score_Type := 90;
   -- Penalty for rook blocked by friendly king that not castled
   
   type Piece_Pair_Type is array (Color_Type'Range) of Boolean
     with 
       Default_Component_Value => False;
   -- Track whether a piece stands on a square of a certain color. Used for
   -- bishop-pairs
   
   White_Bishop_Pair            : Piece_Pair_Type;
   Black_Bishop_Pair            : Piece_Pair_Type;
   
   Bishop_Pair_Bonus            : constant Tapered_Score_Type := (Opening => 20, End_Game => 40);
   Bishop_Pair_Free_Center      : constant Tapered_Score_Type := (Opening => 8, End_Game => 4);
   
   Fianchetto                   : constant Score_Type := 6;
   
   Trapped_Bishop               : constant Score_Type := 75;
   -- Penalty for bishop on the 6/7th square trapped by enemy pawn
   
   Blocked_Bishop               : constant Score_Type := 40;
   -- Penalty for bishop on his own starting square blocked by friendly pawns
   
   Exposing_Queen               : constant Score_Type := 20;
   -- Penalty for exposing the queen before developing minor pieces.
   
   -----------------------------
   -- King evaluation factors --
   -----------------------------

   White_King_Position_Table : constant Piece_Square_Table_Type :=
     (0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0, -30, -40, -40, -50, -50, -40, -40, -30,   0, -- Rank 8
      0, -30, -40, -40, -50, -50, -40, -40, -30,   0, -- Rank 7
      0, -30, -40, -40, -50, -50, -40, -40, -30,   0, -- Rank 6
      0, -30, -40, -40, -50, -50, -40, -40, -30,   0, -- Rank 5
      0, -20, -30, -30, -40, -40, -30, -30, -20,   0, -- Rank 4
      0, -10, -20, -20, -20, -20, -20, -20, -10,   0, -- Rank 3
      0,  20,  20,   0,   0 ,  0,   0,  20,  20,   0, -- Rank 2
      0,  20,  30,  -5, -10,   0,  -5,  30,  20,   0, -- Rank 1
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0);
   -- F:   A    B    C    D    E    F    G    H

   Black_King_Position_Table : constant Piece_Square_Table_Type :=
     (0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,  20,  30,  -5, -10,   0,  -5,  30,  20,   0, -- Rank 8
      0,  20,  20,   0,   0 ,  0,   0,  20,  20,   0, -- Rank 7
      0, -10, -20, -20, -20, -20, -20, -20, -10,   0, -- Rank 6
      0, -20, -30, -30, -40, -40, -30, -30, -20,   0, -- Rank 4
      0, -30, -40, -40, -50, -50, -40, -40, -30,   0, -- Rank 5
      0, -30, -40, -40, -50, -50, -40, -40, -30,   0, -- Rank 3
      0, -30, -40, -40, -50, -50, -40, -40, -30,   0, -- Rank 2
      0, -30, -40, -40, -50, -50, -40, -40, -30,   0, -- Rank 1
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0);
   -- F:   A    B    C    D    E    F    G    H

   King_End_Game_Position_Table : constant Piece_Square_Table_Type :=
     (0,   0,   0,   0,   0,   0,   0,   0,   0, 0,
      0,   0,   0,   0,   0,   0,   0,   0,   0, 0,
      0, -50, -30, -20, -20, -20, -20, -30, -50, 0,
      0, -30,   0,  10,  10,  10,  10,   0, -30, 0,
      0, -20,  10,  25,  25,  25,  25,  10, -20, 0,
      0, -20,  10,  25,  50,  50,  25,  10, -20, 0,
      0, -20,  10,  25,  50,  50,  25,  10, -20, 0,
      0, -20,  10,  25,  25,  25,  25,  10, -20, 0,
      0, -30,   0,  10,  10,  10,  10,   0, -30, 0,
      0, -50, -30, -20, -20, -20, -20, -30, -50, 0,
      0,   0,   0,   0,   0,   0,   0,   0,   0, 0,
      0,   0,   0,   0,   0,   0,   0,   0,   0, 0);


   Pawn_Shelter         : constant Score_Type := 5;
   Pawn_Shelter_Penalty : constant Score_Type := -20; -- Negative, because it will be subtracted
   Pawn_Shelter_Defuse  : constant Score_Type := -35; -- Negative, because it will be subtracted
   
   Open_File_Near_King        : constant Score_Type := 40;
   Open_File_In_Front_Of_King : constant Score_Type := 20;
   
   Semi_Open_File_Near_King        : constant Score_Type := 15;
   Semi_Open_File_In_Front_Of_King : constant Score_Type := 30;
   
   Pawn_Storm_Severe_Threat   : constant Score_Type := 50;
   Pawn_Storm_Threat          : constant Score_Type := 35;
   Pawn_Storm_Alert           : constant Score_Type := 25;
   
   type Weak_Square_Type is array (Square_Type'Range) of Boolean
     with
       Default_Component_Value => False;
   
   Profilactic_Move_Penalty : constant Score_Type := -6;
   -- A profilactic move is a move meant to avoid future threats such as discovery
   -- checks that leads to a hard positions. If the king is in a kind of position
   -- where it is subject to those threads, it is better to move away the kind
   -- soon. Those preventive moves are called profilactic moves. AdaChess gives
   -- a penalty to certain position to encourage profilactic moves
   
   -----------------------------
   -- Attacking the King Zone --
   -----------------------------
   
   type King_Zone_Type is array (King_Offsets'Range) of Square_Type;
   -- Represent the squares around a king
   
   
   subtype Attack_Count_Type is Piece_Counter_Type range 0 .. 15;
   
   type Attacker_Count_Type is array (Piece_Type'Range) of Attack_Count_Type;
   
   type Attack_Origin_Table is array (Square_Type'Range) of Attack_Count_Type
     with
       Default_Component_Value => 1;
   
   
   type King_Attack_Value_Type is array (Piece_Type'Range) of Score_Type;
   King_Attack_Value : constant King_Attack_Value_Type :=
     (Frame        => 0,
      Empty        => 0,
      White_Pawn   => 0,
      White_Knight => 10,
      White_Bishop => 10,
      White_Rook   => 20,
      White_Queen  => 40,
      White_King   => 0,
      Black_Pawn   => 0,
      Black_Knight => 10,
      Black_Bishop => 10,
      Black_Rook   => 20,
      Black_Queen  => 40,
      Black_King   => 0);
   
   Safe_Contact_Check_Attack : constant King_Attack_Value_Type :=
     (Frame        => 0,
      Empty        => 0,
      White_Pawn   => 0,
      White_Knight => 0,
      White_Bishop => 0,
      White_Rook   => 4,
      White_Queen  => 12,
      White_King   => 0,
      Black_Pawn   => 0,
      Black_Knight => 0,
      Black_Bishop => 0,
      Black_Rook   => 4,
      Black_Queen  => 12,
      Black_King   => 0);
   
   
   type King_Attack_Weight is array (Attack_Count_Type) of Score_Type;
   Attack_Weight        : constant King_Attack_Weight := 
     (0, 0, 50, 75, 90, 95, 98, 99, 99, 99, 99, 99, 99, 99, 99, 99);
   
   Attacking_King_Factor : constant Score_Type := 100;
   
   
   ------------------
   -- Tempo factor --
   ------------------
   
   Tempo_Bonus : constant Score_Type := 15;
   
   
   -----------------------------------------
   -- Work with distances between squares --
   -----------------------------------------
   
   type Distance_Table_Type is array (Board_Type'Range, Board_Type'Range) of Distance_Type
     with
       Default_Component_Value => Unreachable;
   
   Knight_Distance_Table : Distance_Table_Type;
   
   procedure Fill_Knight_Distance_Table;
   -- Calculate the amount of move that a Knight needs from one square to reach
   -- another square, assuming an empty chessboard.
   -- This procedure fills a table from every origin to every destination.
   
   function Raw_Distance
     (Piece : in Piece_Type; Square, Target : in Square_Type)
      return Distance_Type;
   -- Calculate how many moves need a piece to reach a Target square from a
   -- starting Square. The calculation is made as if no pieces stands into
   -- the board. This function does not consider the Tempo factor.
   -- See https://en.wikipedia.org/wiki/Taxicab_geometry
   --
   -- Arguments:
   --   Piece  : The piece to be used for calculation
   --   Square : The starting square of the given piece
   --   Target : The target square the piece has to reach
   -- Returns:
   --    The number of steps, or Unreachable if target is not reachable.

   function Distance
     (Chessboard : in Chessboard_Type;
      Piece          : in Piece_Type;
      Square, Target : in Square_Type)
      return Distance_Type 
     with
       Inline => True;
   -- Calculate how many moves need a piece to reach a Target square from a
   -- starting Square. The calculation is made as if no pieces stands into
   -- the board. This function takes into account the Tempo factor, i.e. the
   -- side to move.
   --
   -- Arguments:
   --   Piece  : The piece to be used for calculation
   --   Square : The starting square of the given piece
   --   Target : The target square the piece has to reach
   --
   -- Returns:
   --    The number of steps, or Unreachable if target is not reachable.
   --
   -- Aspects
   --    Inline

   Invalid_Distance_Calculation : exception;

   -- Call Race function between two pieces (Piece and Opponent), places in their
   -- respectively square (Square, Opponent_Square), to get in a Target square.
   -- Race is called from Piece point of view
   type Distance_Match_Type is
     (Equals,        -- Both Piece and Opponent has same distance to target
      Closer,        -- Piece gets faster than Opponent
      Farther,       -- Piece gets slower than Opponent
      Winner,        -- Opponent piece cannot reach target square, Piece can
      Loser,         -- Piece cannot reach Target square. Opponent can
      Not_Reachable) -- Neither Piece nor Opponent can reach Target in any way
     with 
       Size => 3;

   function Race
     (Chessboard : in Chessboard_Type;
      Piece      : in Piece_Type; Square : in Square_Type;
      Opponent   : in Piece_Type; Opponent_Square : in Square_Type;
      Target     : in Square_Type)
      return Distance_Match_Type
     with
       Inline => True;
   -- Calculate the race between two pieces to detect who is faster in reaching
   -- a specific target square. It does by comparing the amount of movements
   -- that the Piece located in Square needs to reach the Target with the amount
   -- of movements that the Opponent (piece) located to Opponent_Square needs to
   -- reach the same Target square.
   -- Return a value based on Piece perspective
   -- 
   -- Arguments
   --    Piece           : The Piece 
   --    Square          : The location of the Piece
   --    Opponent        : The opponent piece
   --    Opponent_Square : The location of the opponent piece
   --    Target          : The destination square
   -- Returns
   --    The Race evaluation from the Piece perspective
   -- Aspects
   --    Inline

end Chess.Engine.Evaluations.Static_Evaluations;
