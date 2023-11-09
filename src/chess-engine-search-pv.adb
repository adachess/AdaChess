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
with Ada.Exceptions;

with Chess;
with Chess.Hashes;    use Chess.Hashes;
with Chess.Nodes;     use Chess.Nodes;
with Chess.IO;
with Chess.Engine.Search.Heuristics;    use Chess.Engine.Search.Heuristics;

with Chess.Engine.PV; use Chess.Engine.PV;
with Chess.Engine.See; use Chess.Engine.See;
with Chess.Engine.Ponder;              use Chess.Engine.Ponder;
with Chess.Engine.Transposition_Table; use Chess.Engine.Transposition_Table;
with Chess.Engine.Evaluations;          use Chess.Engine.Evaluations;
with Chess.Engine.Evaluations.Static_Evaluations; use Chess.Engine.Evaluations.Static_Evaluations;



package body Chess.Engine.Search.PV is
   
   
------------------------------
-- Initialize_Search_Engine --
------------------------------
   
   procedure Initialize_Search_Engine is
   begin
      Initialize_Search_Heuristics;      
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Initialize_Search_Engine;
   
   
   ----------------------------------
   -- Initialize_Search_Heuristics --
   ----------------------------------
   
   procedure Initialize_Search_Heuristics is
   begin
      
      ---------------------------------------------------
      -- Most Valuable Victim, Least Valuable Attacker --
      ---------------------------------------------------
      
      Mvv_Lva := (others => (others => 0));
      
      -- Score captures in MVV-LVA order
      
      for Captured in Piece_Type'Range loop
         for Capturing in Piece_Type'Range loop
            Mvv_Lva (Captured, Capturing) := 10 * Piece_Score (Captured) - Piece_Score (Capturing);
         end loop;
      end loop;
      
      --------------------------
      -- Most Valuable Victim --
      --------------------------
      
      Mvv := (others => 0);
      
      for Captured in Piece_Type'Range loop
         Mvv (Captured) := Piece_Score (Captured) * MVV_Factor + MVV_Bonus;
      end loop;
      
   end Initialize_Search_Heuristics;
  

   -------------------------
   -- Clear_All_Euristics --
   -------------------------

   procedure Clear_All_Euristics is
   begin

      -----------------------------
      -- Clear History Heuristic --
      -----------------------------

      Initialize_History_Heuristic;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Clear_All_Euristics;
   
   
   -------------------------------
   -- Clear Principal Variation --
   -------------------------------

   procedure Clear_Principal_Variation is
   begin
      Principal_Variation := 
        (others => Principal_Variation_Data_Type'
           (Main_Line             => (others => Empty_Annotated_Move),
            Depth                 => Zero_Depth,
            Current_Move          => Empty_Move,
            Killer_1              => Empty_Move,
            Killer_2              => Empty_Move,
            --  Threat                => Empty_Move,
            --  Reduced               => False,
            Evaluation            => (Score => -Infinity, Game_Phase => In_Progress),
            Predicted_Countermove => Empty_Move));
      
      for I in Depth_Type'Range loop
         for J in Depth_Type'Range loop
            First_Variation (I).Main_Line (J) := Empty_Annotated_Move;
         end loop;
         First_Variation (I).Evaluation :=
           (Score => 0, Game_Phase => In_Progress);
      end loop;
      
     
   end Clear_Principal_Variation;


   
   -----------------------------------
   -- Draw_By_Threefold_Repetitions --
   -----------------------------------
   
   function Draw_By_Threefold_Repetitions (Chessboard : in Chessboard_Type) return Boolean
   is
      
      function Count_Repetitions return Repetition_Type is
         Repetitions     : Repetition_Type := 1; -- The current position
         History_Ply     : History_Depth_Type := Chessboard.History_Ply;
         The_Hash        : constant Hash_Type := Hash;
         History_Hash    : Hash_Type := 0;
         Minimum_History : constant History_Depth_Type := Pre_Frontier_Depth; -- We cannot have repetitions if we haven't played at least 2 full moves
         Fifty           : Fifty_Counter_Type renames Chessboard.Fifty;
      begin
         if History_Ply >= Minimum_History then
            loop
               History_Ply := History_Ply - 2;
               History_Hash := Chessboard.Moves_History (History_Ply).Hash;
               if The_Hash = History_Hash then
                  Repetitions := Repetitions + 1;
               end if;
               exit when Repetitions = 2 or else History_Ply < Minimum_History or else Fifty = 0;
            end loop;
         end if;
         return Repetitions;
   end Count_Repetitions;      

   begin
      return Count_Repetitions = Three_Fold_Repetitions_Count;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Draw_By_Threefold_Repetitions;
   
   

   
   --------------
   -- Rollback --
   --------------

   procedure Rollback (Chessboard : in out Chessboard_Type) is
      Ply  : Depth_Type renames Chessboard.Ply;
      Flag : Flag_Type;
   begin
      while Ply > Zero_Depth loop
         Flag := Chessboard.Moves_History (Chessboard.History_Ply - 1).Move.Flag;
         if Flag = Null_Move then
            Chessboard.Undo_Null_Move;
         else
            Chessboard.Undo;
         end if;
      end loop;
      
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Rollback;

   
   ------------------------
   -- Create_Move_Picker --
   ------------------------      
  
   function Create_Move_Picker 
     (Chessboard   : in out Chessboard_Type;
      Type_Of_Node : in Tree_Node_Type;
      Hash_Move    : in Move_Type;
      Previous_Move : in Move_Type)
      return Search_Move_List_Type 
   is
      
      Ply         : constant Depth_Type := Chessboard.Ply;
      
      Move        : Move_Type;
      Score       : Heuristic_Score := 0;
      See_Score   : Score_Type;
      Phase       : Stage_Type;
      
      Index        : Natural := 0;
      Nb_Of_Moves : constant Natural := Chessboard.Moves_Counter (Ply);
      Moves_List  : Search_Move_List_Type (1 .. Nb_Of_Moves);
      
      First_Move, Last_Move    : Natural;
      
      Pv_Move  : constant Annotated_Move_Type := Principal_Variation (Ply).Main_Line (Ply);
      
      Killer_1 : constant Move_Type := Principal_Variation (Ply).Killer_1;
      Killer_2 : constant Move_Type := Principal_Variation (Ply).Killer_2;
      
   begin
      
      First_Move := Chessboard.Moves_Pointer (Ply);
      Last_Move := Chessboard.Moves_Pointer (Ply + 1) - 1;
      
      for I in First_Move .. Last_Move loop
         Move := Chessboard.Moves_Stack (I);

         Score := History_Threshold;
         Phase := Not_Staged;
         
         See_Score := Static_Exchange_Evaluation_Score (Chessboard, Move);
         
         --------------------------------------
         -- Step 1: Assign a phase to each move
         --------------------------------------
         
         if Type_Of_Node = Pv_Node and then Move = Pv_Move.Move then
            Phase := Main_Phase;
         else
            if Move = Hash_Move then
               -- The very first move to be tried after the PV move is the hash
               -- move.
               Phase := Hash_Phase;
               
            elsif Move_Is_A_Recapture (Move, Previous_Move) then
               Phase := Recaptures;
               
            elsif Move_Is_Capture (Move) then
               -- Captures are sorted next to the PV and Hash moves. Winning and
               -- equal captures are tried first, in a mvv-lva order. Losing
               -- capture after them
               Phase := (if See_Score >= 0 then Non_Losing_Captures else Losing_Captures);
               
            elsif Move = Killer_1 or else Move = Killer_2 then
               -- Killer moves are quiet moves who caused a beta cut-off in the
               -- search. 
               Phase := Killer_Moves;
             
            elsif Move.Check /= No_Check then
               -- Moves that delivers check are sorted next. Exceptional case
               -- are checkmate moves, which are also part of the main phase.
               if Move.Check = Checkmate then
                  Phase := Main_Phase;
               else
                  Phase := Checks;
               end if;
               
            --  elsif Move_Is_Advanced_Passed_Pawn_Push (Move) then
            --     -- The remaining moves are Quiet moves. Among those, the moves
            --     -- which result in promotions or close-to be gets ranked first
            --     Phase := Advanced_Passed_Pawn;
            
            --  elsif Move_Is_Passed_Pawn_Push (Chessboard, Move) then
            --     -- Other passed pawn push is tried before any other quiet move
            --     Phase := Passed_Pawn_Push;
               
            else
               -- The remaining moves are quiet moves who are tried last
               Phase := Quiet;
               
            end if;
         end if; 
         
         pragma Assert (Phase /= Not_Staged, "Found a move with unassigned Phase");
         
         ----------------------------------------
         -- Step 2: Assign a score for each phase
         ----------------------------------------
         
         case Phase is
               
            when Main_Phase => 
               -- The main phase is the PV move in pv-nodes.
               Score := Infinity;
               
            when Hash_Phase =>
               -- This is the main phase for a non-pv nodes. It contains the
               -- move stored into a Transposition Table.
               Score := Infinity;
               
            when Recaptures =>
               -- The third kind of moves are recaptures, if any, sorted via
               -- MVV_LVA. This produces a better performaces compared to a see
               -- based order.
               --  Score := History_Threshold + See_Score;
               Score := History_Threshold + Mvv_Lva (Move.Captured, Move.Piece) / 10;
               
            when Non_Losing_Captures =>
               -- Non losing captures, with SEE >= 0 are scored in a MVV_LVA
               -- way. This produces a better performaces compared to a see
               -- based order.
               --  Score := History_Threshold + See_Score;
               Score := History_Threshold + Mvv_Lva (Move.Captured, Move.Piece) / 10;
               
            when Losing_Captures =>
               -- Losing captures, with SEE < 0, are scored in a MVV_LVA
               -- way. This produces a better performaces compared to a see
               -- based order.
               --  Score := History_Threshold + See_Score;
               Score := History_Threshold + Mvv_Lva (Move.Captured, Move.Piece) / 10;
              
            when Killer_Moves =>
               -- Killer moves are quiet moves that caused a beta cut-off in a
               -- previous search. It is likely that they will cause a cut-off
               -- again, therefore we play them as soon as possible
               Score := (if Move = Killer_1 then 2 else 1);
               
            when Checks =>
               -- Moves that deliver checks are sorted by the heuristic score.
               -- Note that leaf nodes might contains a checkmate which we want
               -- to find immediately, sorting it our as a main phase.
            
               if Move.Check = Checkmate then
                  Score := Infinity;
               else
                  Score := History_Balance (Move);
               end if;
            
               -- An alternative sorting that might be worth to consider is the
               -- one based on the kind of check. Code is commented here below.
            
               --  case Move.Check is
               --     when Direct_Check    => Score := 1;
               --     when Discovery_Check => Score := 2;
               --     when Double_Check    => Score := 3;
               --     when Checkmate       => Score := Infinity;
               --     when Unknown_Check   => Score := 1;
               --     when No_Check        => null;
               --  end case;
            
            --  when Advanced_Passed_Pawn =>
            --     -- Advanced passed pawn are sorted via their heuristic. There's no
            --     -- need to influence the move ordering with promotion-based scoring
            --     -- the moves.
            --     Score := History_Balance (Move);
            --  
            --  when Passed_Pawn_Push =>
            --     -- We just want passed pawn to be evaluated before other quiet
            --     -- move, which means we can rely on the heuristic for them.
            --     Score := History_Balance (Move);
               
            when Quiet =>
               -- Other quiet moves rely on search history data
               Score := History_Balance (Move);

            when Not_Staged => 
               raise Invalid_Stage with Chess.IO.Move_To_String (Move) & " has stage " & Stage_Type'Image (Phase);
         end case;
         
         Index := Index + 1;
         Moves_List (Index) := (Move, Score, Phase);
      end loop;
     
      return Moves_List;
      
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Create_Move_Picker;
   
   
   --------------------
   -- Pick_Next_Move --
   --------------------
   
   function Pick_Next_Move (Moves_List : in out Search_Move_List_Type; Phase : in out Stage_Type; From : in Natural) return Move_Type is
      To         : constant Natural := Moves_List'Last;
      
      Search_Move       : Search_Move_Type;
      
      Best_Score : Integer := Integer'First; -- Ensure a better score will be found
      Best_Index : Natural := From;
      Move_Found : Boolean := False;
      
      Original_Phase : constant Stage_Type := Phase;
   begin
            
      pragma Assert (Phase /= Not_Staged, "Pick Next Move called with invalid *Empty* phase");
      
      loop
         
         -- Step 1: Find the best move.
         -- Find the highest scored moved assigned to the current stage,
         -- if any. This move will be picked/selected
         
         Move_Found := False;
         
         for I in From .. To loop
            Search_Move := Moves_List (I);
            
            if Phase = Search_Move.Phase then
               if Best_Score <= Search_Move.Score then
                  Best_Score := Search_Move.Score;
                  Best_Index := I;
                  Move_Found := True;
               end if;
            end if;
            
         end loop;
         
         -- Step 2: retrieve the move
         -- If a move into the current stage is picked, swap it with the one
         -- located in the "from" position inside the moves list. This index
         -- represent the one containing the move that will be delivered back
         -- and guarantee that moves already picked will not be picked twice.
         -- If, instead, no move at the current stage is available, proceed with
         -- the next stage
         
         if not Move_Found then
            Phase := Stage_Type'Succ (Phase);
         else
            Search_Move := Moves_List (From);
            Moves_List (From) := Moves_List (Best_Index);
            Moves_List (Best_Index) := Search_Move;
         end if;
         
         exit when Move_Found;
         
         pragma Assert (Phase /= Not_Staged, "No move available found in move picker!");
         
      end loop;
     
      return Moves_List (From).Move;
      
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Pick_Next_Move;
   
   
   -------------------
   -- Annotate_Move --
   -------------------
   
   function Annotate_Move (Move              : in Move_Type;
                           See               : in Score_Type;
                           Best_Score        : in Score_Type;
                           Ply               : in Depth_Type;
                           Recapture         : in Boolean;
                           Passed_Pawn_Push  : in Boolean;
                           Nb_Of_Legal_Moves : in Natural) return Annotation_Type
   is

      -- Pivot data, used to find the proper score gain. Those are the data
      -- used to compare the current best_score with the one that tells us
      -- how was the pv before we found this pv. This is the pivot score and
      -- these are the pivot data.
      Pivot_Move         : Move_Type := First_Variation (Zero_Depth).Current_Move;
      Pivot_Depth        : Depth_Type := Zero_Depth;
      Pivot_Score        : Score_Type := 0;
      
      PV_Move            : constant Annotated_Move_Type := Principal_Variation (Zero_Depth).Main_Line (Zero_Depth);

      Brilliant_Treshold   : Integer := 30;
      Good_Treshold        : Integer := 20;
      Interesting_Treshold : Integer := 10;
      
      Brilliant_Scarifice   : constant Integer := (if Best_Score >= 0 then 15 else +Infinity);
      Good_Sacrifice        : constant Integer := 10;
      Interesting_Sacrifice : constant Integer := 5;
      
      Diff                 : Integer := 0;
      Percent              : Integer := 0;
      
      Sacrifice            : Boolean := False;
      
      Annotation           : Annotation_Type := None;
      
   begin
      
      if Move.Check = Checkmate then
         return None;
      elsif Recapture or else Nb_Of_Legal_Moves = 1 then
         return None; 
      end if;
      
      -- In losing position we must accept that we can't do more than survive
      if Best_Score < 0 then
         Interesting_Treshold := Good_Treshold;
         Good_Treshold := Brilliant_Treshold;
         Brilliant_Treshold := +Infinity; -- Hack: in losing position we don't have brilliant moves
      end if;
      
      -- Extract the relevant information by looking the principal variations
      -- in the previous search. We have a Move which we want to annotate. These
      -- information allows as to answer this question: what was the score in
      -- the moment we first found the Move to became the new PV move?
      -- Example: 
      
      -- setboard 2r1r1k1/pp2bpp1/2nqp2p/3p4/3P2NP/1PPB2P1/P2Q2P1/R3R1K1 w - -
      -- Depth   Time        Nodes  Score  Principal Variation
      --  1/2    0.00          122  -0.52  1. Qf2
      --  2/3    0.01          404  -0.31  1. Qf2 h5
      --  3/4    0.03         4231  -0.55  1. Qf2 h5 2. Ne3
      --  4/5    0.07        14004  -0.44  1. Qf2 h5 2. Ne3 Bf6
      --  5/7    0.22        39884  -0.48  1. Qf2 h5 2. Ne3 e5 3. dxe5 Nxe5
      --  6/7    1.17       212558   0.31  1. Nxh6+ Kf8 2. Rf1 f5 3. Ng4 Qxg3
      --  7/8    3.93       713928   0.27  1. Nxh6+ Kf8 2. h5 Red8 3. Ng4 Qxg3 4. Ne3
      --  7/8    4.19       765721   0.27  1. Nxh6+ Kf8 2. h5 Red8 3. Ng4 Qxg3 4. Ne3
      --  8/9    5.50       999361   0.80  1. Nxh6+ Kf8 2. Rf1 Bf6 3. Ng4 Qxg3 4. Nxf6 gxf6
      --
      -- Look at depth 8 (the last line). The best move was Nxh6+ (with a given
      -- score of 0.80) but this move became the pv move at depth 6, and before
      -- we had Qf2 at depth 5, whith a score of -0.48. Therefore, we want to
      -- consider that the score difference in this moment is the difference
      -- between the current best_score we have so far (we get it as argument)
      -- and the score at the last pv change (which is -0.48)
      
      -- If we have no changes in the pv, the one at start is the only pivot we have
      Pivot_Score := First_Variation (Zero_Depth).Evaluation.Score;
      
      -- Decide what is the pivot score by searching the change in the pv
      First_Score_Loop : for D in reverse Frontier_Depth .. First_Variation_Depth - 1 loop
         Pivot_Move := First_Variation (D).Current_Move;
         if Pivot_Move /= PV_Move then
            Pivot_Depth := D;
            exit First_Score_Loop;
         end if;
      end loop First_Score_Loop;
         
      Pivot_Score := First_Variation (Pivot_Depth).Evaluation.Score;
      
      -- Calculate the amount of advantage gained from this move by comparing
      -- the base and current scores.
      if Ply = Zero_Depth then
         Diff := Best_Score - Pivot_Score;
      else
         Diff := First_Variation (Ply).Evaluation.Score - Pivot_Score;
      end if;
    
      
      -- If the engine is losing, we want to calculate the less possible score
      -- loss. In a losing position a move is worth to be annotate if, somehow,
      -- is going to invert the tendency.
      -- But in winning position, the score is alaways given in terms of 
      --  if Diff < 0 then
      --     return None;
      --  else
         Percent := abs Diff / (10 * Ply + 1); -- In terms of centipawn, calculating the gain
      --  end if;
      
      --  Ada.Text_IO.Put_Line ("Move: " & Chess.IO.Move_To_String (Move) & " => "
      --                        & Chess.IO.Move_To_String (First_Variation (Last_Pv_Chage).Current_Move)
      --                        & ", S: " & Score_Type'Image (Score_At_Pv_Change)
      --                        & ", P: " & Score_Type'Image(Percent));
      
      -- Decide if the move represent a sacrifice of a piece
      if See < 0 and then Move.Captured not in Queen_Type then
         Sacrifice := Move.Piece in Queen_Type -- Queen involved in a move with negative see (queen sacrifice)
           or else Move.Piece in Rook_Type | Bishop_Type | Knight_Type; -- Non-pawn involved in a move with negative see (piece sacrifice)
      end if;
      
      -- Case 1: Direct queen sacrifices are often worth to be brilliant moves!
      if Sacrifice and then Move.Piece in Queen_Type then
         if Percent >= Brilliant_Scarifice then
            return Brilliant;
         elsif Percent >= Good_Sacrifice then
            return Good;
         elsif Percent >= Interesting_Treshold / 2 then
            return Interesting;
         end if;
      end if;
         
      -- Case 2: Piece sacrifice resulting in a relative good advantage
      if Sacrifice and then Move.Piece not in Queen_Type then
         if Percent >= Brilliant_Scarifice then
            return Brilliant;
         elsif Percent >= Good_Sacrifice then
            Annotation := Good;
         elsif Percent >= Interesting_Sacrifice then
            Annotation := Interesting;
         end if;
      end if;
         
      -- Case 3: Underpromotion that leads to an advantage.
      if Move.Promotion in Knight_Type | Bishop_Type | Rook_Type then
         if Percent >= Interesting_Treshold then
            return Brilliant;
         else
            Annotation := Good;
         end if;
         
      elsif Move.Promotion in Queen_Type then
         -- Promotions to queen are never moves of particular interest, unless
         -- they represent some case of sacrifice which is already analyzed above
         return Annotation;
      end if;
      
      -- Case 4: Discovery checks and/or special moves that leads to good advantages
      if Move.Check in Discovery_Check | Double_Check and then Percent >= Brilliant_Treshold then
         Annotation := Good;
      --  elsif Move.Flag in Castle | Capture_En_Passant and then Percent >= Brilliant_Treshold then
      --     Annotation := Good;
      end if;
      
      if Move.Check in Direct_Check | Unknown_Check and then Is_Mate (Best_Score) then
         return Annotation;
         
      elsif Move.Captured /= Empty or else Sacrifice then
         -- The capture is either a sacrifice, which is already investigated
         -- above, or represent a normal capture sequence, which has not interest
         -- for us in terms of annotation. In both cases, we just want to return
         -- what we have found so far.
         return Annotation;
      end if;
      
      -- Remaining moves might still be amazing if the search reveal a proper
      -- advantage somewhere at depth search.
      -- This is how the engine decide if the move does worth an annotation: it
      -- take the score got at current ply th e very first time the search
      -- reached that ply and compare it with the new score obtained at the same
      -- ply every time the search get triggered with a deeper search depth.
      
      -- Those are most likely quiet moves which are somehow hard to spot.
      -- To get attention those moves must result in a strong advantage.   
      
      -- Testing this algoritm result in the observation that, when the engine
      -- has a strong advantage (or disadvantage), is very easy to increase that
      -- advantage very fast. A search, for example, that evaluate as +4.00 at
      -- the very first depth might reach depth 10 with +12.00. In this case the
      -- engine cannot decide to annotate based only on the score differences.
      -- A factor must be taken into account, which is a portion of the static
      -- score.
      
      if Is_Mate (Pivot_Score) then
         return Annotation;
      end if;
      
      Brilliant_Treshold := Brilliant_Treshold * Ply;
      Good_Treshold := Good_Treshold * Ply;
      Interesting_Treshold := Interesting_Treshold * Ply;
      
      if Move.Check = Direct_Check then
         Brilliant_Treshold := Brilliant_Treshold * 2;
         Good_Treshold := Good_Treshold * 2;
         Interesting_Treshold := Interesting_Treshold * 2;
      end if;
      
      if Passed_Pawn_Push then
         Brilliant_Treshold := Brilliant_Treshold * Ply;
         Good_Treshold := Good_Treshold * Ply;
         Interesting_Treshold := Interesting_Treshold * Ply;
      end if;
      
      -- Case 5: Base on score
      if Percent >= Brilliant_Treshold then
         return Brilliant;
      elsif Percent >= Good_Treshold then
         Annotation := Good;
      elsif Percent >= Interesting_Treshold and then Annotation /= Good then
         Annotation := Interesting;
      end if;
      
      return Annotation;
      
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Annotate_Move;
  
   
   -----------------
   -- Search Root --
   -----------------
   
   function Search_Root (Chessboard  : in out Chessboard_Type;
                         Alpha, Beta : in Score_Type;
                         Max_Depth   : in Depth_Type;
                         Ponder_Move : in Move_Type := Empty_Move) return Evaluation_Type is
      
      Alpha_Score : Score_Type := Alpha;
      
      Move              : Move_Type := Empty_Move;
      Moves_Searched    : Natural := 0;
      Nb_Of_Legal_Moves : Natural := 0;
      
      Evaluation : Evaluation_Type;
      Best_Score : Score_Type := -Infinity;
      Match_Status : Match_Status_Type := In_Progress;
      
      Best_Move : Move_Type := Empty_Move;
      Recapture : Boolean;
      Pushing_Passed_Pawn : Boolean;
      Annotation : Annotation_Type := None;
      
      Escaping_From_Check : constant Boolean := Chessboard.Has_King_In_Check (Chessboard.Side_To_Move);
      
      Observe_Null_Move : Boolean;
      
      Phase             : Stage_Type := Initial_Phase;

      Ply               : constant Depth_Type := Chessboard.Ply;
      Selective_Depth   : constant Depth_Type := 6;
      
   begin
      
      Principal_Variation (Ply).Depth := Ply;
      
      ---------------------
      -- Move generation --
      ---------------------

      Chessboard.Generate_Moves;
      Nb_Of_Legal_Moves := Chessboard.Moves_Counter (Ply);
      
      if Nb_Of_Legal_Moves = 0 then
         return Evaluation_Type'(Score => Draw, Game_Phase => Draw_By_Stalemate);
      end if;
      
      Alpha_Beta : declare
         Moves_List : Search_Move_List_Type := Create_Move_Picker (Chessboard, Pv_Node, Empty_Move, Empty_Move);
      begin
         
         Phase := Initial_Phase;
                     
         for I in Moves_List'Range loop            
            Move := Pick_Next_Move (Moves_List, Phase, I);
            
            Principal_Variation (Ply).Current_Move := Move;
            
            -------------------------
            -- Checkmate detection --
            -------------------------
            
            if Move.Check = Checkmate then
               Evaluation := Evaluation_Type'(Score => Mate - Ply, Game_Phase => Checkmate);
               Update_Principal_Variation (Move, Ply, Evaluation);
               return Evaluation;
            end if;
         
            Observe_Null_Move := Move_Is_Null_Ok (Move);
            
            Recapture := Move_Is_A_Recapture (Move, Last_Move_Made (Chessboard));
            Pushing_Passed_Pawn := Move_Is_Passed_Pawn_Push (Chessboard, Move);
            
            Chessboard.Play (Move);
               
            if Moves_Searched < Multi_Pv then
               Evaluation := -Principal_Variation_Search (Chessboard, Max_Depth, -Beta, -Alpha_Score, Move);
            else
               Evaluation := -Zero_Window_Search (Chessboard, Max_Depth, -Alpha_Score, Move, Observe_Null_Move);
               if Evaluation.Score > Alpha_Score and then Evaluation.Score < Beta then
                  Evaluation := -Principal_Variation_Search (Chessboard, Max_Depth, -Beta, -Alpha_Score, Move);
               end if;
            end if;
            
            Chessboard.Undo;
            
            ---------------------
            -- Decisional tree --
            ---------------------
            
            << Decisional_Tree >>
            
            if Evaluation.Score > Best_Score then
               Best_Score := Evaluation.Score;
               if Evaluation.Score > Alpha_Score then
                  Alpha_Score := Evaluation.Score;
                  Best_Move := Move;
                  exit when Best_Score >= Beta;
                  if Max_Depth > Frontier_Depth then
                     Annotation := Annotate_Move (Move, Static_Exchange_Evaluation_Score (Chessboard, Move), Best_Score, Ply, Recapture, Pushing_Passed_Pawn, Nb_Of_Legal_Moves);
                  end if;
                  Update_Principal_Variation (Move, Ply, Evaluation, Annotation);
                  Match_Status := Evaluation.Game_Phase;
                  if Max_Depth > Selective_Depth then -- and then not Time_Has_Up then
                     Print_Principal_Variation
                       (Chessboard, Max_Depth, Clock.Thinked_Time, Ponder_Move);
                  end if;
               end if;
            end if;
            
            Moves_Searched := Moves_Searched + 1;
            
            exit when Time_Has_Up;
         end loop;
        
      end Alpha_Beta;
         
      
      return Evaluation_Type'(Score => Best_Score, Game_Phase => Match_Status);
         
   exception
      when Thinking_Time_Expired =>
         Rollback (Chessboard);
         raise;
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Search_Root;
   
   
   --------------------------------
   -- Principal_Variation_Search --
   --------------------------------
   
   function Principal_Variation_Search 
     (Chessboard      : in out Chessboard_Type;
      Max_Depth       : in Depth_Type;
      Alpha, Beta     : in Score_Type;
      Previous_Move   : in Move_Type) return Evaluation_Type
   is
      
      Depth       : Depth_Type := Max_Depth;
      Alpha_Score : Score_Type := Alpha;
      
      Move                     : Move_Type := Empty_Move;
      Nb_Of_Legal_Moves        : Natural := 0;
      Moves_Searched           : Natural := 0;
      
      Annotation : Annotation_Type;
      
      See : See_Score_Type;
      
      Best_Score               : Score_Type := -Infinity;
      Match_Status : Match_Status_Type := In_Progress;
      Best_Move                : Move_Type := Empty_Move;
      Evaluation               : Evaluation_Type;
      
      Dummy : Evaluation_Type;
      
      Hash_Move : Move_Type := Empty_Move;
      
      Observe_Null_Move : Boolean;
      --  Pushing_Passed_Pawn : Boolean;
      
      Extend_Search : Boolean := False;
      
      King_In_Check   : Check_Type renames Previous_Move.Check;
      Escaping_From_Check : constant Boolean := King_In_Check /= No_Check;
      
      Recapture    : Boolean;
      Pushing_Passed_Pawn : Boolean;
      
      Transposition_Table_Entry : Transposition_Table_Entry_Type;
      Phase             : Stage_Type := Initial_Phase;
      
      Ply                      : constant Depth_Type := Chessboard.Ply;
      
   begin
      
      Principal_Variation (Ply).Depth := Ply;
      
      -------------
      -- Horizon --
      -------------

      if Depth = Zero_Depth then
         return Quiescence_Search (Chessboard, Alpha, Beta, Previous_Move);
      end if;
      
      if Ply = Horizon then
         return Evaluate (Chessboard);
      end if;
      
      Search_Nodes := Search_Nodes + 1;
      
      ---------------------
      -- Draw Conditions --
      ---------------------
      
      if Draw_By_Threefold_Repetitions (Chessboard) then 
         if Escaping_From_Check then
            return Evaluation_Type'(Draw, Draw_By_Perpetual_Check);
         else
            return Evaluation_Type'(Draw, Draw_By_Threefold_Repetitions);
         end if;
      elsif Chessboard.Fifty = 100 then
         return Evaluation_Type'(Draw, Draw_By_Fifty_Moves_Rule);
      elsif Evaluate (Chessboard).Game_Phase = Draw_By_Insufficient_Material then
         return Evaluation_Type'(Score => Draw, Game_Phase => Draw_By_Insufficient_Material);
      end if;

      --------------------
      -- Time condition --
      --------------------
      
      if Time_Has_Up and then not Pondering then
         raise Thinking_Time_Expired;
      end if;
      
      --------------------------
      -- Mate distance pruning --
      ---------------------------
      
      if Mate - Ply <= Alpha then
         return Evaluation_Type'(Alpha, Checkmate);
      elsif -Mate + Ply >= Beta then
         return Evaluation_Type'(Beta, Checkmate);
      end if;
      
      ------------------
      -- Hash probing --
      ------------------

      Transposition_Table_Entry := Probe_Transposition_Table (Hash);
      if Transposition_Table_Entry.Depth >= Depth then
         Hash_Move := Transposition_Table_Entry.Move;
      end if;
      
      ----------------------------------
      -- Internal Iterative Deepening --
      ----------------------------------
      
      if Hash_Move = Empty_Move and then Depth >= IID_Depth then
         Dummy := Principal_Variation_Search (Chessboard, Max_Depth - IID_Reduction, Alpha, Beta, Previous_Move);
         Hash_Move := Principal_Variation (Ply).Main_Line (Ply).Move;
      end if;
      
      ---------------------
      -- Move generation --
      ---------------------

      Chessboard.Generate_Moves;
      Nb_Of_Legal_Moves := Chessboard.Moves_Counter (Ply);
      
      Alpha_Beta : declare
         Moves_List   : Search_Move_List_Type := Create_Move_Picker (Chessboard, Pv_Node, Hash_Move, Previous_Move);
         History_Move : Move_Type := Empty_Move;
         Moves_Played : Moves_List_Type (0 .. Moves_List'Last) := (others => Empty_Move);
      begin
         
         for I in Moves_List'Range loop
            Move := Pick_Next_Move (Moves_List, Phase, I);
                        
            Principal_Variation (Ply).Current_Move := Move;
            
            -------------------------
            -- Checkmate detection --
            -------------------------
            
            if Move.Check = Checkmate then
               Evaluation := Evaluation_Type'(Score => Mate - Ply, Game_Phase => Checkmate);
               Principal_Variation (Ply + 1).Depth := Ply + 1;
               goto Decisional_Tree;
            end if;
            
            ------------------
            -- Search Depth --
            ------------------
            
            Extend_Search := False;
            
            Pushing_Passed_Pawn := Move_Is_Passed_Pawn_Push (Chessboard, Move);
            Recapture := Move_Is_A_Recapture (Move => Move, Previous_Move => Previous_Move);
            
            if Recapture then
               See := Static_Recapture_Exchange_Evaluation (Chessboard, Move, Previous_Move);
            else
               See := Static_Exchange_Evaluation_Score (Chessboard, Move);
            end if;
            
            if Move.Check in Direct_Check | Discovery_Check | Double_Check then
               Extend_Search := True;
            elsif Escaping_From_Check and then Nb_Of_Legal_Moves = 1 then -- single reply
               Extend_Search := True;
            elsif Recapture and then Move.Captured not in Pawn_Type and then See >= 0 then
               Extend_Search := True;
            end if;

            Depth := (if Extend_Search then Max_Depth  else Max_Depth - 1);
            
            ---------------------------
            -- Null Move Observation --
            ---------------------------
             
            Observe_Null_Move := Move_Is_Null_Ok (Move);
              
            ----------------------
            -- Recursive Search --
            ----------------------
                           
            Chessboard.Play (Move);
            
            if Moves_Searched < Multi_Pv then
               Evaluation := -Principal_Variation_Search (Chessboard, Depth, -Beta, -Alpha_Score, Move);
            else
               Evaluation := -Zero_Window_Search (Chessboard, Depth, -Alpha_Score, Move, Observe_Null_Move);
               if Evaluation.Score > Alpha_Score and then Evaluation.Score < Beta then
                  Evaluation := -Principal_Variation_Search (Chessboard, Depth, -Beta, -Alpha_Score, Move);
               end if;
            end if;
     
            Chessboard.Undo;
           
            << Decisional_Tree >>
            
            Moves_Played (Moves_Searched) := Move;
            Moves_Searched := Moves_Searched + 1;
            
            ---------------------
            -- Decisional Tree --
            ---------------------
             
            if Evaluation.Score > Best_Score then
               Best_Score := Evaluation.Score;
               if Evaluation.Score > Alpha_Score then
                  Alpha_Score := Evaluation.Score;
                  Best_Move := Move;
                  exit when Best_Score >= Beta;
                  Annotation := Annotate_Move (Move, See, Best_Score, Ply, Recapture, Pushing_Passed_Pawn, Nb_Of_Legal_Moves);
                  Update_Principal_Variation (Move, Ply, Evaluation, Annotation);
                  Match_Status := Evaluation.Game_Phase;
               end if;
            end if;
           
            exit when Best_Score >= Beta;
            exit when Move.Check = Checkmate;
         end loop;
         
         --------------------
         -- Draw Condition --
         --------------------
         
         if Nb_Of_Legal_Moves = 0 then
            return Evaluation_Type'(Score => Draw, Game_Phase => Draw_By_Stalemate);
         end if;
         
         -------------------------
         -- Update History Data --
         -------------------------
         
                 
         if Best_Score >= Beta and then not Move_Is_Capture (Best_Move) then
            
            --  Best_Move.Annotation := Annotation; -- Save the annotation for this move!
            
            pragma Assert (Best_Move /= Empty_Move, "Found a best score but best move not updated!");
                              
            -- If the move that caused a beta cut-off is a non-capturing move
            -- then register it as a successful move. All the other quiet moves
            -- tried so far are unsuccessful.
               
            --  Register_Good_Move (Best_Move, Max_Depth);
            
            Register_As_Successful (Best_Move);
            
            for Index in 0 .. Moves_Searched - 1 loop
               History_Move := Moves_Played (Index);
               if Move_Is_Quiet (History_Move) and then History_Move /= Best_Move then
                  Register_As_Unsuccessful (History_Move);
               end if;
            end loop;
            
            Update_Killer_Move (Best_Move, Ply);
            
         end if;

         
         Evaluation := Evaluation_Type'(Score => Best_Score, Game_Phase => Match_Status);
         
         --------------------------------------
         -- Record Transposition Table Entry --
         --------------------------------------
         
         if Best_Score <= Alpha then
            Record_Transposition_Entry (Hash, Max_Depth, Ply, Evaluation, Empty_Move, Upper_Bound);
         elsif Best_Score >= Beta then
            Record_Transposition_Entry (Hash, Max_Depth, Ply, Evaluation, Best_Move, Lower_Bound);
         else
            Record_Transposition_Entry (Hash, Max_Depth, Ply, Evaluation, Best_Move, Exact_Entry);
         end if;

      end Alpha_Beta;
         
      return Evaluation;
      
   exception
      when Thinking_Time_Expired =>
         Rollback (Chessboard);
         raise;
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Principal_Variation_Search;
   
   
   
   ------------------------
   -- Zero_Window_Search --
   ------------------------
   
   function Zero_Window_Search 
     (Chessboard      : in out Chessboard_Type;
      Max_Depth       : in Depth_Type;
      Beta            : in Score_Type;
      Previous_Move   : in Move_Type;
      Allow_Null_Move : in Boolean) return Evaluation_Type
   is
      
      Depth : Depth_Type := Max_Depth;
      
      Alpha : constant Score_Type := Beta - 1;
      
      Move : Move_Type := Empty_Move;
      Moves_Searched : Natural := 0;
      Nb_Of_Legal_Moves : Natural := 0;
      
      Best_Move : Move_Type := Empty_Move;
      Best_Score : Score_Type := -Infinity;
      Match_Status : Match_Status_Type;
      Evaluation : Evaluation_Type;
      
      Static_Score : Score_Type := -Infinity;
      
      --  Razoring_Score : Score_Type;

      Hash_Move    : Move_Type := Empty_Move;
      
      Observe_Null_Move : Boolean;
      Null_Score   : Score_Type;
      Checkmate_Threat : Boolean := False;
      Null_Refutation_Move : Move_Type := Empty_Move;
      
      Extend_Search : Boolean := False;
      Reduce_Search : Boolean := False;
      
      Reduction_Factor : Depth_Type := 1;
      Avoid_Reduce : Boolean := False;

      Razoring_Score : Score_Type := 0;
        
      Futility_Pruning : Boolean := False;
      Futility_Score   : Score_Type := -Infinity;
      
      Escaping_From_Check : constant Boolean := Previous_Move.Check /= No_Check;
      
      TT_Entry         : Transposition_Table_Entry_Type;
      Hash_Score       : Score_Type := -Infinity;
      Transposition_Table_Hit : Boolean := False;
      Avoid_Null       : Boolean := False;
      
      Phase            : Stage_Type := Initial_Phase;
      
      Dummy            : Score_Type;
      
      Ply              : constant Depth_Type := Chessboard.Ply;
      
      Killer_1         : constant Move_Type := Principal_Variation (Ply).Killer_1;
      Killer_2         : constant Move_Type := Principal_Variation (Ply).Killer_2;
      
      
   begin
            
      Principal_Variation (Ply).Depth := Ply;
      
      -------------
      -- Horizon --
      -------------
      
      if Ply = Horizon then
         return Evaluate (Chessboard);
      end if;
      
      Search_Nodes := Search_Nodes + 1;
      
      
      ---------------------
      -- Draw Conditions --
      ---------------------
     
      if Draw_By_Threefold_Repetitions (Chessboard) then
         if Escaping_From_Check then
            return Evaluation_Type'(Score => Draw, Game_Phase => Draw_By_Perpetual_Check);
         else
            return Evaluation_Type'(Score => Draw, Game_Phase => Draw_By_Threefold_Repetitions);
         end if;
      elsif Chessboard.Fifty = 100 then
         return Evaluation_Type'(Score => Draw, Game_Phase => Draw_By_Fifty_Moves_Rule);
      end if;

      
      --------------------------
      -- Mate distance pruning --
      ---------------------------
      
      if Mate - Ply <= Alpha then
         return Evaluation_Type'(Score => Alpha, Game_Phase => Checkmate);
      elsif -Mate + Ply >= Beta then
         return Evaluation_Type'(Score => Beta, Game_Phase => Checkmate);
      end if;
      
      --------------------
      -- Time condition --
      --------------------
      
      if Time_Has_Up and then not Pondering then
         raise Thinking_Time_Expired;
      end if;
      
      ------------------
      -- Hash probing --
      ------------------

      TT_Entry := Probe_Transposition_Table (Hash);
      
      -- If the Transposition Table contains a valid entry, the content of the
      -- entry represent a previous search from the same chess position of this
      -- tree node. Therefore we can obtain the information that we need
      -- directly from this entry: We have to be aware that the entry could be
      -- stored after a shallower search and a further deeper search proved that
      -- the stored value is not
      
      if TT_Entry.Flag /= Empty_Slot then
      
         Hash_Move := TT_Entry.Move;
         Hash_Score := Adjust_From_Mate (TT_Entry.Evaluation.Score, Ply);
      
         if TT_Entry.Depth >= Max_Depth then
      
            Evaluation := Evaluation_Type'
              (Score => Hash_Score, Game_Phase => TT_Entry.Evaluation.Game_Phase);
      
            if TT_Entry.Flag = Exact_Entry then
               Principal_Variation (Ply).Current_Move := Hash_Move;
               return Evaluation;
            elsif TT_Entry.Flag = Lower_Bound and then Hash_Score >= Beta then
               Principal_Variation (Ply).Current_Move := Hash_Move;
               return Evaluation;
            elsif TT_Entry.Flag = Upper_Bound and then Hash_Score <= Alpha then
               -- The default implementaion does not record a move into the
               -- transposition table if the move is an upper bound. This can
               -- be modified by storing also moves that do not exceed alpha.
               -- This is not suggested as those moves are only going to pollute
               -- the hash table by giving no benefit
               Principal_Variation (Ply).Current_Move := Hash_Move;
               return Evaluation;
            end if;
      
         end if;

         Transposition_Table_Hit := True;
      end if;
           
      
      -----------------------
      -- Quiescence search --
      -----------------------
      
      if Max_Depth = Zero_Depth then
         return Quiescence_Search (Chessboard, Alpha, Beta, Previous_Move);
      end if;
      
      ------------------
      -- Static Score --
      ------------------
      
      Evaluation := Evaluate (Chessboard);
      Static_Score := Evaluation.Score;
      Match_Status := Evaluation.Game_Phase;
      
      if Match_Status = Draw_By_Insufficient_Material then
         return Evaluation;
      end if;
      
      
      ---------------------------
      -- Null Move Observation --
      ---------------------------
      
      if Allow_Null_Move
        and then Depth >= Null_Move_Threshold
        and then not Is_Mate (Beta)
        and then Do_Null_Move (Chessboard)
        and then not Avoid_Null
        and then Static_Score >= Beta
      then
      
         pragma Assert (not Escaping_From_Check, "Cannot call null move while under check");
      
         Principal_Variation (Ply).Current_Move := Empty_Move;
      
         Null_Score := Null_Move_Search (Chessboard, Max_Depth, Beta);
         
         if Null_Score >= Beta then
            return Evaluation;
         end if;
         
         -------------------------------------------------
         -- Record the Move that refutate the null move --
         -------------------------------------------------
      
         --  Null_Refutation_Move := Principal_Variation (Ply + 1).Current_Move;
         
         --  Checkmate_Threat := Is_Losing_Mate (Null_Score);
      
         -- If the null move failed low in a reduced search, there might be some
         -- kind of threat. Trigger the re-search by failing low here which in
         -- turn will makes a fail high
         --  if Checkmate_Threat and then Principal_Variation (Ply - 1).Reduced then
         --     return Evaluation_Type'(Alpha, In_Progress);
         --  end if;
      
      --  else
      --  
      --     if Depth < Null_Move_Threshold and then not Escaping_From_Check and then Static_Score < Beta - Razoring_Margin then
      --        Razoring_Score := Quiescence_Search (Chessboard, Alpha, Beta, Previous_Move).Score;
      --        if Razoring_Score < Beta then
      --           return Evaluation_Type'(Razoring_Score, In_Progress);
      --        end if;
      --     end if;
      
      end if;
      
      
      ---------------------------------------------
      -- Beta Pruning (Reverse Futility Pruning) --
      ---------------------------------------------
      
      --  if Max_Depth = Frontier_Depth and then not Escaping_From_Check then
      --     if Static_Score - Reverse_Futility_Margin >= Beta then
      --        return Evaluation_Type'
      --          (Score => Static_Score - Reverse_Futility_Margin, Game_Phase => Match_Status);
      --     end if;
      --  end if;
      
      -----------------------------
      -- Futility Pruning (Flag) --
      -----------------------------
     
      Futility_Score := Static_Score + Futility_Margin;
      
      Futility_Pruning := False;
      if not Escaping_From_Check and then Max_Depth = Frontier_Depth then
         Futility_Pruning := Futility_Score <= Alpha;
      end if;
      
      
      ---------------------
      -- Move generation --
      ---------------------

      Chessboard.Generate_Moves;
      Nb_Of_Legal_Moves := Chessboard.Moves_Counter (Ply);
      
      Alpha_Beta : declare
         Moves_List : Search_Move_List_Type := Create_Move_Picker (Chessboard, Cut_Node, Hash_Move, Previous_Move);
         History_Move : Move_Type := Empty_Move;
         Moves_Played : Moves_List_Type (0 .. Moves_List'Last) := (others => Empty_Move);
      begin
                     
         for I in Moves_List'Range loop
            Move := Pick_Next_Move (Moves_List, Phase, I);
            
            Principal_Variation(Ply).Current_Move := Move;
            
            ----------------------
            -- Futility_Pruning --
            ----------------------
            
            if Futility_Pruning and then Phase = Quiet and then Futility_Score <= Alpha then
               if Futility_Score > Best_Score then
                  Best_Score := Futility_Score;
               end if;
            end if;
            
            exit when Futility_Pruning and then Phase = Quiet;
            
            Principal_Variation (Ply).Current_Move := Move;
            
            -------------------------
            -- Checkmate detection --
            -------------------------
            
            if Move.Check = Checkmate then
               Evaluation := Evaluation_Type'(Score => Mate - Ply, Game_Phase => Checkmate);
               Principal_Variation (Ply + 1).Depth := Ply + 1;
               goto Move_Played_So_Far;
            end if;
            
            ----------------------
            -- Search Extension --
            ----------------------
            
            Extend_Search := False;
            Reduce_Search := False;
            
            if Move.Check /= No_Check then
               Extend_Search := True;
            elsif Escaping_From_Check and then Nb_Of_Legal_Moves = 1 then -- single reply
               Extend_Search := True;
            end if;
            
            Depth := (if Extend_Search then Max_Depth else Max_Depth - 1);
           
           
            -------------------------
            -- Late Move Reduction --
            -------------------------
            
            if Depth >= Frontier_Depth
              and then not Extend_Search
              and then Phase = Quiet
              and then Moves_Searched >= Late_Move_Count
              and then History_Balance (Move) <= History_Bound
              --  and then Move.From /= Null_Refutation_Move.To
            then
               Reduce_Search := True;
            end if;
            
            Depth := (if Reduce_Search then Depth - Reduction_Factor else Depth);
            
            --  Principal_Variation (Ply).Reduced := Reduce_Search;
               
            
            ---------------------------
            -- Null Move Observation --
            ---------------------------
             
            Observe_Null_Move := Move_Is_Null_Ok (Move);
            
            ----------------------
            -- Recursive Search --
            ----------------------
                           
            Chessboard.Play (Move);
            
            Evaluation := -Zero_Window_Search (Chessboard, Depth, -(Beta - 1), Move, Observe_Null_Move);
            if Reduce_Search and then Evaluation.Score > Alpha then
               --  Principal_Variation (Ply).Reduced := False;
               Evaluation := -Zero_Window_Search (Chessboard, Max_Depth - 1, -(Beta - 1), Move, Observe_Null_Move);
            end if;
            
            Chessboard.Undo;
            
            << Move_Played_So_Far >>
            
            Moves_Played (Moves_Searched) := Move;
            Moves_Searched := Moves_Searched + 1;
                        
            ---------------------
            -- Decisional Tree --
            ---------------------
            
            << Decisional_Tree >>
                        
            if Evaluation.Score > Best_Score then
               Best_Score := Evaluation.Score;
               if Evaluation.Score > Alpha then
                  Best_Move := Move;
                  --  exit when Best_Score >= Beta;
                  --  Update_Principal_Variation (Move, Ply, Evaluation);
                  --  Match_Status := Evaluation.Game_Phase;
               end if;
            end if;

            exit when Best_Score >= Beta;
            exit when Move.Check = Checkmate;
            
            << Moves_Loop_End >>
            
         end loop;
         
         --------------------
         -- Draw Condition --
         --------------------
         
         if Nb_Of_Legal_Moves = 0 then
            return Evaluation_Type'(Score => Draw, Game_Phase => Draw_By_Stalemate);
         end if;
         
         -------------------------
         -- Update History Data --
         -------------------------
                  
         if Best_Score >= Beta and then not Move_Is_Capture (Best_Move) then
               
            pragma Assert (Best_Move /= Empty_Move, "Found a best score but no associated best move!");
            
            -- If the move that caused a beta cut-off is a non-capturing move
            -- then register it as a successful move. All the other quiet moves
            -- tried so far are unsuccessful.

            --  Register_Good_Move (Best_Move, Max_Depth);
               
            Register_As_Successful (Best_Move);
            
            for Index in 0 .. Moves_Searched - 1 loop
               History_Move := Moves_Played (Index);
               if Move_Is_Quiet (History_Move) and then History_Move /= Best_Move then
                  Register_As_Unsuccessful (History_Move);
               end if;
            end loop;
            
            Update_Killer_Move (Best_Move, Ply);
            
         end if;
         
         Evaluation := Evaluation_Type'(Score => Best_Score, Game_Phase => Match_Status);
         
         --------------------------------------
         -- Record Transposition Table Entry --
         --------------------------------------
         
         if Best_Score <= Alpha then
            Record_Transposition_Entry (Hash, Max_Depth, Ply, Evaluation, Empty_Move, Upper_Bound);
         else
            Record_Transposition_Entry (Hash, Max_Depth, Ply, Evaluation, Best_Move, Lower_Bound);
         end if;
         
      end Alpha_Beta;
      
      return Evaluation;   
      
   exception
      when Thinking_Time_Expired =>
         Rollback (Chessboard);
         raise;
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Zero_Window_Search;
   
   
   ------------------------------------------
   -- Require_Zugzwang_Verification_Search --
   ------------------------------------------
   
   function Require_Zugzwang_Verification_Search
     (Chessboard          : in Chessboard_Type;
      Depth               : in Depth_Type) return Boolean
   is
      
      White_Pieces  : Pieces_List_Type renames Chessboard.Pieces_List (White_Pieces_Range);
      Black_Pieces  : Pieces_List_Type renames Chessboard.Pieces_List (Black_Pieces_Range);
      
      function Count_Non_Pawn (Side_To_Mode : Color_Type) return Piece_Counter_Type is
         Non_Pawn : Natural := 0;
      begin
         if Side_To_Mode = White then
            for Square of reverse White_Pieces loop
               if Chessboard.Square (Square) in White_Non_Pawn then
                  Non_Pawn := Non_Pawn + 1;
               end if;
               exit when Non_Pawn > 1;
            end loop;
         else
            for Square of reverse Black_Pieces loop
               if Chessboard.Square (Square) in Black_Non_Pawn then
                  Non_Pawn := Non_Pawn + 1;
               end if;
               exit when Non_Pawn > 1;
            end loop;
         end if;
         return Non_Pawn;
      end Count_Non_Pawn;
      
   begin
      return Depth > Null_Move_Verification_Threshold
        and then Count_Non_Pawn (Chessboard.Side_To_Move) <= 1; -- At most one piece (except king)
   end Require_Zugzwang_Verification_Search;
   
   
   ----------------------
   -- Null_Move_Search --
   ----------------------
   
   function Null_Move_Search 
     (Chessboard          : in out Chessboard_Type;
      Max_Depth           : in Depth_Type;
      Beta                : in Score_Type) 
      return Score_Type
   is
      Evaluation : Evaluation_Type;
      Null_Score : Score_Type renames Evaluation.Score;
   begin
      
      Chessboard.Play_Null_Move;
      Evaluation := -Zero_Window_Search (Chessboard, Max_Depth - R - 1, -Beta + 1, Empty_Move, False);
      Chessboard.Undo_Null_Move;
      
      if Null_Score >= Beta and then Require_Zugzwang_Verification_Search (Chessboard, Max_Depth) then
         Null_Score := Verify_Null_Move_Search (Chessboard, Max_Depth - R - 1 - 1, Beta);
      end if;
      
      return Null_Score;
   end Null_Move_Search;
   
   
   
   -----------------------------
   -- Verify_Null_Move_Search --
   -----------------------------
   
   function Verify_Null_Move_Search
     (Chessboard          : in out Chessboard_Type;
      Max_Depth           : in Depth_Type;
      Beta                : in Score_Type) return Score_Type
   is
      Depth                    : Depth_Type := Max_Depth;
      Alpha                    : constant Score_Type := Beta - 1;
   
      Move                     : Move_Type := Empty_Move;
      Nb_Of_Legal_Moves        : Natural := 0;
   
      Best_Score               : Score_Type := -Infinity;
      Evaluation               : Evaluation_Type;
   
      Extend_Search            : Boolean := False;
   
      Transposition_Table_Entry : Transposition_Table_Entry_Type;
      Hash_Move                : Move_Type := Empty_Move;
   
      Phase                    : Stage_Type := Initial_Phase;
   
      Ply                       : Depth_Type renames Chessboard.Ply;
   
   begin
   
      pragma Assert
        (not Chessboard.Has_King_In_Check (Chessboard.Side_To_Move),
         "Null-Move Verification called when King in check");
      
      -----------------------
      -- Quiescence search --
      -----------------------
      
      if Max_Depth = Zero_Depth then
         return Quiescence_Search (Chessboard, Alpha, Beta, Empty_Move).Score;
      end if;
      
      Search_Nodes := Search_Nodes + 1;
   
      Transposition_Table_Entry := Probe_Transposition_Table (Hash);
      if Transposition_Table_Entry.Depth >= Depth then
         Hash_Move := Transposition_Table_Entry.Move;
      end if;
   
      ---------------------
      -- Move generation --
      ---------------------
   
      Chessboard.Generate_Moves;
      Nb_Of_Legal_Moves := Chessboard.Moves_Counter (Ply);
   
      Alpha_Beta : declare
         Moves_List : Search_Move_List_Type := Create_Move_Picker (Chessboard, Cut_Node, Hash_Move, Empty_Move);
      begin
   
         for I in Moves_List'Range loop
            Move := Pick_Next_Move (Moves_List, Phase, I);
   
            Principal_Variation (Ply).Current_Move := Move;
   
            -------------------------
            -- Checkmate detection --
            -------------------------
   
            if Move.Check = Checkmate then
               Evaluation := Evaluation_Type'(Score => Mate - Ply, Game_Phase => Checkmate);
               Principal_Variation (Ply + 1).Depth := Ply + 1;
               goto Decisional_Tree;
            end if;
   
            -----------------------------
            -- Search depth extensions --
            -----------------------------
   
            Extend_Search := False;
   
            if Move.Check in Direct_Check | Discovery_Check | Double_Check then
               Extend_Search := True;
            elsif Nb_Of_Legal_Moves = 1 then
               Extend_Search := True;
            end if;
   
            Depth := (if Extend_Search then Max_Depth else Max_Depth - 1);
   
            ------------
            -- Search --
            ------------
   
            Chessboard.Play (Move);
            Evaluation := -Zero_Window_Search (Chessboard, Depth, -Alpha, Move, False);
            Chessboard.Undo;
   
            << Decisional_Tree >>
   
            ---------------------
            -- Decisional Tree --
            ---------------------
   
            if Evaluation.Score > Best_Score then
               Best_Score := Evaluation.Score;
            end if;
   
            exit when Best_Score >= Beta;
            exit when Move.Check = Checkmate;
         end loop;
   
         --------------------
         -- Draw Condition --
         --------------------
   
         if Nb_Of_Legal_Moves = 0 then
            return Draw;
         end if;
   
      end Alpha_Beta;
      
      return Best_Score;
   exception
      when Thinking_Time_Expired =>
         Rollback (Chessboard);
         raise;
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Verify_Null_Move_Search;

   
   -----------------------
   -- Quiescence_Search --
   -----------------------

   function Quiescence_Search
     (Chessboard      : in out Chessboard_Type;
      Alpha, Beta     : in Score_Type;
      Previous_Move   : in Move_Type) return Evaluation_Type
   is
   
      Move           : Move_Type;
      Alpha_Score    : Score_Type := Alpha;
      
      Static_Score   : Score_Type;
      Evaluation     : Evaluation_Type;
      
      Nb_Of_Tactical_Moves : Natural;
      
      Escaping_From_Check : constant Boolean := Previous_Move.Check /= No_Check;
      
      Hash_Move : Move_Type := Empty_Move;
      TT_Entry     : Transposition_Table_Entry_Type;
      
      Phase             : Stage_Type := Initial_Phase;
          
      Type_Of_Node : constant Tree_Node_Type := (if Beta = Alpha + 1 then Cut_Node else Pv_Node);

      Ply                 : constant Depth_Type := Chessboard.Ply;
      
      Best_Score          : Score_Type := -Mate + Ply;     
      Match_Status        : Match_Status_Type;
     
   begin
      
      Principal_Variation (Ply).Depth := Ply;
      
      Search_Nodes := Search_Nodes + 1;
      Qnodes := Qnodes + 1;
      
      ---------------------
      -- Draw Conditions --
      ---------------------
     
      if Draw_By_Threefold_Repetitions (Chessboard) then
         if Escaping_From_Check then
            return Evaluation_Type'(Score => Draw, Game_Phase => Draw_By_Perpetual_Check);
         else
            return Evaluation_Type'(Score => Draw, Game_Phase => Draw_By_Threefold_Repetitions);
         end if;
      elsif Chessboard.Fifty = 100 then
         return Evaluation_Type'(Score => Draw, Game_Phase => Draw_By_Fifty_Moves_Rule);
      end if;
     
      -------------
      -- Horizon --
      -------------
      
      Evaluation := Evaluate (Chessboard);
      Static_Score := Evaluation.Score;
      Match_Status := Evaluation.Game_Phase;

      if Ply >= Horizon then
         return Evaluation;
      end if;

      ---------------------------
      -- Mate distance pruning --
      ---------------------------
      
      if Mate - Ply <= Alpha then
         return Evaluation_Type'(Score => Alpha, Game_Phase => Checkmate);
      elsif -Mate + Ply >= Beta then
         return Evaluation_Type'(Score => Beta, Game_Phase => Checkmate);
      end if;
      
      --------------
      -- Standpat --
      --------------
      
      if not Escaping_From_Check then

         Best_Score := Static_Score;
      
         if Best_Score > Alpha_Score then
            Alpha_Score := Best_Score;
            if Best_Score >= Beta then
               return Evaluation_Type'(Score => Best_Score, Game_Phase => In_Progress);
            end if;
         end if;
         
      end if;
      
      ------------------
      -- Hash probing --
      ------------------

      TT_Entry := Probe_Transposition_Table (Hash);
      if TT_Entry.Flag /= Empty_Slot then
         Hash_Move := TT_Entry.Move;
      end if;
         
      ---------------------
      -- Move generation --
      ---------------------
      
      Chessboard.Generate_Tactical_Moves;
      Nb_Of_Tactical_Moves := Chessboard.Moves_Counter (Ply);
      
      -----------------------
      -- Quiescence Search --
      -----------------------
         
      Alpha_Beta : declare
         Moves_List : Search_Move_List_Type := Create_Move_Picker (Chessboard, Type_Of_Node, Hash_Move, Previous_Move);
      begin
         
         for I in Moves_List'Range loop
            Move := Pick_Next_Move (Moves_List, Phase, I);
                        
            Principal_Variation (Ply).Current_Move := Move;
            
            pragma Assert (Escaping_From_Check or else Move_Is_Tactical (Move), "Quiescence generated non-tactical move!");
            
            -------------------------
            -- Checkmate detection --
            -------------------------
            
            if Move.Check = Checkmate then
               Evaluation := Evaluation_Type'(Score => Mate - Ply, Game_Phase => Checkmate);
               Principal_Variation (Ply + 1).Depth := Ply + 1;
               goto Decisional_Tree;
            end if;
            
            ------------
            -- Search --
            ------------
            
            --  if Move.Captured /= Empty or else Move.Check /= No_Check then
            if Move.Captured /= Empty and then Static_Exchange_Evaluation_Score (Chessboard, Move) >= 0 then
               Chessboard.Play (Move);
               Evaluation := -Quiescence_Search (Chessboard, -Beta, -Alpha_Score, Move);
               Chessboard.Undo;
            end if;
            --  end if;

            -------------------
            -- Decisional Tree --
            ---------------------
            
            << Decisional_Tree >>
            
            if Evaluation.Score > Best_Score then
               Best_Score := Evaluation.Score;
               if Evaluation.Score > Alpha_Score then
                  Alpha_Score := Evaluation.Score;
                  exit when Best_Score >= Beta;
                  Update_Principal_Variation (Move, Ply, Evaluation);
                  Match_Status := Evaluation.Game_Phase;
               end if;
            end if;

            << End_Of_Loop >>
            
            exit when Best_Score >= Beta;
            exit when Move.Check = Checkmate;
            
         end loop;
         
      end Alpha_Beta;
      
      -- The Quiescence search only for tactical moves and escape from check
      -- pruning all moves with negative SEE. Since not all legal moves are
      -- generated, there is no point in testing for stalemante as it is done in
      -- other search routines.
      
      return Evaluation_Type'(Score => Best_Score, Game_Phase => Match_Status);
      
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Quiescence_Search;
   
        
   ------------------
   -- Do_Null_Move --
   ------------------
   
   function Do_Null_Move
     (Chessboard : in Chessboard_Type) return Boolean
   is
      Side_To_Move : Color_Type renames Chessboard.Side_To_Move;
   begin
      return Has_Non_Pawn (Chessboard, Side_To_Move);
   end Do_Null_Move;
   
   
   ---------------------
   -- Move_Is_Null_Ok --
   ---------------------

   function Move_Is_Null_Ok (Move : in Move_Type) return Boolean is
   begin
      return Move.Check = No_Check
        and then Move.Flag in Standard_Move | Pawn_Move_Two_Square
        and then Move.Captured = Empty;
        --  and then Move.Promotion not in Queen_Type;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Move_Is_Null_Ok;
   
   
   ------------------
   -- Has_Non_Pawn --
   ------------------
   
   function Has_Non_Pawn (Chessboard : in Chessboard_Type; Side : in Color_Type) return Boolean is
      White_Pieces : Pieces_List_Type renames Chessboard.Pieces_List (White_Pieces_Range);
      Black_Pieces : Pieces_List_Type renames Chessboard.Pieces_List (Black_Pieces_Range);
   begin
      if Side = White then
         return (for some Square of White_Pieces => Chessboard.Square (Square) in White_Non_Pawn);
      else
         return (for some Square of Black_Pieces => Chessboard.Square (Square) in Black_Non_Pawn);
      end if;
   end Has_Non_Pawn;
   
   
   -------------------------
   -- Move_Is_A_Recapture --
   -------------------------
   
   function Move_Is_A_Recapture (Move, Previous_Move : in Move_Type) return Boolean is
   begin
      return Previous_Move.Captured /= Empty 
        and then Move.To = Previous_Move.To 
        and then Move.Captured = Previous_Move.Piece;
   end Move_Is_A_Recapture;

   
   ------------------------------
   -- Move_Is_Passed_Pawn_Push --
   ------------------------------
   
   function Move_Is_Passed_Pawn_Push (Chessboard : in Chessboard_Type; Move : in Move_Type) return Boolean is
      Front_Spawn : Square_Type := Move.To;
      East_Spawn  : Square_Type := Move.To + East;
      West_Spawn  : Square_Type := Move.To + West;
      Pawn_File   : constant Coordinate_Type := File (Move.To);
   begin
   
      case Move.Piece is
         when White_Pawn =>
            -- Search for opponent pawn in the whole front spawn.
            loop
               Front_Spawn := Front_Spawn + North;
               East_Spawn := East_Spawn + North;
               West_Spawn := West_Spawn + North;
   
               exit when Chessboard.Square (Front_Spawn) = Frame; -- same will be for east and west spawn
   
               if Chessboard.Square (Front_Spawn) = Black_Pawn then
                  return False;
               end if;
   
               if Pawn_File /= File_H and then Chessboard.Square (East_Spawn) = Black_Pawn then
                  return False;
               end if;
   
               if Pawn_File /= File_A and then Chessboard.Square (West_Spawn) = Black_Pawn  then
                  return False;
               end if;
   
            end loop;
   
            -- No opponent pawn found on our way. This is a passed pawn!
            return True;
   
         when Black_Pawn =>
             -- Search for opponent pawn in the whole front spawn.
            loop
               Front_Spawn := Front_Spawn + South;
               East_Spawn := East_Spawn + South;
               West_Spawn := West_Spawn + South;
   
               exit when Chessboard.Square (Front_Spawn) = Frame; -- same will be for east and west spawn
   
               if Chessboard.Square (Front_Spawn) = White_Pawn then
                  return False;
               end if;
   
               if Pawn_File /= File_H and then Chessboard.Square (East_Spawn) = White_Pawn then
                  return False;
               end if;
   
               if Pawn_File /= File_A and then Chessboard.Square (West_Spawn) = White_Pawn  then
                  return False;
               end if;
   
            end loop;
   
            -- No opponent pawn found on our way. This is a passed pawn!
            return True;
   
         when others => null;
      end case;
   
      return False;
   end Move_Is_Passed_Pawn_Push;
   
 
   
end Chess.Engine.Search.PV;
