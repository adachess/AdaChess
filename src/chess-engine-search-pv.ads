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


with Chess.Score;  use Chess.Score;
with Chess.Clocks; use Chess.Clocks;


with Chess.Engine.Search.Stages; use Chess.Engine.Search.Stages;
with Chess.Moves.Annotations; use Chess.Moves.Annotations;
with Chess.Engine.Evaluations; use Chess.Engine.Evaluations;
with Chess.Engine.Search.Heuristics; use Chess.Engine.Search.Heuristics;


package Chess.Engine.Search.PV is
   
   
   procedure Initialize_Search_Engine;
   -- Initialize all the data required for the searching engine
   -- This procedure shall be called once when the engine start
   

   -----------
   -- Clock --
   -----------

   Clock : Chess_Clock_Type; 
   
   function Time_Has_Up return Boolean is (Clock.Time_Has_Up);
   -- Detect whether the thinking time has expired
   --
   -- Returns
   --    True if the thinking time has expired, False otherwise
   
   
   type Moves_List_Type is array (Natural range <>) of Move_Type;
        
   
   type Search_Move_Type is
      record
         Move  : Move_Type;
         Score : Heuristic_Score; -- For scoring the move, we need a range wider than Score_Type'Range
         Phase : Stage_Type;
      end record;

   type Search_Move_List_Type is array (Natural range <>) of Search_Move_Type;
   
   
   ----------------------------------
   -- Center control and occupancy --
   ----------------------------------

   --  type Piece_Square_Table_Type is array (Board_Type'Range) of Score_Type;
   --  
   --  Occupancy_Of_The_Centre : constant Piece_Square_Table_Type :=
   --    (0,   0,   0,   0,   0,   0,   0,   0,   0, 0,
   --     0,   0,   0,   0,   0,   0,   0,   0,   0, 0,
   --     0, -20, -10, -10, -10, -10, -10, -10, -20, 0,
   --     0, -10,  -5,   0,   0,   0,   0,  -5, -10, 0,
   --     0, -10,   0,   5,   5,   5,   5,   0, -10, 0,
   --     0, -10,   0,   5,  10,  10,   5,   0, -10, 0,
   --     0, -10,   0,   5,  10,  10,   5,   0, -10, 0,
   --     0, -10,   0,   5,   5,   5,   5,   0, -10, 0,
   --     0, -10,  -5,   0,   0,   0,   5,  -5, -10, 0,
   --     0, -20, -10, -10, -10, -10, -10, -10, -20, 0,
   --     0,   0,   0,   0,   0,   0,   0,   0,   0, 0,
   --     0,   0,   0,   0,   0,   0,   0,   0,   0, 0);

   type Output_Mode_Type is (Silent, Verbose);
   Output_Mode : Output_Mode_Type := Verbose;


   --------------------------------
   -- Internal Iterative Deeping --
   --------------------------------

   IID_Depth : constant Depth_Type := 4;
   IID_Reduction : constant Depth_Type := 2;
   
   --------------
   -- Futility --
   --------------
   
   Futility_Margin : constant Score_Type := 110;   
   Delta_Margin : constant Score_Type := 100;
      
   ----------------------------
   -- Move Reduction Factors --
   ----------------------------

   Late_Move_Count         : constant Positive := 4;
   
   -----------------------
   -- Null move pruning --
   -----------------------
   
   R : constant Depth_Type := 3;
   -- Null Move reduction depth. The letter R is commonly used to represent the
   -- further reduction for a null move observation algorithm
   
   Null_Move_Threshold : constant Depth_Type := R + 2; -- Allow null move when Search Depth > Null_Move_Threshold
   Null_Move_Verification_Threshold : constant Depth_Type := R * 2; -- Perform a null move verification search, if the depth is bigger than this threshold
   

   function Move_Is_Null_Ok (Move : in Move_Type) return Boolean
     with
       Inline => True;
   -- Decide wether a move can be used for null-move observation
   -- 
   -- Arguments
   --    Move : The move to be investigated
   -- Returns
   --    True if the given move can be used for null-move observation, False
   --    otherwise
   -- Aspects
   --    Inline : This function is inlined

   function Has_Non_Pawn (Chessboard : in Chessboard_Type; Side : in Color_Type) return Boolean
     with
       Inline => True;
   -- Verify if the given side has pieces that are non-pawn. King are not 
   -- considered in the search. 
   -- 
   -- Arguments
   --    Chessboard : the current chess game
   --    Side       : the side to search for non-pawns
   -- Returns
   --    True if a non-pawn is found, False otherwise
   -- Aspects
   --    Inline : This function is inlined


   ---------------------------
   -- Extra time management --
   ---------------------------

   -- Save the root move of the principal variation
   -- and it's score and if it change then consider
   -- the idea to think more. Try to allocate and extra
   -- time binded to the score difference.
   Last_Best_Move : Move_Type;
   Last_Best_Score : Score_Type;

   -----------------------
   -- Count Repetitions --
   -----------------------

   type Repetition_Type is range 1 .. 3
     with
       Size => 2, 
       Default_Value => 1;

   Three_Fold_Repetitions_Count : constant Repetition_Type := 2;
   
   function Draw_By_Threefold_Repetitions (Chessboard : in Chessboard_Type) return Boolean;
   -- Detect if the current position represent a draw by threefold repetitions
   --
   -- Arguments
   --    Chessboard : The current chess position and data
   -- Returns
   --    The number of times the current position appears in the match

   --  function Count_Repetitions
   --    (Chessboard : in Chessboard_Type) return Repetition_Type;

   
   --------------------------------
   -- Principal Variation Search --
   --------------------------------
   
     
   procedure Rollback (Chessboard : in out Chessboard_Type);
   -- Restore the original situation up to the root of the search-tree. The
   -- rollback is made by undoing every move from the current tree node to
   -- the root.
   
   function Search_Root
     (Chessboard  : in out Chessboard_Type;
      Alpha, Beta : in Score_Type;
      Max_Depth   : in Depth_Type;
      Ponder_Move : in Move_Type := Empty_Move)
      return Evaluation_Type;
   -- This is the entry point of the serch. It is called root because it is
   -- a recursive tree search. It implements an AlphaBeta (Negamax) search tree
   -- algorithm
   --
   -- Arguments
   --    Alpha, Beta : The bound of the search window 
   --    Max_Depth   : The depth limit where the recursion shall not search further
   --    Search_Mode : A flag that specify which search mode shall be used
   --    Ponder_Move : The current move the engine is pondering on, if any
   -- Returns
   --    The score retrieved from the main line
   
   function Principal_Variation_Search
     (Chessboard      : in out Chessboard_Type;
      Max_Depth       : in Depth_Type;
      Alpha, Beta     : in Score_Type;
      Previous_Move   : in Move_Type) 
      return Evaluation_Type;
   -- Search the Principal Variation, following the main line. If the Multi_Pv
   -- value is set to any number greater than 1, the engine will search multiple 
   -- moves in a PV mode
   --
   -- Arguments
   --    Max_Depth     : The depth limit where the recursion shall not search further
   --    Alpha, Beta   : The bound of the search window
   --    Previous_Move : The move just played on the search tree, used to obtain
   --                    information about the check status
   -- Returns
   --    The score retrieved from the main line
   
   function Zero_Window_Search 
     (Chessboard      : in out Chessboard_Type;
      Max_Depth       : in Depth_Type;
      Beta            : in Score_Type;
      Previous_Move   : in Move_Type;
      Allow_Null_Move : in Boolean) 
      return Evaluation_Type;
   -- Zero-Window based search, used for non-pv nodes
   -- 
   -- Arguments
   --    Max_Depth       : The depth limit where the recursion shall not search further
   --    Beta            : The upper bound of the search window
   --    King_In_Check   : The condition on which the last move checked the king
   --                      that now is on the side to move
   --    Allow_Null_Move : A flag that allows or impede the null move observation
   -- Returns
   --    The score retrieved from the main line
   
   function Null_Move_Search 
     (Chessboard          : in out Chessboard_Type;
      Max_Depth           : in Depth_Type;
      Beta                : in Score_Type) 
      return Score_Type;
   
   function Verify_Null_Move_Search
     (Chessboard : in out Chessboard_Type;
      Max_Depth  : in Depth_Type;
      Beta       : in Score_Type)
   return Score_Type;
   -- Used to perform a Zugzwang verification search if the null move fails high.
   -- This function shall be called only on non-pv nodes
   --
   -- Arguments
   --    Max_Depth : The depth limit where the recurion shall not search further
   --    Beta      : The upper bound of the search window
   -- Returns
   --    The score retrieved from the main line
   
   function Quiescence_Search
     (Chessboard      : in out Chessboard_Type;
      Alpha, Beta     : in Score_Type;
      Previous_Move   : in Move_Type)
      return Evaluation_Type;
   -- Search function used to stabilize the tree and reduce the impact of the 
   -- so-called horizon effect, caused by the limitation of the search.
   -- In this search, the engine tries to find a stable position by only searching
   -- for tactical moves.
   -- See: https://www.chessprogramming.org/Horizon_Effect
   -- AdaChess makes here extensive use of the check-investigation algorithm
   -- making this quiescence search very smart
   --
   -- Arguments
   --    Max_Depth     : The depth limit where the recursion shall not search further
   --    Alpha, Beta   : The bound of the search window
   --    King_In_Check : The condition on which the last move checked the king
   --                    that now is on the side to move
   -- Returns
   --    The score retrieved so far from the main line
   
   
   function Do_Null_Move
     (Chessboard : in Chessboard_Type) return Boolean
     with
       Inline => True;
   -- Verufy that the side to move has at least one non-pawn piece. If
   -- have any pieces other than pawns
   --
   -- Returns
   --    True if the engine can observe the null move, False otherwise
   -- Aspects
   --    Inline
   
   function Require_Zugzwang_Verification_Search
     (Chessboard          : in Chessboard_Type;
      Depth               : in Depth_Type) return Boolean
     with
       Inline => True;
   -- Test whether the null move observation shall peform a Zugzwang verification
   -- or not. 
   --
   -- Arguments
   --    Depth               : The current remaining search depth
   --    Escaping_From_Check : This parameter tell us if the current position is
   --                          an escape from check
   -- Returns
   --    True if the Zugzwang verification is needed, False otherwise
      
   ----------------
   -- Move Order --
   ----------------
         
   function Create_Move_Picker
     (Chessboard   : in out Chessboard_Type;
      Type_Of_Node : in Tree_Node_Type;
      Hash_Move    : in Move_Type;
      Previous_Move : in Move_Type)
      return Search_Move_List_Type 
     with
       Inline => True;
   -- Purpose of the move-picker is to sort all the available move from the
   -- strongest one to the weakest one. We don't know which move is actually the
   -- best one - otherwise, we just make it! - but we can estimate it via the
   -- heuristic obtained in the Search.
   --
   -- Arguments
   --    Chessboard   : the current chess position and data
   --    Type_Of_Node : the classification of the node in the search tree
   --    Hash_Move    : the hashed move found for the current position, if any
   -- Returns
   --    A list of scored move, not sorted
   -- Aspects
   --    Inline : the function is inlined 
   
   function Pick_Next_Move
     (Moves_List : in out Search_Move_List_Type;
      Phase      : in out Stage_Type;
      From       : in Natural) 
      return Move_Type;
   -- Select the next move that the search engine shall play. The selection will
   -- take the higest scored move inside of the given phase. If no other move
   -- are available for the given phase, it searched in the next phase.
   -- Once a move is selected, it will be moved in the position targeted by the
   -- index "From". In this way, the next move can be searched by scanning only
   -- the remaining list instead of the entire list
   --
   -- Arguments
   --    Move_List : The list of all generated moves, scored and phased
   --    Phase     : The current move orderingp hase
   --    From      : The current index in the moves list
   -- Returns
   --    The move with the highest score in the current phase or in the next one

   
   function Annotate_Move (Move              : in Move_Type;
                           See               : in Score_Type;
                           Best_Score        : in Score_Type;
                           Ply               : in Depth_Type;
                           Recapture         : in Boolean;
                           Passed_Pawn_Push  : in Boolean;
                           Nb_Of_Legal_Moves : in Natural) return Annotation_Type;
   -- Annotate a move according to the value that it provides. The annotation of
   -- a move is done from the perspective of the engine. A move that leads to an
   -- importand advantage gain is worth to be annotated (and maybe investigate
   -- further). Annotation is done according to the following conditions:
   -- * Queen sacrifices that leads to an advantage or a mate are always annotate
   --   as brilliant move. The queen can sacrifice itself or can be a sacrifice
   --   due to a move ov another piece that let the queen to be captured
   -- * 
   --
   -- Note: in order to be correct, the annotation must be called BEFORE updating
   -- the principal variation. Since the algorithm reflect the difference in the
   -- value between the current (old) PV and the score given with the move (which
   -- is going to be part of the new PV).
   --
   -- Arguments
   --    Move              : The move that we want to annotate
   --    See               : The value of the Static Exchange Evaluation
   --    Best_Score        : The best score found in the search
   --    Ply               : The current search depth
   --    Recapture         : A flag that tell us if this move represent a recapture
   --    Nb_Of_Legal_Moves : The amount of legal moves in the position
   --
   -- Returns
   --    The annotation value for the given move

   --------------------
   -- Clear Euristic --
   --------------------

   procedure Initialize_Search_Heuristics;
   -- Initialize the heuristic data used during the search. This function shall
   -- be called once at startup
   
   procedure Clear_All_Euristics;
   -- Clear the heuristic build during the (previous) search
   
   procedure Clear_Principal_Variation;
   -- Delete the principal variation build during the (previous) search
   
private
   
   function Move_Is_Null_Move (Move : in Move_Type) return Boolean is
     (Move.Flag = Null_Move);
   
   function Move_Is_Advanced_Passed_Pawn_Push (Move : in Move_Type) return Boolean is
     (Move.Piece in Pawn_Type and then Move.To in Rank_7 | Rank_8 | Rank_2 | Rank_1 and then Move.Captured = Empty);
   
   function Move_Is_Passed_Pawn_Push
     (Chessboard : in Chessboard_Type; Move : in Move_Type) return Boolean;

   function Move_Is_Pawn_Push (Move : in Move_Type) return Boolean is
     (Move.Piece in Pawn_Type and then (Move.To = Move.From + North or else Move.To = Move.From + South or else Move.Flag = Pawn_Move_Two_Square));
   
   function Move_Is_A_Recapture 
     (Move, Previous_Move : in Move_Type)
      return Boolean
     with 
       Inline => True;
   -- Verify if this move represent a recaputre of a prievious piece just captured
   -- on the same square by the opponent player
   --
   -- Arguments
   --    Move          : The current move to test whether is a recapture or not
   --    Previous_Move : The last move made before the Move is going to be played
   -- Returns
   --    True if this is a recapturing move, False otherwise
   
   ---------------
   -- Heuristic --
   ---------------
   
   type Mvv_Lva_Score_Table_Type is array (Piece_Type'Range, Piece_Type'Range) of Score_Type;
   Mvv_Lva : Mvv_Lva_Score_Table_Type := (others => (others => 0));
   
   
   type MVV_Score_Table_Type is array (Piece_Type'Range) of Score_Type;
   Mvv : MVV_Score_Table_Type := (others => 0);
   
   
   type Piece_Score_Table_Type is array (Piece_Type'Range) of Score_Type;
   Piece_Score : constant Piece_Score_Table_Type :=
     (Frame      => 0, Empty => 0, -- Hack ;)
      White_Pawn => 100, White_Knight => 300, White_Bishop => 300,
      White_Rook => 500, White_Queen => 900, White_King => 32767,
      Black_Pawn => 100, Black_Knight => 300, Black_Bishop => 300,
      Black_Rook => 500, Black_Queen => 900, Black_King => 32767);
   
   MVV_LVA_Factor : constant Score_Type := 10;
   MVV_LVA_Bonus  : constant Score_Type := 1000;

   MVV_Factor : constant Score_Type := 10;
   MVV_Bonus  : constant Score_Type := 0;
   
   
   type Board_Center_Control_Type is array (Board_Type'Range) of Score_Type;

   Move_To_The_Center : constant Board_Center_Control_Type :=
     (0,   0,   0,   0,   0,   0,   0,   0,   0, 0,
      0,   0,   0,   0,   0,   0,   0,   0,   0, 0,
      0,   0,   1,   1,   1,   1,   1,   1,   0, 0,
      0,   1,   1,   2,   2,   2,   2,   1,   1, 0,
      0,   1,   2,   3,   3,   3,   3,   2,   1, 0,
      0,   1,   2,   3,   4,   4,   3,   2,   1, 0,
      0,   1,   2,   3,   4,   4,   3,   2,   1, 0,
      0,   1,   2,   3,   3,   3,   3,   2,   1, 0,
      0,   1,   1,   2,   2,   2,   2,   1,   1, 0,
      0,   0,   1,   1,   1,   1,   1,   1,   0, 0,
      0,   0,   0,   0,   0,   0,   0,   0,   0, 0,
      0,   0,   0,   0,   0,   0,   0,   0,   0, 0);
      
end Chess.Engine.Search.PV;
