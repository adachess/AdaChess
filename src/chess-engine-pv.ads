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


with Chess.Moves.Annotations; use Chess.Moves.Annotations;
with Chess.Engine.Evaluations; use Chess.Engine.Evaluations;

package Chess.Engine.PV is

   Multi_Pv : Positive := 1;
   
   -------------------------
   -- Principal Variation --
   -------------------------

   --  type Collection_Type is array (Depth_Type'Range, Depth_Type'Range) of Move_Type;
   --  type Collection_Depth_Type is array (Depth_Type'Range) of Depth_Type;
   type Collection_Type is array (Depth_Type'Range) of Annotated_Move_Type;
   type Mate_Killer_Type is array (Depth_Type'Range) of Move_Type;

   
   type Principal_Variation_Data_Type is 
      record
         Main_Line             : Collection_Type;
         Depth                 : Depth_Type;
         Current_Move          : Move_Type;
         Killer_1              : Move_Type;
         Killer_2              : Move_Type;
         Evaluation            : Evaluation_Type;
         Predicted_Countermove : Move_Type;
      end record;
   
   type Principal_Variation_Type is array (Depth_Type'Range) of Principal_Variation_Data_Type;
         
 
   Principal_Variation : Principal_Variation_Type;
   First_Variation     : Principal_Variation_Type;
   First_Variation_Depth : Depth_Type;
      

   procedure Update_Principal_Variation
     (Move : in Move_Type;
      Ply  : in Depth_Type;
      Evaluation : in Evaluation_Type;
      Annotation : in Annotation_Type := None)
     with 
       Inline => True;
   -- Update the main line of the triangular Principal Variation with the given
   -- move at the given Ply
   --
   -- Arguments
   --    Move : The move that represent the new main line
   --    Ply  : The ply where the move has to be saved as new main line
   -- Aspects
   --    Inline
   
   procedure Update_Killer_Move (Move : in Move_Type; Ply : in Depth_Type)
     with
       Inline => True,
       Pre => Move /= Empty_Move and not Move_Is_Capture (Move);
   -- Assign this move to the Killer-Moves list for the current main line. Note
   -- that a killer move is always a quiet move as for captures and checks
   -- we rely on other heuristic
   -- 
   -- Arguments 
   --    Move : The killer move found
   --    Ply  : The ply where the move represent a killer move
   -- Aspects
   --    Inline
   --    Precondition : The move is a valid move and is a quiet move
   
   procedure Print_Principal_Variation 
     (Chessboard   : in Chessboard_Type;
      Search_Depth : in Depth_Type;
      Thinked_Time : in Duration;
      Ponder_Move  : Move_Type := Empty_Move);
   -- Print the current main line of the principal variation in the console, with
   -- some further information of interest (calculating time, nodes searched and
   -- so on).
   --
   -- Arguments
   --    Search_Depth : The depth search in the search tree
   --    Thinked_Time : The amount of time the engine spent thinking for the 
   --                   current move
   --    Ponder_Move  : The move the engine is pondering, if in ponder mode
    

end Chess.Engine.PV;

