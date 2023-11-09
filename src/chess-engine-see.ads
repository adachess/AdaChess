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


with Chess;
with Chess.Score; use Chess.Score;
with Chess.Engine; use Chess.Engine;


package Chess.Engine.See is

   type Capture_Result_Type is
     (Winning, Equal, Losing); 
   
   subtype See_Score_Type is Score_Type range -32767 .. +32767;
   -- A SEE cannot gain more material than the highest piece value. If the
   -- score assigned to the Queen changes, this value has to change too
     
   --------------------------------
   -- Static_Exchange_Evaluation --
   --------------------------------
   
   function Static_Exchange_Evaluation_Score 
     (Chessboard : in out Chessboard_Type; 
      Move       : in Move_Type)
      return See_Score_Type;
   -- Evaluate a sequence of moves towards a certain square (the destination
   -- square) to establish the material balance after the sequence of capture.
   -- A positive result indicates that the exchange sequece gain material, while
   -- a negative value indicates that material will be lost.
   -- Note that the very first move that trigger the SEE evaluation could also
   -- be a non-capturing move.
   --
   -- Arguments
   --    Chessboard : The Chess data where the SEE will be evaluated
   --    Move       : The Move that start the sequece, in the square designated
   --                 by the destination square of the given move
   -- Returns
   --    The score difference in the static exchange
   
   function Static_Exchange_Evaluation
     (Chessboard : in out Chessboard_Type; Move : in Move_Type)
      return Capture_Result_Type;
   -- Same as the Static_Exchange_Evaluation_Score but it will only inform if
   -- a move leads to a Winning capture sequece, a Losing one or an equal one
   --
   -- Arguments
   --    Chessboard : The Chess data where the SEE will be evaluated
   --    Move       : The Move that start the sequece, in the square designated
   --                 by the destination square of the given move
   -- Returns
   --    The result of the static exchange in terms of winning/equal/losing capture
   
   
   function Static_Recapture_Exchange_Evaluation 
     (Chessboard : in out Chessboard_Type; Recapture_Move, Previous_Move : in Move_Type)
      return Score_Type;
   -- Evaluate statically an exchange of material by evaluating a re-capture.
   
   
private
   
   type See_Piece_Score_Type is array (Piece_Type'Range) of Score_Type
     with 
       Default_Component_Value => 0;
   
   Piece_Score   : constant See_Piece_Score_Type :=
     (Frame      => -Infinity, Empty => 0, -- Hack ;)
      White_Pawn => 100, White_Knight => 300, White_Bishop => 300,
      White_Rook => 500, White_Queen => 900, White_King => 32767,
      Black_Pawn => 100, Black_Knight => 300, Black_Bishop => 300,
      Black_Rook => 500, Black_Queen => 900, Black_King => 32767);

end Chess.Engine.See;
