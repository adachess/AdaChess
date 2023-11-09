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


package Chess.Engine.Perfts is

   ------------------
   -- Perft Search --
   ------------------

   -- Perft is a short for Performance Tester The perft can be used for both
   -- measuring the speed of the move generator and the correctness of it.
   -- AdaChess perft engine collects info while generating all legal moves,
   -- like captures, promotions, checks and so on.
   -- Type "perft 5" from any position, in console, to see them.
   type Perft_Node_Type is mod 2 ** 46; -- Handle up to perft 10 from starting position

   type Move_Path_Collector_Type is
      record
         Nodes            : Perft_Node_Type := 0;
         Captures         : Perft_Node_Type := 0;
         En_Passant       : Perft_Node_Type := 0;
         Castles          : Perft_Node_Type := 0;
         Promotions       : Perft_Node_Type := 0;
         Checks           : Perft_Node_Type := 0;
         Discovery_Checks : Perft_Node_Type := 0;
         Double_Checks    : Perft_Node_Type := 0;
         Checkmates       : Perft_Node_Type := 0;
      end record;

   Perft_Move_Paths : Move_Path_Collector_Type;


   procedure Perft (Chessboard : in out Chessboard_Type; Depth : in Depth_Type);
   -- Perform a Perft on a current position. The result is printed in a pretty
   -- print way. Perft is used for finding bugs and benchmarking the move
   -- generator.
   --
   -- Arguments:
   --    Chessboard : The current chess position
   --    Depth      : The depth to reach with the perft search

   procedure Divide (Chessboard : in out Chessboard_Type; Depth : in Depth_Type);
   -- Split the perft counter for each moves. Use this function when the perft
   -- value given by your engine does not match with the expected one.
   --
   -- Arguments:
   --    Chessboard : The current chess position
   --    Depth      : The depth to reach with the perft-divide search


end Chess.Engine.Perfts;
