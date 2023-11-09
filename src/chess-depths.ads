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


package Chess.Depths is

   -- The History Depth is how many half-moves can we play from the
   -- beginning of the game. A game that is longer that this value
   -- will raise an overflow.
   subtype History_Depth_Type is Natural range 0 .. 512;

   -- The depth is how many deep we can go while thinking and playing moves
   -- It's very rare that we think deeper than 32 ply, so for moves list
   -- we can use a small subset and save memory (and a small improvement)
   subtype Depth_Type is History_Depth_Type range 1 .. 256;

   Zero_Depth         : constant Depth_Type := Depth_Type'First;
   Frontier_Depth     : constant Depth_Type := Zero_Depth + 1;
   Pre_Frontier_Depth : constant Depth_Type := Frontier_Depth + 1;

   Horizon            : constant Depth_Type := Depth_Type'Last - 1;
   Pre_Horizon        : constant Depth_Type := Horizon - 2;

end Chess.Depths;
