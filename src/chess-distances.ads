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


package Chess.Distances is

   -----------------------------------------
   -- Work with distances between squares --
   -----------------------------------------

   subtype Distance_Type is Natural range 0 .. 9;
   -- A distance represent the number of moves a piece require to reach a certain
   -- square from his current position. The distance is just a number
   -- Type used to count the number of moves required to reach a certain square.
   -- If a piece cannot reach a square (example, a bishop on white squares
   -- cannot reach any of the black squares, or a pawn cannot reach squares
   -- that are not in its trajectory) then a special Unreachable value shall be.
   -- Further value is required to consider a Tempo
   
   No_Distance : constant Distance_Type := 0;
   Unreachable : constant Distance_Type := Distance_Type'Last;

   
end Chess.Distances;
