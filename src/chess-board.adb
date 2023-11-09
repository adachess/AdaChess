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


package body Chess.Board is
   
   ----------
   -- File --
   ----------

   function File (Square : in Square_Type) return Coordinate_Type is
   begin
      --  return (Square mod 10) - 1; -- Alternative version, a bit slower but less memory-usage
      return Files (Square);
   end File;


   ----------
   -- Rank --
   ----------

   function Rank (Square : in Square_Type) return Coordinate_Type is
   begin
      return 10 - (Square / 10);
      --  return Ranks (Square); -- Alternative version. A bit slower and more memory-usage
   end Rank;


   --------------
   -- Diagonal --
   --------------

   function Diagonal (Square : in Square_Type) return Coordinate_Type is
   begin
      return Diagonals (Square);
   end Diagonal;


   -------------------
   -- Anti_Diagonal --
   -------------------

   function Anti_Diagonal (Square : in Square_Type) return Coordinate_Type is
   begin
      --  return Rank(Square) + File(Square) - 1;
      return Anti_Diagonals (Square);
   end Anti_Diagonal;
   
   
end Chess.Board;
