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


with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Numerics.Discrete_Random;

with Chess.Matches; use Chess.Matches;


package Chess.Books is

   subtype Count_Type is Ada.Containers.Count_Type;

   package Random_Opening is new Ada.Numerics.Discrete_Random (Count_Type);
   Seed_Generator : Random_Opening.Generator;

   type Book_Type is interface;

private

   package Book_Package is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Match_Type);

   Opening_Book : Book_Package.Vector; -- Contains all the opening books loaded

end Chess.Books;
