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


with Chess.Moves; use Chess.Moves;
with Chess.Hashes;  use Chess.Hashes;


package Chess.History is


   type History_Move_Type is
      record
         Move  : Move_Type;
         Hash  : Hash_Type;
         Fifty : Natural;
      end record;
   --  pragma Pack (History_Move_Type);

   -- override "=" comparator for moves type
   function "=" (Left, Right : in History_Move_Type) return Boolean is
     (Left.Move = Right.Move);

   Empty_History_Move : constant History_Move_Type :=
     History_Move_Type'
       (Move  => Empty_Move,
        Hash  => 0,
        Fifty => 0);

end Chess.History;
