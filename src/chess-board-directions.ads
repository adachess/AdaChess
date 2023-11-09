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


package Chess.Board.Directions is

   -----------------------------------------------
   -- Offsets for moving piece around the board --
   -----------------------------------------------
   
   subtype Direction_Type is Integer range -21 .. +21;

   North            : constant Direction_Type := -10;
   North_North_East : constant Direction_Type := -19;
   North_East       : constant Direction_Type := -9;
   North_East_East  : constant Direction_Type := -8;
   East             : constant Direction_Type := +1;
   South_East_East  : constant Direction_Type := +12;
   South_East       : constant Direction_Type := +11;
   South_South_East : constant Direction_Type := +21;
   South            : constant Direction_Type := +10;
   South_South_West : constant Direction_Type := +19;
   South_West       : constant Direction_Type := +9;
   South_West_West  : constant Direction_Type := +8;
   West             : constant Direction_Type := -1;
   North_West_West  : constant Direction_Type := -12;
   North_West       : constant Direction_Type := -11;
   North_North_West : constant Direction_Type := -21;
   
   No_Direction     : constant Direction_Type := 0;
   
   -----------------------------------------------
   -- Offsets for moving piece around the board --
   -----------------------------------------------

   subtype Long_Offset_Type is Direction_Type range 1 .. 8;
   subtype Short_Offset_Type is Direction_Type range 1 .. 4;

   type Short_Direction_Offset is array (Short_Offset_Type) of Direction_Type;
   type Long_Direction_Offset is array (Long_Offset_Type) of Direction_Type;

   Knight_Offsets : constant array (Long_Offset_Type) of Direction_Type :=
     (North_North_East, North_East_East,
      South_East_East, South_South_East,
      South_South_West, South_West_West,
      North_West_West, North_North_West);

   Bishop_Offsets : constant Short_Direction_Offset :=
     (North_West, North_East, South_East, South_West);

   Rook_Offsets   : constant Short_Direction_Offset :=
     (North, East, South, West);

   Queen_Offsets  : constant Long_Direction_Offset :=
     (North, North_East, East, South_East,
      South, South_West, West, North_West);

   King_Offsets   : constant Long_Direction_Offset :=
     (North, North_East, East, South_East,
      South, South_West, West, North_West);

end Chess.Board.Directions;
