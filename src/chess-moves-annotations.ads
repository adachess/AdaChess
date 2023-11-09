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


package Chess.Moves.Annotations is

   type Annotation_Type is
     (None,
      Blunder,
      Mistake, 
      Dobious,
      Interesting, -- A move that leads to a small improvement
      Good,        -- A move that brings a valuable advatages
      Brilliant)   -- Amazing move!
     with 
       Size => 3,
       Default_Value => None;
   
   type Annotated_Move_Type is record
      Move       : Move_Type;
      Annotation : Annotation_Type;
   end record;
   
   ---------
   -- "=" --
   ---------
   
   function "=" (Left, Right : in Annotated_Move_Type) return Boolean is
     (Left.Move = Right.Move);
   
   function "=" (Left : in Move_Type; Right : in Annotated_Move_Type) return Boolean is
     (Left = Right.Move);
   
   function "=" (Left : in Annotated_Move_Type; Right : in Move_Type) return Boolean is
     (Left.Move = Right);
   
   
   Empty_Annotated_Move : constant Annotated_Move_Type :=
     Annotated_Move_Type'(Move       => Empty_Move,
                          Annotation => None);
   

end Chess.Moves.Annotations;
