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

with Chess.Board.Directions; use Chess.Board.Directions;


package Chess.Board.Attacks_Data is

   ------------------
   -- Attacks data --
   ------------------

   type Attack_Type is
      record
         Origin : Square_Type; -- square where the attacks begins
         Piece  : Piece_Type;
      end record;
   --  pragma Pack (Attack_Type);

   subtype Attack_Range_Type is Natural range 1 .. 8;
   type Attack_Array_Type is array (Attack_Range_Type'Range) of Attack_Type;
   
   
   ----------------------------
   -- Attack Collection Data --
   ----------------------------

   type Attack_Collection_Type is
      record
         Attacker            : Attack_Array_Type;
         Number_Of_Attackers : Natural;
      end record;
   --  pragma Pack (Attack_Collection_Type);
   
   
   ----------------------------
   -- Attacks Direction Data --
   ----------------------------

   function Attacks_From_North
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type)
      return Boolean
     with
       Inline => True;
   
   function Attacks_From_South
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type)
      return Boolean 
     with
       Inline => True;
   
   function Attacks_From_East
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) 
      return Boolean
     with 
       Inline => True;
   
   function Attacks_From_West
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type)
      return Boolean
     with
       Inline => True;
   
   function Attacks_From_North_East
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type)
      return Boolean
     with
       Inline => True;
   
   function Attacks_From_North_West
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) 
      return Boolean 
     with
       Inline => True;
   
   function Attacks_From_South_East
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type)
      return Boolean 
     with
       Inline => True;
   
   function Attacks_From_South_West
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type)
      return Boolean 
     with 
       Inline => True;
   
   function Attacks_From_North_North_East
      (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type)
       return Boolean 
     with 
       Inline => True;
   
   function Attacks_From_North_East_East
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type)
      return Boolean 
     with
       Inline => True;
   
   function Attacks_From_South_East_East
      (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type)
       return Boolean
     with
       Inline => True;
   
   function Attacks_From_South_South_East
      (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type)
       return Boolean
     with 
       Inline => True;
   
   function Attacks_From_South_South_West
      (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type)
       return Boolean
     with
       Inline => True;
   
   function Attacks_From_South_West_West
      (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type)
       return Boolean 
     with 
       Inline => True;
   
   function Attacks_From_North_West_West
      (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type)
       return Boolean 
     with 
       Inline => True;
   
   function Attacks_From_North_North_West
      (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type)
       return Boolean 
     with 
       Inline => True;
   

   function Attacks_From_Placeholder
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type)
      return Boolean;
   -- This function is an empty-function used as placeholder for the array of
   -- pointer to functions (see below) when the attacking direction does not
   -- represent any real direction.

   
   -------------
   -- Attacks --
   -------------
   
   type Attacks_Direction_Access is access function
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean;
   
   -- The attacks algorithm is based on an array of pointer to the above attacks
   -- functions. The array is initialized when AdaChess starts in such a way to
   -- map the attacking direction to the specific attacks_from_<> function.
   -- Note that the valid directions are the ones specified in the with'ed 
   -- package and all the others value are unused. Those values will be covered
   -- via the Attacks_From_Placeholder hack-function.
   
   type Attacks_Directions_Type is array (Direction_Type'Range) of Attacks_Direction_Access;
   Attacks_To : Attacks_Directions_Type := (others => Attacks_From_Placeholder'Access);
   

end Chess.Board.Attacks_Data;
