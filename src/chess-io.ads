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


with Ada.Characters.Handling;

with Chess.Engine;    use Chess.Engine;
with Chess.Notations; use Chess.Notations;
with Chess.Moves;     use Chess.Moves;
with Chess.Moves.Annotations; use Chess.Moves.Annotations;


package Chess.IO is
   
   function Move_To_String (Move : in Move_Type; Notation : in Notation_Type := Default_Notation)
                            return String;
   -- Convert a move into its string representation. The conversion is done
   -- according with the desired notation type. The conversion will resolve
   -- ambiguities, if any, according with the ambiguity flag set into the
   -- move itself.
   --
   -- Arguments:
   --    Move     : The move to code into a string
   --    Notation : The desired notation to use for the conversion
   -- Returns:
   --    A string with the move coded in it
  
   function Move_To_String (Annotated_Move : in Annotated_Move_Type; Notation : in Notation_Type := Default_Notation)
      return String;
   -- Convert a move into its string representation. The conversion is done
   -- according with the desired notation type. The conversion will resolve
   -- ambiguities, if any, according with the ambiguity flag set into the
   -- move itself. Any annotation will be added to the resulting string
   --
   -- Arguments:
   --    Annotated_Move : The move to code into a string
   --    Notation       : The desired notation to use for the conversion
   -- Returns:
   --    A string with the move coded in it
      
   procedure Print_Move (Annotated_Move : in Annotated_Move_Type; Notation : in Notation_Type := Default_Notation)
     with
       Inline => True;
   -- Display a move to the console with the chosed notation. The desired
   -- notation will be used. If no notation is given, the one that is currently
   -- in use will be selected.
   --
   -- Arguments:
   --    Annotated_Move : The move to display
   --    Notation       : The notation to be used.
   -- Aspects
   --    Inline
   
   procedure Print_Move (Move : in Move_Type; Notation : in Notation_Type := Default_Notation)
     with
       Inline => True;
   -- Display a move to the console with the chosed notation. The desired
   -- notation will be used. If no notation is given, the one that is currently
   -- in use will be selected.
   --
   -- Arguments:
   --    Move     : The move to display
   --    Notation : The notation to be used.
   -- Aspects
   --    Inline

   procedure Print_Game (Chessboard    : in Chessboard_Type; 
                         Move_Notation : in Notation_Type := Default_Notation;
                         Pretty_Print  : in Boolean := True);
   -- Display the entire game in console. The format is a PGN-friendly format.
   -- The entire game will be displayed 
   --
   -- Arguments:
   --    Chessboard    : The chessboard containing the current story of the game
   --    Move_Notation : Represent the desired output notation for each move
   --    Pretty_Print  : If True, will display the game in columns, if False
   --                    will output a single long line
   
   
private
   
   function Lower_Case
     (C : in Character) return Character 
      renames Ada.Characters.Handling.To_Lower;
   
   function Upper_Case 
     (C : in Character) return Character
      renames Ada.Characters.Handling.To_Upper;

end Chess.IO;
