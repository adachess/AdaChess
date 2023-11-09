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


with Ada.Characters.Handling;

package body Chess.Protocols is

   
   function Parse_Input (Input : in String) return Protocol_Command_Type is

      function Uppercase (Source : in String) return String renames
        Ada.Characters.Handling.To_Upper;
      
      Command : constant String := Uppercase (Input);
      
   begin
      
      for Protocol_Command in Protocol_Command_Type loop
         if Command = Uppercase (Protocol_Command'Image) then
            return Protocol_Command;
         end if;
      end loop;
      
      -- If the given command is not recognized, it maight be one of those that
      -- are not 1:1 mapped into their string representation
      
      if Command = "NEW" then
         return New_Game;
      elsif Input'Length = 0 then
         return Noop;
      elsif Command = "?" then
         return Question_Mark;
      elsif Command = "EXIT" then
         return Exit_Command;
      end if;
      
      
      return Unknown;
   end Parse_Input;
   
end Chess.Protocols;
