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


with Ada.Text_IO;

with Chess.Colors;
with Chess.Pieces;
with Chess.Board;

package body Chess.IO.Consoles is

   overriding procedure Render
     (Engine     : in Simple_Console_Render_Engine;
      Chessboard : in Chessboard_Type) 
   is
      
      use Chess.Colors;
      use Chess.Pieces;
      use Chess.Board;
      
      Row     : Coordinate_Type := 8;
      Piece   : Piece_Type;
      
   begin
      
      for I in Board_Type'Range loop
         Piece := Chessboard.Square (I);
         if Piece /= Frame then
            if Piece =  Empty then
               Ada.Text_IO.Put ('.');
            else
               Ada.Text_IO.Put (Symbols (Piece));
            end if;
         end if;
         Ada.Text_IO.Put (" ");
         case I is
            when 9 | 19 | 29 | 39 | 49 | 59 | 69 | 79 | 89 | 99 | 109 =>
               if I in 20 .. 99 then
                  Ada.Text_IO.Put (" " & Coordinate_Type'Image (Row));
                  Row := Row - 1;
               end if;
               Ada.Text_IO.New_Line;
            when others => null;
         end case;
      end loop;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line (ASCII.CR & "  a b c d e f g h");
      Ada.Text_IO.New_Line;
      if Chessboard.En_Passant (Chessboard.History_Ply) /= 0 then
         Ada.Text_IO.Put_Line ("En passant: " & Pc_Sqr (Chessboard.En_Passant (Chessboard.History_Ply)));
         Ada.Text_IO.New_Line;
      end if;
      if Chessboard.Attacks (White, Chessboard.Black_King_Position) then
         Ada.Text_IO.Put_Line ("Black has king in check");
      end if;
      if Chessboard.Attacks (Black, Chessboard.White_King_Position) then
         Ada.Text_IO.Put_Line ("White has king in check");
      end if;
      Ada.Text_IO.New_Line;
   end Render;
   
   
   
   procedure Display_On_Console (Chessboard : in Chessboard_Type) is
   begin
      Selected_Render_Engine.Render (Chessboard);
   end Display_On_Console;
   

end Chess.IO.Consoles;
