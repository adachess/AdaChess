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


package Chess.IO.Consoles is
   
   package Render_Interface is
      
      type Render_Engine is interface;
      
      procedure Render 
        (Engine     : in Render_Engine; 
         Chessboard : in Chessboard_Type) is abstract;
      
   end Render_Interface;
   
   
   procedure Display_On_Console (Chessboard : in Chessboard_Type);
   
private
   
   
   
   type Console_Render_Engine is abstract new Render_Interface.Render_Engine with null record;

   procedure Render 
     (Engine     : in Console_Render_Engine;
      Chessboard : in Chessboard_Type) is abstract;
   
   
   type Simple_Console_Render_Engine is new Console_Render_Engine with null record;
   overriding procedure Render
     (Engine     : in Simple_Console_Render_Engine;
      Chessboard : in Chessboard_Type);
   
   Selected_Render_Engine : Render_Interface.Render_Engine'Class
     := Simple_Console_Render_Engine'(null record);
   
end Chess.IO.Consoles;
