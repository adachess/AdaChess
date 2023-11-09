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


package Chess.Engine.Ponder is

   type Ponder_Mode is (On, Off)
     with Default_Value => Off, Size => 1;
   -- When Ponder_Mode is On (Enabled) the engine will use the opponent time
   -- to continue his thinking-search, based on a predicted move. This move
   -- represent the move that AdaChess believes as the most probable response
   -- from the opponent side.
   
   type Ponder_Guess is (Hit, Miss)
     with Default_Value => Miss, Size => 1;
   
   
   Pondering : Boolean := False;
   
   Wait_Pondering_Answer : Boolean := False; -- Sync pondering with main thread
   
end Chess.Engine.Ponder;
