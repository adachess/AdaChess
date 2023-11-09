--
--  AdaChess - Smart Chess Engine
--
--  Copyright (C) 2013-2023 - Alessandro Iavicoli
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


package Chess.Engine.Search.Stages is
  
   -- Stages represent the move ordering priority to be used in the search. The
   -- engine assign a stage to each move and then perform the search according
   -- to the highest priority to the lowest.
   -- If a move can be assigned to more than one stage (for example, captures
   -- that deliver checks), it will be assigned to the one with the highest
   -- priority.

   type Stage_Type is  
     (Main_Phase,           -- Principal Variation Move
      Hash_Phase,           -- Move found in the Transposition Table
      Recaptures,           -- Move that are re-captures
      Non_Losing_Captures,  -- Captures with SEE >= 0
      Losing_Captures,      -- Captures with SEE < 0
      Killer_Moves,         -- Killer moves
      Checks,               -- Moves that deliver check
      --  Advanced_Passed_Pawn, -- Passed pawn on 7th/2nd rank and promotion
      --  Passed_Pawn_Push,     -- Passed pawn push
      Quiet,                -- Any other non capturing, quiet moves
      Not_Staged)
     with
       Default_Value => Not_Staged;
   
   Initial_Phase : constant Stage_Type := Stage_Type'First;
   
   Invalid_Stage : exception;
   -- Raised when a Move does not obtain any valid stage

end Chess.Engine.Search.Stages;
