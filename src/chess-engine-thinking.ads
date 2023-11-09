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


with Chess.Score;


package Chess.Engine.Thinking is


   function Think (Chessboard : in out Chessboard_Type) return Move_Type;
   -- Search for the best move until a Timeout signal arrives. The trigger
   -- for the timeout is the available thinking time.
   -- The thinking process is performed by searching the principal
   -- variation, i.e. the main line
   --
   -- Arguments
   --    Chessboard : the current chess state
   -- Returns
   --    Return the best move found for the given position

   procedure Ponder (Chessboard : in out Chessboard_Type);
   -- Pondering is essentially like Think during the opponent time, by
   -- assuming that the opponent will play a specific expected move.
   -- The trigger to stop searching is given either when:
   -- 1) The oppoennt plays an unexpected move or when
   -- 2) The expected move is played and the searching time is expired,
   --
   -- The pondering process, differently to the thinking process, fills the
   -- Principal Variation and the Transposition Table without returning
   -- any move (i.e. without taking any decision)
   --
   -- Arguments:
   --    Chessboard : the current chess state

   function Benchmark (Chessboard : in out Chessboard_Type) return Move_Type;
   -- Perform a Search to benchmark the search engine. This function is
   -- the same as Think, but it disable book moves and single-reply moves
   --
   -- Arguments
   --    Chessboard : The current Chess state
   -- Returns
   --    The Best move found through the Principal Variation

   -------------
   -- Analyze --
   -------------

   --  procedure Analyze (Chessboard : in out Chessboard_Type);
   -- Perform an infinite search. The search stop when the Analyze_Mode
   -- flag is set to off, or when a new chess-state occurs, such as a move
   -- is taken back or is made.
   --
   -- Similar to the Pondering behaviour, this procedure fill the Principal
   -- Variation and the Transposition Table but doesn't return any move
   --
   -- Arguments:
   --    Chessboard : the current chess state

private

   procedure Run_Iterative_Deepening
     (Chessboard   : in out Chessboard_Type;
      Search_Depth : in out Depth_Type;
      Ponder_Move  : in Move_Type := Empty_Move);
   -- Perform the search based on a progressive deeper depth. If then engine is
   -- pondering, a move has to be provided.
   --
   -- Arguments
   --    Ponder_Move : A move representing the expected countermove of the opponent


   use Chess.Score;

   type Aspiration_Window_Type is record
      Alpha : Score_Type := -Infinity;
      Beta  : Score_Type := +Infinity;
   end record;

   Aspiration_Window_Size : constant Score_Type := 33;

end Chess.Engine.Thinking;
