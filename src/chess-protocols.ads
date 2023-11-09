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


package Chess.Protocols is

   
   -- Supported communication protocols
   type Communication_Protocol_Type is
     (No_Gui_Connection, Winboard, Universal_Chess_Interface) with Size => 2;
   
   type Winboard_Mode_Type is (Play_White, Play_Black, Forcemode, Analyze) with Size => 2;
   
   type Protocol_Command_Type is
     (Unknown,          -- Any command that is not recognized
      Noop,             -- No operation, sent when empty string is given
      -- Winboard related commands
      Xboard,           -- Switch the engine to using Winboard/Xboard protocol
      New_Game,         -- Start a new game. Map the 'new' command
      Protover,         -- Protocol Version 
      Accepted,         -- Feedback from Winboard after Feature command set
      Rejected,         --
      Variant,          --
      Quit,             -- Exit program
      Random,           -- Switch between random mode, on/off
      Force,            --
      Go,               -- Start thinking on the current position
      Playother,        --
      White,            --
      Black,            --
      Level,            --
      St,               -- Set fixed thinking time per move
      Sd,               -- Set fixed depth search per move
      Nps,              --
      Time,             -- Set the current available thinking time
      Otim,             -- Set opponent time
      Move,             -- move <move> send the opponent move to the engine
      Usermove,         -- Sent the move (from xboard to the engine) with command
      Question_Mark,    -- Force the engine to move immediately. Map the <?> command
      Ping,             -- Test the communication GUI-Engine.
      Draw,             --
      Result,           --
      Setboard,         -- Send a FEN string with the board configuration
      Edit,             --
      Hint,             --
      Bk,               --
      Undo,             -- Takeback a move
      Remove,           --
      Hard,             -- Ponder ON
      Easy,             -- Ponder OFF
      Post,             -- Show thinking output
      Nopost,           -- Hide thinking output
      Analyze,          -- Activate the analyze mode
      Name,             --
      Rating,           --
      Ics,              --
      Computer,         --
      Pause,            --
      Resume,           --
      Memory,           -- Allow the engine to allocate memory up to to a specific size
      Cores,            --
      Egtpath,          -- Path to the EGTP (End_Game Tablebases)
      Option,           --
      -- UCI protocol command
      Uci,              -- Switch the engine to the UCI communication protocol
      Debug,            -- Turn on/off debug mode
      Isready,          -- 
      Setoption,
      Register,
      Ucinewgame,       -- Start a new game
      Position,         -- Set a specific position given in FEN notation
      -- go, 
      Stop,             --
      Ponderhit,        -- 
      -- Quit, 
      -- Internal command
      RunEpdTestSuite,  -- Run the epd test suite
      Multipv,          -- Set the amount of Multi_Pv, default is 1
      Perft,            -- Run a perft on a current position perft <depth>
      Divide,           -- Run a divided perft from the current position divide <depth>
      Bench, Benchmark, -- Benchmark the current position
      Moves,            -- Display all possible moves from the current position
      Sort,             -- Display, sorted, all legal movem from the current position
      Game,             -- Export the current game in PGN-friendly format
      Eval,             -- Call Evaluation from current position
      Evalall,          -- Evaluate each move from the current position
      Seetest,          -- Run a test for SEE
      Setepd,           -- Read EPD data from command line
      Tune,             -- Run the Tuning engine to tune the evaluation parameters 
      Fensave,          -- Print a string representing the FEN notation of the board
      Invert,           -- Change side to move
      Mirror,           -- Mirror current position
      Display,          -- Show the chessboard in the console
      Exit_Command,     -- Alternative to the quit command (exit)
      Usage);           -- Print out engine usage
   
   -- See: http://hgm.nubati.net/newspecs.html
   -- for details about each command
   
   function Parse_Input (Input : in String) return Protocol_Command_Type;
   -- Detect the command given the Input. If no command can be matched, the
   -- special Unknown command will be used
   --
   -- Arguments:
   --    Input : The string to parse
   --
   -- Returns:
   --     The command corresponding to the input
   
   
   Principal_Variation_Post : Boolean := True;
   
   -----------------------------
   -- Communication Protocols --
   -----------------------------

   -- Holds the communication protocol in use
   Communication_Protocol     : Communication_Protocol_Type := No_Gui_Connection;

end Chess.Protocols;
