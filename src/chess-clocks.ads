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


with Timers;
with Timers.Countdown;

with Chess.Depths; use Chess.Depths;
with Chess.Nodes;  use Chess.Nodes;

package Chess.Clocks is
   
   Forever : constant Duration := Duration'Last;
   
   Minimal_Time_Span : constant duration := 0.0;
   
   
   type Chess_Clock_Type is tagged limited private;
   
   
   procedure Start (Clock : in out Chess_Clock_Type)
     with
       Inline => True;
   -- Start the clock. Calculate the time that has to be reserved for thinking
   -- and start the timer.
   --
   -- Argument
   --    Clock : The Chess clock to start
   -- Aspects
   --    Inline : The procedure is inlined
   
   procedure Stop (Clock : in out Chess_Clock_Type)
     with
       Inline => True;
   -- Pause the clock.
   -- 
   -- Argument
   --    Clock : The Chess clock to pause
   -- Aspects
   --    Inline : The procedure is inlined
   
   procedure Interrupt (Clock : in out Chess_Clock_Type)
     with
       Inline => True;
   -- Stop the clock immediately and set the thinking time to expired to force
   -- the engine to drop immediately the search operations.
   --
   -- Aspects
   --    Inline : The procedure is inlined
   
   procedure Set_Fixed_Time_Per_Move
     (Clock : in out Chess_Clock_Type; Time_Per_Move : in Duration)
     with
       Pre => Time_Per_Move >= Minimal_Time_Span;
   -- Set the current timer control as fixed time per move and set the time
   -- for each move to the given one. In this time control mode the engine is
   -- not allowed to allocate any extra thiking time. The time eventually not
   -- used if not accumulated, therefore is lost.
   --
   -- Arguments
   --    Time_Per_Mode : The amount of time in seconds for each move
   -- Aspects
   --    Preconditions : The time for each moves shall be at least Minimal_Time_Span
   
   procedure Set_Max_Search_Depth (Clock : in out Chess_Clock_Type; Depth_Limit : in Depth_Type);
   -- Set the maximum search depth allowed in the searching tree.
   -- 
   -- Arguments
   --    Max_Depth : The depth limit
   
   procedure Set_Level (Clock     : in out Chess_Clock_Type;
                        Moves     : in Natural;
                        Minutes   : in Natural;
                        Increment : in Natural);
   -- Read the value given by the Level command and set them into the clock. If
   -- the moves are zero, set the Blitz time control mode. In the other cases
   -- set the Tournament time control mode. If the moves is set to 1, this is
   -- not a fixed time per move. The difference is that the time not used here
   -- can be accumulated while the fixed_time_per move does not accumulate it.
   --
   -- Arguments
   --    Moves     : The number of moves per time control
   --    Minutes   : The time expressed in minutes per time control
   --    Increment : The time increment for each move, expressed in seconds
   
   
   procedure Update_Time_Left
     (Clock : in out Chess_Clock_Type; Time_Left : in Duration; Moves : in Natural)
     with
       Pre => Time_Left >= Minimal_Time_Span;
   -- Update the available time left according to the UI value. This is done
   -- via the Time command. The clock needs to know how many moves has been
   -- played by the engine to calculate how much time has to be allocated
   -- for the next move.
   
   procedure Update_Opponent_Time_Left
     (Clock : in out Chess_Clock_Type; Time_Left : in Duration)
     with
       Pre => Time_Left >= Minimal_Time_Span;
   -- Set the remaining time left for the opponet. This value is set directly
   -- from the UI via the "otim" command.
   
   function Search_Depth_Limit (Clock : in Chess_Clock_Type) return Depth_Type
     with Post => Search_Depth_Limit'Result in Frontier_Depth .. Horizon;
   -- Return the maximum search depth allowed. In normal conditions the engine
   -- can search up to the maximum search depth but via a command specified in
   -- the chess engine communication protocol it is possible to limit this value
   -- to any desired. Note that the search will stop either when the given depth
   -- is reached or when the time limit is reached.
   --
   -- Returns
   --    The maximum depth allowed to be reached by the search engine
   -- Aspects
   --    Postocondition : The maximum depth is not higher than the orizon
   
   function Thinked_Time (Clock : in Chess_Clock_Type) return Duration
     with
       Inline => True;
   -- Expose the amount of thinked time until the current moment (i.e. when this
   -- funcion is called
   --
   -- Aspects
   --    Inline : The function is inlined
   
   
   function Time_Has_Up (Clock : in Chess_Clock_Type) return Boolean
     with 
       Inline => True;
   
   ----------------
   -- Exceptions --
   ----------------
   
   Thinking_Time_Expired : exception;
   -- AdaChess will raise this exception when the thinking time has over. The 
   -- search in progress shall stop immediately.
   
   Not_Enough_Time : exception;
   -- Raise this exception when the given time to think is less than a minimum
   -- number. AdaChess require some time to reset all data and under this value
   -- no search makes sense.
   
private

   --  package Internal_Timer is new Timers;
   package Timer is new Timers.Countdown;
   
   type Search_Control_Type is
     (None,               -- Nothing specified
      Fixed_Search_Depth, -- Stop search at a specific depth
      Mate_Search,        -- Search until a mate is found
      Nodes);             -- Limit search to given node numbers
   
   type Time_Control_Type is
     (Fixed_Time_Per_Move, -- Search for a specific time
      Blitz,               -- Time control for the entire game
      Infinite,            -- Analyze mode
      Tournament);         -- Time control for tournaments        
   
   type Chess_Clock_Type is tagged limited 
      record
         Time_Control  : Time_Control_Type;
         -- Handle the current time control for the given clock
         -- AdaChess will calculate the thinking time by taking
         -- into account the time control mode
         
         Search_Control : Search_Control_Type;
         -- Handle, if any, limitations to the search as maximum depth or
         -- nodes
         
         Time_Left    : Duration;
         -- Represent the amount of time available. According to 
         -- the time control can be the time left for a single move
         -- or for the rest of the game
         
         Thinking_Time : Duration;
         -- Represent the total amount of time allocated for thinking on a
         -- specific move. This amout 
         
         Opponent_Time : Duration;
         -- Tracks the time left for the opponent. According to the time
         -- control in use, this value can represent the time left for a single
         -- move or for the rest of the game
         
         Increment    : Duration := 0.0;
         -- Handle the time increment, in seconds for each move. When AdaChess
         -- detect that an increment is given after each move, the engine will
         -- consider it when it needs to allocate extra thinking time.
      
         Moves_Per_Time : Natural := 0;
         -- Represent the number of moves for the current time control in
         -- a tournament mode. If the value is zero, the meaning is Blitz
         
         Nb_Of_Moves  : Natural := 0;
         -- Represent the number of moves played by the engine. It does
         -- not represent the Ply. The clock will tune itself considering 
         -- how many moves has been played
         
         Depth_Limit   : Depth_Type := Horizon;
         -- Represent the maximum search depth the engine is allow to search.
         -- In combination with other timer controls, the engine will search 
         -- until a stop condition is found. When no other time control are
         -- specified, the engine will search exactly the depth specified here
         
         Node_Limit    : Node_Type := Node_Type'Last;
         -- Represent the maxmumun number of search nodes the engine should evaluate
         -- In combination with other timer controls, the engine will search a stop
         -- condition is found. When no other time control are specified, the engine
         -- will search exactly the amount of node specified

      end record;
   
   
   type Time_Status_Type is (Expired, Not_Expired)
     with
       Default_Value => Not_Expired,
       Size => 1;
   
   Time_Status  : Time_Status_Type := Not_Expired;
   
end Chess.Clocks;
