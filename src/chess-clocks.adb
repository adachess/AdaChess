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


package body Chess.Clocks is

   
   -----------
   -- Start --
   -----------
   
   procedure Start (Clock : in out Chess_Clock_Type) is
      Thinking_Time  : Duration := 0.0;
      Moves_Per_Time : Natural := 0;
   begin
      
      case Clock.Time_Control is     
         when Fixed_Time_Per_Move =>
            -- In this time control we have exactly this amount of time per
            -- each move, therefore the thinking time is the given time
            Thinking_Time := Clock.Time_Left;
            
         when Blitz =>
            -- In blitz mode, the time control is for the rest of the game
            -- therefore it must be calculated an average amount for the
            -- current move. To avoid the engine to reach the end of the game
            -- in a rush, the time allocated for a move will increase when
            -- the game has a long progress
            if Clock.Nb_Of_Moves < 40 then
               if Clock.Nb_Of_Moves <= 3 and then Clock.Time_Left <= 120.0 then
                  Thinking_Time := Clock.Time_Left / 10.0;
               else
                  Thinking_Time := Clock.Time_Left / 40.0;
               end if;
            elsif Clock.Nb_Of_Moves < 80 then
               Thinking_Time := Clock.Time_Left / 20.0;
            else
               Thinking_Time := Clock.Time_Left / 10.0;
            end if;
    
         when Infinite =>
            -- As the time says, this time control mode aims to calculate until
            -- a manual operation will modify the chess status. This time mode
            -- is also used for analyzing a position in real time.
            Thinking_Time := Forever;
            
         when Tournament =>
            -- Divide the value for the given number of moves by knowing that
            -- once the moves per time are played, the timer will be reset to
            -- new values
            -- If the game is just started, and we have enough time (so for the
            -- first 5 moves if the time left is at least 2 minutes) we can 
            -- let the engine to think for a while and fill the transposition
            -- table.
            pragma Assert (Clock.Moves_Per_Time > 0, "Invalid Tournament mode when moves per time is set to 0 (Zero)");
            
            Moves_Per_Time := Clock.Nb_Of_Moves mod Clock.Moves_Per_Time;
            Thinking_Time := Clock.Time_Left / ((Clock.Moves_Per_Time - Moves_Per_Time) * 1.0);

            if Clock.Nb_Of_Moves <= 4 and then Clock.Time_Left <= 120.0 then -- 2 minutes
               Thinking_Time := Thinking_Time * 2.0;
            end if;
            
      end case;
      
      Clock.Thinking_Time := Thinking_Time;
      
      Time_Status := Not_Expired;
      Timer.Start;
   end Start;
   
   ----------
   -- Stop --
   ----------
   
   procedure Stop (Clock : in out Chess_Clock_Type) is
      pragma Unreferenced (Clock);
   begin
      Timer.Stop;
   end Stop;
   
   ---------------
   -- Interrupt --
   ---------------
   
   procedure Interrupt (Clock : in out Chess_Clock_Type) is
   begin
      Clock.Stop;
      Time_Status := Expired;
   end Interrupt;
   
   -----------------------------
   -- Set_Fixed_Time_Per_Move --
   -----------------------------
   
   procedure Set_Fixed_Time_Per_Move
     (Clock : in out Chess_Clock_Type; Time_Per_Move : in Duration) is
   begin
      Clock.Time_Control := Fixed_Time_Per_Move;
      Clock.Time_Left := Time_Per_Move;
      Clock.Thinking_Time := Time_Per_Move;
   end Set_Fixed_Time_Per_Move;
      
   
   --------------------------
   -- Set_Max_Search_Depth --
   --------------------------
   
   procedure Set_Max_Search_Depth (Clock : in out Chess_Clock_Type; Depth_Limit : in Depth_Type) is
   begin
      Clock.Depth_Limit := Depth_Limit;
   end Set_Max_Search_Depth; 
   
   
   ---------------
   -- Set_Level --
   ---------------
   
   procedure Set_Level
     (Clock     : in out Chess_Clock_Type;
      Moves     : in Natural;
      Minutes   : in Natural;
      Increment : in Natural) 
   is
      Seconds        : constant Duration := Duration (Minutes * 60);
      Time_Increment : constant Duration := Duration (Increment);
   begin
      Clock.Moves_Per_Time := Moves;
      Clock.Time_Control := (if Moves = 0 then Blitz else Tournament);
      Clock.Time_Left := Seconds;
      Clock.Increment := Time_Increment;
   end Set_Level;
   
   
   --------------------------
   -- Update_Residual_Time --
   --------------------------
   
   procedure Update_Time_Left (Clock : in out Chess_Clock_Type; Time_Left : in Duration; Moves : in Natural) is
   begin
      Clock.Nb_Of_Moves := Moves;
      Clock.Time_Left := Time_Left;
   end Update_Time_Left;
   
   
   -------------------------------
   -- Update_Opponent_Time_Left -- 
   -------------------------------
   
   procedure Update_Opponent_Time_Left (Clock : in out Chess_Clock_Type; Time_Left : in Duration) is
   begin
      Clock.Opponent_Time := Time_Left;
   end Update_Opponent_Time_Left;
   
   
   ------------------
   -- Thinked_Time --
   ------------------
   
   function Thinked_Time (Clock : Chess_Clock_Type) return Duration is
      pragma Unreferenced (Clock);
   begin
      return Timer.Elapsed_Time;
   end Thinked_Time;
   
   
   ------------------------
   -- Search_Depth_Limit --
   ------------------------
   
   function Search_Depth_Limit (Clock : in Chess_Clock_Type) return Depth_Type is
   begin
      return Clock.Depth_Limit;
   end Search_Depth_Limit;


   -----------------
   -- Time_Has_Up --
   -----------------

   function Time_Has_Up (Clock : in Chess_Clock_Type) return Boolean is
      use Timer;
   begin
      if Time_Status = Not_Expired then
         Time_Status := 
           (if Clock.Thinked_Time >= Clock.Thinking_Time then Expired else Not_Expired);
      end if;
      return Time_Status = Expired;
   end Time_Has_Up;
   
   
end Chess.Clocks;
