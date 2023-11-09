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


with Ada.Real_Time;


generic
package Timers.Countdown is
   
   pragma Elaborate_Body;

   
   procedure Start;
   -- Start the timer. 
   
   procedure Stop;
   -- Stop the timer.
   
   
   function Elapsed_Time return Duration;
   -- Retrieve the time passed since the timer has been started last time
   -- If the timer is still running, the elapsed time is the corresponding
   -- time, if the timer has stopped (paused) the elapsed time is calculate as
   -- the time difference between the stop-time and the start-time
   --
   -- Returns
   --    The time difference since 
   
private
   
   Start_Time   : Ada.Real_Time.Time;
   Stop_Time    : Ada.Real_Time.Time;
   
   Timer_Status : Timer_Status_Type;
   

end Timers.Countdown;
