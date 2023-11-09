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
