package body Timers.Countdown is
   
   
   -----------
   -- Start --
   -----------
   
   procedure Start is
   begin
      Timer_Status := Started;
      Start_Time := Ada.Real_Time.Clock;
   end Start;
   
   ----------
   -- Stop --
   ----------
   
   procedure Stop is
   begin
      Stop_Time := Ada.Real_Time.Clock;
      Timer_Status := Stopped;
   end Stop;
   
   
   ------------------
   -- Elapsed_Time --
   ------------------
   
   function Elapsed_Time return Duration is
      use Ada.Real_Time;
      Time_Difference : Duration;
   begin
      case Timer_Status is
         when Not_Yet_Started =>
            Time_Difference := 0.0;
         when Started =>
            Time_Difference := To_Duration (Ada.Real_Time.Clock - Start_Time);
         when Stopped =>
            Time_Difference := To_Duration (Stop_Time - Start_Time);
      end case;
      
      return Time_Difference;
   end Elapsed_Time;
   
   
begin
   Timer_Status := Not_Yet_Started;
end Timers.Countdown;
