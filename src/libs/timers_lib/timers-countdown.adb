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
