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


with Ada.Task_Identification;
with System.Multiprocessors;
with System.Multiprocessors.Dispatching_Domains;


package ACMultiprocessor is

   --------------
   -- Wrappers --
   --------------

   subtype CPU_Range is System.Multiprocessors.CPU_Range;

   Not_A_Specific_CPU : constant CPU_Range := System.Multiprocessors.Not_A_Specific_CPU;

   function Number_Of_CPUs return CPU_Range renames System.Multiprocessors.Number_Of_CPUs;
   -- Detect the current amount of CPUs available in the system.
   --
   -- Returns
   --    The number of CPUs in the system

   procedure Assign_To_CPU
     (CPU     : CPU_Range;
      Task_Id : Ada.Task_Identification.Task_Id)
      renames System.Multiprocessors.Dispatching_Domains.Set_CPU;
   -- Select a CPU that will run a task. This procedure is a renaming of the
   -- Ada standard library Set_CPU.
   -- Use this function to distribute the calculation over multiple CPUs.
   --
   -- Arguments
   --    CPU     : The target CPU number to assign the task
   --    Task_Id : The task to assign at that CPU

end ACMultiprocessor;
