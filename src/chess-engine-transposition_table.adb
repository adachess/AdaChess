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


with Ada.Text_IO;
with Ada.Unchecked_Deallocation;


package body Chess.Engine.Transposition_Table is

   ------------------
   -- Generate_Key --
   ------------------

   function Generate_Key (Hash_Value : Hash_Type) return Transposition_Table_Key_Type is
      Table_Size : Transposition_Table_Key_Type renames Transposition_Table_Hash_Size;
   begin
      if not Transposition_Table_Is_Active then
         return No_Key;
      end if;
      return Hash_Value mod Table_Size;
   end Generate_Key;
   
   
   -------------------------------
   -- Init_Transposition_Table --
   -------------------------------

   procedure Init_Transposition_Table (Size : in Megabyte)is

      procedure Free is new Ada.Unchecked_Deallocation
        (Transposition_Table_Type, Transposition_Table_Access_Type);
      
   begin
      Free (Transposition_Table);

      if Size = 0 then
         Transposition_Table_Is_Active := False;
      else
         Transposition_Table_Is_Active := True;
         
         Allocate_Memory (Size);
         Clear_Transposition_Table;
      end if;

   end Init_Transposition_Table;
   
   
   -------------------------------
   -- Clear_Transposition_Table --
   -------------------------------
   
   procedure Clear_Transposition_Table is
      subtype Allocated is Hash_Type range
        Transposition_Table_Key_Type'First .. Transposition_Table_Hash_Size;
   begin
      if Transposition_Table_Is_Active then
         Transposition_Table (Allocated'Range) := (others => Empty_Entry);
      end if;
   end Clear_Transposition_Table;
   
   
   ---------------------
   -- Adjust_For_Mate --
   ---------------------
   
   function Adjust_For_Mate (Score : in Score_Type; Ply : in Depth_Type) return Score_Type is
      Mate_Score : Score_Type := Score;
   begin
      if Score >= Mate - 128 then
         Mate_Score := Score + Ply;
      elsif Score <= -Mate + 128 then
         Mate_Score := Score - Ply;
      end if;
      return Mate_Score;
   end Adjust_For_Mate;
   
   
   ----------------------
   -- Adjust_From_Mate --
   ----------------------
   
   function Adjust_From_Mate (Score : in Score_Type; Ply : in Depth_Type) return Score_Type is
      Mate_Score : Score_Type := Score;
   begin
      if Score >= Mate - 128 then
         Mate_Score := Score - Ply;
      elsif Score <= -Mate + 128 then
         Mate_Score := Score + Ply;
      end if;
      return Mate_Score;
   end Adjust_From_Mate;


   --------------------------------
   -- Record_Transposition_Entry --
   --------------------------------

   procedure Record_Transposition_Entry
     (Hash_Value  : in Hash_Type;
      Depth       : in Depth_Type;
      Ply         : in Depth_Type;
      Evaluation  : in Evaluation_Type;
      Move        : in Move_Type;
      Flag        : in Entry_Type)
   is   
      Key         : Transposition_Table_Key_Type;
      Table_Entry : Transposition_Table_Entry_Type;
      Store       : Boolean := True;
   begin
      
      if not Transposition_Table_Is_Active then
         return;
      end if;
      
      if Move = Empty_Move then -- or else Evaluation.Game_Phase not in Checkmate | In_Progress then
         Store := False;
      else
         Table_Entry := Probe_Transposition_Table (Hash_Value);
         if Table_Entry.Depth >= Depth then
            Store := False;
         end if;
      end if;
      
      if Store then
         Key := Generate_Key (Hash_Value);
         Transposition_Table (Key) := Transposition_Table_Entry_Type'
           (Depth      => Depth,
            Evaluation => Evaluation_Type'
              (Score => Adjust_For_Mate (Evaluation.Score, Ply), Game_Phase => Evaluation.Game_Phase),
            Move       => Move,
            Flag       => Flag);
      end if;

   end Record_Transposition_Entry;
   
   
   -------------------------------
   -- Clear_Transposition_Entry --
   -------------------------------

   procedure Clear_Transposition_Entry
     (Hash_Value  : in Hash_Type)
   is   
      Key         : Transposition_Table_Key_Type;
   begin
      
      if not Transposition_Table_Is_Active then
         return;
      end if;
      
      Key := Generate_Key (Hash_Value);
      Transposition_Table (Key) := Empty_Entry;
      
   end Clear_Transposition_Entry;
   

   -------------------------------
   -- Probe_Transposition_Table --
   -------------------------------

   function Probe_Transposition_Table (Hash_Value : in Hash_Type) return Transposition_Table_Entry_Type is
      Key      : Transposition_Table_Key_Type;
      TT_Entry : Transposition_Table_Entry_Type := Empty_Entry;
   begin
      
      if Transposition_Table_Is_Active then
         Key := Generate_Key (Hash_Value);
         TT_Entry := Transposition_Table (Key);
      end if;
      
      return TT_Entry;
   end Probe_Transposition_Table;
   
   
   ---------------------
   -- Allocate_Memory --
   ---------------------
   
   procedure Allocate_Memory (Desired_Size : Megabyte) is
      Size_In_Megabytes : Megabyte := Desired_Size;
      Table_Entry_Size  : constant Natural := Transposition_Table_Entry_Type'Size;
   begin
      
      Allocation_Loop : loop
         Allocation_Block : declare
            Size_In_Bytes : Natural;
         begin
            -- Calculate the size in Bytes
            Size_In_Bytes := Size_In_Megabytes * (8 * 1_000_000 / Table_Entry_Size); -- 8 bit, 1_000_000 byte
            
            -- Allocate the calculated size
            Transposition_Table_Hash_Size := Transposition_Table_Key_Type (Size_In_Bytes);
            Transposition_Table := new Transposition_Table_Type 
              (0 .. Transposition_Table_Hash_Size);
            
            if Size_In_Megabytes <= Small_Memory_Size_Limit then
               Ada.Text_IO.Put_Line ("Warning! Hash size of" & Natural'Image (Size_In_Megabytes) & "  Megabyte is small Suggested memory is " & Megabyte'Image (Small_Memory_Size_Limit) & "Mb or more!");
            end if;
            
            Ada.Text_IO.Put_Line ("Allocated" & Natural'Image (Size_In_Megabytes) & "MB for Hash");
            exit Allocation_Loop;
         exception
            when Constraint_Error =>
               Ada.Text_IO.Put ("Warning: Attempt to allocate" & Megabyte'Image (Size_In_Megabytes) & "Mb failed. ");
               Ada.Text_IO.Put_Line ("Not enough memory! Will retry by allocating" & Megabyte'Image (Size_In_Megabytes / 2) & "Mb");
               Size_In_Megabytes := Size_In_Megabytes / 2; -- retry with half of the requested size
         end Allocation_Block;
      end loop Allocation_Loop;
      
   end Allocate_Memory;

  
end Chess.Engine.Transposition_Table;
