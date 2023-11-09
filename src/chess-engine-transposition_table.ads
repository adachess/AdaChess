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


with Chess.Score; use Chess.Score;
with Chess.Engine.Evaluations; use Chess.Engine.Evaluations;


package Chess.Engine.Transposition_Table is
   
   subtype Megabyte is Natural range 0 .. 65536; -- Hadle up to 64Gb of RAM

   --------------------------
   -- Transposition Tables --
   --------------------------

   -- Define the size of the transposition table in the range of an Hash type
   subtype Transposition_Table_Key_Type is Hash_Type;
   
   Transposition_Table_Hash_Size : Transposition_Table_Key_Type := 0;
   
   Transposition_Table_Is_Active : Boolean := False; -- Should be always sync with Transposition_Table_Hash_Size = 0


   type Entry_Type is
     (Empty_Slot, Exact_Entry, Upper_Bound, Lower_Bound)
     with 
       Size => 2,
       Default_Value => Empty_Slot;
   
   type Transposition_Table_Entry_Type is
      record
         Depth      : Depth_Type;
         Evaluation : Evaluation_Type;
         Move       : Move_Type;
         Flag       : Entry_Type;
      end record;
   --  pragma Pack (Transposition_Table_Entry_Type); -- Causes STORAGE_ERROR on Linux machine
   

   type Transposition_Table_Type is
     array (Transposition_Table_Key_Type range <>) of Transposition_Table_Entry_Type;

   
   type Transposition_Table_Access_Type is access Transposition_Table_Type;
   Transposition_Table : Transposition_Table_Access_Type;
   
   
   procedure Init_Transposition_Table (Size : in Megabyte);
   -- Create a new Table with empty value on it. If a transposition table was
   -- already allocaded, the memory will first be released than will be claimed
   -- again with the given size.
   -- If the the size is too large and cannot be allocate, the engine will
   -- automatically retry by halving the desired size
   -- A value of 0 (Zero) deactivate the transposition table
   --
   -- Arguments:
   --    Size : The desired size of the table, in Megabytes, or 0 to deactivate
   --           the Transposition Table use.
   
   procedure Clear_Transposition_Table;
   -- Reset all the data inside the Transposition table
   
   
   function Adjust_For_Mate
     (Score : in Score_Type; Ply : in Depth_Type)
      return Score_Type;
   -- Adjust the score to the mate-lenght, if the score represent a checkmate
   -- 
   -- Arguments
   --    Score : The score to be saved into the Transposition table
   --    Ply   : The current Ply, to find the right distance-to-mate
   -- Returns
   --    If the score is a mate score, it will adjust to the distance to mate.
   --    If the score is not a mate score, it will return the original value
   
   function Adjust_From_Mate
     (Score : in Score_Type; Ply : in Depth_Type)
      return Score_Type;
   -- Adjust the score from mate-length, if the score represent a checkmate
   -- 
   -- Arguments
   --    Score : The score retrieved from the Transposition table
   --    Ply   : The current Ply, to find the right distance-to-mate
   -- Returns
   --    If the score is a mate score, it will adjust to the distance to mate.
   --    If the score is not a mate score, it will return the original value
     
   
   procedure Record_Transposition_Entry
     (Hash_Value : in Hash_Type;
      Depth      : in Depth_Type;
      Ply        : in Depth_Type;
      Evaluation : in Evaluation_Type;
      Move       : in Move_Type;
      Flag       : in Entry_Type)
     with 
       Inline => True;
   -- Store a move inside the transposition hash table. In case of collision
   -- a decision of updating the key/value will be taken, based on the the depth
   -- and the info related to the entry.
   --
   -- Arguments:
   --    Hash_Value : The hashed chess position
   --    Depth      : The search depth when the value is stored
   --    Ply        : The current ply search depth
   --    Score      : The Score of the chess position
   --    Move       : The move we are storing for the given position
   --    Flag       : An entry to save the search node type
   -- Aspects
   --    Inline
   
   procedure Clear_Transposition_Entry (Hash_Value : in Hash_Type)
     with
       Inline => True;
   -- Clear a specific entry inside of the transposition table. The entry has
   -- to be found from the given hash value, representing the chess position
   --
   -- Arguments
   --    Hash_Value : The hashed chess position
   -- Aspects
   --    Inline

   function Probe_Transposition_Table
     (Hash_Value : in Hash_Type)
      return Transposition_Table_Entry_Type with Inline => True;
   -- Look if the give key exists inside the Transposition table
   -- and, if so, return the corresponding table entry
   --
   -- Arguments
   --    Hash_Value : The hashed of the current chess position
   -- Returns
   --    The table entry if exists, or an empty move otherwise
   
   ----------------
   -- Exceptions --
   ----------------
   
   Transposition_Lookup_Exception : exception;
   -- Raise this exception if an invalid data is found in the table
   
private
      
   Empty_Entry : constant Transposition_Table_Entry_Type := 
     Transposition_Table_Entry_Type'
       (Depth      => Zero_Depth,
        Evaluation => Evaluation_Type'(Score => 0, Game_Phase => In_Progress),
        Move       => Empty_Move,
        Flag       => Empty_Slot);
   
   No_Key              : constant Transposition_Table_Key_Type := 0;
   -- This is a special key used to avoid lookup into the table. When this
   -- key is encountered, the table entry shall not be used.
   
   
   function Generate_Key (Hash_Value : Hash_Type)
     return Transposition_Table_Key_Type with Inline => True;
   -- Generate a valid Key for the Transposition Hash Table
   -- 
   -- Arguments:
   --    An Hash value representing an hashed chess position
   -- Returns:
   --    A Key for the Transposition Hash Table
   
   
   Small_Memory_Size_Limit : constant Megabyte := 16;
   
   procedure Allocate_Memory (Desired_Size : Megabyte)
     with
       Pre => Desired_Size >= 1;
   -- Allocate the desired memory for the Transposition Hash Table.
   -- If the desired size is smaller that a lower limit, it will warn that the
   -- table size is too small (and the amount of collision will lead to an
   -- instable search that in turn will make the engine play bad chess
   --
   -- Arguments:
   --    Desired_Size : The requested memory size to allocate for the TT
   -- Aspects
   --    Precondition : The size of the table shall be non-negative 
   

end Chess.Engine.Transposition_Table;
