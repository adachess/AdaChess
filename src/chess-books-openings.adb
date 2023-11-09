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
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Directories;


with Chess;
with Chess.IO; use Chess.IO;
with Chess.IO.Pgn;  use Chess.IO.Pgn;
with Chess.Depths; use Chess.Depths;
with Chess.Notations;

with String_Lib; use String_Lib;


package body Chess.Books.Openings is
   
   
   --------------------
   -- Load_All_Books --
   --------------------
   
   procedure Load_All_Books (Book_Dir : in String := ".") is
     
      procedure Try_As_Simple_Book_Format (Item : Ada.Directories.Directory_Entry_Type) is
         Name : constant String := Ada.Directories.Simple_Name (Item);
      begin
         Simple_Book_File_Name_List.Append (To_Bounded_String (Book_Dir & "\" & Name));
      end Try_As_Simple_Book_Format;
      
      procedure Try_As_Pgn (Item : Ada.Directories.Directory_Entry_Type) is
         Name : constant String := Ada.Directories.Simple_Name (Item);
      begin
         Pgn_Book_File_Name_List.Append (To_Bounded_String (Book_Dir & "\" & Name));
      end Try_As_Pgn;
      
   begin
      
      -- Look up for simple opening book format
      Ada.Directories.Search
        (Directory => Book_Dir, 
         Pattern   => "*.book", 
         Filter    => (others => True), 
         Process   => Try_As_Simple_Book_Format'Access);
      
      -- Look up for pgn opening book
      Ada.Directories.Search
        (Directory => Book_Dir, 
         Pattern   => "*.pgn", 
         Filter    => (others => True), 
         Process   => Try_As_Pgn'Access);
      
   end Load_All_Books;
   
   
   ----------------------
   -- Add_Opening_Book --
   ----------------------
   
   procedure Add_Opening_Book (Book_File_Name : in String) is
      
      procedure Try_As_Pgn (Item : Ada.Directories.Directory_Entry_Type) is
         Name : constant Book_File_Names.Bounded_String := To_Bounded_String (Book_File_Name);
      begin
         if Ada.Directories.Simple_Name (Item) = Book_File_Name then
            Pgn_Book_File_Name_List.Append (Name);
         end if;
      end Try_As_Pgn;
      
      procedure Try_As_Simple (Item : Ada.Directories.Directory_Entry_Type) is
         Name : constant Book_File_Names.Bounded_String := To_Bounded_String (Book_File_Name);
      begin
         if Ada.Directories.Simple_Name (Item) = Book_File_Name then
            Simple_Book_File_Name_List.Append (Name);
         end if;
      end Try_As_Simple;
      
   begin
      
      Ada.Directories.Search
        (Directory => ".", 
         Pattern   => "*.pgn", 
         Filter    => (others => True), 
         Process   => Try_As_Pgn'Access);
      
      Ada.Directories.Search
        (Directory => ".", 
         Pattern   => "*.book", 
         Filter    => (others => True), 
         Process   => Try_As_Simple'Access);
      
   end Add_Opening_Book;
   
   
   ----------------------------
   -- Load_All_Opening_Books --
   ----------------------------
   
   procedure Load_All_Opening_Books is
   begin
      for Book of Simple_Book_File_Name_List loop
         Simple_Opening_Book.Load_From_File (Book_File_Names.To_String (Book));
      end loop;
      
      for Book of Pgn_Book_File_Name_List loop
         Ada.Text_IO.Put_Line ("Loading " & Book_File_Names.To_String (Book) & "..");
         
         Load_Book_Block : begin
            Portable_Game_Notation_Book.Load_From_File (Book_File_Names.To_String (Book));
         exception
            when others =>
               Ada.Text_IO.Put_Line ("Failed to load " & Book_File_Names.To_String (Book) & "!");
         end Load_Book_Block;
         
      end loop;
   end Load_All_Opening_Books;
   
   
   -------------------------
   -- Save_As_Simple_Book --
   -------------------------
  
   procedure Save_As_Simple_Book (File_Name : in String) is
      use type Count_Type;
      use Notations;
      Move : Move_Type;
      File : Ada.Text_IO.File_Type;
      Total_Entries : constant Natural := Book_Entries; -- How many matches loaded into the book
      Current_Match : Natural := 0;
   begin

      if not Ada.Directories.Exists (File_Name) then
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, File_Name);
      else
         Ada.Text_IO.Open (File => File,
                           Mode => Ada.Text_IO.Out_File,
                           Name => File_Name);
      end if;
      
      for Match of Opening_Book loop
         Current_Match := Current_Match + 1;
         for I in 0 .. Match.Length - 1 loop
            Move := Match.Get_Move_At_Index (I);
            Ada.Text_IO.Put (File, Chess.IO.Move_To_String (Move, Standard_Algebraic) & ' ');
         end loop;
         Ada.Text_IO.New_Line (File);
         Ada.Text_IO.Put_Line ("Converted match" & Natural'Image (Current_Match) & " of" & Natural'Image (Total_Entries));
      end loop;
      
      Ada.Text_IO.Close (File);
      
   exception
      when others =>
         if Ada.Text_IO.Is_Open (File) then
            Ada.Text_IO.Close (File);
         end if;
   end Save_As_Simple_Book;   
   
   
   ------------------------------------
   -- Load Simple Opening Book Forma --
   ------------------------------------
  
   procedure Load_From_File (Book : in Simple_Opening_Book_Type; File_Name : String) is
      pragma Unreferenced (Book);
   
      Book_File : Ada.Text_IO.File_Type;
   
      Book_Line : String (1 .. 1024); -- Should be enough!
      Last : Natural;
   
      Match : Chess.Matches.Match_Type;
   
   
      function Read_Next_Move (Current_Book_Line : in String; Move_Number : in Natural) return String is
      begin
         return Extract_Token_At
           (Source => Current_Book_Line, Token_Number => Move_Number, Delimiter => ' ');
      end Read_Next_Move;
   
   
      procedure Parse_Line (Current_Book_Line : in String) is
         Chessboard        : Chessboard_Type;
         Move              : Move_Type;
         Next_Move         : Natural := 1;
      begin
         Chessboard.Initialize;
         Create_New (Match => Match);
   
         loop
            Move := Chess.Engine.Parse_Move
              (Chessboard => Chessboard, Input => Read_Next_Move (Current_Book_Line, Next_Move));
            exit when Move = Empty_Move; -- Works also for invalid moves
   
            Chessboard.Play (Move); -- Update chessboard status
            Match.Append (Move);
            Next_Move := Next_Move + 1;
   
         end loop;
   
      exception
         when Not_Enough_Tokens => -- No other moves in the book line
            null;
         when Invalid_Book_Line =>
            Ada.Text_IO.Put_Line ("Skip invalid book line: " & Current_Book_Line);
         when others =>
            Ada.Text_IO.Put_Line (Current_Book_Line);
            raise Wrong_File_Format;
      end Parse_Line;
   
   
      procedure Add_Opening_Line (Opening_Line : in String) is
         Line : constant String := String_Lib.Trim
           (Source => Opening_Line, Delimiter => Whitespace, Side => Both);
      begin
         if not Is_Empty (Line) then
            Parse_Line (Line);
            Opening_Book.Append (New_Item => Match);
         end if;
      exception
         when Invalid_Book_Line =>
            null; -- Continue with the other book lines
      end Add_Opening_Line;
   
   begin
   
      Ada.Text_IO.Open (File => Book_File, Mode => Ada.Text_IO.In_File, Name => File_Name);
   
      loop
         exit when Ada.Text_IO.End_Of_File (File => Book_File);
   
         Ada.Text_IO.Get_Line (File => Book_File, Item => Book_Line, Last => Last);
         Add_Opening_Line (Book_Line (1 .. Last));
      end loop;
   
      Ada.Text_IO.Close (File => Book_File);
   
   exception
      when Ada.IO_Exceptions.Name_Error => --  File not found
         Ada.Text_IO.Put_Line ("Warning: Book " & File_Name & " not found");
      when Wrong_File_Format =>
         Ada.Text_IO.Put_Line ("Warning: invalid book file format for " & File_Name);
      when E: others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Close (Book_File);
   end Load_From_File;
   
   
   -------------------------------------
   -- Load Portable Game Notation Book --
   --------------------------------------
   
   procedure Load_From_File (Book : in Portable_Game_Notation_Book_Type; File_Name : String) is
      pragma Unreferenced (Book);
      
      Book_File : Ada.Text_IO.File_Type;
      Book_Line : String (1 .. Max_Match_Size);
      Last      : Natural;
      
      Start_New_Game    : Boolean := False;
      
      Chessboard : Chessboard_Type;
      Match     : Chess.Matches.Match_Type;
      The_Match : Match_String := (others => ' ');
      The_Match_Len : Natural := Match_String'First;
      
      Match_Name : Header_String;
      
      
      function Read_Next_Move (Current_Book_Line : in String; Move_Number : in Natural) return String is
      begin
         return Extract_Token_At 
           (Source => Current_Book_Line, Token_Number => Move_Number, Delimiter => Whitespace);
      end Read_Next_Move;      
      
      
      procedure Parse_Line (Line : in String) is
         Current_Book_Line : constant String := String_Lib.Trim 
           (Source => Line, Delimiter => ' ', Side => Both);
         Header_Info       : Header_Info_Type;
      begin
         if Current_Book_Line'Length > 0 then -- Skip empty lines
            if Is_Header (Current_Book_Line) then
               Header_Info := Extract_Header_Info_Type (Current_Book_Line);
               if Header_Info = Event then
                  Start_New_Game := True;
                  Match_Name := Extract_Header (Current_Book_Line);
               end if;
            else
               The_Match (The_Match_Len .. The_Match_Len + Current_Book_Line'Length) := Current_Book_Line & Whitespace;
               The_Match_Len := The_Match_Len + Current_Book_Line'Length + 1;
            end if;
         end if;
      exception
         when others =>
            Ada.Text_IO.Put_Line (Current_Book_Line);
            raise;
      end Parse_Line;
      
      procedure Parse_Match is
         Move : Move_Type;
         Next_Move : Natural := 1;
      begin
         loop
            
            -- The Match is now in the form of:
            -- 1 e4 e5 2 Nf3 Nc6 2 d4 d6 ..
            -- the parser shall skip the move number when encounter them
            
            Next_Move_Block : declare
               Current_Book_Line : constant String := The_Match (1 .. The_Match_Len);
               Current_Book_Move : constant String := Read_Next_Move (Current_Book_Line, Next_Move);
            begin
               -- Try to parse the next move. If the string is the move-number
               -- just skip it and go for the next one
               Move := Chessboard.Parse_Move (Current_Book_Move);
               if Move = Empty_Move then
                  Next_Move := Next_Move + 1;
                  Move := Chessboard.Parse_Move (Read_Next_Move (Current_Book_Line, Next_Move));
               end if;
            exception
               when Not_Enough_Tokens =>
                  null; -- Game ends here :=
               when others =>
                  Ada.Text_IO.Put_Line ("Cannot parse " & Current_Book_Move & " at line " & Current_Book_Line);
                  raise;
            end Next_Move_Block;
            
            exit when Move = Empty_Move;
            
            Match.Append (Move);
            Chessboard.Play (Move);
            Next_Move := Next_Move + 1;
            
         end loop;
      exception
         when Not_Enough_Tokens | Token_Not_Found =>
            null;
         when E : others =>
            Ada.Text_IO.Put_LIne (Ada.Exceptions.Exception_Name (E));
            Ada.Text_IO.Put_LIne (Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_LIne (Ada.Exceptions.Exception_Information (E));
            Ada.Text_IO.Put_Line ("Warning: invalid game found " & Match_Name);
      end Parse_Match;
      
      
      use type Count_Type;
      
   begin
      
      Ada.Text_IO.Open (File => Book_File, Mode => Ada.Text_IO.In_File, Name => File_Name);
      
      loop 
         exit when Ada.Text_IO.End_Of_File (File => Book_File);
         
         Chessboard.Initialize;
         Create_New (Match => Match);
         The_Match := (others => Whitespace);
         The_Match_Len := Match_String'First;
         
         Match_Loop : loop -- Read an entire match in the pgn format
            Ada.Text_IO.Get_Line (File => Book_File, Item => Book_Line, Last => Last);
            Parse_Line (Book_Line (1 .. Last));
            exit Match_Loop when Start_New_Game or else Ada.Text_IO.End_Of_File (File => Book_File);
         end loop Match_Loop;
         
         The_Match (The_Match_Len + 1 .. The_Match'Last) := (others => Whitespace);
         Format_Pgn_Match (Match => The_Match (1 .. The_Match_Len));
         
         Parse_Match_Block : begin
            Parse_Match;
            if Match.Length > 0 then
               Opening_Book.Append (New_Item => Match);
            end if;
         end Parse_Match_Block;
         
         Start_New_Game := False;
         
      end loop;
      
      Ada.Text_IO.Close (File => Book_File);
      
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Close (Book_File);
   end Load_From_File;
   
   
   ---------------
   -- Open_Book --
   ---------------
   
   procedure Open_Book is
   begin
      Avoid_Book := False;
   end Open_Book;
   
   
   ----------------
   -- Close_Book --
   ----------------
   
   procedure Close_Book is
   begin
      Avoid_Book := True;
   end Close_Book;
   
   
   ------------------
   -- Book_Entries --
   ------------------
   
   function Book_Entries return Natural is
   begin
      return Natural (Opening_Book.Length);
   end Book_Entries;
   
   
   ---------------
   -- Book_Move --
   ---------------
   
   function Book_Move (Chessboard : in Chessboard_Type) return Move_Type is
      use type Count_Type;
      Candidates    : Match_Type; -- Will contain only valid book lines, not a real match
      Move          : Move_Type;
      Current_Match : Match_Type;
      Choice : Count_Type;
   begin
      
      if Avoid_Book then
         return Empty_Move;
      end if;
      
      Random_Opening.Reset (Seed_Generator);
      Candidates.Create_New;
      
      Current_Match.Create_New_From_Game (Chessboard);

      for Match of Opening_Book loop
         if Current_Match < Match then -- Ensure that a next-move exists in the book
            Move := Match.Get_Move_At_Index (Current_Match.Length);
            Candidates.Append (Move);
         end if;
      end loop;
      
      if Candidates.Length > 0 then
         Choice := (Random_Opening.Random (Seed_Generator) mod Candidates.Length) + Count_Type'First;
         return Candidates.Get_Move_At_Index (Index => Choice);
      end if;
                                 
      return Empty_Move;
   end Book_Move;

end Chess.Books.Openings;
