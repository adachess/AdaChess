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


with Ada.Strings.Bounded;

with Chess.Engine; use Chess.Engine;
with Chess.Moves; use Chess.Moves;


package Chess.Books.Openings is
         
   procedure Add_Opening_Book (Book_File_Name : in String);
   -- Open the current file name as opening book. If the file extensions is
   -- known the book format is automatically detected, otherwise it will be
   -- guess as simple book format
   --
   -- Arguments
   --    Book_File_Name : The file name containing the opening book data
   
   
   procedure Load_All_Opening_Books;
   -- Open all the opening book in the book list. This operation may take some
   -- time if the book is a large pgn database.
      
   
   procedure Save_As_Simple_Book (File_Name : in String);
   -- Save the current loaded opening book data as new book in the Simple format
   
   
   -----------------------------------
   -- Enable/Disable Books handling --
   -----------------------------------
   
   function Book_Move (Chessboard : in Chessboard_Type) return Move_Type;
   -- Search the opening book to find a good move. If the book contains many
   -- available entries, a random one will be selected - as ususal
   -- 
   -- Arguments
   --    Chessboard : The current chess status
   -- Returns
   --    A valid move from the book, if any, or Empty_Move if no move is found
   
   
   procedure Open_Book;
   -- Inform the engine that the opening book, contains all the books loaded,
   -- can be used to play the opening moves
   
   procedure Close_Book;
   -- Inform the engine that the opening book shall not be used. This is useful
   -- for example when benchmarking or testing.
   
   function Book_Entries return Natural;
   -- Detect how many book entries are available. Every book entry represent
   -- an opening line, i.e. a squence of moves from the starting position.
   -- Note that multiple opening books can be loaded simultaniously: this might
   -- lead to duplicated entires. The value returned from this function is NOT
   -- the number of unique book lines, but the number of entries loaded.
   --
   -- Returns
   --    The number of book lines available
  
   
   procedure Load_All_Books (Book_Dir : in String := ".");
   -- Scan an entire directory and loads all the PGN opening books available.
   -- Please use this function carefoully: loading PGN is an expensive operation
   -- due to the parser ability to extract the game by removing all the comments
   -- and the variation from a PGN file, containing any number of game inside.
   -- Huge PGN require ages to be parsed and many GUI are configured to reallocate
   -- the executable/binary file at each game. That is, make sure that the total
   -- time the engine takes to load all PGN books is acceptable (either by using
   -- small PGNs or by loading a small number, or even better by selecting each
   -- book via command line instead of using this procedure.
   --
   -- Arguments
   --    Book_Dir : The directory where to search for opening books, default is
   --               the current working directory
   
   
   Wrong_File_Format : exception;
   -- Raised when the given book does not contain the expected format or valid data
   
   Invalid_Book_Line : exception;
   -- Raised when a book line represent a non legal move
   
   Book_Not_Found : exception;
   -- Raised if a book file is not found
   
private
   
   
   type Opening_Book_Type is interface and Book_Type;
   -- An opening book is defined as a sequence of move from the beginning
   -- of the game up to a certain limit. AdaChess merge all opening books
   -- in a single entity, the Opening_Book, and all the other books are meant
   -- to open/parse book files and save the information inside this unique
   -- entity.
   
   procedure Load_From_File (Book : in Opening_Book_Type; File_Name : String) is abstract;
   -- Read a book from a file and load all the chess matches on it into the
   -- internal opening book. This is an abstract procedure meant to be
   -- overridden by specific book implementations.
   --
   -- Arguments
   --    Book : The opening book to read and parse
  
   
   -------------------------
   -- Simple Opening Book --
   -------------------------
   
   type Simple_Opening_Book_Type is new Opening_Book_Type with null record;
   -- A simple opening book is an opening book where a match is represented by
   -- a sequence of moves and each line represent a chess match.
   -- Each line consists on a sequence of move, written in any of the known
   -- notations (e2e4, Bf5+, Ng1xf3, ...). Please refer to the Chess.Notations
   -- package for more information about chess notations
   
   overriding
   procedure Load_From_File (Book : in Simple_Opening_Book_Type; File_Name : String);
   -- Read the content of a file containing, for each line, a sequence of move
   -- from the starting position.
   --
   -- Arguments
   --    Book : The opening book to read and parse

   
   ----------------------------
   -- Fen-style Opening Book --
   ----------------------------
   
   --  type Forsyth_Edwards_Notation_Book_Type is new Opening_Book_Type with null record;
   -- This is an opening book composed by a line containing the fen encoding of
   -- a position and a list of possible move from that position. 
   --  
   --  overriding
   --  procedure Load (Book : in Forsyth_Edwards_Notation_Book_Type; File_Name : String);
   -- Read the content of a file containing, for each line, a FEN position and
   -- a sequence of moves to be intended as opening lines.
   --
   -- Arguments
   --    Book : The opening book to read and parse
   
   
   ----------------------
   -- PGN opening book --
   ----------------------
   
   type Portable_Game_Notation_Book_Type is new Opening_Book_Type with null record;
   
   overriding
   procedure Load_From_File (Book : in Portable_Game_Notation_Book_Type; File_Name : String);
   -- Read the content of a PGN file and save each match as a sequence of opening
   -- moves. Note: AdaChess will verify that each move found in the book is
   -- legal
   --
   -- Arguments
   --    Book : The opening book to read and parse
   
   
   
   Avoid_Book : Boolean := False;
   
   
   Simple_Opening_Book         : Simple_Opening_Book_Type;
   Portable_Game_Notation_Book : Portable_Game_Notation_Book_Type;
   
   
   -------------------
   -- Lists of Book --
   -------------------
   
   package Book_File_Names is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 64);
   use Book_File_Names;
   
   package Opening_Book_Files is new Ada.Containers.Vectors 
     (Index_Type => Natural, Element_Type => Book_File_Names.Bounded_String);
   
   Simple_Book_File_Name_List : Opening_Book_Files.Vector;
   Pgn_Book_File_Name_List    : Opening_Book_Files.Vector;
   
   
end Chess.Books.Openings;
