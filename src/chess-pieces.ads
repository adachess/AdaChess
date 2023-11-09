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


package Chess.Pieces is

   -----------------------
   -- Piece description --
   -----------------------

   type Piece_Type is
     (Frame, Empty,
      White_Pawn, White_Knight, White_Bishop, White_Rook, White_Queen, White_King,
      Black_Pawn, Black_Knight, Black_Bishop, Black_Rook, Black_Queen, Black_King)
     with Size => 4;

   for Piece_Type use
     (Frame        => 0,
      Empty        => 1,
      White_Pawn   => 2,
      White_Knight => 3,
      White_Bishop => 4,
      White_Rook   => 5,
      White_Queen  => 6,
      White_King   => 7,
      Black_Pawn   => 8,
      Black_Knight => 9,
      Black_Bishop => 10,
      Black_Rook   => 11,
      Black_Queen  => 12,
      Black_King   => 13);

   
   -- "Not" a piece tell us if a piece is empty or "Not" ;-)
   function "not" (Piece : in Piece_Type) return Boolean is
     (if Piece = Empty then True else False)
     with Pre => (Piece /= Frame);
   -- Shortcut to detect if a Piece data represent a real chess piece into the
   -- board. This function will return False if and only if the Piece is Empty
   -- otherwise will return False.
   --
   -- Arguments:
   --    Piece : The piece to be submitted for the test
   -- Returns:
   --     True if given piece is a chess piece, False if it is Empty.
   -- Preconditions:
   --    Piece argument shall not be a Frame
   
   subtype Board_Piece_Type is Piece_Type 
     with
       Static_Predicate => Board_Piece_Type in Empty | White_Pawn | White_Knight | White_Bishop | White_Rook | White_Queen | White_King | Black_Pawn | Black_Knight | Black_Bishop | Black_Rook | Black_Queen | Black_King;
           
         
   subtype Chess_Piece_Type is Piece_Type 
     with
       Static_Predicate => Chess_Piece_Type in White_Pawn | White_Knight | White_Bishop | White_Rook | White_Queen | White_King | Black_Pawn | Black_Knight | Black_Bishop | Black_Rook | Black_Queen | Black_King;
   
   subtype White_Piece_Type is Piece_Type 
     with
       Static_Predicate => White_Piece_Type in White_Pawn | White_Knight | White_Bishop | White_Rook | White_Queen | White_King;
   
   subtype Black_Piece_Type is Piece_Type 
     with
       Static_Predicate => Black_Piece_Type in Black_Pawn | Black_Knight | Black_Bishop | Black_Rook | Black_Queen | Black_King;

   
   subtype White_Promotion_Type is Piece_Type
     with Static_Predicate => White_Promotion_Type in White_Knight | White_Bishop | White_Rook | White_Queen;
   subtype Black_Promotion_Type is Piece_Type
     with Static_Predicate => Black_Promotion_Type in Black_Knight | Black_Bishop | Black_Rook | Black_Queen;
       
   
   subtype White_Non_Pawn is Piece_Type 
     with Static_Predicate => White_Non_Pawn in White_Knight | White_Bishop | White_Rook | White_Queen;
   subtype Black_Non_Pawn is Piece_Type
     with Static_Predicate => Black_Non_Pawn in Black_Knight | Black_Bishop | Black_Rook | Black_Queen;
   
   subtype Pawn_Type   is Piece_Type 
     with Static_Predicate => Pawn_Type   in White_Pawn | Black_Pawn;
   subtype Knight_Type is Piece_Type
     with Static_Predicate => Knight_Type in White_Knight | Black_Knight;
   subtype Bishop_Type is Piece_Type 
     with Static_Predicate => Bishop_Type in White_Bishop | Black_Bishop;
   subtype Rook_Type   is Piece_Type 
     with Static_Predicate => Rook_Type   in White_Rook | Black_Rook;
   subtype Queen_Type  is Piece_Type 
     with Static_Predicate => Queen_Type  in White_Queen | Black_Queen;
   subtype King_Type   is Piece_Type 
     with Static_Predicate => King_Type   in White_King | Black_King;

   subtype Slide_Type is Piece_Type
     with Static_Predicate => Slide_Type in Bishop_Type | Rook_Type | Queen_Type;
   
   type Symbols_Type is array (Piece_Type'Range) of Character;
   Symbols : constant Symbols_Type :=
     (Frame        => ' ', Empty => ' ',
      White_Pawn   => 'P', White_Knight => 'N', White_Bishop => 'B',
      White_Rook   => 'R', White_Queen  => 'Q', White_King   => 'K',
      Black_Pawn   => 'p', Black_Knight => 'n', Black_Bishop => 'b',
      Black_Rook   => 'r', Black_Queen  => 'q', Black_King   => 'k');
   
   -------------------
   -- Piece Counter --
   -------------------
   
   subtype Piece_Counter_Type is Natural range 0 .. 16;
   subtype Pawn_Counter_Type is Piece_Counter_Type range 0 .. 8;
   
end Chess.Pieces;
