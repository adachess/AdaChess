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


package Chess.IO.Fen is
   
   -- This package handle FEN data. FEN is an achronym for Forsyth-Edwards
   -- Notation and represent a compact way to describe a chess position.
   -- AdaChess is able to read and write chess position in FEN format.
   -- 
   -- Further info about FEN description can be found at the followint link:
   -- https://www.chessprogramming.org/Forsyth-Edwards_Notation
   
   
   procedure From_String (Chessboard : out Chessboard_Type; Fen : in String);
   -- Convert a Forsyth-Edwards string into an internal board state.
   --
   -- The input FEN should contain the pieces data, the side to move, castle
   -- rights and en-passant square. The ply and fifty move counter are optional
   -- and, if not provided, they will be set up to an initial value
   -- Example FEN is: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
   --
   -- Arguments
   --    Chessboard : A reference to the chessboard
   --    Fen        : The string representing a FEN-style position
   -- Exceptions:
   --     Invalid_Fen : raised if the given input does not represent a
   --     valid FEN.
   
   function Fen_Save_To_String (Chessboard : in Chessboard_Type) return String;
   -- Convert an internal board state into a Forsyth-Edward string
   --
   -- The output is guaranteed to be a full valid FEN string with all the
   -- information on it.
   --
   -- Returns:
   --    A string representing the FEN description of the current board state
    
   
   Invalid_Fen : exception;
   -- This exception signals that a FEN string does not represent a valid
   -- chess position. 
   

end Chess.IO.Fen;
