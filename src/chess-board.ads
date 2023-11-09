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


with Chess.Colors; use Chess.Colors;
with Chess.Pieces; use Chess.Pieces;

package Chess.Board is

   subtype Square_Type is Natural range 0 .. 119;
   -- Squares are the place where the game is played.
   -- The engine represent a valid square in a matrix of 10x12 (120) elements
   -- disposed in an array. Using the single dimension makes everything easier
   -- and faster

   ----------------
   -- Chessboard --
   ----------------

   type Board_Type is array (Square_Type'Range) of Piece_Type;
   -- The internal board representation is an array of 10x12 slot.
   -- Inside the slot there are the pieces. The frame allowd us to detect moves
   -- out of the board. The palyable board is centered
   --
   -- x x x x x x x x x x
   -- x x x x x x x x x x
   -- x r n b q k b n r x
   -- x p p p p p p p p x
   -- x . . . . . . . . x
   -- x . . . . . . . . x
   -- x . . . . . . . . x
   -- x . . . . . . . . x
   -- x P P P P P P P P x
   -- x R N B Q K B N R x
   -- x x x x x x x x x x
   -- x x x x x x x x x x


   ----------------------
   -- Square constants --
   ----------------------

   A8 : constant Square_Type := 21;
   B8 : constant Square_Type := 22;
   C8 : constant Square_Type := 23;
   D8 : constant Square_Type := 24;
   E8 : constant Square_Type := 25;
   F8 : constant Square_Type := 26;
   G8 : constant Square_Type := 27;
   H8 : constant Square_Type := 28;

   A7 : constant Square_Type := 31;
   B7 : constant Square_Type := 32;
   C7 : constant Square_Type := 33;
   D7 : constant Square_Type := 34;
   E7 : constant Square_Type := 35;
   F7 : constant Square_Type := 36;
   G7 : constant Square_Type := 37;
   H7 : constant Square_Type := 38;

   A6 : constant Square_Type := 41;
   B6 : constant Square_Type := 42;
   C6 : constant Square_Type := 43;
   D6 : constant Square_Type := 44;
   E6 : constant Square_Type := 45;
   F6 : constant Square_Type := 46;
   G6 : constant Square_Type := 47;
   H6 : constant Square_Type := 48;

   A5 : constant Square_Type := 51;
   B5 : constant Square_Type := 52;
   C5 : constant Square_Type := 53;
   D5 : constant Square_Type := 54;
   E5 : constant Square_Type := 55;
   F5 : constant Square_Type := 56;
   G5 : constant Square_Type := 57;
   H5 : constant Square_Type := 58;

   A4 : constant Square_Type := 61;
   B4 : constant Square_Type := 62;
   C4 : constant Square_Type := 63;
   D4 : constant Square_Type := 64;
   E4 : constant Square_Type := 65;
   F4 : constant Square_Type := 66;
   G4 : constant Square_Type := 67;
   H4 : constant Square_Type := 68;

   A3 : constant Square_Type := 71;
   B3 : constant Square_Type := 72;
   C3 : constant Square_Type := 73;
   D3 : constant Square_Type := 74;
   E3 : constant Square_Type := 75;
   F3 : constant Square_Type := 76;
   G3 : constant Square_Type := 77;
   H3 : constant Square_Type := 78;

   A2 : constant Square_Type := 81;
   B2 : constant Square_Type := 82;
   C2 : constant Square_Type := 83;
   D2 : constant Square_Type := 84;
   E2 : constant Square_Type := 85;
   F2 : constant Square_Type := 86;
   G2 : constant Square_Type := 87;
   H2 : constant Square_Type := 88;

   A1 : constant Square_Type := 91;
   B1 : constant Square_Type := 92;
   C1 : constant Square_Type := 93;
   D1 : constant Square_Type := 94;
   E1 : constant Square_Type := 95;
   F1 : constant Square_Type := 96;
   G1 : constant Square_Type := 97;
   H1 : constant Square_Type := 98;

   No_En_Passant : constant Square_Type := 0;

   ---------------------
   -- Frame definition --
   ----------------------

   type Square_Flag_Type is array (Square_Type'Range) of Boolean;
   --  Pragma Pack (Square_Flag_Type);

   Square_Is_Frame : Square_Flag_Type :=
     (True, True,  True,  True,  True,  True,  True,  True,  True,  True,
      True, True,  True,  True,  True,  True,  True,  True,  True,  True,
      True, False, False, False, False, False, False, False, False, True,
      True, False, False, False, False, False, False, False, False, True,
      True, False, False, False, False, False, False, False, False, True,
      True, False, False, False, False, False, False, False, False, True,
      True, False, False, False, False, False, False, False, False, True,
      True, False, False, False, False, False, False, False, False, True,
      True, False, False, False, False, False, False, False, False, True,
      True, False, False, False, False, False, False, False, False, True,
      True, True,  True,  True,  True,  True,  True,  True,  True,  True,
      True, True,  True,  True,  True,  True,  True,  True,  True,  True);


   ---------------------------------------------------------------------------
   -- Functions, utilities to find cordinates of a square in the ChessBoard --
   ---------------------------------------------------------------------------

   subtype Coordinate_Type is Integer range -1 .. 14;
   -- The Coordinate system in normal chess is expresses by File and Ranks.
   -- Normally the Rank is expressed via a letter, while File through number,
   -- like e3 for example. However, the internal representation is
   -- a number in the range of all possible value, including diagonal. The
   -- special value -1  (minus one) represent an invalid coordinate.


   function File (Square : in Square_Type) return Coordinate_Type
     with
       Inline => True;
   -- Extract the File of the given square.
   --
   -- Arguments
   --    Square : the square to get on which file stands
   -- Returns
   --    The File where the square stands

   function Rank (Square : in Square_Type) return Coordinate_Type
     with
       Inline => True;
   -- Extract the Rank of the given square.
   --
   -- Arguments
   --    Square : the square to get on which tank stands

   function Diagonal (Square : in Square_Type) return Coordinate_Type
     with
       Inline => True;
   -- Extract the Diagonal of the given square.
   --
   -- Arguments
   --    Square : the square to get on which diagonal stands

   function Anti_Diagonal (Square : in Square_Type) return Coordinate_Type
     with
       Inline => True;
   -- Extract the Anti Diagonal of the given square.
   --
   -- Arguments
   --    Square : the square to get on which anti-diagonal stands


   Rank_1         : constant Coordinate_Type := 1;
   Rank_2         : constant Coordinate_Type := 2;
   Rank_3         : constant Coordinate_Type := 3;
   Rank_4         : constant Coordinate_Type := 4;
   Rank_5         : constant Coordinate_Type := 5;
   Rank_6         : constant Coordinate_Type := 6;
   Rank_7         : constant Coordinate_Type := 7;
   Rank_8         : constant Coordinate_Type := 8;

   File_A         : constant Coordinate_Type := 0;
   File_B         : constant Coordinate_Type := 1;
   File_C         : constant Coordinate_Type := 2;
   File_D         : constant Coordinate_Type := 3;
   File_E         : constant Coordinate_Type := 4;
   File_F         : constant Coordinate_Type := 5;
   File_G         : constant Coordinate_Type := 6;
   File_H         : constant Coordinate_Type := 7;


   ------------------
   -- Square Color --
   ------------------

   type Color_Square_Table_Type is array (Board_Type'Range) of Color_Type;

   Color_Board         : constant Color_Square_Table_Type :=
     (White, Black, White, Black, White, Black, White, Black, White, Black,
      Black, White, Black, White, Black, White, Black, White, Black, White,
      Black, White, Black, White, Black, White, Black, White, Black, White,
      White, Black, White, Black, White, Black, White, Black, White, Black,
      Black, White, Black, White, Black, White, Black, White, Black, White,
      White, Black, White, Black, White, Black, White, Black, White, Black,
      Black, White, Black, White, Black, White, Black, White, Black, White,
      White, Black, White, Black, White, Black, White, Black, White, Black,
      Black, White, Black, White, Black, White, Black, White, Black, White,
      White, Black, White, Black, White, Black, White, Black, White, Black,
      White, Black, White, Black, White, Black, White, Black, White, Black,
      Black, White, Black, White, Black, White, Black, White, Black, White);


   -----------------------------------------------
   -- Output the square in a human readable way --
   -----------------------------------------------

   type Square_String_Representation_Type is array (Square_Type'Range) of String (1 .. 2);
   --  pragma Pack (Square_String_Representation_Type);


   Pc_Sqr : constant Square_String_Representation_Type :=
     ("  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ",
      "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ",
      "  ", "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8", "  ",
      "  ", "a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7", "  ",
      "  ", "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6", "  ",
      "  ", "a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5", "  ",
      "  ", "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4", "  ",
      "  ", "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3", "  ",
      "  ", "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2", "  ",
      "  ", "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1", "  ",
      "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ",
      "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ");
   -- Convert a square number into his human readable name.
   -- Note: the name Pc_Sqr exists for historical reason in AdaChess


private

   ----------------------------
   -- Files and Ranks tables --
   ----------------------------

   type Board_Coordinate_Type is array (Board_Type'Range) of Coordinate_Type;
   --  pragma Pack (Board_Coordinate_Type);

   Files : constant Board_Coordinate_Type :=
     (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1,  0,  1,  2,  3,  4,  5,  6,  7, -1,
      -1,  0,  1,  2,  3,  4,  5,  6,  7, -1,
      -1,  0,  1,  2,  3,  4,  5,  6,  7, -1,
      -1,  0,  1,  2,  3,  4,  5,  6,  7, -1,
      -1,  0,  1,  2,  3,  4,  5,  6,  7, -1,
      -1,  0,  1,  2,  3,  4,  5,  6,  7, -1,
      -1,  0,  1,  2,  3,  4,  5,  6,  7, -1,
      -1,  0,  1,  2,  3,  4,  5,  6,  7, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1);

   Ranks : constant Board_Coordinate_Type :=
     (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1,  8,  8,  8,  8,  8,  8,  8,  8, -1,
      -1,  7,  7,  7,  7,  7,  7,  7,  7, -1,
      -1,  6,  6,  6,  6,  6,  6,  6,  6, -1,
      -1,  5,  5,  5,  5,  5,  5,  5,  5, -1,
      -1,  4,  4,  4,  4,  4,  4,  4,  4, -1,
      -1,  3,  3,  3,  3,  3,  3,  3,  3, -1,
      -1,  2,  2,  2,  2,  2,  2,  2,  2, -1,
      -1,  1,  1,  1,  1,  1,  1,  1,  1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1);

   Diagonals : constant Board_Coordinate_Type:=
     (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1,  7,  6,  5,  4,  3,  2,  1,  0, -1,
      -1,  6,  5,  4,  3,  2,  1,  0,  8, -1,
      -1,  5,  4,  3,  2,  1,  0,  8,  9, -1,
      -1,  4,  3,  2,  1,  0,  8,  9, 10, -1,
      -1,  3,  2,  1,  0,  8,  9, 10, 11, -1,
      -1,  2,  1,  0,  8,  9, 10, 11, 12, -1,
      -1,  1,  0,  8,  9, 10, 11, 12, 13, -1,
      -1,  0,  8,  9, 10, 11, 12, 13, 14, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1);

   Anti_Diagonals : constant Board_Coordinate_Type:=
     (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1,  7,  8,  9, 10, 11, 12, 13, 14, -1,
      -1,  6,  7,  8,  9, 10, 11, 12, 13, -1,
      -1,  5,  6,  7,  8,  9, 10, 11, 12, -1,
      -1,  4,  5,  6,  7,  8,  9, 10, 11, -1,
      -1,  3,  4,  5,  6,  7,  8,  9, 10, -1,
      -1,  2,  3,  4,  5,  6,  7,  8,  9, -1,
      -1,  1,  2,  3,  4,  5,  6,  7,  8, -1,
      -1,  0,  1,  2,  3,  4,  5,  6,  7, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1);


end Chess.Board;
