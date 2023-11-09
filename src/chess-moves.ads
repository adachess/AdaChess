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


with Chess.Pieces; use Chess.Pieces;
with Chess.Board;  use Chess.Board;


package Chess.Moves is

   -- Helps to output moves in the correct way while using
   -- the algebraic notation
   type Ambiguous_Flag_Type is
     (Ambiguous_None, Ambiguous_File, Ambiguous_Rank, Ambiguous_Both)
     with Size => 2;

   -- Flags are 1-bit information that will help the move
   -- generator to perform some operation faster.
   type Flag_Type is
     (Standard_Move, Pawn_Move_Two_Square, Capture_En_Passant,
      Promotion, Castle, Null_Move, No_Move) 
     with
       Size => 3;

   type Castle_Type is
     (No_Castle, White_Kingside, White_Queenside, Black_Kingside, Black_Queenside) with Size => 3;
   
   ------------
   -- Checks --
   ------------
   
   type Check_Type is
     (No_Check,   
      Direct_Check, 
      Discovery_Check,
      Double_Check,
      Checkmate,
      Unknown_Check)
     with Size => 3;
   -- List of all possible type of checks that could appear in a game. Each of
   -- the listed type is exclusive. A double check, for example, implicitely
   -- implies a discovery check involved. 
   -- A special value, called Unknown_Check, is used when a FEN position with
   -- a king in check appears.
   

   -- Helper function to cast a Check as a boolean.
   function "not" (Checks : in Check_Type) return Boolean is
     (if Checks = No_Check then True else False);
   

   -- This is the move description in the engine.
   -- Instances of this record will be used more than other
   -- structured types in AdaChess. So, the smaller is this record,
   -- the faster is the engine computation. However, it's a good idea to
   -- keep some interesting information to perform some play/undo operation
   -- faster using a small amount of memory.
   type Move_Type is
      record
         Piece          : Board_Piece_Type;
         Captured       : Board_Piece_Type;
         From           : Square_Type;
         To             : Square_Type;
         Flag           : Flag_Type;
         Promotion      : Board_Piece_Type;
         Check          : Check_Type;
         Ambiguous_Flag : Ambiguous_Flag_Type;
      end record;
   --  pragma Pack (Move_Type);


   function "=" (Left, Right : in Move_Type) return Boolean is
     (Left.From = Right.From and then Left.To = Right.To
      and then (Left.Piece = Right.Piece and then Left.Captured = Right.Captured)
      and then Left.Promotion = Right.Promotion and then Left.Check = Right.Check
      and then Left.Flag = Right.Flag);

   Empty_Move : constant Move_Type := 
     Move_Type'(Piece          => Empty,
                Captured       => Empty,
                From           => 0,
                To             => 0,
                Flag           => No_Move,
                Promotion      => Empty,
                Check          => No_Check,
                Ambiguous_Flag => Ambiguous_None);
   
   
   ---------------------
   -- Move_is_Capture --
   ---------------------
   
   function Move_Is_Capture (Move : in Move_Type) return Boolean is
     (Move.Captured /= Empty or else Move.Flag = Capture_En_Passant);
   
end Chess.Moves;
