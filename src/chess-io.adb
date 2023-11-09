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


with Ada.Text_IO;
with Ada.Strings.Bounded;

with Chess.Depths;  use Chess.Depths;
with Chess.Pieces; use Chess.Pieces;
with Chess.Board;  use Chess.Board;
with String_Lib;


package body Chess.IO is
   
   ---
   
   --------------------
   -- Move_To_String --
   --------------------
   
   function Move_To_String (Move : in Move_Type; Notation : in Notation_Type := Default_Notation) return String is
      Annotated_Move : constant Annotated_Move_Type := Annotated_Move_Type'(Move => Move, Annotation => None);
   begin
      return Move_To_String (Annotated_Move => Annotated_Move, Notation => Notation);
   end Move_To_String;
   
   
   --------------------
   -- Move_To_String --
   --------------------

   function Move_To_String (Annotated_Move : in Annotated_Move_Type; Notation : in Notation_Type := Default_Notation) return String is
      
      package Bounded_String is new
        Ada.Strings.Bounded.Generic_Bounded_Length (Max => 10);
      use Bounded_String;
      
      Move       : Move_Type renames Annotated_Move.Move;
      Annotation : Annotation_Type renames Annotated_Move.Annotation;
      
      ----------------------
      -- Move_To_Winboard --
      ----------------------

      function Move_To_Pure_Algebraic return String is
         Output : Bounded_String.Bounded_String;
      begin
         Output := Bounded_String.To_Bounded_String ("");
         
         Output := Output & Pc_Sqr (Move.From);
         Output := Output & Pc_Sqr (Move.To);
         
         if Move.Promotion /= Empty then
            Output := Output & Lower_Case (Symbols (Move.Promotion));
         end if;
         
         return Bounded_String.To_String (Output);      
      end Move_To_Pure_Algebraic;

      -----------------------
      -- Move_To_Algebraic --
      -----------------------

      function Move_To_Standard_Algebraic return String is
         Output : Bounded_String.Bounded_String;
      begin
         
         Output := Bounded_String.To_Bounded_String ("");
         
        if Move.Flag = Castle then
            if File (Move.To) = File_G then
               Output := Output & "O-O";
            else
               Output := Output & "O-O-O";
            end if;
            
         else
            
            if Move.Piece not in Pawn_Type then
               Output := Output & Upper_Case (Symbols (Move.Piece));
            end if;

            -- Resolve ambiguities, if any
            case Move.Ambiguous_Flag is
               when Ambiguous_Both =>
                  Output := Output & Pc_Sqr (Move.From) (1 .. 2);
               when Ambiguous_Rank =>
                  Output := Output & Pc_Sqr (Move.From) (2 .. 2);
               when Ambiguous_File =>
                  Output := Output & Pc_Sqr (Move.From) (1 .. 1);
               when Ambiguous_None => null;
            end case;

            -- Look for captures
            if Move.Captured /= Empty or else Move.Flag = Capture_En_Passant then
               if Move.Piece in Pawn_Type then
                  Output := Output & Pc_Sqr (Move.From) (1 .. 1);
               end if;
               Output := Output & 'x';
            end if;

            -- Decode destination square
            Output := Output & Pc_Sqr (Move.To);

            -- Show promotion piece
            if Move.Promotion /= Empty then
               Output := Output & '=' & Lower_Case (Symbols (Move.Promotion));
            end if;

         end if;
      
          case Move.Check is
            when No_Check => null;
            when Direct_Check | Discovery_Check | Unknown_Check =>
               Output := Output & '+';
            when Double_Check =>
               Output := Output & "++";
            when Checkmate =>
               Output := Output & '#';
         end case;
         
         case Annotation is
            when None => null;
            when Blunder => 
               Output := Output & "??";
            when Mistake =>
               Output := Output & "?";
            when Dobious =>
               Output := Output & "?!";
            when Interesting =>
               Output := Output & "!?";
            when Good =>
               Output := Output & "!";
            when Brilliant =>
               Output := Output & "!!";
         end case;

         return Bounded_String.To_String(Output);
      end Move_To_Standard_Algebraic;

      ----------------------------
      -- Move_To_Long_Algebraic --
      ----------------------------

      function Move_To_Long_Algebraic return String is
         Output : Bounded_String.Bounded_String;
      begin
         
         Output := Bounded_String.To_Bounded_String ("");
         
         if Move.Flag = Castle then
            if File (Move.To) = File_G then
               Output := Output & "O-O";
            else
               Output := Output & "O-O-O";
            end if;
            
         else
            
            if Move.Piece not in Pawn_Type then
               Output := Output & Upper_Case (Symbols (Move.Piece));
            end if;
            
            Output := Output & Pc_Sqr (Move.From);
         
            -- Look for capture or not capture symbol
            if Move.Captured = Empty and then Move.Flag /= Capture_En_Passant then
              Output := Output & '-';
            else
               Output := Output & 'x';
            end if;
                  
           -- Decode destination square
            Output := Output & Pc_Sqr (Move.To);
         
            -- Show promotion piece
            if Move.Promotion /= Empty then
               Output := Output & '=' & Lower_Case (Symbols (Move.Promotion));
            end if;
         
         end if;
         
          case Move.Check is
            when No_Check => null;
            when Direct_Check | Discovery_Check | Unknown_Check =>
               Output := Output & '+';
            when Double_Check =>
               Output := Output & "++";
            when Checkmate =>
               Output := Output & '#';
         end case;
         
         case Annotation is
            when None => null;
            when Blunder => 
               Output := Output & "??";
            when Mistake =>
               Output := Output & "?";
            when Dobious =>
               Output := Output & "?!";
            when Interesting =>
               Output := Output & "!?";
            when Good =>
               Output := Output & "!";
            when Brilliant =>
               Output := Output & "!!";
         end case;
         
         
         return Bounded_String.To_String (Output);
      end Move_To_Long_Algebraic;
      
      ------------------
      -- Move_To_ICCF --
      ------------------
      
      function Move_To_ICCF return String is
         use String_Lib;
         Promotion_Code : constant array (Piece_Type'Range) of String (1 .. 1) :=
           (Empty        => " ",
            White_Knight => "4", White_Bishop => "3", White_Rook => "2", White_Queen => "1",
            Black_Knight => "4", Black_Bishop => "3", Black_Rook => "2", Black_Queen => "1",
            others       => (" "));
      begin
         return ""
           & Trim (Source    => Natural'Image (File (Move.From) + 1),
                   Delimiter => Whitespace,
                   Side      => Both)
           & Trim (Source    => Natural'Image (Rank (Move.From)),
                   Delimiter => Whitespace,
                   Side      => Both)
           & Trim (Source    => Natural'Image (File (Move.To) + 1),
                   Delimiter => Whitespace,
                   Side      => Both)
           & Trim (Source    => Natural'Image (Rank (Move.To)),
                   Delimiter => Whitespace,
                   Side      => Both)
           & Trim (Source    => Promotion_Code (Move.Promotion),
                   Delimiter => Whitespace,
                   Side      => Both);
      end Move_To_ICCF;
      
   begin
      case Notation is
         when Pure_Algebraic =>
            return Move_To_Pure_Algebraic;
         when Standard_Algebraic =>
            return Move_To_Standard_Algebraic;
         when Long_Algebraic => 
            return Move_To_Long_Algebraic;
         when ICCF =>
            return Move_To_ICCF;
      end case;
   end Move_To_String;

   
   ----------------
   -- Print_Move --
   ----------------
   
   procedure Print_Move (Annotated_Move : in Annotated_Move_Type; Notation : in Notation_Type := Default_Notation) is
      Move_Str : constant String := Move_To_String (Annotated_Move => Annotated_Move, Notation => Notation);
   begin
      Ada.Text_IO.Put (Move_Str);
   end Print_Move;
   
   
   ----------------
   -- Print_Move --
   ----------------

   procedure Print_Move (Move : in Move_Type; Notation : in Notation_Type := Default_Notation) is
      Move_Str : constant String := Move_To_String (Move, Notation);
   begin
      Ada.Text_IO.Put (Move_Str);
   end Print_Move;
   
   
   ---------------
   -- Print_Game --
   ----------------

   procedure Print_Game (Chessboard    : in Chessboard_Type; 
                         Move_Notation : in Notation_Type := Default_Notation;
                         Pretty_Print  : in Boolean := True) is
      Move          : Move_Type;
      Move_Ply      : Depth_Type;
      White_To_Move : Boolean;
      Black_To_Move : Boolean;

      Max_Length : Natural := 1;

      -- Find the longest string to output a move.
      -- This will be used for pretty print
      procedure Find_Longest_White_Move_String is
      begin
         for I in Zero_Depth .. Chessboard.History_Ply - 1 loop
            Move := Chessboard.Moves_History (I).Move;
            if Move.Piece in White_Piece_Type then
               Move_Codification : declare
                  Move_String : constant String := Move_To_String (Move, Move_Notation);
                  Len         : constant Natural := Move_String'Length;
               begin
                  if Len > Max_Length then
                     Max_Length := Len;
                  end if;
               end Move_Codification;
            end if;
         end loop;
         Max_Length := Max_Length - 1; -- remove leading white space
      end Find_Longest_White_Move_String;
      
   begin
      
      Ada.Text_IO.New_Line;
      
      if Pretty_Print then
         Find_Longest_White_Move_String;
      end if;

      for I in Zero_Depth .. Chessboard.History_Ply - 1 loop
         Move := Chessboard.Moves_History (I).Move;
         Move_Ply := (if I mod 2 = 0 then I / 2 else I / 2 + 1);
         White_To_Move := Move.Piece in White_Piece_Type;
         Black_To_Move := not White_To_Move;
            
         if White_To_Move then
            if Pretty_Print then
               Ada.Text_IO.New_Line;
            end if;
            Ada.Text_IO.Put (History_Depth_Type'Image (Move_Ply) & '.');
         end if;

         if Black_To_Move and then I = Zero_Depth then
            Ada.Text_IO.Put (History_Depth_Type'Image (Move_Ply) & ". ..");
            for I in 2 .. Max_Length loop
               Ada.Text_IO.Put (" ");
            end loop;
         end if;
         
         Ada.Text_IO.Put (" ");
         
         Move_String_Block : declare
            Move_String : constant String := Move_To_String (Move, Move_Notation);
            Len         : constant Natural := Move_String'Length;
         begin
            Ada.Text_IO.Put (Move_String);
            for I in Len .. Max_Length loop
               Ada.Text_IO.Put (" ");
            end loop;
         end Move_String_Block;
            
      end loop;
      
      Ada.Text_IO.New_Line;
   end Print_Game;
      
end Chess.IO;
