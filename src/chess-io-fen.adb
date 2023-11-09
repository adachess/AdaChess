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

with Chess;
with Chess.Colors; use Chess.Colors;
with Chess.Pieces; use Chess.Pieces;
with Chess.Board;  use Chess.Board;
with Chess.Board.Directions; use Chess.Board.Directions;
with Chess.Depths; use Chess.Depths;

with String_Lib;

package body Chess.IO.Fen is

   -----------------
   -- From_String --
   -----------------
   
   procedure From_String (Chessboard : out Chessboard_Type; Fen : in String) is
      use String_Lib;
      
      ---------------------
      -- Fen_Load_Pieces --
      ---------------------
   
      procedure Fen_Load_Pieces (Chessboard : in out Chessboard_Type; Fen : in String) is
         Item   : Character;
         Next   : Natural := 0;
         Square : Square_Type;
         Sq     : Integer := 1;
         type Standard_Board is array (1 .. 64) of Square_Type;
         Board  : constant Standard_Board :=
           (A8, B8, C8, D8, E8, F8, G8, H8,
            A7, B7, C7, D7, E7, F7, G7, H7,
            A6, B6, C6, D6, E6, F6, G6, H6,
            A5, B5, C5, D5, E5, F5, G5, H5,
            A4, B4, C4, D4, E4, F4, G4, H4,
            A3, B3, C3, D3, E3, F3, G3, H3,
            A2, B2, C2, D2, E2, F2, G2, H2,
            A1, B1, C1, D1, E1, F1, G1, H1);
      begin
      
         -- Sq := Board'First;
      
         for I in Fen'Range loop
            Item := Fen (I);
            Square := Board (Sq);
            case Item is
               when 'P' => Chessboard.Square (Square) := White_Pawn;
               when 'N' => Chessboard.Square (Square) := White_Knight;
               when 'B' => Chessboard.Square (Square) := White_Bishop;
               when 'R' => Chessboard.Square (Square) := White_Rook;
               when 'Q' => Chessboard.Square (Square) := White_Queen;
               when 'K' => Chessboard.Square (Square) := White_King;
                  Chessboard.White_King_Position := Square;
               when 'p' => Chessboard.Square (Square) := Black_Pawn;
               when 'n' => Chessboard.Square (Square) := Black_Knight;
               when 'b' => Chessboard.Square (Square) := Black_Bishop;
               when 'r' => Chessboard.Square (Square) := Black_Rook;
               when 'q' => Chessboard.Square (Square) := Black_Queen;
               when 'k' => Chessboard.Square (Square) := Black_King;
                  Chessboard.Black_King_Position := Square;
               
               when '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' => 
                  --  for J in Sq .. Sq + Character'Pos (Item) - Character'Pos ('1') loop
                  --     Square := Board (J);
                  --     Chessboard.Square (Square) := Empty;
                  --  end loop;
                  Sq := Sq + Character'Pos (Item) - Character'Pos ('1');
               
               when others => Sq := Sq - 1; -- when '/' has found.
            end case;
	 
            if Sq < Board'Last then
               Sq := Sq + 1;
            end if;
	 
            Next := Next + 1;
            exit when Item = ' '; -- board setup ends here!	 
         end loop;
      
         -- Update pieces-list
         for I in Chessboard.Square'Range loop
            if Chessboard.Square (I) in White_Piece_Type then
               Chessboard.Add_White_Piece (I);
            elsif Chessboard.Square (I) in Black_Piece_Type then
               Chessboard.Add_Black_Piece (I);
            end if;
         end loop;
      
      end Fen_Load_Pieces;
   
      ---------------------------
      -- Fen_Load_Side_To_Move --
      ---------------------------
   
      procedure Fen_Load_Side_To_Move (Chessboard : in out Chessboard_Type; Fen : in String) is
      begin
         if Fen = "w" then
            Chessboard.Side_To_Move := White;
         elsif Fen = "b" then
            Chessboard.Side_To_Move := Black;
         else
            raise Invalid_Fen with "Side to move is not specified";
         end if;
      end Fen_Load_Side_To_Move;
      
      ---------------------------
      -- Fen_Load_Castle_Flags --
      ---------------------------
   
      procedure Fen_Load_Castle_Flags (Chessboard : in out Chessboard_Type; Fen : in String) is
         Item : Character;
      begin
         for I in Fen'Range loop
            Item := Fen (I);
            case Item is
               when 'K' => 
                  Chessboard.White_Castle_Kingside := (others => True);
               when 'Q' => 
                  Chessboard.White_Castle_Queenside := (others => True);
               when 'k' =>
                  Chessboard.Black_Castle_Kingside := (others => True);
               when 'q' => 
                  Chessboard.Black_Castle_Queenside := (others => True);
               when '-' =>
                  Chessboard.White_Castle_Kingside := (others => False);
                  Chessboard.White_Castle_Queenside := (others => False);
                  Chessboard.Black_Castle_Kingside := (others => False);
                  Chessboard.Black_Castle_Queenside := (others => False);
                  
               when others => null;
            end case;
         end loop;
      end Fen_Load_Castle_Flags;
   
   
      -------------------------
      -- Fen_Load_En_Passant --
      -------------------------
   
      procedure Fen_Load_En_Passant (Chessboard : in out Chessboard_Type; Fen : in String) is
      begin
         if Fen /= "-" then
            for I in Pc_Sqr'Range loop
               if Fen = Pc_Sqr (I) then
                  Chessboard.En_Passant (Chessboard.History_Ply) := I;
                  return;
               end if;
            end loop;
         end if;
      end Fen_Load_En_Passant;
   
   
      ---------------------------------
      -- Fen_Load_Fifty_Move_Counter --
      ---------------------------------
   
      procedure Fen_Load_Fifty_Move_Counter (Chessboard : in out Chessboard_Type; Fen : in String) is
      begin
         if Fen'Length = 0 then
            Chessboard.Fifty := 0;
         else
            Chessboard.Fifty := Natural'Value (Fen);
         end if;
      end Fen_Load_Fifty_Move_Counter;
   
   
      ------------------------
      -- Fen_Load_Ply_Depth --
      ------------------------
   
      procedure Fen_Load_Ply_Depth (Chessboard : in out Chessboard_Type; Fen : in String) is
      begin
         pragma Unreferenced (Fen);
         -- We ignore the initial value
         Chessboard.History_Ply := Zero_Depth;
      end Fen_Load_Ply_Depth;
      
      
      En_Passant : Square_Type := No_En_Passant;
      
   begin
      
      Chessboard.Reset;
      
      ------------
      -- Pieces --
      ------------
      
      Piece_Position_Block : declare
         Fen_Data : constant String := Extract_Token_At
           (Source => Fen, Token_Number => 1, Delimiter => Whitespace);
      begin
         Fen_Load_Pieces (Chessboard, Fen_Data);
      end Piece_Position_Block;
      
      ------------------
      -- Side to move --
      ------------------
     
      Side_To_Move_Block : declare
         Fen_Data : constant String := Extract_Token_At
           (Source => Fen, Token_Number => 2, Delimiter => Whitespace);
      begin
         Fen_Load_Side_To_Move (Chessboard, Fen_Data);
      end Side_To_Move_Block;
      
      -------------------
      -- Castle Rights --
      -------------------
      
      Castle_Rights_Block : declare
         Fen_Data : constant String := Extract_Token_At
           (Source => Fen, Token_Number => 3, Delimiter => Whitespace);
      begin
         Fen_Load_Castle_Flags (Chessboard, Fen_Data);
      end Castle_Rights_Block;
      
      ----------------
      -- En passant --
      ----------------
      
      En_Passant_Block : declare
         Fen_Data : constant String := Extract_Token_At
           (Source => Fen, Token_Number => 4, Delimiter => Whitespace);
      begin
         Fen_Load_En_Passant (Chessboard, Fen_Data);
      end En_Passant_Block;
      
      --------------------------------
      -- Fifty move counter and Ply --
      --------------------------------
      
      -- From now on, it is possible to leave the other information away and
      -- still the FEN input data can be considered valid. Te parser will look
      -- for those information but, if not found, just skip them.
      
      Optional_Fen : begin
         
         Fifty_Block : declare
            Fen_Data : constant String := Extract_Token_At
              (Source => Fen, Token_Number => 5, Delimiter => Whitespace);
         begin
            Fen_Load_Fifty_Move_Counter (Chessboard, Fen_Data);
         end Fifty_Block;
           
         Ply_Block : declare
            Fen_Data : constant String := Extract_Token_At
              (Source => Fen, Token_Number => 6, Delimiter => Whitespace);
         begin
            Fen_Load_Ply_Depth (Chessboard, Fen_Data);
         end Ply_Block;
         
      exception
         when others =>
            null; -- Ignore missing/invalid data
      end Optional_Fen;
      
      -----------------------
      -- Test FEN validity --
      -----------------------
      
      if Chessboard.Side_To_Move = White then
         if Chessboard.Has_King_In_Check (Black) then
            raise Invalid_Fen with "Black king is in check when it is White turn";
         end if;
      else
         if Chessboard.Has_King_In_Check (White) then
            raise Invalid_Fen with "White King is un check when it is Black turn";
         end if;
      end if;
      
      En_Passant := Chessboard.En_Passant (Chessboard.History_Ply);
      
      if En_Passant /= No_En_Passant then
         if Chessboard.Side_To_Move = White then -- Black last move
            if Rank (En_Passant) /= Rank_6 or else Chessboard.Square (En_Passant + South) /= Black_Pawn then
               raise Invalid_Fen with "En-passant square is not valid";
            end if;
         else -- White made last move
            if Rank (En_Passant) /= Rank_3 or else Chessboard.Square (En_Passant + North) /= White_Pawn then
               raise Invalid_Fen with "En-passant square is not valid";
            end if;
         end if;
      end if;
         
   exception
      when Invalid_Fen =>
         raise;
      when others => 
         raise Invalid_Fen with "Invalid FEN string";
   end From_String;
   
   
   ------------------------
   -- Fen_Save_To_String --
   ------------------------
   
     
   function Fen_Save_To_String (Chessboard : in Chessboard_Type) return String is
      
      Fen : String (1 .. 256) := (others => ' ');
      Fen_Length : Natural := 0;
      
      Piece : Piece_Type;
      Empty_Sq_Counter : Natural := 0;
      
      Borders : constant array (1 .. 8) of Square_Type := 
        (H8 + 1, H7 + 1, H6 + 1, H5 + 1, H4 + 1, H3 + 1, H2 + 1, H1 + 1);
      
      History_Ply : History_Depth_Type renames Chessboard.History_Ply;
      
      function Empty_Value (Value : in Natural) return Character is
         Amount : constant String := Natural'Image (Value);
      begin
         return Amount (Amount'Last);
      end Empty_Value;
      
   begin
      -- output the square
      for Sq in Chessboard.Square'Range loop
         Piece := Chessboard.Square (Sq);
         
         case Piece is
            when Empty => 
               Empty_Sq_Counter := Empty_Sq_Counter + 1;
            when Frame => null;
            when others => 
               if Empty_Sq_Counter > 0 then
                  Fen (Fen'First + Fen_Length) := Empty_Value (Empty_Sq_Counter);
                  Fen_Length := Fen_Length + 1;
               end if;
               Fen (Fen'First + Fen_Length) := Symbols (Piece);
               Fen_Length := Fen_Length + 1;
               Empty_Sq_Counter := 0;
         end case;
         
         if (for some Border of Borders => Border = Sq) then
            if Empty_Sq_Counter > 0 then
               Fen (Fen'First + Fen_Length) := Empty_Value (Empty_Sq_Counter);
                 Fen_Length := Fen_Length + 1;
            end if;
            if Sq <= H1 then -- avoid '/' in the end of fen
               Fen (Fen'First + Fen_Length) := '/';
               Fen_Length := Fen_Length + 1;
               Empty_Sq_Counter := 0;
            end if;
         end if;
      
      end loop;
      
      Fen (Fen'First + Fen_Length) := ' ';
      Fen_Length := Fen_Length + 1;
      
      -- Add the side to move
      if Chessboard.Side_To_Move = White then
         Fen (Fen'First + Fen_Length) := 'w';
         Fen_Length := Fen_Length + 1;
      else 
         Fen (Fen'First + Fen_Length) := 'b';
         Fen_Length := Fen_Length + 1;
      end if;
      
      Fen (Fen'First + Fen_Length) := ' ';
      Fen_Length := Fen_Length + 1;
      
      -- Castle Rights
      if not Chessboard.White_Castle_Kingside (History_Ply) and then not Chessboard.White_Castle_Queenside (History_Ply)
        and then not Chessboard.Black_Castle_Kingside (History_Ply) and then not Chessboard.Black_Castle_Queenside (History_Ply)
      then
         Fen (Fen'First + Fen_Length) := '-';
         Fen_Length := Fen_Length + 1;
      else
         if Chessboard.White_Castle_Kingside (History_Ply) then
            Fen (Fen'First + Fen_Length) := 'K';
            Fen_Length := Fen_Length + 1;
         end if;
      
         if Chessboard.White_Castle_Queenside (History_Ply) then
            Fen (Fen'First + Fen_Length) := 'Q';
            Fen_Length := Fen_Length + 1;
         end if;
      
         if Chessboard.Black_Castle_Kingside (History_Ply) then
            Fen (Fen'First + Fen_Length) := 'k';
            Fen_Length := Fen_Length + 1;
         end if;
      
         if Chessboard.Black_Castle_Queenside (History_Ply) then
            Fen (Fen'First + Fen_Length) := 'q';
            Fen_Length := Fen_Length + 1;
         end if;
      end if;
         
      Fen (Fen'First + Fen_Length) := ' ';
      Fen_Length := Fen_Length + 1;
      
      -- En-passant
      if Chessboard.En_Passant (History_Ply) /= 0 then
         Fen (Fen'First + Fen_Length .. Fen'First + Fen_Length + 1) := Pc_Sqr (Chessboard.En_Passant (History_Ply));
         Fen_Length := Fen_Length + 2;
      else
         Fen (Fen'First + Fen_Length) := '-';
         Fen_Length := Fen_Length + 1;
      end if;
      
      -- Fifty moves counter
--        declare
--           Fifty_Counter : constant String := Integer'Image (Chessboard.Fifty (History_Ply - 1));
--           Len           : constant Natural := Fifty_Counter'Length;
--        begin
--           for I in Fifty_Counter'Range loop
--              Fen (Fen'First + Fen_Length + I - 1) := Fifty_Counter (I);
--           end loop;
--           Fen_Length := Fen_Length + Len;
--        end;
--        
--        -- Ply depth
--        declare
--           Ply_Depth : constant String := Integer'Image (History_Ply / 2);
--        begin
--           for I in Ply_Depth'Range loop
--              Fen (Fen'First + Fen_Length + I - 1) := Ply_Depth (I);
--           end loop;
--           Fen_Length := Fen_Length + Ply_Depth'Length;
--        end;
      
      return Fen (Fen'First .. Fen'First + Fen_Length - 1);
      
   end Fen_Save_To_String;
   

end Chess.IO.Fen;
