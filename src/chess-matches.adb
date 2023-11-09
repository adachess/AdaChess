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

with Chess.IO;
with Chess.Pieces;
with Chess.Depths;


package body Chess.Matches is

   
   ----------------
   -- Create_New --
   ----------------
   
   procedure Create_New (Match : out Match_Type; Start : in Count_Type := 1) is
   begin
      Match := Match_Type'
        (Moves  => Empty_Match,
         Status => In_Progress,
         Start  => Start);
   end Create_New;
   
   
   --------------------------
   -- Create_New_From_Game --
   --------------------------
   
   procedure Create_New_From_Game (Match : out Match_Type; Chessboard : in Chessboard_Type) is
      use Chess.Depths;
      Move : Move_Type := Empty_Move;
   begin
      Match.Create_New (Start => 1);
      for I in Zero_Depth .. Chessboard.History_Ply - 1 loop
         Move := Chessboard.Moves_History (I).Move;
         Match.Append (Move);
      end loop;
   end Create_New_From_Game;
   
   
   -----------
   -- Append --
   ------------
   
   procedure Append (Match : in out Match_Type; Move : in Move_Type) is
   begin
      Match.Moves.Append (Move);
   end Append;
   
   
   ---------------------
   -- Delete_Last_Move --
   ----------------------
   
   procedure Delete_Last_Move (Match : in out Match_Type) is
   begin
      if Match.Moves.Length > 0 then
         Match.Moves.Delete_Last;
      end if;
   end Delete_Last_Move;
   
   
   ------------
   -- Length --
   ------------
   
   function Length (Match : in Match_Type) return Count_Type is
   begin
      return Match.Moves.Length;
   end Length;
   
   
   -----------------------
   -- Get_Move_At_Index --
   -----------------------
   
   function Get_Move_At_Index (Match : in Match_Type; Index : Count_Type) return Move_Type is
   begin
      return Match.Moves (Index);
   end Get_Move_At_Index;
   
   
   -------------
   -- Display --
   -------------
   
   procedure Display (Match : in Match_Type; Format : in Boolean := True) is
      use Chess.Pieces;
      Nb_Of_Moves : constant Count_Type := Match.Moves.Length;

      Move : Move_Type := Empty_Move;
      Move_Number : Count_Type := Match.Start;
      
      Len         : Natural := 0; -- Length of a move-string
      Longest     : Natural := 0; -- Longest length of a move-string
      Space       : Natural := 0; -- The space to allocate for each colum
      
      function S (Amount : in Natural) return String is
         Dummy : constant String (1 .. Amount) := (others => ' ');
      begin         
         return Dummy;
      end S;
      
   begin
      if Nb_Of_Moves = 0 then
         Ada.Text_IO.Put_Line ("So far, game has not yet started");
         
      elsif Format then
         -- Search the longest move-string first, and detect the space to
         -- allocate for each column.
         for Move of Match.Moves loop
            Len := Chess.IO.Move_To_String (Move)'Length;
            if Len > Longest then
               Longest := Len;
            end if;
         end loop;
         
         for I in Match.Moves.First_Index .. Match.Moves.Last_Index loop
            Move := Match.Moves (I);
            Len := Chess.IO.Move_To_String (Move)'Length;
            
            -- Calculate the space to add to the move-string to align all moves
            -- to the longest move-string found
            Space := Longest - Len + 1;
            
            if Move.Piece in Black_Piece_Type then
               if I = Match.Moves.First_Index then
                  Ada.Text_IO.Put (Count_Type'Image (Move_Number) & ". .." & S (Longest - 1));
               end if;
               Ada.Text_IO.Put (Chess.IO.Move_To_String (Move));
               if I /= Match.Moves.Last_Index then
                  Ada.Text_IO.New_Line;
               end if;
            end if;
            
            if Move.Piece in White_Piece_Type then
               Ada.Text_IO.Put (Count_Type'Image (Move_Number) & ". ");
               Ada.Text_IO.Put (Chess.IO.Move_To_String (Move) & S (Space));
            end if;
            
            if Move.Piece in Black_Piece_Type then
               Move_Number := Move_Number + 1;
            end if;
               
         end loop;
         
      else
         for Move of Match.Moves loop
            Chess.Io.Print_Move (Move);
            Ada.Text_IO.Put (' ');
         end loop;
      end if;
   
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Match status: " & Match_Status_Type'Image (Match.Status));
      
   end Display;
   
   
   --------
   -- "=" -
   --------
   
   function "=" (Left, Right : Match_Type) return Boolean is
      First : Count_Type renames Left.Moves.First_Index;
      Last  : Count_Type renames Left.Moves.Last_Index;
   begin
      return Left.Moves.Length = Right.Moves.Length
        and then (for all I in First .. Last => Left.Moves (I) = Right.Moves (I));
   end "=";
   
   
   ----------
   -- "<=" --
   ----------
   
   function "<" (Left, Right : Match_Type) return Boolean is
      First : Count_Type renames Left.Moves.First_Index;
      Last  : Count_Type renames Left.Moves.Last_Index;
   begin
      return Left.Moves.Length < Right.Moves.Length
        and then (for all I in First .. Last => Left.Moves (I) = Right.Moves (I));
   end "<";
          
   

end Chess.Matches;
