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


with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions;

with Chess.IO; use Chess.IO;
with Chess.Notations; use Chess.Notations;
with Chess.Board.Attacks_Data; use Chess.Board.Attacks_Data;


package body Chess.Engine is


   -------------------
   -- Moves_Counter --
   -------------------

   function Moves_Counter (Chessboard : in Chessboard_Type; Ply : in Depth_Type) return Natural is
   begin
      return Chessboard.Moves_Pointer (Ply + 1) - Chessboard.Moves_Pointer (Ply);
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Moves_Counter;


   ---------------------
   -- Add_White_Piece --
   ---------------------

   procedure Add_White_Piece (Chessboard : in out Chessboard_Type; Square : in Square_Type) is
      White_Pieces         : Pieces_List_Type renames Chessboard.Pieces_List (White_Pieces_Range);
      White_Pieces_Counter : Piece_Counter_Type renames Chessboard.White_Pieces_Counter;
   begin
      pragma Assert (White_Pieces_Counter < 16, "White Pieces are more than allowed");
      White_Pieces_Counter := White_Pieces_Counter + 1;
      White_Pieces (White_Pieces_Counter) := Square;
      Chessboard.Piece_Table (Square) := White_Pieces_Counter;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Add_White_Piece;


   ---------------------
   -- Add_Black_Piece --
   ---------------------

   procedure Add_Black_Piece (Chessboard : in out Chessboard_Type; Square : in Square_Type) is
      Black_Pieces         : Pieces_List_Type   renames Chessboard.Pieces_List (Black_Pieces_Range);
      Black_Pieces_Counter : Piece_Counter_Type renames Chessboard.Black_Pieces_Counter;
   begin
      pragma Assert (Black_Pieces_Counter < 16, "Black Pieces are more than allowed");
      Black_Pieces_Counter := Black_Pieces_Counter + 1;
      Black_Pieces (Black_Pieces_Counter + Black_Pieces_Start_Index) := Square;
      Chessboard.Piece_Table (Square) := Black_Pieces_Counter + Black_Pieces_Start_Index;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Add_Black_Piece;


   ------------------------
   -- Delete_White_Piece --
   ------------------------

   procedure Delete_White_Piece (Chessboard : in out Chessboard_Type; Square : in Square_Type) is
      Index                : Square_Type;
      White_Pieces         : Pieces_List_Type   renames Chessboard.Pieces_List (White_Pieces_Range);
      White_Pieces_Counter : Piece_Counter_Type renames Chessboard.White_Pieces_Counter;
   begin
      Index := Chessboard.Piece_Table (Square);
      White_Pieces (Index) := White_Pieces (White_Pieces_Counter);
      White_Pieces (White_Pieces_Counter) := 0;
      White_Pieces_Counter := White_Pieces_Counter - 1;
      Chessboard.Piece_Table (Square) := 0;
      Chessboard.Piece_Table (White_Pieces (Index)) := Index;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Delete_White_Piece;


   ------------------------
   -- Delete_Black_Piece --
   ------------------------

   procedure Delete_Black_Piece (Chessboard : in out Chessboard_Type; Square : in Square_Type) is
      Index                : Square_Type;
      Black_Pieces         : Pieces_List_Type   renames Chessboard.Pieces_List (Black_Pieces_Range);
      Black_Pieces_Counter : Piece_Counter_Type renames Chessboard.Black_Pieces_Counter;
   begin
      Index := Chessboard.Piece_Table (Square);
      Black_Pieces (Index) := Black_Pieces (Black_Pieces_Start_Index + Black_Pieces_Counter);
      Black_Pieces (Black_Pieces_Start_Index + Black_Pieces_Counter) := 0;
      Black_Pieces_Counter := Black_Pieces_Counter - 1;
      Chessboard.Piece_Table (Square) := 0;
      Chessboard.Piece_Table (Black_Pieces (Index)) := Index;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Delete_Black_Piece;


   ------------------------
   -- Update_White_Piece --
   ------------------------

   procedure Update_White_Piece (Chessboard : in out Chessboard_Type; From, To : in Square_Type) is
      Index        : Square_Type;
      White_Pieces : Pieces_List_Type renames Chessboard.Pieces_List (White_Pieces_Range);

   begin
      Index := Chessboard.Piece_Table (From);
      pragma Assert (Index > 0, "Invalid white Index value:" & Square_Type'Image (Index));
      Chessboard.Piece_Table (From) := 0;
      Chessboard.Piece_Table (To) := Index;
      White_Pieces (Index) := To;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Update_White_Piece;


   ------------------------
   -- Update_Black_Piece --
   ------------------------

   procedure Update_Black_Piece (Chessboard : in out Chessboard_Type; From, To : in Square_Type) is
      Index : Square_Type;
      Black_Pieces : Pieces_List_Type renames Chessboard.Pieces_List (Black_Pieces_Range);
   begin
      Index := Chessboard.Piece_Table (From);
      pragma Assert (Index > 0, "Invalid black index value: " & Square_Type'Image (Index));
      Chessboard.Piece_Table (From) := 0;
      Chessboard.Piece_Table (To) := Index;
      Black_Pieces (Index) := To;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Update_Black_Piece;


   ---------------------
   -- Initialize_Hash --
   ---------------------

   procedure Initialize_Hash (Chessboard : in out Chessboard_Type) is
   begin
      Reset (Seed);

      -----------------------
      -- Initialize Pieces --
      -----------------------

      for Piece in Piece_Type loop
         for Square in Board_Type'Range loop
            Hash_Pieces (Piece, Square) := Random (Seed);
         end loop;
      end loop;

      ---------------------------
      -- Initialize En-passant --
      ---------------------------

      for Square in Board_Type'Range loop
         Hash_En_Passant (Square) := Random (Seed);
      end loop;

      ------------------------
      -- Initialize Castles --
      ------------------------

      for Castle in Hash_Castle'Range loop
         Hash_Castle (Castle) := Random (Seed);
      end loop;

      -----------------------------
      -- Initialize Side to move --
      -----------------------------

      Hash_Side := Random (Seed);

      Update_Hash (Chessboard);

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Initialize_Hash;


   -----------------
   -- Update_Hash --
   -----------------

   procedure Update_Hash (Chessboard : in out Chessboard_Type) is
      History_Ply : History_Depth_Type renames Chessboard.History_Ply;
      White_Pieces : Pieces_List_Type renames Chessboard.Pieces_List (White_Pieces_Range);
      Black_Pieces : Pieces_List_Type renames Chessboard.Pieces_List (Black_Pieces_Range);
   begin

      Hash := 0;

      ----------------------------
      -- Hash piece information --
      ----------------------------

      for Sq of White_Pieces loop
         exit when Sq = 0;
         Hash := Hash xor Hash_Pieces (Chessboard.Square (Sq), Sq);
      end loop;

      for Sq of Black_Pieces loop
         exit when Sq = 0;
         Hash := Hash xor Hash_Pieces (Chessboard.Square (Sq), Sq);
      end loop;

      ----------------------------
      -- Hash En_Passant square --
      ----------------------------

      if Chessboard.En_Passant (History_Ply) /= No_En_Passant then
         Hash := Hash xor Hash_En_Passant (Chessboard.En_Passant (History_Ply));
      end if;

      --------------------------
      -- Hash Castling rights --
      --------------------------

      if Chessboard.White_Castle_Kingside (History_Ply) then
         Hash := Hash xor Hash_Castle (4);
      end if;

      if Chessboard.White_Castle_Queenside (History_Ply) then
         Hash := Hash xor Hash_Castle (3);
      end if;

      if Chessboard.Black_Castle_Kingside (History_Ply) then
         Hash := Hash xor Hash_Castle (2);
      end if;

      if Chessboard.Black_Castle_Queenside (History_Ply) then
         Hash := Hash xor Hash_Castle (1);
      end if;

      -----------------------
      -- Hash side to move --
      -----------------------

      if Chessboard.Side_To_Move = Black then
         Hash := Hash xor Hash_Side;
      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Update_Hash;


   ----------------------
   -- Clear_Moves_List --
   ----------------------

   procedure Clear_Moves_List (Chessboard : in out Chessboard_Type) is
      Ply : Depth_Type renames Chessboard.Ply;
   begin
      Chessboard.Moves_Pointer (Ply + 1) := Chessboard.Moves_Pointer (Ply);
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Clear_Moves_List;


   --------------------
   -- Generate_Moves --
   --------------------

   procedure Generate_Moves (Chessboard : in out Chessboard_Type) is
      Piece         : Piece_Type;
      Target        : Square_Type;
      Pin_Direction : Direction_Type := 0;
      En_Passant    : Square_Type renames Chessboard.En_Passant (Chessboard.History_Ply);
      White_Pieces  : Pieces_List_Type renames Chessboard.Pieces_List (White_Pieces_Range);
      Black_Pieces  : Pieces_List_Type renames Chessboard.Pieces_List (Black_Pieces_Range);
   begin

      Chessboard.Force_Validity_Test := False;

      Chessboard.Clear_Moves_List;

      if Chessboard.Side_To_Move = White then

         if Chessboard.White_Has_King_In_Check then
            Chessboard.Generate_Check_Evasion;
            return;
         end if;


         for Square of White_Pieces loop
            exit when Square = 0;

            Piece := Chessboard.Square (Square);

            Pin_Direction := (if Piece = White_King then No_Direction else Chessboard.Absolute_Pin_Direction (Square));

            case Piece is
               when White_Pawn =>
                  Target := Square + North;
                  if Pin_Direction in No_Direction | North | South then
                     if Chessboard.Square (Target) = Empty then
                        if Target in A8 .. H8 then
                           Chessboard.Register_Move (Square, Target, Promotion);
                        else
                           Chessboard.Register_Move (Square, Target);
                        end if;
                        if Square in A2 .. H2 then
                           Target := Target + North;
                           if Chessboard.Square (Target) = Empty then
                              Chessboard.Register_Move (Square, Target, Pawn_Move_Two_Square);
                           end if;
                        end if;
                     end if;
                  end if;
                  -- captures
                  if Pin_Direction in No_Direction | North_West then
                     if File (Square) /= File_A then
                        Target := Square + North_West;
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           if Target in A8 .. H8 then
                              Chessboard.Register_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Move (Square, Target);
                           end if;
                        elsif Target = En_Passant then
                           Chessboard.Register_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

                  if Pin_Direction in No_Direction | North_East then
                     if File (Square) /= File_H then
                        Target := Square + North_East;
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           if Target in A8 .. H8 then
                              Chessboard.Register_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Move (Square, Target);
                           end if;
                        elsif Target = En_Passant then
                           Chessboard.Register_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

               when White_Knight =>
                  if Pin_Direction = No_Direction then
                     for Offset of Knight_Offsets loop
                        Target := Square + Offset;
                        if Chessboard.Square (Target) = Empty or else Chessboard.Square (Target) in Black_Piece_Type then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end loop;
                  end if;

               when White_Bishop =>
                  for Offset of Bishop_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Chessboard.Register_Move (Square, Target);
                           Target := Target + Offset;
                        end loop;
                        -- does end with capture?
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when White_Rook =>
                  for Offset of Rook_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Chessboard.Register_Move (Square, Target);
                           Target := Target + Offset;
                        end loop;
                        -- does end with capture?
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when White_Queen =>
                  for Offset of Queen_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Chessboard.Register_Move (Square, Target);
                           Target := Target + Offset;
                        end loop;
                        -- does end with capture?
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when White_King =>
                  for Offset of King_Offsets loop
                     Target := Square + Offset;
                     if Chessboard.Square (Target) = Empty or else Chessboard.Square (Target) in Black_Piece_Type then
                        Chessboard.Register_Move (Square, Target);
                     end if;
                  end loop;

               when others => null;
            end case;
         end loop;

         -- look for castle

         if not Chessboard.Force_Validity_Test then

            if Chessboard.White_Castle_Kingside (Chessboard.History_Ply) then
               if Chessboard.Square (F1) = Empty and then Chessboard.Square (G1) = Empty then
                  if not Chessboard.Attacks (Black, F1) and then not Chessboard.Attacks (Black, G1) then
                     Chessboard.Register_Move (From => E1, To => G1, Flag => Castle);
                  end if;
               end if;
            end if;

            if Chessboard.White_Castle_Queenside (Chessboard.History_Ply) then
               if Chessboard.Square (D1) = Empty and then Chessboard.Square (C1) = Empty and then Chessboard.Square (B1) = Empty then
                  if not Chessboard.Attacks (Black, D1) and then not Chessboard.Attacks (Black, C1) then
                     Chessboard.Register_Move (From => E1, To => C1, Flag => Castle);
                  end if;
               end if;
            end if;

         end if;

      else

         if Chessboard.Black_Has_King_In_Check then
            Chessboard.Generate_Check_Evasion;
            return;
         end if;


         for Square of Black_Pieces loop
            exit when Square = 0;
            Piece := Chessboard.Square (Square);

            Pin_Direction := (if Piece = Black_King then No_Direction else Chessboard.Absolute_Pin_Direction (Square));

            case Piece is
               when Black_Pawn =>
                  Target := Square + South;
                  if Pin_Direction in No_Direction | South | North then
                     if Chessboard.Square (Target) = Empty then
                        if Target in A1 .. H1 then
                           Chessboard.Register_Move (Square, Target, Promotion);
                        else
                           Chessboard.Register_Move (Square, Target);
                        end if;
                        if Square in A7 .. H7 then
                           Target := Target + South;
                           if Chessboard.Square (Target) = Empty then
                              Chessboard.Register_Move (Square, Target, Pawn_Move_Two_Square);
                           end if;
                        end if;
                     end if;
                  end if;

                  -- captures
                  if Pin_Direction in No_Direction | South_East then
                     if File (Square) /= File_H then
                        Target := Square + South_East;
                        if Chessboard.Square (Target) in White_Piece_Type then
                           if Target in A1 .. H1 then
                              Chessboard.Register_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Move (Square, Target);
                           end if;
                        elsif Target = En_Passant then
                           Chessboard.Register_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

                  if Pin_Direction in No_Direction | South_West then
                     if File (Square) /= File_A then
                        Target := Square + South_West;
                        if Chessboard.Square (Target) in White_Piece_Type then
                           if Target in A1 .. H1 then
                              Chessboard.Register_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Move (Square, Target);
                           end if;
                        elsif Target = En_Passant then
                           Chessboard.Register_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

               when Black_Knight =>
                   if Pin_Direction = No_Direction then
                     for Offset of Knight_Offsets loop
                        Target := Square + Offset;
                        if Chessboard.Square (Target) = Empty or else Chessboard.Square (Target) in White_Piece_Type then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end loop;
                  end if;

               when Black_Bishop =>
                  for Offset of Bishop_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Chessboard.Register_Move (Square, Target);
                           Target := Target + Offset;
                        end loop;
                        -- look for capture
                        if Chessboard.Square (Target) in White_Piece_Type then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when Black_Rook =>
                  for Offset of Rook_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Chessboard.Register_Move (Square, Target);
                           Target := Target + Offset;
                        end loop;
                        if Chessboard.Square (Target) in White_Piece_Type then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when Black_Queen =>
                  for Offset of Queen_Offsets loop
                    if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Chessboard.Register_Move (Square, Target);
                           Target := Target + Offset;
                        end loop;
                        if Chessboard.Square (Target) in White_Piece_Type then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when Black_King =>
                  for Offset of King_Offsets loop
                     Target := Square + Offset;
                     if Chessboard.Square (Target) = Empty or else Chessboard.Square (Target) in White_Piece_Type then
                        Chessboard.Register_Move (Square, Target);
                     end if;
                  end loop;

               when others => null;
            end case;
         end loop;

         -- look for castle moves

         if not Chessboard.Force_Validity_Test then

            if Chessboard.Black_Castle_Kingside (Chessboard.History_Ply) then
               if Chessboard.Square (F8) = Empty and then Chessboard.Square (G8) = Empty then
                  if not Chessboard.Attacks (White, F8) and then not Chessboard.Attacks (White, G8) then
                     Chessboard.Register_Move (From => E8, To => G8, Flag => Castle);
                  end if;
               end if;
            end if;

            if Chessboard.Black_Castle_Queenside (Chessboard.History_Ply) then
               if Chessboard.Square (D8) = Empty and then Chessboard.Square (C8) = Empty and then Chessboard.Square (B8) = Empty then
                  if not Chessboard.Attacks (White, D8) and then not Chessboard.Attacks (White, C8) then
                     Chessboard.Register_Move (From => E8, To => C8, Flag => Castle);
                  end if;
               end if;
            end if;

         end if;

      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Generate_Moves;


   -----------------------------
   -- Genearate_Check_Evasion --
   -----------------------------

   procedure Generate_Check_Evasion(Chessboard : in out Chessboard_Type) is
      Target              : Square_Type := 0;
      Type_Of_Check       : constant Check_Type := Last_Move_Made (Chessboard).Check;
      Attack_To_The_King  : Attack_Collection_Type;
      Attacker            : Piece_Type := Empty;
      Origin              : Square_Type := 0;
      Direction           : Direction_Type := No_Direction;
      Sliding_Attack      : Boolean := False;
      Sq                  : Square_Type := 0;
      White_King_Position : Square_Type renames Chessboard.White_King_Position;
      Black_King_Position : Square_Type renames Chessboard.Black_King_Position;
   begin

      Chessboard.Clear_Moves_List;

      if Type_Of_Check = Checkmate then
         return;
      end if;

      if Chessboard.Side_To_Move = White then

         ------------------
         -- King escapes --
         ------------------

         Chessboard.Force_Validity_Test := True;

         for Offset of King_Offsets loop
            Target := White_King_Position + Offset;
            if Chessboard.Square (Target) = Empty or else Chessboard.Square (Target) in Black_Piece_Type then
               Chessboard.Register_Move (White_King_Position, Target);
            end if;
         end loop;

         Chessboard.Force_Validity_Test := False;

         if Type_Of_Check = Double_Check then
            return;
         end if;

         -------------------------------
         -- Obtain attack information --
         -------------------------------

         if Type_Of_Check = Discovery_Check then
            Attack_To_The_King := Chessboard.Attacking_Square
              (Side     => Black,
               Square   => White_King_Position,
               Only_One => True);

            pragma Assert (Attack_To_The_King.Number_Of_Attackers = 1, "Bug: Expected 1 piece attacking in a discovery check but different value found");

            Attacker := Attack_To_The_King.Attacker (Attack_Array_Type'First).Piece;
            Origin := Attack_To_The_King.Attacker (Attack_Array_Type'First).Origin;

         elsif Type_Of_Check = Unknown_Check then
            Attack_To_The_King := Chessboard.Attacking_Square
              (Side     => Black,
               Square   => White_King_Position,
               Only_One => False);

            pragma Assert (Attack_To_The_King.Number_Of_Attackers in 1 .. 2, "Bug: For an unknown check only 1 or two pieces can simultaneously attack");

            if Attack_To_The_King.Number_Of_Attackers = 2 then
               return; -- Now we know: it is a double check!
            end if;

            Attacker := Attack_To_The_King.Attacker (Attack_Array_Type'First).Piece;
            Origin := Attack_To_The_King.Attacker (Attack_Array_Type'First).Origin;

         else
            if Chessboard.Last_Move_Made.Flag = Castle then
               Attacker := Black_Rook;
               Origin := (if Chessboard.Last_Move_Made.To = G8 then F8 else D8);
            else
               Attacker := (if Chessboard.Last_Move_Made.Promotion = Empty then Chessboard.Last_Move_Made.Piece else Chessboard.Last_Move_Made.Promotion);
               Origin := Chessboard.Last_Move_Made.To;
            end if;
         end if;

         Direction := (if Attacker in Black_Bishop | Black_Rook | Black_Queen then Find_Sliding_Direction (Origin, White_King_Position) else No_Direction);
         Sliding_Attack := (if Direction = No_Direction then False else True);

         ---------------------------
         -- Neutralize the attack --
         ---------------------------

         -- Pawn first
         Sq := Origin + South_East;
         if Chessboard.Square (Sq) = White_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
            if Origin in A8 .. H8 then
               Chessboard.Register_Move (Sq, Origin, Promotion);
            else
               Chessboard.Register_Move (Sq, Origin);
            end if;
         end if;
         Sq := Origin + South_West;
         if Chessboard.Square (Sq) = White_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
            if Origin in A8 .. H8 then
               Chessboard.Register_Move (Sq, Origin, Promotion);
            else
               Chessboard.Register_Move (Sq, Origin);
            end if;
         end if;

         -- En-passant
         if Chessboard.En_Passant (Chessboard.History_Ply) /= 0 then
            Sq := Chessboard.En_Passant (Chessboard.History_Ply) + South_West;
            if Chessboard.Square (Sq) = White_Pawn then
               Chessboard.Register_Move (Sq, Sq + North_East, Capture_En_Passant);
            end if;
            Sq := Chessboard.En_Passant (Chessboard.History_Ply) + South_East;
            if Chessboard.Square (Sq) = White_Pawn then
               Chessboard.Register_Move (Sq, Sq + North_West, Capture_En_Passant);
            end if;
         end if;

         -- Other pieces now
         Target := Origin; -- Everything starts here :-)
         while Target /= White_King_Position loop
            -- Look for bishop/queen blocks
            for Offset of Bishop_Offsets loop
               Sq := Target + Offset;
               while Chessboard.Square (Sq) = Empty loop
                  Sq := Sq + Offset;
               end loop;
               if Chessboard.Square (Sq) = White_Bishop or else Chessboard.Square (Sq) = White_Queen then
                  if Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                     Chessboard.Register_Move (Sq, Target);
                  end if;
               end if;
            end loop;
            -- look for rook/queen blocks
            for Offset of Rook_Offsets loop
               Sq := Target + Offset;
               while Chessboard.Square (Sq) = Empty loop
                  Sq := Sq + Offset;
               end loop;
               if Chessboard.Square (Sq) = White_Rook or else Chessboard.Square (Sq) = White_Queen then
                  if Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                     Chessboard.Register_Move (Sq, Target);
                  end if;
               end if;
            end loop;
            -- look for knight blocks
            for Offset of Knight_Offsets loop
               if Chessboard.Square (Target + Offset) = White_Knight then
                  if Chessboard.Absolute_Pin_Direction (Target + Offset) = No_Direction then
                     Chessboard.Register_Move (Target + Offset, Target);
                  end if;
               end if;
            end loop;
            -- look for pawn blocks
            if Target /= Origin then
               Sq := Target + South;
               if Chessboard.Square (Sq) = White_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                  if Target in A8 .. H8 then
                     Chessboard.Register_Move (Sq, Target, Promotion);
                  else
                     Chessboard.Register_Move (Sq, Target);
                  end if;
               end if;
               if Chessboard.Square (Sq) = Empty then
                  Sq := Target + South + South;
                  if Chessboard.Square (Sq) = White_Pawn and then Rank (Sq) = Rank (A2) and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                     Chessboard.Register_Move (Sq, Target, Pawn_Move_Two_Square);
                  end if;
               end if;
            end if;

            Target := Target - Direction; -- we are going backward
            exit when not Sliding_Attack; -- because non-sliding attacker can oly be captured
         end loop;


      else -- black to move

         ------------------
         -- King escapes --
         ------------------

         Chessboard.Force_Validity_Test := True;

         for Offset of King_Offsets loop
            Target := Black_King_Position + Offset;
            if Chessboard.Square (Target) = Empty or else Chessboard.Square (Target) in White_Piece_Type then
               Chessboard.Register_Move (Black_King_Position, Target);
            end if;
         end loop;

         Chessboard.Force_Validity_Test := False;

         if Type_Of_Check = Double_Check then
            return;
         end if;

         -------------------------------
         -- Obtain attack information --
         -------------------------------

         if Type_Of_Check = Discovery_Check then
            Attack_To_The_King := Chessboard.Attacking_Piece
              (Side     => White,
               Square   => Black_King_Position,
               Only_One => True);

            pragma Assert (Attack_To_The_King.Number_Of_Attackers = 1, "Failed to lookup attackers to the king 1");

            Attacker := Attack_To_The_King.Attacker (Attack_Array_Type'First).Piece;
            Origin := Attack_To_The_King.Attacker (Attack_Array_Type'First).Origin;

         elsif Type_Of_Check = Unknown_Check then
            Attack_To_The_King := Chessboard.Attacking_Piece
              (Side     => White,
               Square   => Black_King_Position,
               Only_One => False);

            pragma Assert (Attack_To_The_King.Number_Of_Attackers in 1 .. 2, "Failed to lookup attackers to the king 2");

            if Attack_To_The_King.Number_Of_Attackers = 2 then
               return; -- Now we know: it is a double check!
            end if;

            Attacker := Attack_To_The_King.Attacker (Attack_Array_Type'First).Piece;
            Origin := Attack_To_The_King.Attacker (Attack_Array_Type'First).Origin;

         else
            if Chessboard.Last_Move_Made.Flag = Castle then
               Attacker := White_Rook;
               Origin := (if Chessboard.Last_Move_Made.To = G1 then F1 else D1);
            else
               Attacker := (if Chessboard.Last_Move_Made.Promotion = Empty then Chessboard.Last_Move_Made.Piece else Chessboard.Last_Move_Made.Promotion);
               Origin := Chessboard.Last_Move_Made.To;
            end if;
         end if;

         Direction := (if Attacker in White_Bishop | White_Rook | White_Queen then Find_Sliding_Direction (Origin, Black_King_Position) else No_Direction);
         Sliding_Attack := (if Direction = No_Direction then False else True);

         ---------------------------
         -- Neutralize the attack --
         ---------------------------

         -- Pawn first
         Sq := Origin + North_East;
         if Chessboard.Square (Sq) = Black_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
            if Origin in A1 .. H1 then
               Chessboard.Register_Move (Sq, Origin, Promotion);
            else
               Chessboard.Register_Move (Sq, Origin);
            end if;
         end if;
         Sq := Origin + North_West;
         if Chessboard.Square (Sq) = Black_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
            if Origin in A1 .. H1 then
               Chessboard.Register_Move (Sq, Origin, Promotion);
            else
               Chessboard.Register_Move (Sq, Origin);
            end if;
         end if;

         -- En-Passant
         if Chessboard.En_Passant (Chessboard.History_Ply) /= 0 then
            Sq := Chessboard.En_Passant (Chessboard.History_Ply) + North_West;
            if Chessboard.Square (Sq) = Black_Pawn then
               Chessboard.Register_Move (Sq, Sq + South_East, Capture_En_Passant);
            end if;
            Sq := Chessboard.En_Passant (Chessboard.History_Ply) + North_East;
            if Chessboard.Square (Sq) = Black_Pawn then
               Chessboard.Register_Move (Sq, Sq + South_West, Capture_En_Passant);
            end if;
         end if;

         -- Other pieces now
         Target := Origin; -- Everything starts here :-)
         while Target /= Black_King_Position loop
            -- Look for bishop/queen blocks
            for Offset of Bishop_Offsets loop
               Sq := Target + Offset;
               while Chessboard.Square (Sq) = Empty loop
                  Sq := Sq + Offset;
               end loop;
               if Chessboard.Square (Sq) = Black_Bishop or else Chessboard.Square (Sq) = Black_Queen then
                  if Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                     Chessboard.Register_Move (Sq, Target);
                  end if;
               end if;
            end loop;
            -- look for rook/queen blocks
            for Offset of Rook_Offsets loop
               Sq := Target + Offset;
               while Chessboard.Square (Sq) = Empty loop
                  Sq := Sq + Offset;
               end loop;
               if Chessboard.Square (Sq) = Black_Rook or else Chessboard.Square (Sq) = Black_Queen then
                  if Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                     Chessboard.Register_Move (Sq, Target);
                  end if;
               end if;
            end loop;
            -- look for knight blocks
            for Offset of Knight_Offsets loop
               if Chessboard.Square (Target + Offset) = Black_Knight then
                  if Chessboard.Absolute_Pin_Direction (Target + Offset) = No_Direction then
                     Chessboard.Register_Move (Target + Offset, Target);
                  end if;
               end if;
            end loop;
            -- look for pawn blocks
            if Target /= Origin then
               Sq := Target + North;
               if Chessboard.Square (Sq) = Black_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                  if Target in A1 .. H1 then
                     Chessboard.Register_Move (Sq, Target, Promotion);
                  else
                     Chessboard.Register_Move (Sq, Target);
                  end if;
               end if;
               if Chessboard.Square (Sq) = Empty then
                  Sq := Target + North + North;
                  if Chessboard.Square (Sq) = Black_Pawn and then Rank (Sq) = Rank (A7) and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                     Chessboard.Register_Move (Sq, Target, Pawn_Move_Two_Square);
                  end if;
               end if;
            end if;

            Target := Target - Direction; -- we are going backward
            exit when not Sliding_Attack; -- because non-sliding attacker can oly be captured
         end loop;

      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Generate_Check_Evasion;


    -------------------------------
   -- Generate_See_Check_Evasion --
   --------------------------------

   procedure Generate_See_Check_Evasion(Chessboard : in out Chessboard_Type; See_Target : in Square_Type) is
      Target              : Square_Type := 0;
      Type_Of_Check       : constant Check_Type := Chessboard.Last_Move_Made.Check;
      Attack_To_The_King  : Attack_Collection_Type;
      Origin              : Square_Type := See_Target;

      Sq                  : Square_Type := 0;
      White_King_Position : Square_Type renames Chessboard.White_King_Position;
      Black_King_Position : Square_Type renames Chessboard.Black_King_Position;
   begin

      pragma Assert (Type_Of_Check /= No_Check, "Fatal error: Check-Evasion called when King is NOT in check");

      Chessboard.Clear_Moves_List;

      -- Conditions where capturing a piece will not solve the check
      -- leads to no further search for check evasion

      if Type_Of_Check in Double_Check | Checkmate then
         return;
      end if;


      if Chessboard.Side_To_Move = White then

         ------------------
         -- King escapes --
         ------------------

         Chessboard.Force_Validity_Test := True;

         for Offset of King_Offsets loop
            Target := White_King_Position + Offset;
            if Target = See_Target then
               Chessboard.Register_Move (White_King_Position, Target);
            end if;
         end loop;

         Chessboard.Force_Validity_Test := False;

         if Type_Of_Check = Discovery_Check then
            return;
         end if;

         -------------------------------
         -- Obtain attack information --
         -------------------------------

        if Type_Of_Check = Unknown_Check then
            Attack_To_The_King := Chessboard.Attacking_Piece
              (Side     => Black,
               Square   => White_King_Position,
               Only_One => False);

            pragma Assert (Attack_To_The_King.Number_Of_Attackers in 1 .. 2, "Bug: For an unknown check only 1 or two pieces can simultaneously attack");

            if Attack_To_The_King.Number_Of_Attackers = 2 then
               return; -- Now we know: it is a double check!
            end if;

            Origin := Attack_To_The_King.Attacker (Attack_Array_Type'First).Origin;

         else
            if Chessboard.Last_Move_Made.Flag = Castle then
               Origin := (if Chessboard.Last_Move_Made.To = G8 then F8 else D8);
            else
               Origin := Chessboard.Last_Move_Made.To;
            end if;
         end if;

         ---------------------------
         -- Neutralize the attack --
         ---------------------------

         -- Pawn first
         Sq := Origin + South_East;
         if Chessboard.Square (Sq) = White_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
            if Origin in A8 .. H8 then
               Chessboard.Register_Move (Sq, Origin, Promotion);
            else
               Chessboard.Register_Move (Sq, Origin);
            end if;
         end if;
         Sq := Origin + South_West;
         if Chessboard.Square (Sq) = White_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
            if Origin in A8 .. H8 then
               Chessboard.Register_Move (Sq, Origin, Promotion);
            else
               Chessboard.Register_Move (Sq, Origin);
            end if;
         end if;

         -- En-passant
         if Chessboard.En_Passant (Chessboard.History_Ply) /= 0 then
            Sq := Chessboard.En_Passant (Chessboard.History_Ply) + South_West;
            if Chessboard.Square (Sq) = White_Pawn then
               Chessboard.Register_Move (Sq, Sq + North_East, Capture_En_Passant);
            end if;
            Sq := Chessboard.En_Passant (Chessboard.History_Ply) + South_East;
            if Chessboard.Square (Sq) = White_Pawn then
               Chessboard.Register_Move (Sq, Sq + North_West, Capture_En_Passant);
            end if;
         end if;

         -- Other pieces now
         Target := Origin; -- Everything starts here :-)

         -- Look for bishop/queen blocks
         for Offset of Bishop_Offsets loop
            Sq := Target + Offset;
            while Chessboard.Square (Sq) = Empty loop
               Sq := Sq + Offset;
            end loop;
            if Chessboard.Square (Sq) = White_Bishop or else Chessboard.Square (Sq) = White_Queen then
               if Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                  Chessboard.Register_Move (Sq, Target);
               end if;
            end if;
         end loop;
         -- look for rook/queen blocks
         for Offset of Rook_Offsets loop
            Sq := Target + Offset;
            while Chessboard.Square (Sq) = Empty loop
               Sq := Sq + Offset;
            end loop;
            if Chessboard.Square (Sq) = White_Rook or else Chessboard.Square (Sq) = White_Queen then
               if Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                  Chessboard.Register_Move (Sq, Target);
               end if;
            end if;
         end loop;
         -- look for knight blocks
         for Offset of Knight_Offsets loop
            if Chessboard.Square (Target + Offset) = White_Knight then
               if Chessboard.Absolute_Pin_Direction (Target + Offset) = No_Direction then
                  Chessboard.Register_Move (Target + Offset, Target);
               end if;
            end if;
         end loop;
         -- look for pawn blocks
         if Target /= Origin then
            Sq := Target + South;
            if Chessboard.Square (Sq) = White_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
               if Target in A8 .. H8 then
                  Chessboard.Register_Move (Sq, Target, Promotion);
               else
                  Chessboard.Register_Move (Sq, Target);
               end if;
            end if;
            if Chessboard.Square (Sq) = Empty then
               Sq := Target + South + South;
               if Chessboard.Square (Sq) = White_Pawn and then Rank (Sq) = Rank (A2) and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                  Chessboard.Register_Move (Sq, Target, Pawn_Move_Two_Square);
               end if;
            end if;
         end if;

      else -- black to move

         ------------------
         -- King escapes --
         ------------------

         Chessboard.Force_Validity_Test := True;

         for Offset of King_Offsets loop
            Target := Black_King_Position + Offset;
            if Target = See_Target then
               Chessboard.Register_Move (Black_King_Position, Target);
            end if;
         end loop;

         Chessboard.Force_Validity_Test := False;

         if Type_Of_Check = Discovery_Check then
            return;
         end if;

         -------------------------------
         -- Obtain attack information --
         -------------------------------

        if Type_Of_Check = Unknown_Check then
            Attack_To_The_King := Chessboard.Attacking_Piece
              (Side     => White,
               Square   => Black_King_Position,
               Only_One => False);

            pragma Assert (Attack_To_The_King.Number_Of_Attackers in 1 .. 2, "Failed to lookup attackers to the king 2");

            if Attack_To_The_King.Number_Of_Attackers = 2 then
               return; -- Now we know: it is a double check!
            end if;

            Origin := Attack_To_The_King.Attacker (Attack_Array_Type'First).Origin;

         else
            if Chessboard.Last_Move_Made.Flag = Castle then
               Origin := (if Chessboard.Last_Move_Made.To = G1 then F1 else D1);
            else
               Origin := Chessboard.Last_Move_Made.To;
            end if;
         end if;

         ---------------------------
         -- Neutralize the attack --
         ---------------------------

         -- Pawn first
         Sq := Origin + North_East;
         if Chessboard.Square (Sq) = Black_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
            if Origin in A1 .. H1 then
               Chessboard.Register_Move (Sq, Origin, Promotion);
            else
               Chessboard.Register_Move (Sq, Origin);
            end if;
         end if;
         Sq := Origin + North_West;
         if Chessboard.Square (Sq) = Black_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
            if Origin in A1 .. H1 then
               Chessboard.Register_Move (Sq, Origin, Promotion);
            else
               Chessboard.Register_Move (Sq, Origin);
            end if;
         end if;

         -- En-Passant
         if Chessboard.En_Passant (Chessboard.History_Ply) /= 0 then
            Sq := Chessboard.En_Passant (Chessboard.History_Ply) + North_West;
            if Chessboard.Square (Sq) = Black_Pawn then
               Chessboard.Register_Move (Sq, Sq + South_East, Capture_En_Passant);
            end if;
            Sq := Chessboard.En_Passant (Chessboard.History_Ply) + North_East;
            if Chessboard.Square (Sq) = Black_Pawn then
               Chessboard.Register_Move (Sq, Sq + South_West, Capture_En_Passant);
            end if;
         end if;

         -- Other pieces now
         Target := Origin; -- Everything starts here :-)

         -- Look for bishop/queen blocks
         for Offset of Bishop_Offsets loop
            Sq := Target + Offset;
            while Chessboard.Square (Sq) = Empty loop
               Sq := Sq + Offset;
            end loop;
            if Chessboard.Square (Sq) = Black_Bishop or else Chessboard.Square (Sq) = Black_Queen then
               if Chessboard.Absolute_Pin_Direction (Sq) = 0 then
                  Chessboard.Register_Move (Sq, Target);
               end if;
            end if;
         end loop;
         -- look for rook/queen blocks
         for Offset of Rook_Offsets loop
            Sq := Target + Offset;
            while Chessboard.Square (Sq) = Empty loop
               Sq := Sq + Offset;
            end loop;
            if Chessboard.Square (Sq) = Black_Rook or else Chessboard.Square (Sq) = Black_Queen then
               if Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                  Chessboard.Register_Move (Sq, Target);
               end if;
            end if;
         end loop;
         -- look for knight blocks
         for Offset of Knight_Offsets loop
            if Chessboard.Square (Target + Offset) = Black_Knight then
               if Chessboard.Absolute_Pin_Direction (Target + Offset) = No_Direction then
                  Chessboard.Register_Move (Target + Offset, Target);
               end if;
            end if;
         end loop;
         -- look for pawn blocks
         if Target /= Origin then
            Sq := Target + North;
            if Chessboard.Square (Sq) = Black_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = 0 then
               if Target in A1 .. H1 then
                  Chessboard.Register_Move (Sq, Target, Promotion);
               else
                  Chessboard.Register_Move (Sq, Target);
               end if;
            end if;
            if Chessboard.Square (Sq) = Empty then
               Sq := Target + North + North;
               if Chessboard.Square (Sq) = Black_Pawn and then Rank (Sq) = Rank (A7) and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                  Chessboard.Register_Move (Sq, Target, Pawn_Move_Two_Square);
               end if;
            end if;
         end if;

      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Generate_See_Check_Evasion;


   ----------------------
   -- King_Has_Escapes --
   ----------------------

   function King_Has_Escapes (Chessboard : in out Chessboard_Type; Type_Of_Check : in Check_Type) return Boolean is
      Target              : Square_Type := 0;
      Attack_To_The_King  : Attack_Collection_Type;
      Attacker            : Piece_Type := Empty;
      Origin              : Square_Type := 0;
      Direction           : Direction_Type := No_Direction;
      Sliding_Attack      : Boolean;
      Sq                  : Square_Type := 0;
      White_King_Position : Square_Type renames Chessboard.White_King_Position;
      Black_King_Position : Square_Type renames Chessboard.Black_King_Position;
   begin

      if Chessboard.Side_To_Move = White then

         ------------------
         -- King escapes --
         ------------------

         Chessboard.Square (White_King_Position) := Empty;

         for Offset of King_Offsets loop
            Target := White_King_Position + Offset;
            if Chessboard.Square (Target) = Empty or else Chessboard.Square (Target) in Black_Piece_Type then
               if not Chessboard.Attacks (Black, Target) then
                  Chessboard.Square (White_King_Position) := White_King;
                  return True;
               end if;
            end if;
         end loop;

         Chessboard.Square (White_King_Position) := White_King;

         if Type_Of_Check = Double_Check then
            return False;
         end if;

         -------------------------------
         -- Obtain attack information --
         -------------------------------

         if Type_Of_Check = Discovery_Check then
            Attack_To_The_King := Chessboard.Attacking_Piece
              (Side     => Black,
               Square   => White_King_Position,
               Only_One => True);

            pragma Assert (Attack_To_The_King.Number_Of_Attackers = 1, "Failed to lookup attackers to the king 7");

            Attacker := Attack_To_The_King.Attacker (Attack_Array_Type'First).Piece;
            Origin := Attack_To_The_King.Attacker (Attack_Array_Type'First).Origin;

         elsif Type_Of_Check = Unknown_Check then
            Attack_To_The_King := Chessboard.Attacking_Piece
              (Side     => Black,
               Square   => White_King_Position,
               Only_One => False);

            pragma Assert (Attack_To_The_King.Number_Of_Attackers in 1 .. 2, "Failed to lookup attackers to the king 8");

            if Attack_To_The_King.Number_Of_Attackers = 2 then
               return False; -- Now we know: it is a double check!
            end if;

            Attacker := Attack_To_The_King.Attacker (Attack_Array_Type'First).Piece;
            Origin := Attack_To_The_King.Attacker (Attack_Array_Type'First).Origin;

         else
            if Chessboard.Last_Move_Made.Flag = Castle then
               Attacker := Black_Rook;
               Origin := (if Chessboard.Last_Move_Made.To = G8 then F8 else D8);
            else
               Attacker := (if Chessboard.Last_Move_Made.Promotion = Empty then Chessboard.Last_Move_Made.Piece else Chessboard.Last_Move_Made.Promotion);
               Origin := Chessboard.Last_Move_Made.To;
            end if;
         end if;

         Direction := (if Attacker in Black_Bishop | Black_Rook | Black_Queen then Find_Sliding_Direction (Origin, White_King_Position) else No_Direction);
         Sliding_Attack := (if Direction = No_Direction then False else True);

         ---------------------------
         -- Neutralize the attack --
         ---------------------------

         -- Pawn first
         Sq := Origin + South_East;
         if Chessboard.Square (Sq) = White_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
            if Origin in A8 .. H8 then
               return True;
            else
               return True;
            end if;
         end if;
         Sq := Origin + South_West;
         if Chessboard.Square (Sq) = White_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
            if Origin in A8 .. H8 then
               return True;
            else
               return True;
            end if;
         end if;

         -- Extremely rare case
         if Chessboard.En_Passant (Chessboard.History_Ply) /= 0 then
            Sq := Chessboard.En_Passant (Chessboard.History_Ply) + South_West;
            if Chessboard.Square (Sq) = White_Pawn then
               return True;
            end if;
            Sq := Chessboard.En_Passant (Chessboard.History_Ply) + South_East;
            if Chessboard.Square (Sq) = White_Pawn then
               return True;
            end if;
         end if;

         -- Other pieces now
         Target := Origin; -- Everything starts here :-)
         while Target /= White_King_Position loop
            -- Look for bishop/queen blocks
            for Offset of Bishop_Offsets loop
               Sq := Target + Offset;
               while Chessboard.Square (Sq) = Empty loop
                  Sq := Sq + Offset;
               end loop;
               if Chessboard.Square (Sq) = White_Bishop or else Chessboard.Square (Sq) = White_Queen then
                  if Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                     return True;
                  end if;
               end if;
            end loop;
            -- look for rook/queen blocks
            for Offset of Rook_Offsets loop
               Sq := Target + Offset;
               while Chessboard.Square (Sq) = Empty loop
                  Sq := Sq + Offset;
               end loop;
               if Chessboard.Square (Sq) = White_Rook or else Chessboard.Square (Sq) = White_Queen then
                  if Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                     return True;
                  end if;
               end if;
            end loop;
            -- look for knight blocks
            for Offset of Knight_Offsets loop
               if Chessboard.Square (Target + Offset) = White_Knight then
                  if Chessboard.Absolute_Pin_Direction (Target + Offset) = No_Direction then
                     return True;
                  end if;
               end if;
            end loop;
            -- look for pawn blocks
            if Target /= Origin then
               Sq := Target + South;
               if Chessboard.Square (Sq) = White_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                  if Target in A8 .. H8 then
                     return True;
                  else
                     return True;
                  end if;
               end if;
               if Chessboard.Square (Sq) = Empty then
                  Sq := Target + South + South;
                  if Chessboard.Square (Sq) = White_Pawn and then Rank (Sq) = Rank (A2) and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                     return True;
                  end if;
               end if;
            end if;

            Target := Target - Direction; -- we are searching backward
            exit when not Sliding_Attack; -- because non-sliding attacker can only be captured
         end loop;


      else -- black to move

         ------------------
         -- King escapes --
         ------------------

         Chessboard.Square (Black_King_Position) := Empty;

         for Offset of King_Offsets loop
            Target := Black_King_Position + Offset;
            if Chessboard.Square (Target) = Empty or else Chessboard.Square (Target) in White_Piece_Type then
               if not Chessboard.Attacks (White, Target) then
                  Chessboard.Square (Black_King_Position) := Black_King;
                  return True;
               end if;
            end if;
         end loop;

         Chessboard.Square (Black_King_Position) := Black_King;

         if Type_Of_Check = Double_Check then
            return False;
         end if;

         -------------------------------
         -- Obtain attack information --
         -------------------------------

         if Type_Of_Check = Discovery_Check then
            Attack_To_The_King := Chessboard.Attacking_Piece
              (Side     => White,
               Square   => Black_King_Position,
               Only_One => True);

            pragma Assert (Attack_To_The_King.Number_Of_Attackers = 1, "Failed to lookup attackers to the king 9 ");

            Attacker := Attack_To_The_King.Attacker (Attack_Array_Type'First).Piece;
            Origin := Attack_To_The_King.Attacker (Attack_Array_Type'First).Origin;

         elsif Type_Of_Check = Unknown_Check then
            Attack_To_The_King := Chessboard.Attacking_Piece
              (Side     => White,
               Square   => Black_King_Position,
               Only_One => False);

            pragma Assert (Attack_To_The_King.Number_Of_Attackers in 1 .. 2, "Failed to lookup attackers to the king 10");

            if Attack_To_The_King.Number_Of_Attackers = 2 then
               return False; -- Now we know: it is a double check!
            end if;

            Attacker := Attack_To_The_King.Attacker (Attack_Array_Type'First).Piece;
            Origin := Attack_To_The_King.Attacker (Attack_Array_Type'First).Origin;

         else
            if Chessboard.Last_Move_Made.Flag = Castle then
               Attacker := White_Rook;
               Origin := (if Chessboard.Last_Move_Made.To = G1 then F1 else D1);
            else
               Attacker := (if Chessboard.Last_Move_Made.Promotion = Empty then Chessboard.Last_Move_Made.Piece else Chessboard.Last_Move_Made.Promotion);
               Origin := Chessboard.Last_Move_Made.To;
            end if;
         end if;

         Direction := (if Attacker in White_Bishop | White_Rook | White_Queen then Find_Sliding_Direction (Origin, Black_King_Position) else No_Direction);
         Sliding_Attack := (if Direction = No_Direction then False else True);

         ---------------------------
         -- Neutralize the attack --
         ---------------------------

         -- Pawn first
         Sq := Origin + North_East;
         if Chessboard.Square (Sq) = Black_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
            if Origin in A1 .. H1 then
               return True;
            else
               return True;
            end if;
         end if;
         Sq := Origin + North_West;
         if Chessboard.Square (Sq) = Black_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
            if Origin in A1 .. H1 then
               return True;
            else
               return True;
            end if;
         end if;

         -- Extremely rare case
         if Chessboard.En_Passant (Chessboard.History_Ply) /= 0 then
            Sq := Chessboard.En_Passant (Chessboard.History_Ply) + North_West;
            if Chessboard.Square (Sq) = Black_Pawn then
               return True;
            end if;
            Sq := Chessboard.En_Passant (Chessboard.History_Ply) + North_East;
            if Chessboard.Square (Sq) = Black_Pawn then
               return True;
            end if;
         end if;

         -- Other pieces now
         Target := Origin; -- Everything starts here :-)
         while Target /= Black_King_Position loop
            -- Look for bishop/queen blocks
            for Offset of Bishop_Offsets loop
               Sq := Target + Offset;
               while Chessboard.Square (Sq) = Empty loop
                  Sq := Sq + Offset;
               end loop;
               if Chessboard.Square (Sq) = Black_Bishop or else Chessboard.Square (Sq) = Black_Queen then
                  if Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                     return True;
                  end if;
               end if;
            end loop;
            -- look for rook/queen blocks
            for Offset of Rook_Offsets loop
               Sq := Target + Offset;
               while Chessboard.Square (Sq) = Empty loop
                  Sq := Sq + Offset;
               end loop;
               if Chessboard.Square (Sq) = Black_Rook or else Chessboard.Square (Sq) = Black_Queen then
                  if Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                     return True;
                  end if;
               end if;
            end loop;
            -- look for knight blocks
            for Offset of Knight_Offsets loop
               if Chessboard.Square (Target + Offset) = Black_Knight then
                  if Chessboard.Absolute_Pin_Direction (Target + Offset) = No_Direction then
                     return True;
                  end if;
               end if;
            end loop;
            -- look for pawn blocks
            if Target /= Origin then
               Sq := Target + North;
               if Chessboard.Square (Sq) = Black_Pawn and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                  if Target in A1 .. H1 then
                     return True;
                  else
                     return True;
                  end if;
               end if;
               if Chessboard.Square (Sq) = Empty then
                  Sq := Target + North + North;
                  if Chessboard.Square (Sq) = Black_Pawn and then Rank (Sq) = Rank (A7) and then Chessboard.Absolute_Pin_Direction (Sq) = No_Direction then
                     return True;
                  end if;
               end if;
            end if;

            Target := Target - Direction; -- we are going backward
            exit when not Sliding_Attack; -- because non-sliding attacker can only be captured
         end loop;

      end if;

      return False;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end King_Has_Escapes;


   -----------------------------
   -- Generate_Tactical_Moves --
   -----------------------------

   procedure Generate_Tactical_Moves (Chessboard : in out Chessboard_Type) is
      Piece         : Piece_Type;
      Target        : Square_Type;
      Pin_Direction : Direction_Type := 0;
      En_Passant    : Square_Type      renames Chessboard.En_Passant (Chessboard.History_Ply);
      White_Pieces  : Pieces_List_Type renames Chessboard.Pieces_List (White_Pieces_Range);
      Black_Pieces  : Pieces_List_Type renames Chessboard.Pieces_List (Black_Pieces_Range);
   begin

      Chessboard.Force_Validity_Test := False;

      Chessboard.Clear_Moves_List;

      if Chessboard.Side_To_Move = White then

         if Chessboard.White_Has_King_In_Check then
            Chessboard.Generate_Check_Evasion;
            return;
         end if;


         for Square of White_Pieces loop
            exit when Square = 0;

            Piece := Chessboard.Square (Square);

            Pin_Direction := (if Piece = White_King then No_Direction else Chessboard.Absolute_Pin_Direction (Square));

            case Piece is
               when White_Pawn =>
                  Target := Square + North;
                  if Pin_Direction in No_Direction | North | South then
                     if Chessboard.Square (Target) = Empty then
                        if Target in A8 .. H8 then
                           Chessboard.Register_Tactical_Move (Square, Target, Promotion);
                        else
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                        if Square in A2 .. H2 then
                           Target := Target + North;
                           if Chessboard.Square (Target) = Empty then
                              Chessboard.Register_Tactical_Move (Square, Target, Pawn_Move_Two_Square);
                           end if;
                        end if;
                     end if;
                  end if;
                  -- captures
                  if Pin_Direction in No_Direction | North_West then
                     if File (Square) /= File_A then
                        Target := Square + North_West;
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           if Target in A8 .. H8 then
                              Chessboard.Register_Tactical_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Tactical_Move (Square, Target);
                           end if;
                        elsif Target = En_Passant then
                           Chessboard.Register_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

                  if Pin_Direction in No_Direction | North_East then
                     if File (Square) /= File_H then
                        Target := Square + North_East;
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           if Target in A8 .. H8 then
                              Chessboard.Register_Tactical_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Tactical_Move (Square, Target);
                           end if;
                        elsif Target = En_Passant then
                           Chessboard.Register_Tactical_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

               when White_Knight =>
                  if Pin_Direction = No_Direction then
                     for Offset of Knight_Offsets loop
                        Target := Square + Offset;
                        if Chessboard.Square (Target) = Empty or else Chessboard.Square (Target) in Black_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end loop;
                  end if;

               when White_Bishop =>
                  for Offset of Bishop_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Chessboard.Register_Tactical_Move (Square, Target);
                           Target := Target + Offset;
                        end loop;
                        -- does end with capture?
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when White_Rook =>
                  for Offset of Rook_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Chessboard.Register_Tactical_Move (Square, Target);
                           Target := Target + Offset;
                        end loop;
                        -- does end with capture?
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when White_Queen =>
                  for Offset of Queen_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Chessboard.Register_Tactical_Move (Square, Target);
                           Target := Target + Offset;
                        end loop;
                        -- does end with capture?
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when White_King =>
                  for Offset of King_Offsets loop
                     Target := Square + Offset;
                     if Chessboard.Square (Target) = Empty or else Chessboard.Square (Target) in Black_Piece_Type then
                        Chessboard.Register_Tactical_Move (Square, Target);
                     end if;
                  end loop;

               when others => null;
            end case;
         end loop;

         -- look for castle

         if not Chessboard.Force_Validity_Test then

            if Chessboard.White_Castle_Kingside (Chessboard.History_Ply) then
               if Chessboard.Square (F1) = Empty and then Chessboard.Square (G1) = Empty then
                  if not Chessboard.Attacks (Black, F1) and then not Chessboard.Attacks (Black, G1) then
                     Chessboard.Register_Tactical_Move (From => E1, To => G1, Flag => Castle);
                  end if;
               end if;
            end if;

            if Chessboard.White_Castle_Queenside (Chessboard.History_Ply) then
               if Chessboard.Square (D1) = Empty and then Chessboard.Square (C1) = Empty and then Chessboard.Square (B1) = Empty then
                  if not Chessboard.Attacks (Black, D1) and then not Chessboard.Attacks (Black, C1) then
                     Chessboard.Register_Tactical_Move (From => E1, To => C1, Flag => Castle);
                  end if;
               end if;
            end if;

         end if;

      else

         if Chessboard.Black_Has_King_In_Check then
            Chessboard.Generate_Check_Evasion;
            return;
         end if;


         for Square of Black_Pieces loop
            exit when Square = 0;
            Piece := Chessboard.Square (Square);

            Pin_Direction := (if Piece = Black_King then No_Direction else Chessboard.Absolute_Pin_Direction (Square));

            case Piece is
               when Black_Pawn =>
                  Target := Square + South;
                  if Pin_Direction in No_Direction | South | North then
                     if Chessboard.Square (Target) = Empty then
                        if Target in A1 .. H1 then
                           Chessboard.Register_Tactical_Move (Square, Target, Promotion);
                        else
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                        if Square in A7 .. H7 then
                           Target := Target + South;
                           if Chessboard.Square (Target) = Empty then
                              Chessboard.Register_Tactical_Move (Square, Target, Pawn_Move_Two_Square);
                           end if;
                        end if;
                     end if;
                  end if;

                  -- captures
                  if Pin_Direction in No_Direction | South_East then
                     if File (Square) /= File_H then
                        Target := Square + South_East;
                        if Chessboard.Square (Target) in White_Piece_Type then
                           if Target in A1 .. H1 then
                              Chessboard.Register_Tactical_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Tactical_Move (Square, Target);
                           end if;
                        elsif Target = En_Passant then
                           Chessboard.Register_Tactical_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

                  if Pin_Direction in No_Direction | South_West then
                     if File (Square) /= File_A then
                        Target := Square + South_West;
                        if Chessboard.Square (Target) in White_Piece_Type then
                           if Target in A1 .. H1 then
                              Chessboard.Register_Tactical_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Tactical_Move (Square, Target);
                           end if;
                        elsif Target = En_Passant then
                           Chessboard.Register_Tactical_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

               when Black_Knight =>
                  if Pin_Direction = No_Direction then
                     for Offset of Knight_Offsets loop
                        Target := Square + Offset;
                        if Chessboard.Square (Target) = Empty or else Chessboard.Square (Target) in White_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end loop;
                  end if;

               when Black_Bishop =>
                  for Offset of Bishop_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Chessboard.Register_Tactical_Move (Square, Target);
                           Target := Target + Offset;
                        end loop;
                        -- look for capture
                        if Chessboard.Square (Target) in White_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when Black_Rook =>
                  for Offset of Rook_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Chessboard.Register_Tactical_Move (Square, Target);
                           Target := Target + Offset;
                        end loop;
                        if Chessboard.Square (Target) in White_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when Black_Queen =>
                  for Offset of Queen_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Chessboard.Register_Tactical_Move (Square, Target);
                           Target := Target + Offset;
                        end loop;
                        if Chessboard.Square (Target) in White_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when Black_King =>
                  for Offset of King_Offsets loop
                     Target := Square + Offset;
                     if Chessboard.Square (Target) = Empty or else Chessboard.Square (Target) in White_Piece_Type then
                        Chessboard.Register_Tactical_Move (Square, Target);
                     end if;
                  end loop;

               when others => null;
            end case;
         end loop;

         -- look for castle moves

         if not Chessboard.Force_Validity_Test then

            if Chessboard.Black_Castle_Kingside (Chessboard.History_Ply) then
               if Chessboard.Square (F8) = Empty and then Chessboard.Square (G8) = Empty then
                  if not Chessboard.Attacks (White, F8) and then not Chessboard.Attacks (White, G8) then
                     Chessboard.Register_Tactical_Move (From => E8, To => G8, Flag => Castle);
                  end if;
               end if;
            end if;

            if Chessboard.Black_Castle_Queenside (Chessboard.History_Ply) then
               if Chessboard.Square (D8) = Empty and then Chessboard.Square (C8) = Empty and then Chessboard.Square (B8) = Empty then
                  if not Chessboard.Attacks (White, D8) and then not Chessboard.Attacks (White, C8) then
                     Chessboard.Register_Tactical_Move (From => E8, To => C8, Flag => Castle);
                  end if;
               end if;
            end if;

         end if;

      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Generate_Tactical_Moves;


   -----------------------
   -- Generate_Captures --
   -----------------------

   procedure Generate_Captures (Chessboard : in out Chessboard_Type) is
      Piece         : Piece_Type;
      Target        : Square_Type;
      Pin_Direction : Direction_Type := 0;
      En_Passant    : Square_Type      renames Chessboard.En_Passant (Chessboard.History_Ply);
      White_Pieces  : Pieces_List_Type renames Chessboard.Pieces_List (White_Pieces_Range);
      Black_Pieces  : Pieces_List_Type renames Chessboard.Pieces_List (Black_Pieces_Range);
   begin

      Chessboard.Force_Validity_Test := False;

      Chessboard.Clear_Moves_List;

      if Chessboard.Side_To_Move = White then

         if Chessboard.White_Has_King_In_Check then
            Chessboard.Generate_Check_Evasion;
            return;
         end if;


         for Square of White_Pieces loop
            exit when Square = 0;

            Piece := Chessboard.Square (Square);

            Pin_Direction := (if Piece = White_King then No_Direction else Chessboard.Absolute_Pin_Direction (Square));

            case Piece is
               when White_Pawn =>

                  if Pin_Direction in No_Direction | North_West then
                     if File (Square) /= File_A then
                        Target := Square + North_West;
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           if Target in A8 .. H8 then
                              Chessboard.Register_Tactical_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Tactical_Move (Square, Target);
                           end if;
                        elsif Target = En_Passant then
                           Chessboard.Register_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

                  if Pin_Direction in No_Direction | North_East then
                     if File (Square) /= File_H then
                        Target := Square + North_East;
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           if Target in A8 .. H8 then
                            Chessboard.Register_Tactical_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Tactical_Move (Square, Target);
                           end if;
                        elsif Target = En_Passant then
                           Chessboard.Register_Tactical_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

               when White_Knight =>
                  if Pin_Direction = No_Direction then
                     for Offset of Knight_Offsets loop
                        Target := Square + Offset;
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end loop;
                  end if;

               when White_Bishop =>
                  for Offset of Bishop_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Target := Target + Offset;
                        end loop;
                        -- does end with capture?
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when White_Rook =>
                  for Offset of Rook_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Target := Target + Offset;
                        end loop;
                        -- does end with capture?
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when White_Queen =>
                  for Offset of Queen_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Target := Target + Offset;
                        end loop;
                        -- does end with capture?
                        if Chessboard.Square (Target) in Black_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when White_King =>
                  for Offset of King_Offsets loop
                     Target := Square + Offset;
                     if Chessboard.Square (Target) in Black_Piece_Type then
                        Chessboard.Register_Tactical_Move (Square, Target);
                     end if;
                  end loop;

               when others => null;
            end case;
         end loop;



      else

         if Chessboard.Black_Has_King_In_Check then
            Chessboard.Generate_Check_Evasion;
            return;
         end if;


         for Square of Black_Pieces loop
            exit when Square = 0;
            Piece := Chessboard.Square (Square);

            Pin_Direction := (if Piece = Black_King then No_Direction else Chessboard.Absolute_Pin_Direction (Square));

            case Piece is
               when Black_Pawn =>
                  if Pin_Direction in No_Direction | South_East then
                     if File (Square) /= File_H then
                        Target := Square + South_East;
                        if Chessboard.Square (Target) in White_Piece_Type then
                           if Target in A1 .. H1 then
                              Chessboard.Register_Tactical_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Tactical_Move (Square, Target);
                           end if;
                        elsif Target = En_Passant then
                           Chessboard.Register_Tactical_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

                  if Pin_Direction in No_Direction | South_West then
                     if File (Square) /= File_A then
                        Target := Square + South_West;
                        if Chessboard.Square (Target) in White_Piece_Type then
                           if Target in A1 .. H1 then
                              Chessboard.Register_Tactical_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Tactical_Move (Square, Target);
                           end if;
                        elsif Target = En_Passant then
                           Chessboard.Register_Tactical_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

               when Black_Knight =>
                  if Pin_Direction = No_Direction then
                     for Offset of Knight_Offsets loop
                        Target := Square + Offset;
                        if Chessboard.Square (Target) in White_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end loop;
                  end if;

               when Black_Bishop =>
                  for Offset of Bishop_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Target := Target + Offset;
                        end loop;
                        -- look for capture
                        if Chessboard.Square (Target) in White_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when Black_Rook =>
                  for Offset of Rook_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Target := Target + Offset;
                        end loop;
                        if Chessboard.Square (Target) in White_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when Black_Queen =>
                  for Offset of Queen_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        Target := Square + Offset;
                        while Chessboard.Square (Target) = Empty loop
                           Target := Target + Offset;
                        end loop;
                        if Chessboard.Square (Target) in White_Piece_Type then
                           Chessboard.Register_Tactical_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when Black_King =>
                  for Offset of King_Offsets loop
                     Target := Square + Offset;
                     if Chessboard.Square (Target) in White_Piece_Type then
                        Chessboard.Register_Tactical_Move (Square, Target);
                     end if;
                  end loop;

               when others => null;
            end case;
         end loop;

      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Generate_Captures;


   ------------------------
   -- Generate_See_Moves --
   ------------------------

   procedure Generate_See_Moves (Chessboard : in out Chessboard_Type; Target : in Square_Type) is
      To            : Square_Type;
      Piece         : Piece_Type;
      Pin_Direction : Direction_Type := No_Direction;
      White_Pieces  : Pieces_List_Type renames Chessboard.Pieces_List (White_Pieces_Range);
      Black_Pieces  : Pieces_List_Type renames Chessboard.Pieces_List (Black_Pieces_Range);
   begin

      pragma Assert (Chessboard.Square (Target) /= Frame, "Invalid call for Generate_See_Moves, target out of board");

      Chessboard.Clear_Moves_List;

      Chessboard.Force_Validity_Test := False;

      if Chessboard.Side_To_Move = White then

         if Chessboard.White_Has_King_In_Check then
            Chessboard.Generate_See_Check_Evasion (Target);
            return;
         end if;

         for Square of White_Pieces loop
            exit when Square = 0;

            Piece := Chessboard.Square (Square);

            Pin_Direction := (if Piece = White_King then No_Direction else Chessboard.Absolute_Pin_Direction (Square));

            case Piece is
               when White_Pawn =>

                  if Pin_Direction in No_Direction | North_West then
                     if File (Square) /= File_A then
                        if Square + North_West = Target then
                           if Target in A8 .. H8 then
                              Chessboard.Register_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Move (Square, Target);
                           end if;
                        elsif Target = Chessboard.En_Passant (Chessboard.History_Ply) then
                           Chessboard.Register_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

                  if Pin_Direction in No_Direction | North_East then
                     if File (Square) /= File_H then
                        if Square + North_East = Target then
                           if Target in A8 .. H8 then
                              Chessboard.Register_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Move (Square, Target);
                           end if;
                        elsif Target = Chessboard.En_Passant (Chessboard.History_Ply) then
                           Chessboard.Register_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

               when White_Knight =>
                  if Pin_Direction = No_Direction then
                     for Offset of Knight_Offsets loop
                        if Square + Offset = Target then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end loop;
                  end if;

               when White_Bishop =>
                  for Offset of Bishop_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        To := Square + Offset;
                        while Chessboard.Square (To) = Empty loop
                           To := To + Offset;
                        end loop;
                        -- does end with capture?
                        if To = Target then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when White_Rook =>
                  for Offset of Rook_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        To := Square + Offset;
                        while Chessboard.Square (To) = Empty loop
                           To := To + Offset;
                        end loop;
                        -- does end with capture?
                        if To = Target then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when White_Queen =>
                  for Offset of Queen_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        To := Square + Offset;
                        while Chessboard.Square (To) = Empty loop
                           To := To + Offset;
                        end loop;
                        -- does end with capture?
                        if To = Target then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when White_King =>
                  for Offset of King_Offsets loop
                     if Square + Offset = Target then
                        Chessboard.Register_Move (Square, Target);
                     end if;
                  end loop;

               when others => null;
            end case;

         end loop;

      else

         if Chessboard.Black_Has_King_In_Check then
            Chessboard.Generate_See_Check_Evasion (Target);
            return;
         end if;

         for Square of Black_Pieces loop
            exit when Square = 0;
            Piece := Chessboard.Square (Square);

            Pin_Direction := (if Piece = Black_King then No_Direction else Chessboard.Absolute_Pin_Direction (Square));

            case Piece is
               when Black_Pawn =>
                  if Pin_Direction in No_Direction | South_East then
                     if File (Square) /= File_H then
                        if Target = Square + South_East then
                           if Target in A1 .. H1 then
                              Chessboard.Register_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Move (Square, Target);
                           end if;
                        elsif Target = Chessboard.En_Passant (Chessboard.History_Ply) then
                           Chessboard.Register_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

                  if Pin_Direction in No_Direction | South_West then
                     if File (Square) /= File_A then
                        if Target = Square + South_West then
                           if Target in A1 .. H1 then
                              Chessboard.Register_Move (Square, Target, Promotion);
                           else
                              Chessboard.Register_Move (Square, Target);
                           end if;
                        elsif Target = Chessboard.En_Passant (Chessboard.History_Ply) then
                           Chessboard.Register_Move (Square, Target, Capture_En_Passant);
                        end if;
                     end if;
                  end if;

               when Black_Knight =>
                  if Pin_Direction = No_Direction then
                     for Offset of Knight_Offsets loop
                        if Target = Square + Offset then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end loop;
                  end if;

               when Black_Bishop =>
                  for Offset of Bishop_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        To := Square + Offset;
                        while Chessboard.Square (To) = Empty loop
                           To := To + Offset;
                        end loop;
                        -- does end with capture?
                        if To = Target then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when Black_Rook =>
                  for Offset of Rook_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        To := Square + Offset;
                        while Chessboard.Square (To) = Empty loop
                           To := To + Offset;
                        end loop;
                        -- does end with capture?
                        if To = Target then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when Black_Queen =>
                  for Offset of Queen_Offsets loop
                     if Pin_Direction in No_Direction | Offset | -Offset then
                        To := Square + Offset;
                        while Chessboard.Square (To) = Empty loop
                           To := To + Offset;
                        end loop;
                        -- does end with capture?
                        if To = Target then
                           Chessboard.Register_Move (Square, Target);
                        end if;
                     end if;
                  end loop;

               when Black_King =>
                  for Offset of King_Offsets loop
                     if Target = Square + Offset then
                        Chessboard.Register_Move (Square, Target);
                     end if;
                  end loop;

               when others => null;
            end case;

         end loop;

      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Generate_See_Moves;


   -------------------
   -- Register_Move --
   -------------------

   procedure Register_Move (Chessboard : in out Chessboard_Type; From, To : in Square_Type) is
      Move : Move_Type;
   begin
      Move := Move_Type'
        (Piece          => Chessboard.Square (From),
         Captured       => Chessboard.Square (To),
         From           => From,
         To             => To,
         Flag           => Standard_Move,
         Promotion      => Empty,
         Check          => No_Check,
         Ambiguous_Flag => Ambiguous_None);
      Chessboard.Register_Move (Move);
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Register_Move;


   -------------------
   -- Register_Move --
   -------------------

   procedure Register_Move (Chessboard : in out Chessboard_Type; From, To : in Square_Type; Flag : in Flag_Type) is
      Move : Move_Type;
   begin
      Move := Move_Type'
        (Piece          => Chessboard.Square (From),
         Captured       => Chessboard.Square (To),
         From           => From,
         To             => To,
         Flag           => Flag,
         Promotion      => Empty,
         Check          => No_Check,
         Ambiguous_Flag => Ambiguous_None);
      if Flag = Promotion then
         if Chessboard.Side_To_Move = White then
            for Promoted in White_Promotion_Type loop
               Move.Promotion := Promoted;
               Chessboard.Register_Move (Move);
            end loop;
         else
             for Promoted in Black_Promotion_Type Loop
               Move.Promotion := Promoted;
               Chessboard.Register_Move (Move);
            end loop;
         end if;
      else
         Chessboard.Register_Move (Move);
      end if;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Register_Move;


   -------------------
   -- Register_Move --
   -------------------

   procedure Register_Move (Chessboard : in out Chessboard_Type; Move : in Move_Type) is
      Ply          : Depth_Type renames Chessboard.Ply;
      Moves_Number : Natural renames Chessboard.Moves_Pointer (Ply + 1);
      The_Move     : Move_Type;
   begin

      pragma Assert
        (Moves_Number in Move_Stack_Range_Type'Range,
         "Moves number in Register_Move failed with value:" & Natural'Image (Moves_Number));

      -------------------
      -- Legality Test --
      -------------------

      if Chessboard.Move_Leaves_King_In_Check (Move) then
         return;
      end if;

      The_Move := Move_Type'
        (Piece          => Move.Piece,
         Captured       => Move.Captured,
         From           => Move.From,
         To             => Move.To,
         Flag           => Move.Flag,
         Promotion      => Move.Promotion,
         Check          => Chessboard.Move_Checks_Opponent_King (Move),
         Ambiguous_Flag => Chessboard.Detect_Ambiguous_Move_Notation (Move));

      Chessboard.Moves_Stack (Moves_Number) := The_Move;
      Moves_Number := Moves_Number + 1;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Register_Move;


   ----------------------------
   -- Register_Tactical_Move --
   ----------------------------

   procedure Register_Tactical_Move (Chessboard : in out Chessboard_Type; From, To : in Square_Type) is
      Move : Move_Type;
   begin
      Move := Move_Type'
        (Piece          => Chessboard.Square (From),
         Captured       => Chessboard.Square (To),
         From           => From,
         To             => To,
         Flag           => Standard_Move,
         Promotion      => Empty,
         Check          => No_Check,
         Ambiguous_Flag => Ambiguous_None);
      Chessboard.Register_Tactical_Move (Move);
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Register_Tactical_Move;


   -------------------
   -- Register_Move --
   -------------------

   procedure Register_Tactical_Move (Chessboard : in out Chessboard_Type; From, To : in Square_Type; Flag : in Flag_Type) is
      Move : Move_Type := Empty_Move;
   begin
      Move := Move_Type'
        (Piece          => Chessboard.Square (From),
         Captured       => Chessboard.Square (To),
         From           => From,
         To             => To,
         Flag           => Flag,
         Promotion      => Empty,
         Check          => No_Check,
         Ambiguous_Flag => Ambiguous_None);
      if Flag = Promotion then
         if Chessboard.Side_To_Move = White then
            for Promoted in White_Promotion_Type Loop
               Move.Promotion := Promoted;
               Chessboard.Register_Tactical_Move (Move);
            end loop;
         else
            for Promoted in Black_Promotion_Type Loop
               Move.Promotion := Promoted;
               Chessboard.Register_Tactical_Move (Move);
            end loop;
         end if;
      else
         Chessboard.Register_Tactical_Move (Move);
      end if;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Register_Tactical_Move;


   -------------------
   -- Register_Move --
   -------------------

   procedure Register_Tactical_Move (Chessboard : in out Chessboard_Type; Move : in Move_Type) is
      Ply          : Depth_Type renames Chessboard.Ply;
      Moves_Number : Natural renames Chessboard.Moves_Pointer (Ply + 1);
      The_Move     : Move_Type := Move;
   begin

      pragma Assert
        (Moves_Number in Move_Stack_Range_Type'Range,
         "Moves number in Register_Tactical_Move failed with value:" & Natural'Image (Moves_Number));

      -- Before registering a move we must check for
      -- it's validity according to the rule of the game
      -- Some of the informations we need are obtained
      -- in the move generator. Others must be verified here ;-)

      if Chessboard.Move_Leaves_King_In_Check (Move) then
         return;
      end if;

      The_Move := Move_Type'
        (Piece          => Move.Piece,
         Captured       => Move.Captured,
         From           => Move.From,
         To             => Move.To,
         Flag           => Move.Flag,
         Promotion      => Move.Promotion,
         Check          => Chessboard.Move_Checks_Opponent_King (Move),
         Ambiguous_Flag => Ambiguous_None); -- Will be set later, only if necessary

      if Move_Is_Tactical (The_Move) then
         The_Move.Ambiguous_Flag := Chessboard.Detect_Ambiguous_Move_Notation (The_Move);
         Chessboard.Moves_Stack (Moves_Number) := The_Move;
         Moves_Number := Moves_Number + 1;
      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Register_Tactical_Move;


   ------------------------------------
   -- Detect_Ambiguous_Move_Notation --
   ------------------------------------

   function Detect_Ambiguous_Move_Notation (Chessboard : in out Chessboard_Type; Move : in Move_Type) return Ambiguous_Flag_Type is
      Piece                           : constant Piece_Type := Move.Piece;
      Square                          : constant Square_Type := Move.From;
      Target                          : Square_Type;
      Slide_Target                    : Square_Type;
      Absolute_Pin_Offset             : Direction_Type := No_Direction;
      Rank_Disambiguation_Required    : Boolean := False;
      File_Disambiguation_Required    : Boolean := False;
      Disaligned_Disambiguation_Required : Boolean := False;
   begin

      if Default_Notation /= Standard_Algebraic then
         return Ambiguous_None;
      end if;

      case Piece is
         when White_Knight | Black_Knight =>
            for Offset of Knight_Offsets loop
               Target := Move.To + Offset;
               if Target /= Square and then Chessboard.Square (Target) = Piece and then Absolute_Pin_Direction (Chessboard, Target) = No_Direction then
                  -- If two knights are on a same rank, then the target attacking square is on file
                  if Rank (Square) = Rank (Target) then
                     File_Disambiguation_Required := True;
                  elsif File (Square) = File (Target) then
                     Rank_Disambiguation_Required := True;
                  else
                     -- Another Knight can reach the target attack square,
                     -- but that Knight is neither on the same file nor on
                     -- the same rank as the current Knight.
                     -- We have to investigate further to decide which kind
                     -- of disambiguation will be "greedy"
                     Disaligned_Disambiguation_Required := True;
                  end if;
               end if;
            end loop;

         when White_Bishop | Black_Bishop =>
            Slide_Target := Move.To;
            for Offset of Bishop_Offsets loop
               Target := Slide_Target + Offset;
               while Chessboard.Square (Target) = Empty loop
                  Target := Target + Offset;
               end loop;
               if Target /= Square and then Chessboard.Square (Target) = Piece then
                  Absolute_Pin_Offset := Absolute_Pin_Direction (Chessboard, Target);
                  if Absolute_Pin_Offset in No_Direction | Offset | -Offset then
                     if Rank (Target) = Rank (Square) then
                        File_Disambiguation_Required := True;
                     elsif File (Target) = File (Square) then
                        Rank_Disambiguation_Required := True;
                     else
                        -- Another Bishop can reach the target attack square,
                        -- but that Bishop is neither on the same file nor on
                        -- the same rank as the moving Bishop.
                        -- We have to investigate further to decide which kind
                        -- of disambiguation will be "greedy"
                        Disaligned_Disambiguation_Required := True;
                     end if;
                  end if;
               end if;
            end loop;

         when White_Rook | Black_Rook =>
            Slide_Target := Move.To;
            for Offset of Rook_Offsets loop
               Target := Slide_Target + Offset;
               while Chessboard.Square (Target) = Empty loop
                  Target := Target + Offset;
               end loop;
               if Target /= Square and then Chessboard.Square (Target) = Piece then
                  Absolute_Pin_Offset := Absolute_Pin_Direction (Chessboard, Target);
                  if Absolute_Pin_Offset in No_Direction | Offset | -Offset then
                     if Rank (Target) = Rank (Square) then
                        File_Disambiguation_Required := True;
                     elsif File (Target) = File (Square) then
                        Rank_Disambiguation_Required := True;
                     else
                        -- Another Rook can reach the target attack square,
                        -- but that Rook is neither on the same file nor on
                        -- the same rank as the moving Rook.
                        -- We have to investigate further to decide which kind
                        -- of disambiguation will be "greedy"
                        Disaligned_Disambiguation_Required := True;
                     end if;
                  end if;
               end if;
            end loop;

         when White_Queen | Black_Queen =>
            Slide_Target := Move.To;
            for Offset of Queen_Offsets loop
               Target := Slide_Target + Offset;
               while Chessboard.Square (Target) = Empty loop
                  Target := Target + Offset;
               end loop;
               if Target /= Square and then Chessboard.Square (Target) = Piece then
                  Absolute_Pin_Offset := Absolute_Pin_Direction (Chessboard, Target);
                  if Absolute_Pin_Offset in No_Direction | Offset | -Offset then
                     if Rank (Target) = Rank (Square) then
                        File_Disambiguation_Required := True;
                     elsif File (Target) = File (Square) then
                        Rank_Disambiguation_Required := True;
                     else
                        -- Another Queen can reach the target attack square,
                        -- but that Queen is neither on the same file nor on
                        -- the same rank as the moving Queen.
                        -- We have to investigate further to decide which kind
                        -- of disambiguation will be "greedy"
                        Disaligned_Disambiguation_Required := True;
                     end if;
                  end if;
               end if;
            end loop;

         when others => null;
      end case;


      if File_Disambiguation_Required and then Rank_Disambiguation_Required then
         return Ambiguous_Both;
      end if;

      if Disaligned_Disambiguation_Required then
         if Rank_Disambiguation_Required then
            return Ambiguous_Rank;
         else
            return Ambiguous_File;
         end if;
      end if;

      if File_Disambiguation_Required then
         return Ambiguous_File;
      elsif Rank_Disambiguation_Required then
         return Ambiguous_Rank;
      end if;

      return Ambiguous_None;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Detect_Ambiguous_Move_Notation;


   ----------------------------
   -- Absolute_Pin_Direction --
   ----------------------------

   function Absolute_Pin_Direction (Chessboard : in Chessboard_Type; Square : in Square_Type) return Direction_Type is
      Side          : Color_Type;
      King_Position : Square_Type;
      Direction     : Direction_Type := No_Direction;
      Sq            : Square_Type;
      White_King_Position : Square_Type renames Chessboard.White_King_Position;
      Black_King_Position : Square_Type renames Chessboard.Black_King_Position;
   begin

      pragma Assert (Square /= White_King_Position, "Called 'Is_Absolute_Pinned' for White King is not allowed");
      pragma Assert (Square /= Black_King_Position, "Called 'Is_Absolute_Pinned' for Black King is not allowed");

      if Chessboard.Side_To_Move = White then
         Side := Black;
         King_Position := White_King_Position;
      else
         Side := White;
         King_Position := Black_King_Position;
      end if;

      -- Detect the direction in which to look for attackers
      Direction := Find_Sliding_Direction (Square, King_Position);

      -- Check if there are other pieces between
      -- the king and the square (i.e. in the opposite
      -- direction). If so, then no further search is needed
      if Direction /= No_Direction then
         Sq := Square;
         loop
            Sq := Sq - Direction;
            exit when Sq = King_Position;
            if Chessboard.Square (Sq) /= Empty then
               return No_Direction;
            end if;
         end loop;
      end if;

      -- If opponent attacks the king then the piece
      -- I'm going to move is absolutely pinned and can't be
      -- moved!
      return (if Attacks_To (Direction) (Chessboard.Square, Side, Square) then Direction else No_Direction);

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Absolute_Pin_Direction;


   ------------------------------
   -- Piece_Is_Absolute_Pinned --
   ------------------------------

   function Piece_Is_Absolute_Pinned (Chessboard : in Chessboard_Type; Square : in Square_Type) return Direction_Type is
      Side          : constant Color_Type := (if Chessboard.Square (Square) in White_Piece_Type then White else Black);
      Opponent_Side : constant Color_Type := not Side;
      King_Position : Square_Type;
      Direction     : Direction_Type := No_Direction;
      Sq            : Square_Type;
      White_King_Position : Square_Type renames Chessboard.White_King_Position;
      Black_King_Position : Square_Type renames Chessboard.Black_King_Position;
   begin

      pragma Assert (Square /= White_King_Position, "Called 'Piece_Is_Absolute_Pinned' for White King is not allowed");
      pragma Assert (Square /= Black_King_Position, "Called 'Piece_Is_Absolute_Pinned' for Black King is not allowed");

      if Side = White then
         King_Position := White_King_Position;
      else
         King_Position := Black_King_Position;
      end if;

      -- Detect the direction in which to look for attackers
      Direction := Find_Sliding_Direction (Square, King_Position);

      -- Check if there are other pieces between
      -- the king and the square (i.e. in the opposite
      -- direction). If so, then no further search is needed
      if Direction /= No_Direction then
         Sq := Square;
         loop
            Sq := Sq - Direction;
            exit when Sq = King_Position;
            if Chessboard.Square (Sq) /= Empty then
               return No_Direction;
            end if;
         end loop;
      end if;

      -- If opponent attacks the king then the piece
      -- I'm going to move is absolutely pinned and can't be
      -- moved!
      return (if Attacks_To (Direction) (Chessboard.Square, Opponent_Side, Square) then Direction else No_Direction);

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Piece_Is_Absolute_Pinned;


   -------------------------------
   -- Move_Leaves_King_In_Check --
   -------------------------------

   function Move_Leaves_King_In_Check (Chessboard : in out Chessboard_Type; Move : in Move_Type) return Boolean is
      White_King_Position : Square_Type renames Chessboard.White_King_Position;
      Black_King_Position : Square_Type renames Chessboard.Black_King_Position;
   begin

      if Chessboard.Force_Validity_Test or else Move.Flag = Capture_En_Passant then
         Chessboard.Play (Move);
         if Chessboard.Side_To_Move = White and then Chessboard.Black_Has_King_In_Check then
            Chessboard.Undo;
            return True;
         elsif Chessboard.Side_To_Move = Black and then Chessboard.White_Has_King_In_Check then
            Chessboard.Undo;
            return True;
         end if;
         Chessboard.Undo;

      elsif Move.From = White_King_Position then
         if Chessboard.Attacks (Black, Move.To) then
            return True;
         end if;

      elsif Move.From = Black_King_Position then
         if Chessboard.Attacks (White, Move.To) then
            return True;
         end if;

      end if;

      return False;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Move_Leaves_King_In_Check;


   ------------------------------
   -- Move_Checks_Opponen_King --
   ------------------------------

   function Move_Checks_Opponent_King (Chessboard : in out Chessboard_Type; Move : in Move_Type) return Check_Type is
      Type_Of_Check       : Check_Type := No_Check;
      Side                : Color_Type;
      King_Position       : Square_Type;
      Direction           : Direction_Type := No_Direction;
      Discovery_Direction : Direction_Type := No_Direction;
      Ep_Direction        : Direction_Type := No_Direction;
      Ep_Captured         : Square_Type := 0;
      Type_Of_Castle      : Castle_Type := No_Castle;
      White_King_Position : Square_Type renames Chessboard.White_King_Position;
      Black_King_Position : Square_Type renames Chessboard.Black_King_Position;
      The_Move            : Move_Type := Move;
   begin

      if Chessboard.Side_To_Move = White then
         Side := White;
         King_Position := Black_King_Position;
      else
         Side := Black;
         King_Position := White_King_Position;
      end if;

      ------------------
      -- Direct Check --
      ------------------

      if Move.Piece in Knight_Type or else Move.Promotion in Knight_Type then
         for Offset of Knight_Offsets loop
            if Move.To + Offset = King_Position then
               Type_Of_Check := Direct_Check;
            end if;
            exit when Type_Of_Check /= No_Check;
         end loop;

      elsif Move.Piece = White_Pawn and then Move.Promotion = Empty then
         if Move.To + North_West = Black_King_Position
           or else Move.To + North_East = King_Position then
            Type_Of_Check := Direct_Check;
         end if;

      elsif Move.Piece = Black_Pawn and then Move.Promotion = Empty then
         if Move.To + South_East = King_Position
           or else Move.To + South_West = White_King_Position then
            Type_Of_Check := Direct_Check;
         end if;

      else
         -- Find the direction where the attack come from, if any
         Direction := Find_Sliding_Direction (Move.To, King_Position);

         if Direction /= No_Direction then

            Chessboard.Square (Move.From) := Empty;
            Chessboard.Square (Move.To) := (if Move.Promotion = Empty then Move.Piece else Move.Promotion);

            if Attacks_To (Direction) (Chessboard.Square, Side, King_Position) then
               Type_Of_Check := Direct_Check;
            end if;

            Chessboard.Square (Move.From) := Move.Piece;
            Chessboard.Square (Move.To) := Move.Captured;
         end if;

      end if;

      --------------------------------------
      -- Discovery Check and Double Check --
      --------------------------------------

      if Move.Flag = Capture_En_Passant then
         Ep_Captured := (if Move.Piece in White_Piece_Type then Move.To + South else Move.To + North);
         Chessboard.Square (Ep_Captured) := Empty;
      end if;

      Discovery_Direction := Find_Sliding_Direction (Move.From, King_Position);

      if Move.Piece in King_Type | Pawn_Type then
         -- In this case, before removing the piece to look for discovery attack,
         -- be sure that piece is not moving along discovery line itself!
         if Discovery_Direction = Find_Sliding_Direction (Move.To, King_Position) then
            Discovery_Direction := No_Direction; -- Reset it, then skip discovery check lookup
         end if;
      end if;

      if Discovery_Direction /= No_Direction then
         Chessboard.Square (Move.From) := Empty;

         if Attacks_To (Discovery_Direction) (Chessboard.Square, Side, King_Position) then
            Type_Of_Check := (if Type_Of_Check = No_Check then Discovery_Check else Double_Check);
         end if;

         Chessboard.Square (Move.From) := Move.Piece;
      end if;

      ----------------
      -- En-passant --
      ----------------

      if Move.Flag = Capture_En_Passant then
         Ep_Direction := Find_Sliding_Direction (Ep_Captured, King_Position);
         if Ep_Direction /= Discovery_Direction and then Ep_Direction /= Find_Sliding_Direction (Move.To, King_Position) then
            if Attacks_To (Ep_Direction) (Chessboard.Square, Side, King_Position) then
               Type_Of_Check := (if Type_Of_Check = No_Check then Discovery_Check else Double_Check);
            end if;
         end if;
         Chessboard.Square (Ep_Captured) := (if Move.Piece in White_Piece_Type then Black_Pawn else White_Pawn);
      end if;

      ------------
      -- Castle --
      ------------

      -- In castle moves, the rook involved into the castle can direct check
      -- the opponent king

      if Type_Of_Check = No_Check and then Move.Flag = Castle then

         if Move.From = E1 and then Move.To = G1 then
            Type_Of_Castle := White_Kingside;
         elsif Move.From = E1 and then Move.To = C1 then
            Type_Of_Castle := White_Queenside;
         elsif Move.From = E8 and then Move.To = G8 then
            Type_Of_Castle := Black_Kingside;
         elsif Move.From = E8 and then Move.To = C8 then
            Type_Of_Castle := Black_Queenside;
         else
            raise Invalid_Castle_Move;
         end if;


         case Type_Of_Castle is
            when No_Castle => null;

            when White_Kingside =>
               Chessboard.Square (G1) := White_King;
               Chessboard.Square (E1) := Empty;
               Chessboard.Square (F1) := White_Rook;
               if Attacks_From_South (Chessboard.Square, White, Black_King_Position) then
                  Type_Of_Check := Direct_Check;
               elsif Attacks_From_East (Chessboard.Square, White, Black_King_Position) then
                  Type_Of_Check := Direct_Check;
               end if;
               Chessboard.Square (F1) := Empty;
               Chessboard.Square (G1) := Empty;
               Chessboard.Square (E1) := White_King;

            when White_Queenside =>
               Chessboard.Square (D1) := White_Rook;
               Chessboard.Square (E1) := Empty;
               Chessboard.Square (C1) := White_King;
               if Attacks_From_South (Chessboard.Square, White, Black_King_Position) then
                  Type_Of_Check := Direct_Check;
               elsif Attacks_From_West (Chessboard.Square, White, Black_King_Position) then
                  Type_Of_Check := Direct_Check;
               end if;
               Chessboard.Square (D1) := Empty;
               Chessboard.Square (E1) := White_King;
               Chessboard.Square (C1) := Empty;

            when Black_Kingside =>
               Chessboard.Square (G8) := Black_King;
               Chessboard.Square (E8) := Empty;
               Chessboard.Square (F8) := Black_Rook;
               if Attacks_From_North (Chessboard.Square, Black, White_King_Position) then
                  Type_Of_Check := Direct_Check;
               elsif Attacks_From_East (Chessboard.Square, Black, White_King_Position) then
                  Type_Of_Check := Direct_Check;
               end if;
               Chessboard.Square (E8) := Black_King;
               Chessboard.Square (G8) := Empty;
               Chessboard.Square (F8) := Empty;

            when Black_Queenside =>
               Chessboard.Square (E8) := Empty;
               Chessboard.Square (C8) := Black_King;
               Chessboard.Square (D8) := Black_Rook;
               if Attacks_From_North (Chessboard.Square, Black, White_King_Position) then
                  Type_Of_Check := Direct_Check;
               elsif Attacks_From_West (Chessboard.Square, Black, White_King_Position) then
                  Type_Of_Check := Direct_Check;
               end if;
               Chessboard.Square (E8) := Black_King;
               Chessboard.Square (D8) := Empty;
               Chessboard.Square (C8) := Empty;
         end case;
      end if;

      ---------------
      -- Checkmate --
      ---------------

      if Type_Of_Check /= No_Check then
         The_Move.Check := Type_Of_Check;
         Chessboard.Play_Check_Move (The_Move);
         if not Chessboard.King_Has_Escapes (Type_Of_Check) then
            Type_Of_Check := Checkmate;
         end if;
         Chessboard.Undo_Check_Move;
      end if;

      return Type_Of_Check;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Move_Checks_Opponent_King;


   ----------
   -- Play --
   ----------

   procedure Play (Chessboard : in out Chessboard_Type; Move : in Move_Type) is
      From, To     : Square_Type;
      Piece        : constant Board_Piece_Type := Move.Piece;
      Captured     : constant Board_Piece_Type := Move.Captured;
      Promote      : constant Board_Piece_Type := Move.Promotion;
      Flag         : Flag_Type;
      History_Data : History_Move_Type;

      Ply                 : Depth_Type renames Chessboard.Ply;
      White_King_Position : Square_Type renames Chessboard.White_King_Position;
      Black_King_Position : Square_Type renames Chessboard.Black_King_Position;
      Side_To_Move        : Color_Type renames Chessboard.Side_To_Move;
      History_Ply         : History_Depth_Type renames Chessboard.History_Ply;
      Fifty               : Fifty_Counter_Type renames Chessboard.Fifty;
   begin

      ----------------------------------
      -- Store data to undo this move --
      ----------------------------------

      From := Move.From;
      To := Move.To;
      Flag := Move.Flag;

      History_Data := History_Move_Type'(Move => Move, Hash => Hash, Fifty => Fifty);

      Chessboard.En_Passant (History_Ply + 1) := 0;
      Chessboard.Fifty  := Chessboard.Fifty + 1;
      Chessboard.White_Castle_Kingside (History_Ply + 1) := Chessboard.White_Castle_Kingside (History_Ply);
      Chessboard.White_Castle_Queenside (History_Ply + 1) := Chessboard.White_Castle_Queenside (History_Ply);
      Chessboard.Black_Castle_Kingside (History_Ply + 1) := Chessboard.Black_Castle_Kingside (History_Ply);
      Chessboard.Black_Castle_Queenside (History_Ply + 1) := Chessboard.Black_Castle_Queenside (History_Ply);

      ------------
      -- Castle --
      ------------

      if Flag = Castle then
         case To is
            when G1 => -- white kingside castle
               From := H1;
               To := F1;
               Chessboard.Update_White_Piece (H1, F1);
               Chessboard.White_Castle_Kingside (History_Ply + 1) := False;
            when C1 => -- white queenside castle
               From := A1;
               To := D1;
               Chessboard.Update_White_Piece (A1, D1);
               Chessboard.White_Castle_Queenside (History_Ply + 1) := False;
            when G8 => -- black kingside castle
               From := H8;
               To := F8;
               Chessboard.Update_Black_Piece (H8, F8);
               Chessboard.Black_Castle_Kingside (History_Ply + 1) := False;
            when C8 => -- black queenside castle
               From := A8;
               To := D8;
               Chessboard.Update_Black_Piece (A8, D8);
               Chessboard.Black_Castle_Queenside (History_Ply + 1) := False;
            when others =>
               raise Invalid_Castle_Move;
         end case;
         Chessboard.Square (To) := Chessboard.Square (From);
         Chessboard.Square (From) := Empty;
         -- restore original from/to positions
         From := Move.From;
         To := Move.To;
      end if;

      -- Detect the en-passant square, if any
      if Flag = Pawn_Move_Two_Square then
         if Side_To_Move = White then
            Chessboard.En_Passant (History_Ply + 1) := To + South;
         else
            Chessboard.En_Passant (History_Ply + 1) := To + North;
         end if;
      end if;

      -- Remove the captured piece, if any
      if Captured /= Empty then
         if Side_To_Move = White then
            Chessboard.Delete_Black_Piece (To);
         else
            Chessboard.Delete_White_Piece (To);
         end if;

      elsif Flag = Capture_En_Passant then
         Chessboard.Fifty := 0;
         if Side_To_Move = White then
            Chessboard.Delete_Black_Piece (To + South);
            Chessboard.Square (To + South) := Empty;
         else
            Chessboard.Delete_White_Piece (To + North);
            Chessboard.Square (To + North) := Empty;
         end if;

      end if;

      -- Move the piece in the Chessboard.Square
      Chessboard.Square (To) := Chessboard.Square (From);
      Chessboard.Square (From) := Empty;

      -- Update the other data involved into the move
      if Side_To_Move = White then
         Chessboard.Update_White_Piece (From, To);

         if Chessboard.Square (To) = White_King then
            White_King_Position := To;
            Chessboard.White_Castle_Kingside (History_Ply + 1) := False;
            Chessboard.White_Castle_Queenside (History_Ply + 1) := False;
         end if;

         if Flag = Promotion  then
            if Chessboard.Square (To) in Black_Piece_Type then
               Chessboard.Delete_Black_Piece (To);
            end if;
            Chessboard.Square (To) := Promote;
         end if;

      else
         Chessboard.Update_Black_Piece (From, To);

         if Chessboard.Square (To) = Black_King then
            Black_King_Position := To;
            Chessboard.Black_Castle_Kingside (History_Ply + 1) := False;
            Chessboard.Black_Castle_Queenside (History_Ply + 1) := False;
         end if;

         if Flag = Promotion then
            if Chessboard.Square (To) in White_Piece_Type then
               Chessboard.Delete_White_Piece (To);
            end if;
            Chessboard.Square (To) := Promote;
         end if;
      end if;

      -------------------------
      -- Fifty moves counter --
      -------------------------

      if Piece in Pawn_Type or else Captured /= Empty then
         Chessboard.Fifty := 0;
      end if;

      -------------------
      -- Castle rights --
      -------------------

      if From = A1 or else To = A1 then
         Chessboard.White_Castle_Queenside (History_Ply + 1) := False;
      end if;
      if From = H1 or else To = H1 then
         Chessboard.White_Castle_Kingside (History_Ply + 1) := False;
      end if;
      if From = A8 or else To = A8 then
         Chessboard.Black_Castle_Queenside (History_Ply + 1) := False;
      end if;
      if From = H8 or else To = H8 then
         Chessboard.Black_Castle_Kingside (History_Ply + 1) := False;
      end if;

      -- Finally, set up the data for the next move
      Side_To_Move := not Side_To_Move;
      Chessboard.Moves_History (History_Ply) := History_Data;
      Ply := Ply + 1;
      History_Ply := History_Ply + 1;

      Chessboard.Update_Hash;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         Chess.IO.Print_Game (Chessboard);
         Ada.Text_IO.Put_Line ("Move just played: " & Chess.IO.Move_To_String (Move));
         raise;
   end Play;


   ----------
   -- Undo --
   ----------

   procedure Undo (Chessboard : in out Chessboard_Type) is
      From, To : Square_Type;
      Captured : Piece_Type;
      Flag                : Flag_Type;
      HMove               : History_Move_Type;
      Ply                 : Depth_Type renames Chessboard.Ply;
      Side_To_Move        : Color_Type renames Chessboard.Side_To_Move;
      White_King_Position : Square_Type renames Chessboard.White_King_Position;
      Black_King_Position : Square_Type renames Chessboard.Black_King_Position;
      History_Ply         : History_Depth_Type renames Chessboard.History_Ply;
      Fifty               : Fifty_Counter_Type renames Chessboard.Fifty;
   begin

      Ply := Ply - 1;
      History_Ply := History_Ply - 1;
      Hmove := Chessboard.Moves_History (History_Ply);
      Fifty := Hmove.Fifty;

      Side_To_Move := not Side_To_Move;

      From := Hmove.Move.From;
      To := Hmove.Move.To;
      Flag := Hmove.move.Flag;
      Captured := Hmove.move.Captured;
      Hash := Hmove.Hash;

      if Flag = Castle then
         case To is
            when G1 => -- white kingside castle
               From := F1;
               To := H1;
               Chessboard.Update_White_Piece (From, To);
            when C1 => -- white queenside castle
               From := D1;
               To := A1;
               Chessboard.Update_White_Piece (From, To);
            when G8 => -- black kingside castle
               From := F8;
               To := H8;
               Chessboard.Update_Black_Piece (From, To);
            when C8 => -- black queenside castle
               From := D8;
               To := A8;
               Chessboard.Update_Black_Piece (From, To);
            when others => raise Invalid_Castle_Move;
         end case;
         Chessboard.Square (To) := Chessboard.Square (From);
         Chessboard.Square (From) := Empty;

         From := Chessboard.Moves_History (History_Ply).Move.From;
         To := Chessboard.Moves_History (History_Ply).move.To;
      end if;


      if Side_To_Move = White then
         Chessboard.Update_White_Piece (From => To, To => From);
         if Chessboard.Square (To) = White_King then
            White_King_Position := From;
         end if;
      else
         Chessboard.Update_Black_Piece (From => To, To => From);
         if Chessboard.Square (To) = Black_King then
            Black_King_Position := From;
         end if;
      end if;

      Chessboard.Square (From) := Chessboard.Square (To);
      Chessboard.Square (To) := Captured;


      if Flag = Promotion then
         if Side_To_Move = White then
            Chessboard.Square (From) := White_Pawn;
         else
            Chessboard.Square (From) := Black_Pawn;
         end if;
      end if;

      if Captured /= Empty then -- in Chess_Piece_Type'Range then
         if Side_To_Move = White then
            Chessboard.Add_Black_Piece (To);
         else
            Chessboard.Add_White_Piece (To);
         end if;
      elsif Flag = Capture_En_Passant then
         if Side_To_Move = White then
            Chessboard.Add_Black_Piece (To + South);
            Chessboard.Square (To + South) := Black_Pawn;
         else
            Chessboard.Add_White_Piece (To + North);
            Chessboard.Square (To + North) := White_Pawn;
         end if;
      end if;

      Chessboard.Moves_History (History_Ply + 1) := Empty_History_Move;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         Chess.IO.Print_Game (Chessboard);
   end Undo;


   -------------------
   -- Play_See_Move --
   -------------------

   procedure Play_See_Move (Chessboard : in out Chessboard_Type; Move : in Move_Type) is
      From, To : Square_Type;
      Captured : constant Board_Piece_Type := Move.Captured;
      Promote  : constant Board_Piece_Type := Move.Promotion;
      Flag     : Flag_Type;
      Tmp      : History_Move_Type;
      Ply      : Depth_Type renames Chessboard.Ply;
      White_King_Position : Square_Type renames Chessboard.White_King_Position;
      Black_King_Position : Square_Type renames Chessboard.Black_King_Position;
      Side_To_Move        : Color_Type renames Chessboard.Side_To_Move;
      History_Ply         : History_Depth_Type renames Chessboard.History_Ply;
      Fifty               : Fifty_Counter_Type renames Chessboard.Fifty;
   begin

      -- copy move data into variables
      -- to make it faster development
      From := Move.From;
      To := Move.To;
      Flag := Move.Flag;

      Tmp := History_Move_Type'(Move => Move, Hash => Hash, Fifty => Fifty);

      -- prepare data for the next ply
      Chessboard.En_Passant (History_Ply + 1) := 0;

      -- Detect the en-passant square, if any
      if Flag = Pawn_Move_Two_Square then
         if Side_To_Move = White then
            Chessboard.En_Passant (History_Ply + 1) := To + South;
         else
            Chessboard.En_Passant (History_Ply + 1) := To + North;
         end if;
      end if;

      -- Remove the captured piece, if any
      if Captured in Chess_Piece_Type then
         if Side_To_Move = White then
            Chessboard.Delete_Black_Piece (To);
         else
            Chessboard.Delete_White_Piece (To);
         end if;
      end if;

      -- Move the piece in the ChessBoard
      Chessboard.Square (To) := Chessboard.Square (From);
      Chessboard.Square (From) := Empty;

      -- Update the other data involved into the move
      if Side_To_Move = White then
         Chessboard.Update_White_Piece (From, To);

         if Chessboard.Square (To) = White_King then
            White_King_Position := To;
         end if;

         if Flag = Promotion  then
            if Chessboard.Square (To) in Black_Piece_Type then
               Chessboard.Delete_Black_Piece (To);
            end if;
            Chessboard.Square (To) := Promote;
         end if;

      else
         Chessboard.Update_Black_Piece (From, To);

         if Chessboard.Square (To) = Black_King then
            Black_King_Position := To;
         end if;

         if Flag = Promotion then
            if Chessboard.Square (To) in White_Piece_Type then
               Chessboard.Delete_White_Piece (To);
            end if;
            Chessboard.Square (To) := Promote;
         end if;
      end if;

      -------------------
      -- Castle rights --
      -------------------

      -- Finally, set up the data for the next move
      Side_To_Move := not Side_To_Move;
      Chessboard.Moves_History (History_Ply) := Tmp;
      Ply := Ply + 1;
      History_Ply := History_Ply + 1;

   end Play_See_Move;


   -------------------
   -- Undo_See_Move --
   -------------------

   procedure Undo_See_Move (Chessboard : in out Chessboard_Type) is
      From, To : Square_Type;
      Captured : Piece_Type;
      Flag     : Flag_Type;
      HMove    : History_Move_Type;
      Ply      : Depth_Type renames Chessboard.Ply;
      White_King_Position : Square_Type renames Chessboard.White_King_Position;
      Black_King_Position : Square_Type renames Chessboard.Black_King_Position;
      Side_To_Move        : Color_Type renames Chessboard.Side_To_Move;
      History_Ply         : History_Depth_Type renames Chessboard.History_Ply;
   begin

      Ply := Ply - 1;
      History_Ply := History_Ply - 1;
      Hmove := Chessboard.Moves_History (History_Ply);

      Side_To_Move := not Side_To_Move;

      From := Hmove.move.From;
      To := Hmove.move.To;
      Flag := Hmove.move.Flag;
      Captured := Hmove.Move.Captured;

      if Side_To_Move = White then
         Chessboard.Update_White_Piece (From => To, To => From);
         if Chessboard.Square (To) = White_King then
            White_King_Position := From;
         end if;
      else
         Chessboard.Update_Black_Piece (From => To, To => From);
         if Chessboard.Square (To) = Black_King then
            Black_King_Position := From;
         end if;
      end if;

      Chessboard.Square (From) := Chessboard.Square (To);
      Chessboard.Square (To) := Captured;


      if Flag = Promotion then
         if Side_To_Move = White then
            Chessboard.Square (From) := White_Pawn;
         else
            Chessboard.Square (From) := Black_Pawn;
         end if;
      end if;

      if Side_To_Move = White then
         Chessboard.Add_Black_Piece (To);
      else
         Chessboard.Add_White_Piece (To);
      end if;

      Chessboard.Moves_History (History_Ply + 1) := Empty_History_Move;

   end Undo_See_Move;


   --------------------
   -- Play_Null_Move --
   --------------------

   procedure Play_Null_Move (Chessboard : in out Chessboard_Type) is
      Move : Move_Type := Chessboard.Moves_History (Chessboard.History_Ply - 1).Move;
      History_Data : History_Move_Type;

      Ply                 : Depth_Type renames Chessboard.Ply;
      Side_To_Move        : Color_Type renames Chessboard.Side_To_Move;
      History_Ply         : History_Depth_Type renames Chessboard.History_Ply;
      Fifty               : Fifty_Counter_Type renames Chessboard.Fifty;
   begin

      Move.Flag := Null_Move;

      History_Data := History_Move_Type'
        (Move => Move, Hash => Hash, Fifty => Fifty);

      Chessboard.Moves_History (History_Ply) := History_Data;
      Chessboard.En_Passant (History_Ply + 1) := 0;
      Chessboard.Fifty  := Chessboard.Fifty + 1;
      Chessboard.White_Castle_Kingside (History_Ply + 1) := Chessboard.White_Castle_Kingside (History_Ply);
      Chessboard.White_Castle_Queenside (History_Ply + 1) := Chessboard.White_Castle_Queenside (History_Ply);
      Chessboard.Black_Castle_Kingside (History_Ply + 1) := Chessboard.Black_Castle_Kingside (History_Ply);
      Chessboard.Black_Castle_Queenside (History_Ply + 1) := Chessboard.Black_Castle_Queenside (History_Ply);

      Side_To_Move := not Side_To_Move;

      Ply := Ply + 1;
      History_Ply := History_Ply + 1;

      Chessboard.Update_Hash;
   end Play_Null_Move;


   --------------------
   -- Undo_Null_Move --
   --------------------

   procedure Undo_Null_Move (CHessboard : in out Chessboard_Type) is
      Ply                 : Depth_Type renames Chessboard.Ply;
      Side_To_Move        : Color_Type renames Chessboard.Side_To_Move;
      History_Ply         : History_Depth_Type renames Chessboard.History_Ply;
   begin

      Ply := Ply - 1;
      History_Ply := History_Ply - 1;
      Chessboard.Fifty := Chessboard.Fifty - 1;

      Side_To_Move := not Side_To_Move;

      Chessboard.Moves_History (History_Ply + 1) := Empty_History_Move;
   end Undo_Null_Move;


   ---------------------
   -- Play_Check_Move --
   ---------------------

   procedure Play_Check_Move (Chessboard : in out Chessboard_Type; Move : in Move_Type) is
      From, To : Square_Type;
      Captured : constant Board_Piece_Type := Move.Captured;
      Promote  : constant Board_Piece_Type := Move.Promotion;
      Flag     : Flag_Type;
      Tmp      : History_Move_Type;
      Ply      : Depth_Type renames Chessboard.Ply;
      White_King_Position : Square_Type renames Chessboard.White_King_Position;
      Black_King_Position : Square_Type renames Chessboard.Black_King_Position;
      Side_To_Move        : Color_Type renames Chessboard.Side_To_Move;
      History_Ply         : History_Depth_Type renames Chessboard.History_Ply;
      Fifty               : Fifty_Counter_Type renames Chessboard.Fifty;
   begin

      -- copy move data into variables
      -- to make it faster development
      From := Move.From;
      To := Move.To;
      Flag := Move.Flag;

      Tmp := History_Move_Type'(Move => Move, Hash => Hash, Fifty => Fifty);


      -- prepare data for the next ply
      Chessboard.En_Passant (History_Ply + 1) := 0;

      if Flag = Castle then
         case To is
            when G1 => -- white kingside castle
               From := H1;
               To := F1;
               Chessboard.Update_White_Piece (From, To);
               Chessboard.White_Castle_Kingside (History_Ply + 1) := False;
            when C1 => -- white queenside castle
               From := A1;
               To := D1;
               Chessboard.Update_White_Piece (From, To);
               Chessboard.White_Castle_Queenside (History_Ply + 1) := False;
            when G8 => -- black kingside castle
               From := H8;
               To := F8;
               Chessboard.Update_Black_Piece (From, To);
               Chessboard.Black_Castle_Kingside (History_Ply + 1) := False;
            when C8 => -- black queenside castle
               From := A8;
               To := D8;
               Chessboard.Update_Black_Piece (From, To);
               Chessboard.Black_Castle_Queenside (History_Ply + 1) := False;
            when others => raise Invalid_Castle_Move;
         end case;
         Chessboard.Square (To) := Chessboard.Square (From);
         Chessboard.Square (From) := Empty;
         -- restore original from/to positions
         From := Move.From;
         To := Move.To;
      end if;

      -- Detect the en-passant square, if any
      if Flag = Pawn_Move_Two_Square then
         if Side_To_Move = White then
            Chessboard.En_Passant (History_Ply + 1) := To + South;
         else
            Chessboard.En_Passant (History_Ply + 1) := To + North;
         end if;
      end if;


      -- Remove the captured piece, if any
      if Captured /= Empty then
         if Side_To_Move = White then
            Chessboard.Delete_Black_Piece (To);
         else
            Chessboard.Delete_White_Piece (To);
         end if;

      elsif Flag = Capture_En_Passant then
         if Side_To_Move = White then
            Chessboard.Delete_Black_Piece (To + South);
            Chessboard.Square (To + South) := Empty;
         else
            Chessboard.Delete_White_Piece (To + North);
            Chessboard.Square (To + North) := Empty;
         end if;

      end if;

      -- Move the piece in the Chessboard.Square
      Chessboard.Square (To) := Chessboard.Square (From);
      Chessboard.Square (From) := Empty;

      -- Update the other data involved into the move
      if Side_To_Move = White then
         Chessboard.Update_White_Piece (From, To);

         if Chessboard.Square (To) = White_King then
            White_King_Position := To;
         end if;

         if Flag = Promotion  then
            if Chessboard.Square (To) in Black_Piece_Type then
               Chessboard.Delete_Black_Piece (To);
            end if;
            Chessboard.Square (To) := Promote;
         end if;

      else
         Chessboard.Update_Black_Piece (From, To);

         if Chessboard.Square (To) = Black_King then
            Black_King_Position := To;
         end if;

         if Flag = Promotion then
            if Chessboard.Square (To) in White_Piece_Type then
               Chessboard.Delete_White_Piece (To);
            end if;
            Chessboard.Square (To) := Promote;
         end if;
      end if;

      -- Finally, set up the data for the next move
      Side_To_Move := not Side_To_Move;
      Chessboard.Moves_History (History_Ply) := Tmp;
      Ply := Ply + 1;
      History_Ply := History_Ply + 1;

   end Play_Check_Move;


   ---------------------
   -- Undo_Check_Move --
   ---------------------

   procedure Undo_Check_Move (Chessboard : in out Chessboard_Type) is
      From, To : Square_Type;
      Captured : Piece_Type;
      Flag     : Flag_Type;
      HMove    : History_Move_Type;
      Ply      : Depth_Type renames Chessboard.Ply;
      White_King_Position : Square_Type renames Chessboard.White_King_Position;
      Black_King_Position : Square_Type renames Chessboard.Black_King_Position;
      Side_To_Move        : Color_Type renames Chessboard.Side_To_Move;
      History_Ply         : History_Depth_Type renames Chessboard.History_Ply;
   begin

      Ply := Ply - 1;
      History_Ply := History_Ply - 1;
      Hmove := Chessboard.Moves_History (History_Ply);

      Side_To_Move := not Side_To_Move;

      From := Hmove.Move.From;
      To := Hmove.Move.To;
      Flag := Hmove.Move.Flag;
      Captured := Hmove.move.Captured;

      if Flag = Castle then
         case To is
            when G1 => -- white kingside castle
               From := F1;
               To := H1;
               Chessboard.Update_White_Piece (From, To);
            when C1 => -- white queenside castle
               From := D1;
               To := A1;
               Chessboard.Update_White_Piece (From, To);
            when G8 => -- black kingside castle
               From := F8;
               To := H8;
               Chessboard.Update_Black_Piece (From, To);
            when C8 => -- black queenside castle
               From := D8;
               To := A8;
               Chessboard.Update_Black_Piece (From, To);
            when others => raise Invalid_Castle_Move;
         end case;
         Chessboard.Square (To) := Chessboard.Square (From);
         Chessboard.Square (From) := Empty;

         From := Chessboard.Moves_History (History_Ply).move.From;
         To := Chessboard.Moves_History (History_Ply).move.To;
      end if;

      if Side_To_Move = White then
         Chessboard.Update_White_Piece (From => To, To => From);
         if Chessboard.Square (To) = White_King then
            White_King_Position := From;
         end if;
      else
         Chessboard.Update_Black_Piece (From => To, To => From);
         if Chessboard.Square (To) = Black_King then
            Black_King_Position := From;
         end if;
      end if;

      Chessboard.Square (From) := Chessboard.Square (To);
      Chessboard.Square (To) := Captured;

      if Flag = Promotion then
         if Side_To_Move = White then
            Chessboard.Square (From) := White_Pawn;
         else
            Chessboard.Square (From) := Black_Pawn;
         end if;
      end if;

      if Captured /= Empty then -- in Chess_Piece_Type'Range then
         if Side_To_Move = White then
            Chessboard.Add_Black_Piece (To);
         else
            Chessboard.Add_White_Piece (To);
         end if;
      elsif Flag = Capture_En_Passant then
         if Side_To_Move = White then
            Chessboard.Add_Black_Piece (To + South);
            Chessboard.Square (To + South) := Black_Pawn;
         else
            Chessboard.Add_White_Piece (To + North);
            Chessboard.Square (To + North) := White_Pawn;
         end if;
      end if;

      Chessboard.Moves_History (History_Ply + 1) := Empty_History_Move;

   end Undo_Check_Move;


   --------------------
   -- Last_Move_Made --
   --------------------

   function Last_Move_Made (Chessboard : in Chessboard_Type) return Move_Type is
      History_Ply : History_Depth_Type renames Chessboard.History_Ply;
      Unknown_Move : Move_Type := Empty_Move;
   begin
      if Chessboard.History_Ply = Zero_Depth then
         -- If no move has been played yet, we can be either in the initial
         -- position or in a new FEN position just loaded. In this case, the
         -- FEN can start with the king under check and we need to detect it.
         -- Note that it is possible to further investigate for double check and
         -- for checkmates, but there's no way to distinguish direct check
         -- to discovery check; therefore we can't get rid of the Unknown_Check.
         -- This further investigation will be performed while generating
         -- evasions, if needed
         if Chessboard.Has_King_In_Check (Chessboard.Side_To_Move) then
            Unknown_Move.Check := Unknown_Check;
         end if;

         return Unknown_Move;
      else
         return Chessboard.Moves_History (History_Ply - 1).Move;
      end if;
   end Last_Move_Made;


   ---------------------
   -- Mirror_Position --
   ---------------------

   procedure Mirror_Position (Chessboard : in out Chessboard_Type) is
      Piece                  : Piece_Type;
      Mirrored_Piece         : Piece_Type;

      Ply                    : Depth_Type renames Chessboard.Ply;

      White_Kingside_Castle  : Boolean;
      White_Queenside_Castle : Boolean;
      Black_Kingside_Castle  : Boolean;
      Black_Queenside_Castle : Boolean;

      Mirrored_Board         : constant array (Square_Type'Range) of Square_Type
        := (0,  0,  0,  0,  0,  0,  0,  0,  0, 0,
            0,  0,  0,  0,  0,  0,  0,  0,  0, 0,
            0, A1, B1, C1, D1, E1, F1, G1, H1, 0,
            0, A2, B2, C2, D2, E2, F2, G2, H2, 0,
            0, A3, B3, C3, D3, E3, F3, G3, H3, 0,
            0, A4, B4, C4, D4, E4, F4, G4, H4, 0,
            0, A5, B5, C5, D5, E5, F5, G5, H5, 0,
            0, A6, B6, C6, D6, E6, F6, G6, H6, 0,
            0, A7, B7, C7, D7, E7, F7, G7, H7, 0,
            0, A8, B8, C8, D8, E8, F8, G8, H8, 0,
            0, 0,  0,  0,  0,  0,  0,   0,  0, 0,
            0, 0,  0,  0,  0,  0,  0,   0,  0, 0);

      Mirrored_Square        : Square_Type;
      Current_Board          : Board_Type;

   begin

      -------------------------------------------------
      -- Copy the current board into a temporary one --
      -------------------------------------------------

      for I in Square_Type'Range loop
         Current_Board (I) := Chessboard.Square (I);
      end loop;

      ------------------------------
      -- Reset current chessboard --
      ------------------------------

      Chessboard.Square := (others => Frame);

      Chessboard.Pieces_List := (others => 0);
      Chessboard.Piece_Table := (others => 0);
      Chessboard.White_Pieces_Counter := 0;
      Chessboard.Black_Pieces_Counter := 0;

      ---------------------------
      -- Mirror the Chessboars --
      ---------------------------

      for I in Square_Type'Range loop
         Piece := Current_Board (I);
         case Piece is
            when Frame        => Mirrored_Piece := Frame;
            when Empty        => Mirrored_Piece := Empty;
            when White_Pawn   => Mirrored_Piece := Black_Pawn;
            when White_Knight => Mirrored_Piece := Black_Knight;
            when White_Bishop => Mirrored_Piece := Black_Bishop;
            when White_Rook   => Mirrored_Piece := Black_Rook;
            when White_Queen  => Mirrored_Piece := Black_Queen;
            when White_King   => Mirrored_Piece := Black_King;
            when Black_Pawn   => Mirrored_Piece := White_Pawn;
            when Black_Knight => Mirrored_Piece := White_Knight;
            when Black_Bishop => Mirrored_Piece := White_Bishop;
            when Black_Rook   => Mirrored_Piece := White_Rook;
            when Black_Queen  => Mirrored_Piece := White_Queen;
            when Black_King   => Mirrored_Piece := White_King;
         end case;

         Mirrored_Square := Mirrored_Board (I);
         Chessboard.Square (Mirrored_Square) := Mirrored_Piece;

         if Mirrored_Piece in White_Piece_Type then
            Chessboard.Add_White_Piece (Mirrored_Square);
            if Mirrored_Piece = White_King then
               Chessboard.White_King_Position := Mirrored_Square;
            end if;
         elsif Mirrored_Piece in Black_Piece_Type then
            Chessboard.Add_Black_Piece (Mirrored_Square);
            if Mirrored_Piece = Black_King then
               Chessboard.Black_King_Position := Mirrored_Square;
            end if;
         end if;


      end loop;

      -----------------------
      -- Swap side to move --
      -----------------------

      Chessboard.Side_To_Move := not Chessboard.Side_To_Move;

      ------------------------
      -- Swap castle rights --
      ------------------------

      White_Kingside_Castle  := Chessboard.White_Castle_Kingside (Ply);
      White_Queenside_Castle := Chessboard.White_Castle_Queenside (Ply);
      Black_Kingside_Castle  := Chessboard.Black_Castle_Kingside (Ply);
      Black_Queenside_Castle := Chessboard.Black_Castle_Queenside (Ply);

      Chessboard.White_Castle_Kingside (Ply)  := Black_Kingside_Castle;
      Chessboard.White_Castle_Queenside (Ply) := Black_Queenside_Castle;
      Chessboard.Black_Castle_Kingside (Ply)  := White_Kingside_Castle;
      Chessboard.Black_Castle_Queenside (Ply) := White_Queenside_Castle;

      -- Note: in mirroring the board the en-passant square remains the same
      -- even though some other programs prefer to reset the en-passant square.
      -- AdaChess uses the mirroring mainly for testing functionality and for
      -- finding asymmetric value in the evaluation function, therefore the
      -- en-passant square is kept as is.

      -------------------------
      -- Reset moves counter --
      -------------------------

      Chessboard.Moves_History := (others => Empty_History_Move);

      Chessboard.Initialize_Hash;

   end Mirror_Position;


   ---------------------------
   -- Find_Sliding_Direction--
   ---------------------------

   function Find_Sliding_Direction (Origin, Destination : in Square_Type) return Direction_Type is
   begin
      return Directions_Table (Origin, Destination);
   end Find_Sliding_Direction;


   -------------------------------
   -- Preload_Sliding_Direction --
   --------------------------------

   procedure Preload_Sliding_Direction is

      function Get_Direction (Origin, Destination : in Square_Type) return Direction_Type is
         Direction : Direction_Type := No_Direction;
      begin

         if File (Origin) = File (Destination) then
            if Origin < Destination then
               Direction := North;
            else
               Direction := South;
            end if;

         elsif Rank (Origin) = Rank (Destination) then
            if Origin < Destination then
               Direction := West;
            else
               Direction := East;
            end if;

         elsif Diagonal (Origin) = Diagonal (Destination)  then
            if Origin < Destination then
               Direction := North_East;
            else
               Direction := South_West;
            end if;

         elsif Anti_Diagonal (Origin) = Anti_Diagonal (Destination) then
            if Origin < Destination then
               Direction := North_West;
            else
               Direction := South_East;
            end if;

         end if;

         return Direction;
      end Get_Direction;

   begin
      for Origin in Board_Type'Range loop
         for Destination in Board_Type'Range loop
            Directions_Table (Origin, Destination) := Get_Direction (Origin, Destination);
         end loop;
      end loop;
   end Preload_Sliding_Direction;


   -------------
   -- Attacks --
   -------------

   function Attacks (Chessboard : in Chessboard_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Target : Square_Type;
   begin

      if Side = Black then

         -- look for bishop/queen/king attacks
         for Offset of Bishop_Offsets loop
            Target := Square + Offset;
            if Chessboard.Square (Target) = Black_King then
               return True;
            end if;
            while Chessboard.Square (Target) = Empty loop
               Target := Target + Offset;
            end loop;
            if Chessboard.Square (Target) = Black_Queen
              or else Chessboard.Square (Target) = Black_Bishop then
               return true;
            end if;
         end loop;
         -- look for rook/queen/king attacks
         for Offset of Rook_Offsets loop
            Target := Square + Offset;
            if Chessboard.Square (Target) = Black_King then
               return True;
            end if;
            while Chessboard.Square (Target) = Empty loop
               Target := Target + Offset;
            end loop;
            if Chessboard.Square (Target) = Black_Queen
              or else Chessboard.Square (Target) = Black_Rook then
               return true;
            end if;
         end loop;
         -- look for knight attacks
         for Offset of Knight_Offsets loop
            Target := Square + Offset;
            if Chessboard.Square (Target) = Black_Knight then
               return True;
            end if;
         end loop;
         -- look for pawn attacks
         if Chessboard.Square (Square + North_West) = Black_Pawn then
            return True;
         end if;
         if Chessboard.Square (Square + North_East) = Black_Pawn then
            return True;
         end if;

      else -- Side = White

         -- look for bishop/queen/king attacks
         for Offset of Bishop_Offsets loop
            Target := Square + Offset;
            if Chessboard.Square (Target) = White_King then
               return True;
            end if;
            while Chessboard.Square (Target) = Empty loop
               Target := Target + Offset;
            end loop;
            if Chessboard.Square (Target) = White_Queen
              or else Chessboard.Square (Target) = White_Bishop then
               return True;
            end if;
         end loop;
         -- look for rook/queen/king attacks
         for Offset of Rook_Offsets loop
            Target := Square + Offset;
            if Chessboard.Square (Target) = White_King then
               return true;
            end if;
            while Chessboard.Square (Target) = Empty loop
               Target := Target + Offset;
            end loop;
            if Chessboard.Square (Target) = White_Queen
              or else Chessboard.Square (Target) = White_Rook then
               return true;
            end if;
         end loop;
         -- look for knight attacks
         for Offset of Knight_Offsets loop
            Target := Square + Offset;
            if Chessboard.Square (Target) = White_Knight then
               return true;
            end if;
         end loop;
         -- look for pawn attacks
         if Chessboard.Square (Square + South_East) = White_Pawn then
            return true;
         end if;
         if Chessboard.Square (Square + South_West) = White_Pawn then
            return True;
         end if;

      end if;

      return False;
   end Attacks;


   ----------------------
   -- Attacking_Square --
   ----------------------

   function Attacking_Square (Chessboard : in Chessboard_Type; Side : in Color_Type; Square : in Square_Type; Only_One : in Boolean) return Attack_Collection_Type is
      Target : Square_Type;
      Atk    : Attack_Collection_Type := Attack_Collection_Type'(Attacker => (others => (0, Empty)),
                                                                 Number_Of_Attackers => 0);

      procedure Register_Attack (Origin : in Square_Type) is
      begin
         Atk.Number_Of_Attackers := Atk.Number_Of_Attackers + 1;
         Atk.Attacker (Atk.Number_Of_Attackers).Origin := Origin;
         Atk.Attacker (Atk.Number_Of_Attackers).Piece := Chessboard.Square (Origin);
      end Register_Attack;

   begin

      if Side = Black then

         -- look for bishop/queen/king attacks
         for Offset of Bishop_Offsets loop
            Target := Square + Offset;
            if Chessboard.Square (Target) = Black_King then
               Register_Attack (Target);
               if Only_One then
                  return Atk;
               end if;
            end if;
            while Chessboard.Square (Target) = Empty loop
               Target := Target + Offset;
            end loop;
            if Chessboard.Square (Target) in Black_Queen | Black_Bishop then
               Register_Attack (Target);
               if Only_One then
                  return Atk;
               end if;
            end if;
         end loop;

         -- look for rook/queen/king attacks
         for offset of Rook_Offsets loop
            Target := Square + Offset;
            if Chessboard.Square (Target) = Black_King then
               Register_Attack (Target);
               if Only_One then
                  return Atk;
               end if;
            end if;
            while Chessboard.Square (Target) = Empty loop
               Target := Target + Offset;
            end loop;
            if Chessboard.Square (Target) in Black_Queen | Black_Rook then
               Register_Attack (Target);
               if Only_One then
                  return Atk;
               end if;
            end if;
         end loop;
         -- look for knight attacks
         for Offset of Knight_Offsets loop
            Target := Square + Offset;
            if Chessboard.Square (Target) = Black_Knight then
               Register_Attack (Target);
               if Only_One then
                  return Atk;
               end if;
            end if;
         end loop;
         -- look for pawn attacks
         Target := Square + North_West;
         if Chessboard.Square (Target) = Black_Pawn then
            Register_Attack (Target);
            if Only_One then
               return Atk;
            end if;
         end if;
         Target := Square + North_East;
         if Chessboard.Square (Target) = Black_Pawn then
            Register_Attack (Target);
            if Only_One then
               return Atk;
            end if;
         end if;

      else -- Side = White

         -- look for bishop/queen/king attacks
         for Offset of Bishop_Offsets loop
            Target := Square + Offset;
            if Chessboard.Square (Target) = White_King then
               Register_Attack (Target);
               if Only_One then
                  return Atk;
               end if;
            end if;
            while Chessboard.Square (Target) = Empty loop
               Target := Target + Offset;
            end loop;
            if Chessboard.Square (Target) in White_Queen | White_Bishop then
               Register_Attack (Target);
               if Only_One then
                  return Atk;
               end if;
            end if;
         end loop;
         -- look for rook/queen/king attacks
         for Offset of Rook_Offsets loop
            Target := Square + Offset;
            if Chessboard.Square (Target) = White_King then
               Register_Attack (Target);
               if Only_One then
                  return Atk;
               end if;
            end if;
            while Chessboard.Square (Target) = Empty loop
               Target := Target + Offset;
            end loop;
            if Chessboard.Square (Target) in White_Queen | White_Rook then
               Register_Attack (Target);
               if Only_One then
                  return Atk;
               end if;
            end if;
         end loop;
         -- look for knight attacks
         for Offset of Knight_Offsets loop
            Target := Square + Offset;
            if Chessboard.Square (Target) = White_Knight then
               Register_Attack (Target);
               if Only_One then
                  return Atk;
               end if;
            end if;
         end loop;
         -- look for pawn attacks
         Target := Square + South_East;
         if Chessboard.Square (Target) = White_Pawn then
            Register_Attack (Target);
            if Only_One then
               return Atk;
            end if;
         end if;
         Target := Square + South_West;
         if Chessboard.Square (Target) = White_Pawn then
            Register_Attack (Target);
            if Only_One then
               return Atk;
            end if;
         end if;

      end if;

      return Atk;
   end Attacking_Square;


   ----------------------
   -- Defending_Square --
   ----------------------

   function Defending_Square (Chessboard : in Chessboard_Type; Side : in Color_Type; Square : in Square_Type) return Attack_Collection_Type is
      Attacking_Piece : Attack_Collection_Type;
      Piece        : Piece_Type;
      Origin          : Square_Type;

      Atk              : Attack_Collection_Type := Attack_Collection_Type'
        (Attacker => (others => (0, Empty)),
         Number_Of_Attackers => 0);
   begin

      Attacking_Piece := Attacking_Square
        (Chessboard => Chessboard,
         Side       => Side,
         Square     => Square,
         Only_One   => False);

      for I in 1 .. Attacking_Piece.Number_Of_Attackers loop
         Piece := Attacking_Piece.Attacker (I).Piece;
         Origin := Attacking_Piece.Attacker (I).Origin;
         if Piece in King_Type or else Piece_Is_Absolute_Pinned (Chessboard, Origin) = No_Direction then
            Atk.Number_Of_Attackers := Atk.Number_Of_Attackers + 1;
            Atk.Attacker (Atk.Number_Of_Attackers).Origin := Origin;
            Atk.Attacker (Atk.Number_Of_Attackers).Piece := Piece;
         end if;
      end loop;

      return Atk;
   end Defending_Square;


   -----------------------
   -- Has_King_In_Check --
   -----------------------

   function Has_King_In_Check (Chessboard : in Chessboard_Type; Side : in Color_Type) return Boolean is
      White_King_Position : Square_Type renames Chessboard.White_King_Position;
      Black_King_Position : Square_Type renames Chessboard.Black_King_Position;
   begin
      if Side = White then
         return Chessboard.Attacks (Side => Black, Square => White_King_Position);
      else
         return Chessboard.Attacks (Side => White, Square => Black_King_Position);
      end if;
   end Has_King_In_Check;


   -----------------------------
   -- White_Has_King_In_Check --
   -----------------------------

   function White_Has_King_In_Check (Chessboard : in out Chessboard_Type) return Boolean is
      White_King_Position : Square_Type renames Chessboard.White_King_Position;
   begin
      return Chessboard.Attacks (Side => Black, Square => White_King_Position);
   end White_Has_King_In_Check;


   -----------------------------
   -- Black_Has_King_In_Check --
   -----------------------------

   function Black_Has_King_In_Check (Chessboard : in out Chessboard_Type) return Boolean is
      Black_King_Position : Square_Type renames Chessboard.Black_King_Position;
   begin
      return Chessboard.Attacks (Side => White, Square => Black_King_Position);
   end Black_Has_King_In_Check;



   -----------
   -- Reset --
   -----------

   procedure Reset (Chessboard : in out Chessboard_Type) is
   begin
      Chessboard.Side_To_Move := White;

      Chessboard.Square :=
        (Frame, Frame, Frame, Frame, Frame, Frame, Frame, Frame, Frame, Frame,
         Frame, Frame, Frame, Frame, Frame, Frame, Frame, Frame, Frame, Frame,
         Frame, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Frame,
         Frame, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Frame,
         Frame, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Frame,
         Frame, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Frame,
         Frame, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Frame,
         Frame, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Frame,
         Frame, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Frame,
         Frame, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Frame,
         Frame, Frame, Frame, Frame, Frame, Frame, Frame, Frame, Frame, Frame,
         Frame, Frame, Frame, Frame, Frame, Frame, Frame, Frame, Frame, Frame);

      Chessboard.Pieces_List := (others => 0);
      Chessboard.Piece_Table := (others => 0);

      Chessboard.White_Pieces_Counter := 0;
      Chessboard.Black_Pieces_Counter := 0;

      Chessboard.White_King_Position := 0;
      Chessboard.Black_King_Position := 0;

      Chessboard.White_Castle_Queenside := (others => False);
      Chessboard.White_Castle_Kingside := (others => False);
      Chessboard.Black_Castle_Queenside := (others => False);
      Chessboard.Black_Castle_Kingside := (others => False);

      Chessboard.Moves_Pointer := (others => 1);
      Chessboard.Moves_Stack := (others => Empty_Move);

      Chessboard.Ply := Zero_Depth;
      Chessboard.History_Ply := Zero_Depth;
      Chessboard.Moves_History := (others => Empty_History_Move);

      Chessboard.En_Passant := (others => 0);
      Chessboard.Fifty := 0;

      -- Set up all the Attacks_To functions:
      Attacks_To := (others => Attacks_From_Placeholder'Access);
      Attacks_To (North) := Attacks_From_North'Access;
      Attacks_To (South) := Attacks_From_South'Access;
      Attacks_To (East) := Attacks_From_East'Access;
      Attacks_To (West) := Attacks_From_West'Access;
      Attacks_To (North_East) := Attacks_From_North_East'Access;
      Attacks_To (North_West) := Attacks_From_North_West'Access;
      Attacks_To (South_East) := Attacks_From_South_East'Access;
      Attacks_To (South_West) := Attacks_From_South_West'Access;
      Attacks_To (North_North_East) := Attacks_From_North_North_East'Access;
      Attacks_To (North_East_East) := Attacks_From_North_East_East'Access;
      Attacks_To (South_South_East) := Attacks_From_South_South_East'Access;
      Attacks_To (South_East_East) := Attacks_From_South_East_East'Access;
      Attacks_To (North_North_West) := Attacks_From_North_North_West'Access;
      Attacks_To (North_West_West) := Attacks_From_North_West_West'Access;
      Attacks_To (South_South_West) := Attacks_From_South_South_West'Access;
      Attacks_To (South_West_West) := Attacks_From_South_West_West'Access;

      Hash := 0;

   end Reset;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Chessboard : in out Chessboard_Type) is
   begin

      Chessboard.Reset;

      Chessboard.Square :=
        (Frame, Frame, Frame, Frame,   Frame,   Frame,  Frame, Frame,   Frame,   Frame,
         Frame, Frame,   Frame,   Frame,  Frame, Frame,   Frame,   Frame, Frame, Frame,
         Frame, Black_Rook, Black_Knight, Black_Bishop, Black_Queen, Black_King, Black_Bishop, Black_Knight, Black_Rook, Frame,
         Frame, Black_Pawn, Black_Pawn, Black_Pawn, Black_Pawn, Black_Pawn, Black_Pawn, Black_Pawn, Black_Pawn, Frame,
         Frame, Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty, Frame,
         Frame, Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty, Frame,
         Frame, Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty, Frame,
         Frame, Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty, Frame,
         Frame, White_Pawn, White_Pawn, White_Pawn, White_Pawn, White_Pawn, White_Pawn, White_Pawn, White_Pawn, Frame,
         Frame, White_Rook, White_Knight, White_Bishop, White_Queen, White_King, White_Bishop, White_Knight, White_Rook, Frame,
         Frame, Frame,   Frame,   Frame,  Frame, Frame,   Frame,   Frame, Frame, Frame,
         Frame, Frame,   Frame,   Frame,  Frame, Frame,   Frame,   Frame, Frame, Frame);

      -- Initialize the array of pieces for both sides

      Chessboard.Add_White_Piece (A1);
      Chessboard.Add_White_Piece (B1);
      Chessboard.Add_White_Piece (C1);
      Chessboard.Add_White_Piece (D1);
      Chessboard.Add_White_Piece (E1);
      Chessboard.Add_White_Piece (F1);
      Chessboard.Add_White_Piece (G1);
      Chessboard.Add_White_Piece (H1);
      Chessboard.Add_White_Piece (A2);
      Chessboard.Add_White_Piece (B2);
      Chessboard.Add_White_Piece (C2);
      Chessboard.Add_White_Piece (D2);
      Chessboard.Add_White_Piece (E2);
      Chessboard.Add_White_Piece (F2);
      Chessboard.Add_White_Piece (G2);
      Chessboard.Add_White_Piece (H2);
      -- Black
      Chessboard.Add_Black_Piece (A8);
      Chessboard.Add_Black_Piece (B8);
      Chessboard.Add_Black_Piece (C8);
      Chessboard.Add_Black_Piece (D8);
      Chessboard.Add_Black_Piece (E8);
      Chessboard.Add_Black_Piece (F8);
      Chessboard.Add_Black_Piece (G8);
      Chessboard.Add_Black_Piece (H8);
      Chessboard.Add_Black_Piece (A7);
      Chessboard.Add_Black_Piece (B7);
      Chessboard.Add_Black_Piece (C7);
      Chessboard.Add_Black_Piece (D7);
      Chessboard.Add_Black_Piece (E7);
      Chessboard.Add_Black_Piece (F7);
      Chessboard.Add_Black_Piece (G7);
      Chessboard.Add_Black_Piece (H7);

      Chessboard.White_King_Position := E1;
      Chessboard.Black_King_Position := E8;

      Chessboard.White_Castle_Queenside := (others => True);
      Chessboard.White_Castle_Kingside := (others => True);
      Chessboard.Black_Castle_Queenside := (others => True);
      Chessboard.Black_Castle_Kingside := (others => True);

      Chessboard.Side_To_Move := White;

      Search_Nodes := 0;

      Chessboard.Initialize_Hash;

   end Initialize;


   ------------------------
   -- Display_On_Console --
   ------------------------

   --  procedure Display_On_Console (Chessboard : in Chessboard_Type) is
   --     Row     : Coordinate_Type := 8;
   --     Piece   : Piece_Type;
   --  begin
   --
   --     --        Display_Piece_Table;
   --
   --     for I in Board_Type'Range loop
   --        Piece := Chessboard.Square (I);
   --        if Piece /= Frame then
   --           if Piece =  Empty then
   --              Put ('.');
   --           else
   --              Put (Symbols (Piece));
   --           end if;
   --        end if;
   --        Put (" ");
   --        case I is
   --           when 9 | 19 | 29 | 39 | 49 | 59 | 69 | 79 | 89 | 99 | 109 =>
   --              if I in 20 .. 99 then
   --                 Ada.Text_IO.Put (" " & Coordinate_Type'Image (Row));
   --                 Row := Row - 1;
   --              end if;
   --              New_Line;
   --           when others => null;
   --        end case;
   --     end loop;
   --     New_Line;
   --     Put_Line (ASCII.CR & "  a b c d e f g h");
   --     New_Line;
   --     if Chessboard.En_Passant (Chessboard.History_Ply) /= 0 then
   --        Put_Line ("En passant: " & Pc_Sqr (Chessboard.En_Passant (Chessboard.History_Ply)));
   --        New_Line;
   --     end if;
   --     if Chessboard.Attacks (White, Chessboard.Black_King_Position) then
   --        Put_Line ("Black has king in check");
   --     end if;
   --     if Chessboard.Attacks (Black, Chessboard.White_King_Position) then
   --        Put_Line ("White has king in check");
   --     end if;
   --     New_Line;
   --  end Display_On_Console;


   -------------------------
   -- Display_Piece_Table --
   -------------------------

   procedure Display_Piece_Table (Chessboard : in out Chessboard_Type) is
      Row     : Coordinate_Type := 8;
      Piece   : Piece_Type;
      Piece_Table   : Piece_Table_Type renames Chessboard.Piece_Table;
      White_Pieces  : Pieces_List_Type renames Chessboard.Pieces_List (White_Pieces_Range);
      Black_Pieces  : Pieces_List_Type renames Chessboard.Pieces_List (Black_Pieces_Range);
   begin
      New_Line;

      Put ("White pieces: ");
      for Square of White_Pieces loop
         exit when Square = 0;
         Piece := Chessboard.Square (Square);
         Put (Symbols (Piece) & Pc_Sqr (Square));
         Put (" ");
      end loop;
      New_Line;

      Put ("Black pieces: ");
      for Square of Black_Pieces loop
         exit when Square = 0;
         Piece := Chessboard.Square (Square);
         Put (Symbols (Piece) & Pc_Sqr (Square));
         Put (" ");
      end loop;
      New_Line (2);

      for I in Board_Type'Range loop
         Piece := Chessboard.Square (I);
         if Piece /= Frame then
            if Piece_Table (I) = 0 and then Piece_Table (I) = 0 then
               Put (" .");
            else
               if Chessboard.Square (I) in White_Piece_Type then
                  if Piece_Table (I) < 10 then
                     Put (" ");
                  end if;
                  Put (Piece_Table (I), 0);
               else
                  if Piece_Table (I) < 10 then
                     Put (" ");
                  end if;
                  Put (Piece_Table (I), 0);
               end if;
            end if;
         end if;
         Put (" ");
         case I is
            when 9 | 19 | 29 | 39 | 49 | 59 | 69 | 79 | 89 | 99 | 109 =>
               if I in 20 .. 109 then
                  Put (Row, 0);
                  Row := Row - 1;
               end if;
               New_Line;
            when others => null;
         end case;
      end loop;
      New_Line;
      Put_Line (Ascii.Cr & "   a  b  c  d  e  f  g  h");
      New_Line;
   end Display_Piece_Table;


   ----------------------
   -- Print_Moves_List --
   ----------------------

   procedure Print_Moves_List (Chessboard : in out Chessboard_Type) is
      Move : Move_Type;
      Ply  : Depth_Type renames Chessboard.Ply;
   begin
      for I in Chessboard.Moves_Pointer (Ply) .. Chessboard.Moves_Pointer (Ply + 1) - 1 loop
         Move := Chessboard.Moves_Stack (I);
         Print_Move (Move);
         Ada.Text_IO.Put (" ");
      end loop;
      New_Line;
   end Print_Moves_List;


   ----------------
   -- Parse_Move --
   ----------------

   function Parse_Move (Chessboard : in out Chessboard_Type; Input : in String) return Move_Type is
      Move     : Move_Type := Empty_Move;
      Ply      : Depth_Type renames Chessboard.Ply;
   begin

      -- Always generate all moves first.
      Chessboard.Generate_Moves;

      for I in Chessboard.Moves_Pointer (Ply) .. Chessboard.Moves_Pointer (Ply + 1) - 1 loop
         Move := Chessboard.Moves_Stack (I);
         for Notation in Notation_Type'Range loop
            if To_Lower (Move_To_String (Move, Notation)) = To_Lower (Input) then
               return Move;
            end if;
         end loop;
      end loop;

      return Empty_Move;

   end Parse_Move;


end Chess.Engine;
