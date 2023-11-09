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
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;

with Ada.Calendar;

with Chess.Pieces; use Chess.Pieces;
with Chess.Moves; use Chess.Moves;
with Chess.Notations; use Chess.Notations;

with Chess.IO; use Chess.IO;

package body Chess.Engine.Perfts is

   -----------
   -- Perft --
   -----------

   procedure Perft (Chessboard : in out Chessboard_Type; Depth : in Depth_Type) is

      package Perft_Node_IO is new Ada.Text_IO.Modular_IO (Perft_Node_Type);

      ------------------
      -- Perft_Search --
      ------------------

      procedure Perft_Search (Depth : in Depth_Type) is
         Move : Move_Type;
         Ply  : Depth_Type renames Chessboard.Ply;
      begin
         Chessboard.Generate_Moves;

         for I in Chessboard.Moves_Pointer (Ply) .. Chessboard.Moves_Pointer (Ply + 1) - 1 loop
            Move := Chessboard.Moves_Stack (I);

            if Depth = 1 then

               -- Update stats about captures
               if Move.Captured /= Empty then
                  Perft_Move_Paths.Captures := Perft_Move_Paths.Captures + 1;
               end if;

               -- Update stats about En_Passant
               if Move.Flag = Capture_En_Passant then
                  Perft_Move_Paths.En_Passant := Perft_Move_Paths.En_Passant + 1;
                  Perft_Move_Paths.Captures := Perft_Move_Paths.Captures + 1;
               end if;

               -- Update stats about Castles
               if Move.Flag = Castle then
                  Perft_Move_Paths.Castles := Perft_Move_Paths.Castles + 1;
               end if;

               -- Update stats about Promotions
               if Move.Promotion /= Empty then
                  Perft_Move_Paths.Promotions := Perft_Move_Paths.Promotions + 1;
               end if;

               -- Update stats about Checks/Checkmates
               if Move.Check /= No_Check then
                  Perft_Move_Paths.Checks := Perft_Move_Paths.Checks + 1;
                  if Move.Check = Discovery_Check then
                     Perft_Move_Paths.Discovery_Checks := Perft_Move_Paths.Discovery_Checks + 1;
                  end if;
                  if Move.Check = Double_Check then
                     Perft_Move_Paths.Double_Checks := Perft_Move_Paths.Double_Checks + 1;
                  end if;
                  if Move.Check = Checkmate then
                     Perft_Move_Paths.Checkmates := Perft_Move_Paths.Checkmates + 1;
                  end if;
               end if;

               Perft_Move_Paths.Nodes := Perft_Move_Paths.Nodes + 1;
            else
               Chessboard.Play (Move);
               Perft_Search (Depth => Depth - 1);
               Chessboard.Undo;
            end if;
         end loop;
      end Perft_Search;

      Start_Time, Stop_Time : Ada.Calendar.Time;
      Elapsed_Time          : Duration := 0.0;
      Milliseconds          : Natural := 0;
      Nps                   : Perft_Node_Type := 0;

      use type Ada.Calendar.Time;
      
   begin

      Ada.Text_IO.Put_Line
        ("Depth       Nodes        Captures      En-Passant         Castles      Promotions          Checks    Discovery C.       Double C.      Checkmates   Time");

      for I in 1 .. Depth loop
         -- Reset path counters at each iteration
         Perft_Move_Paths := Move_Path_Collector_Type'
           (Nodes            => 0,
            Captures         => 0,
            En_Passant       => 0,
            Castles          => 0,
            Promotions       => 0,
            Checks           => 0,
            Discovery_Checks => 0,
            Double_Checks    => 0,
            Checkmates       => 0);

         Start_Time := Ada.Calendar.Clock;
         Perft_Search (I);
         Stop_Time := Ada.Calendar.Clock;

         Elapsed_Time := Stop_Time - Start_Time;

         Ada.Integer_Text_IO.Put (I, 0);
         Ada.Text_IO.Put (" ");

         Perft_Node_IO.Put (Perft_Move_Paths.Nodes);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.Captures);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.En_Passant);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.Castles);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.Promotions);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.Checks);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.Discovery_Checks);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.Double_Checks);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.Checkmates);
         Ada.Text_IO.Put (" ");
         Ada.Float_Text_IO.Put (Item => Float (Elapsed_Time), Fore => 3, Aft => 2, Exp => 0);

         Ada.Text_IO.New_Line;
      end loop;

      Ada.Text_IO.New_Line;

      -- Extract speed information
      Milliseconds := Natural (Elapsed_Time * 100.0);
      if  Milliseconds > 0 then
         Nps := (Perft_Move_Paths.Nodes * 100) / Perft_Node_Type (Milliseconds);
      end if;
      Ada.Text_IO.Put_Line ("Nps:" & Perft_Node_Type'Image (Nps));

   end Perft;


   ------------
   -- Divide --
   ------------

   procedure Divide (Chessboard : in out Chessboard_Type; Depth : in Depth_Type) is
      
      package Perft_Node_IO is new Ada.Text_Io.Modular_IO (Perft_Node_Type);
      
      Ply : Depth_Type renames Chessboard.Ply;

      -------------------
      -- Divide_Search --
      -------------------

      procedure Divide_Search (Depth : in Depth_Type) is
         Move : Move_Type;
      begin
         Chessboard.Generate_Moves;
         for I in Chessboard.Moves_Pointer (Ply) .. Chessboard.Moves_Pointer (Ply + 1) - 1 loop
            Move := Chessboard.Moves_Stack (I);
            Chessboard.Play (Move);
            if Depth = 1 then
               
               -- Update stats about captures
               if Move.Captured /= Empty then
                  Perft_Move_Paths.Captures := Perft_Move_Paths.Captures + 1;
               end if;

               -- Update stats about En_Passant
               if Move.Flag = Capture_En_Passant then
                  Perft_Move_Paths.En_Passant := Perft_Move_Paths.En_Passant + 1;
                  Perft_Move_Paths.Captures := Perft_Move_Paths.Captures + 1;
               end if;

               -- Update stats about Castles
               if Move.Flag = Castle then
                  Perft_Move_Paths.Castles := Perft_Move_Paths.Castles + 1;
               end if;

               -- Update stats about Promotions
               if Move.Promotion /= Empty then
                  Perft_Move_Paths.Promotions := Perft_Move_Paths.Promotions + 1;
               end if;

               -- Update stats about Checks/Checkmates
               if Move.Check /= No_Check then
                  Perft_Move_Paths.Checks := Perft_Move_Paths.Checks + 1;
                  if Move.Check = Discovery_Check then
                     Perft_Move_Paths.Discovery_Checks := Perft_Move_Paths.Discovery_Checks + 1;
                  end if;
                  if Move.Check = Double_Check then
                     Perft_Move_Paths.Double_Checks := Perft_Move_Paths.Double_Checks + 1;
                  end if;
                  if Move.Check = Checkmate then
                     Perft_Move_Paths.Checkmates := Perft_Move_Paths.Checkmates + 1;
                  end if;
               end if;
               
               Perft_Move_Paths.Nodes := Perft_Move_Paths.Nodes + 1;
               
            else
               Divide_Search (Depth => Depth - 1);
            end if;
            Chessboard.Undo;
         end loop;
      end Divide_Search;
      
      -----------------------
      -- Pretty_Print_Move --
      -----------------------
      
      procedure Pretty_Print_Move (Move : Move_Type; Notation : Notation_Type := Default_Notation) is
         Max_Size : constant Natural := 10; -- Space to be used for each move
         Move_String : constant String := Move_To_String (Move, Notation);
      begin
         Ada.Text_IO.Put (Move_String);
         for I in Move_String'Last .. Max_Size loop
            Ada.Text_IO.Put (' ');
         end loop;
      end Pretty_Print_Move;
      
      ---------------------
      -- Print_Path_Data --
      ---------------------
      
      procedure Print_Path_Data (Perft_Move_Paths : in Move_Path_Collector_Type) is
      begin
         Perft_Node_IO.Put (Perft_Move_Paths.Nodes);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.Captures);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.En_Passant);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.Castles);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.Promotions);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.Checks);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.Discovery_Checks);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.Double_Checks);
         Ada.Text_IO.Put (" ");
         Perft_Node_IO.Put (Perft_Move_Paths.Checkmates);
         Ada.Text_IO.New_Line;
      end Print_Path_Data;
      

      Move : Move_Type;
      

   begin
      
      Chessboard.Generate_Moves;
      
      Ada.Text_IO.Put_Line
        ("Move                 Nodes        Captures      En-Passant         Castles      Promotions          Checks    Discovery C.       Double C.      Checkmates");


      if Depth = Zero_Depth then
         
         Perft_Move_Paths := Move_Path_Collector_Type'
           (Nodes            => 0,
            Captures         => 0,
            En_Passant       => 0,
            Castles          => 0,
            Promotions       => 0,
            Checks           => 0,
            Discovery_Checks => 0,
            Double_Checks    => 0,
            Checkmates       => 0);

         for I in Chessboard.Moves_Pointer (Ply) .. Chessboard.Moves_Pointer (Ply + 1) - 1 loop
            Move := Chessboard.Moves_Stack (I);
            Pretty_Print_Move (Move);
            Print_Path_Data (Perft_Move_Paths);
         end loop;

      else

         for I in Chessboard.Moves_Pointer (Ply) .. Chessboard.Moves_Pointer (Ply + 1) - 1 loop
            
            Perft_Move_Paths := Move_Path_Collector_Type'
              (Nodes            => 0,
               Captures         => 0,
               En_Passant       => 0,
               Castles          => 0,
               Promotions       => 0,
               Checks           => 0,
               Discovery_Checks => 0,
               Double_Checks    => 0,
               Checkmates       => 0);
            
            Move := Chessboard.Moves_Stack (I);
            Search_Nodes := 0;
            Chessboard.Play (Move);
            
            Pretty_Print_Move (Move);
            Divide_Search (Depth - 1);
            Print_Path_Data (Perft_Move_Paths);
            
            Chessboard.Undo;
         end loop;

      end if;
   end Divide;


end Chess.Engine.Perfts;
