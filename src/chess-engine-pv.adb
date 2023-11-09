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
with Ada.Float_Text_IO;
with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Fixed;

with Chess.IO;
with Chess.Score;
with Chess.Protocols;

package body Chess.Engine.PV is

   --------------------------------
   -- Update_Principal_Variation --
   --------------------------------

   procedure Update_Principal_Variation
     (Move       : in Move_Type; 
      Ply        : in Depth_Type;
      Evaluation : in Evaluation_Type;
      Annotation : in Annotation_Type := None) 
   is
      Pv_Depth : constant Depth_Type := Principal_Variation (Ply + 1).Depth; 
      Delivers_Checkmate : constant Boolean := Move.Check = Checkmate;
      Pv_Move            : Annotated_Move_Type;
   begin
      
      Principal_Variation (Ply).Main_Line (Ply).Move := Move;
      Principal_Variation (Ply).Main_Line (Ply).Annotation := Annotation;
      
      for I in Ply + 1 .. Pv_Depth loop
         Pv_Move := Principal_Variation (Ply + 1).Main_Line (I);
         exit when Pv_Move = Empty_Annotated_Move;
         Principal_Variation (Ply).Main_Line (I) := Pv_Move;
      end loop;
         
      Principal_Variation (Ply).Evaluation := Principal_Variation (Ply + 1).Evaluation;
      
      Principal_Variation (Ply).Depth := Pv_Depth;
      if Delivers_Checkmate then
         Principal_Variation (Ply).Main_Line (Pv_Depth + 1) := Empty_Annotated_Move;
      elsif Pv_Depth > Ply then
         Principal_Variation (Ply).Main_Line (Pv_Depth) := Empty_Annotated_Move;
      end if;
      
      -- Update the opponent best countermove so far, to be used in pondering.
      if Ply = Zero_Depth then
         if Move.Check /= Checkmate and then Principal_Variation (Zero_Depth).Depth > Frontier_Depth then
            Principal_Variation (Zero_Depth).Predicted_Countermove :=
              Principal_Variation (Zero_Depth).Main_Line (Frontier_Depth).Move;
         end if;
      end if;
      
      Principal_Variation (Ply).Evaluation := Evaluation;
      
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Update_Principal_Variation;
   
   
   --------------------------
   -- Update_Killer_Move --
   --------------------------

   procedure Update_Killer_Move (Move : in Move_Type; Ply : in Depth_Type) is
   begin
      if Move /= Principal_Variation (Ply).Killer_1 then
         Principal_Variation (Ply).Killer_2 := Principal_Variation (Ply).Killer_1;
         Principal_Variation (Ply).Killer_1 := Move;
      end if;
   end Update_Killer_Move;
   
   
   -------------------------------
   -- Print_Principal_Variation --
   -------------------------------

   procedure Print_Principal_Variation
     (Chessboard   : in Chessboard_Type; 
      Search_Depth : in Depth_Type;
      Thinked_Time : in Duration;
      Ponder_Move  : Move_Type := Empty_Move)
   is
      use Chess.Protocols;
      use Chess.Score;
      use Chess.IO;
      
      package Nodes_IO is new Ada.Text_IO.Modular_IO (Chess.Nodes.Node_Type);
      package Depth_Type_IO is new Ada.Text_IO.Integer_IO (Depth_Type);

      Ply                       : constant Depth_Type := Chessboard.Ply;
      Principal_Variation_Depth : Depth_Type := Principal_Variation (Ply).Depth;

      ---------------------
      -- Print Checkmate --
      ---------------------

      procedure Print_Checkmate is
         type Result_Type is (Win, Lose);
         Result : constant Result_Type := (if Principal_Variation (Zero_Depth).Evaluation.Score > 0 then Win else Lose);
         Distance_To_Mate : Depth_Type;
      begin
         -- Calculate the distance to mate, in full moves
         Distance_To_Mate := (Principal_Variation_Depth / 2)
           + (if Result = Win then Principal_Variation_Depth mod 2 else 0);
         
         -- Print the Mate symbol and align the PV-Output
         Ada.Text_IO.Put ("  " & (if Distance_To_Mate < 10 then "  " else " ") );
         if Result = Win then
            Ada.Text_IO.Put ("+M" & Ada.Strings.Fixed.Trim (Depth_Type'Image (Distance_To_Mate), Ada.Strings.Both));
         else
            Ada.Text_IO.Put ("-M" & Ada.Strings.Fixed.Trim (Depth_Type'Image (Distance_To_Mate), Ada.Strings.Both));
         end if;
      end Print_Checkmate;

      Total_Nodes : constant Node_Type := Search_Nodes;

      Move_Ply                  : History_Depth_Type := Zero_Depth;
      Half_Move                 : History_Depth_Type := Zero_Depth;

      Annotated_Move            : Annotated_Move_Type;
      Move                      : Move_Type renames Annotated_Move.Move;
      White_To_Move             : Boolean;
      Black_To_Move             : Boolean;
      Is_Checkmate              : Boolean := False;

   begin

      --  -- Find the principal variation depth first
      --  loop
      --     Principal_Variation_Depth := Principal_Variation_Depth + 1;
      --     exit when Principal_Variation (Zero_Depth).Main_Line (Principal_Variation_Depth) = Empty_Move;
      --  end loop;

      if Principal_Variation_Post = False then
         return;
      end if;
      
      if Communication_Protocol = Winboard then

         Ada.Text_IO.Put (Depth_Type'Image (Search_Depth));
         Ada.Text_IO.Put (' ');
         Ada.Text_IO.Put (Score_Type'Image (Principal_Variation (Zero_Depth).Evaluation.Score));
         Ada.Text_IO.Put (' ');
         Ada.Text_IO.Put (Natural'Image (Natural (Thinked_Time) * 100));
         Ada.Text_IO.Put (' ');
         Nodes_IO.Put (Total_Nodes);

      else

         --------------------
         -- Console output --
         --------------------

         -- Depth    Time   Nodes    Score   Principal Variation
         --  1/1     0.00      22     0.40   1. e4
         --  2/2     0.00     135     0.00   1. e4 e5
         --  3/3     0.00     713     0.20   1. e4 d5  2. d3
         --  4/4     0.00    2519     0.00   1. e4 d5  2. exd5 Qxd5
         --  5/5     0.03   11397     0.20   1. e4 e5  2. Nc3 Nc6  3. d3

         if Search_Depth < 10 then
            Ada.Text_IO.Put (' '); -- Align formatted print
         end if;
         Depth_Type_IO.Put (Search_Depth, Width => 0);

         Ada.Text_IO.Put ('/');
         Depth_Type_IO.Put (Principal_Variation_Depth, Width => 0);
         Ada.Text_IO.Put (' ');

         if Principal_Variation_Depth < 10 then
            Ada.Text_IO.Put (' '); -- Align formatted print
         end if;

         Ada.Float_Text_IO.Put (Item => Float (Thinked_Time), Fore => 3, Aft => 2, Exp => 0);
         Ada.Text_IO.Put (' ');

         Nodes_IO.Put (Total_Nodes, Width => 12);
         
         -- In case of checkmate, find the distance-to-mate
         Is_Checkmate := abs Principal_Variation(Zero_Depth).Evaluation.Score > Mate - 128;
         if Is_Checkmate then
            Print_Checkmate;
         else
            Ada.Text_IO.Put (' ');
            Ada.Float_Text_IO.Put (Item => Float (Principal_Variation(Zero_Depth).Evaluation.Score) / 100.0, Fore => 3, Aft => 2, Exp => 0);
         end if;

      end if;

      -------------------------
      -- Principal_Variation --
      -------------------------

      Ada.Text_IO.Put (' ');

      if Ponder_Move /= Empty_Move then
         Ada.Text_IO.Put ('(' & Move_To_String (Ponder_Move) & ") ");
      end if;

      for I in Zero_Depth .. Principal_Variation_Depth loop
         Annotated_Move := Principal_Variation (Zero_Depth).Main_Line (I);
         
         --  if Move = Empty_Move then
         --     Ada.Text_IO.Put_Line ("Empty move found");
         --  end if;
           
         -- The empty move is a sentinel that our pv has ended.
         exit when Move = Empty_Move;
         
         -- Print out the current move number symbol followed by the dots in a
         -- similar way as the PGN format shows the moves list
         Move_Ply := Chessboard.History_Ply + I;
         Half_Move := Move_Ply / 2 + Move_Ply mod 2; -- (if Move_Ply mod 2 = 0 then Move_Ply / 2 else Move_Ply / 2 + 1);
         
         White_To_Move := Move.Piece in White_Piece_Type;
         Black_To_Move := not White_To_Move;

         if White_To_Move then
            Ada.Text_IO.Put (History_Depth_Type'Image (Half_Move) & '.');
         elsif Black_To_Move and then I = Zero_Depth then
            Ada.Text_IO.Put (History_Depth_Type'Image (Half_Move) & ". ..");
         end if;

         Ada.Text_IO.Put (' ');
         Chess.IO.Print_Move (Annotated_Move);
         
         -- In case of checkmate line, there's no need to go further
         -- otherwise, the PV ends in the second to last depth line
         exit when Move.Check = Checkmate;
      end loop;
      
      -------------------
      -- Draw notation --
      -------------------
      
      case Principal_Variation (Zero_Depth).Evaluation.Game_Phase is
         when In_Progress | Checkmate => null; -- Already processed
         when Draw_By_Stalemate => 
            Ada.Text_IO.Put (" {1/2 - 1/2} (Stalemate)");
         when Draw_By_Fifty_Moves_Rule => 
            Ada.Text_IO.Put (" {1/2 - 1/2} (Fifty move rules)");
         when Draw_By_Insufficient_Material =>
            Ada.Text_IO.Put (" {1/2 - 1/2} (Insufficient material)");
         when Draw_By_Threefold_Repetitions =>
            Ada.Text_IO.Put (" {1/2 - 1/2} (Threefold repetitions)");
         when Draw_By_Perpetual_Check =>
            Ada.Text_IO.Put (" {1/2 - 1/2} (Perpetual check)");
      end case;
      

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Flush;
      
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Print_Principal_Variation;


end Chess.Engine.PV;
