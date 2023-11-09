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

with Chess;
with Chess.Depths;  use Chess.Depths;
with Chess.Clocks; use Chess.Clocks;
with Chess.Engine.PV;     use Chess.Engine.PV;
with Chess.Engine.Ponder; use Chess.Engine.Ponder;
with Chess.Engine.Search.PV; use Chess.Engine.Search.PV;
with Chess.Engine.Evaluations;

with Chess.Protocols; use Chess.Protocols;
with Chess.Books;
with Chess.Books.Openings; use Chess.Books.Openings;

with Chess.IO;

package body Chess.Engine.Thinking is

  

   -----------
   -- Think --
   -----------

   function Think (Chessboard : in out Chessboard_Type) return Move_Type is
      The_Book_Move     : Move_Type;
      Single_Reply      : Move_Type := Empty_Move;
      Search_Depth      : Depth_Type := Zero_Depth;
      Best_Move         : Move_Type;
      Ply               : Depth_Type renames Chessboard.Ply;
   begin
      
      ---------------
      -- Book Move --
      ---------------
      
      The_Book_Move := Book_Move (Chessboard);
      if The_Book_Move /= Empty_Move then
         Ada.Text_IO.Put_Line (Chess.IO.Move_To_String (The_Book_Move) & " (Book Move)");
         return The_Book_Move;
      end if;
      
      ------------------
      -- Single Reply --
      ------------------
      
      Chessboard.Generate_Moves;
      if Chessboard.Moves_Counter (Ply) = 1 then
         Single_Reply := Chessboard.Moves_Stack (Chessboard.Moves_Pointer (Ply));
         return Single_Reply;
      end if;
      
      -------------------------
      -- Iterative Deepening --
      -------------------------
      
      Search_Nodes := 0;
      Qnodes := 0;

      Chessboard.Ply := Zero_Depth;

      if Principal_Variation_Post and then Communication_Protocol = No_Gui_Connection then
         Ada.Text_IO.Put_Line ("Depth   Time        Nodes  Score  Principal Variation");
      end if;

      ------------------------------
      -- Iterative Deeping Search --
      ------------------------------
      
      Run_Iterative_Deepening (Chessboard, Search_Depth);
      
      Best_Move := Principal_Variation (Zero_Depth).Main_Line (Zero_Depth).Move;
      
      --  Principal_Variation (Zero_Depth).Predicted_Countermove :=
      --    Principal_Variation (Zero_Depth).Main_Line (Frontier_Depth);
      
      return Best_Move;

   exception
      when Thinking_Time_Expired =>
         Print_Principal_Variation (Chessboard   => Chessboard,
                                    Search_Depth => Search_Depth,
                                    Thinked_Time => Clock.Thinked_Time);
         
         pragma Assert (Chessboard.Ply = Zero_Depth, "Rollback failure in Think!");
         
         Best_Move := Principal_Variation (Zero_Depth).Main_Line (Zero_Depth).Move;
         
         return Best_Move;
   end Think;


   -----------
   -- Ponder --
   -----------

   procedure Ponder (Chessboard : in out Chessboard_Type) is 
      Pondering_Move : Move_Type renames Chessboard.Moves_History (Chessboard.History_Ply - 1).Move;
      Search_Depth   : Depth_Type := Zero_Depth;
   begin

      -- Reset all counters and parameteres
      Search_Nodes := 0;

      Chessboard.Ply := Zero_Depth;

      if Principal_Variation_Post and then Communication_Protocol = No_Gui_Connection then
         Ada.Text_IO.Put_Line ("Depth   Time        Nodes  Score  Principal Variation");
      end if;

      ------------------------------
      -- Iterative Deeping Search --
      ------------------------------
      
      Run_Iterative_Deepening (Chessboard, Search_Depth, Pondering_Move);
   
      --  Save the predicted opponent move to be used in ponder
      --  Principal_Variation (Zero_Depth).Predicted_Countermove :=
      --    Principal_Variation (Zero_Depth).Main_Line (Frontier_Depth);
      

      Print_Principal_Variation (Chessboard   => Chessboard,
                                 Search_Depth => Search_Depth,
                                 Thinked_Time => Clock.Thinked_Time,
                                 Ponder_Move  => Pondering_Move);

   exception
      when Thinking_Time_Expired =>
         Print_Principal_Variation (Chessboard   => Chessboard,
                                    Search_Depth => Search_Depth,
                                    Thinked_Time => Clock.Thinked_Time,
                                    Ponder_Move  => Pondering_Move);
   end Ponder;
   
   
   
   ---------------
   -- Benchmark --
   ---------------

   function Benchmark (Chessboard : in out Chessboard_Type) return Move_Type is
      Search_Depth  : Depth_Type := Zero_Depth;
      Best_Move         : Move_Type;
   begin
   
      -- Reset all counters and parameteres
      Search_Nodes := 0;
      Qnodes := 0;
   
      Chessboard.Ply := Zero_Depth;
   
      -- If needed, print header on console
      if Principal_Variation_Post and then Communication_Protocol = No_Gui_Connection then
         Ada.Text_IO.Put_Line ("Depth   Time        Nodes  Score  Principal Variation");
      end if;
   
      ------------------------------
      -- Iterative Deeping Search --
      ------------------------------
   
      Search_Depth := Zero_Depth;
      
      Run_Iterative_Deepening (Chessboard, Search_Depth);
      
      Best_Move := Principal_Variation (Zero_Depth).Main_Line (Zero_Depth).Move;
      return Best_Move;
   
   exception
      when Thinking_Time_Expired =>
   
         Print_Principal_Variation (Chessboard   => Chessboard,
                                    Search_Depth => Search_Depth,
                                    Thinked_Time => Clock.Thinked_Time);
   
         Ada.Text_IO.Put_Line ("Nodes/Qnodes:" & Node_Type'Image (Search_Nodes - Qnodes) & "/" & Node_Type'Image (Qnodes));
   
         pragma Assert (Chessboard.Ply = Zero_Depth, "Rollback failure in Think!");
   
         Best_Move := Principal_Variation (Zero_Depth).Main_Line (Zero_Depth).Move;
         return Best_Move;
   
   end Benchmark;
   
   
   -----------------------------
   -- Run_Iterative_Deepening --
   -----------------------------
   
   procedure Run_Iterative_Deepening 
     (Chessboard   : in out Chessboard_Type; 
      Search_Depth : in out Depth_Type;
      Ponder_Move  : in Move_Type := Empty_Move)
   is
      use Chess.Engine.Evaluations;
      Aspiration_Window : Aspiration_Window_Type;
   begin
   
      Aspiration_Window := Aspiration_Window_Type'
        (Alpha => -Infinity, Beta => +Infinity);

      --  if Search_Depth = Zero_Depth then
      --     First_Variation (Search_Depth).Evaluation.Score := 0;
      --  else
      --     First_Variation (Search_Depth).Evaluation.Score := Principal_Variation (Zero_Depth).Evaluation.Score;
      --  end if;
   
      loop
         
         First_Variation_Depth := Search_Depth;
  
         Principal_Variation (Zero_Depth).Evaluation := Search_Root
           (Chessboard        => Chessboard,
            Alpha             => Aspiration_Window.Alpha,
            Beta              => Aspiration_Window.Beta,
            Max_Depth         => Search_Depth,
            Ponder_Move       => Ponder_Move);
   
         exit when Time_Has_Up and then not Pondering;
   
         -- Update main line with current data
         Last_Best_Move := Principal_Variation (Zero_Depth).Main_Line (Zero_Depth).Move;
         Last_Best_Score := Principal_Variation (Zero_Depth).Evaluation.Score;
   
         Print_Principal_Variation (Chessboard   => Chessboard,
                                    Search_Depth => Search_Depth,
                                    Thinked_Time => Clock.Thinked_Time,
                                    Ponder_Move  => Ponder_Move);
         
         -- Save the very first result of this PV to compare with the gain we
         -- get by searching deeper
         First_Variation (Search_Depth).Evaluation := Principal_Variation (Zero_Depth).Evaluation;
         First_Variation (Search_Depth).Current_Move := Principal_Variation (Zero_Depth).Main_Line (Zero_Depth).Move;
         
         -----------------------
         -- Aspiration Window --
         -----------------------
         
         if Principal_Variation (Zero_Depth).Evaluation.Score <= Aspiration_Window.Alpha or else Principal_Variation (Zero_Depth).Evaluation.Score >= Aspiration_Window.Beta then
            Aspiration_Window := Aspiration_Window_Type'
              (Alpha => -Infinity,
               Beta  => +Infinity);
         
         else

            Aspiration_Window := Aspiration_Window_Type'
              (Alpha => Principal_Variation (Zero_Depth).Evaluation.Score - Aspiration_Window_Size,
               Beta  => +Infinity); -- Principal_Variation (Zero_Depth).Evaluation.Score + Aspiration_Window_Size);
         
            --  Aspiration_Window := Aspiration_Window_Type'
            --    (Alpha => Principal_Variation (Zero_Depth).Evaluation.Score - Aspiration_Window_Size,
            --     Beta  => Principal_Variation (Zero_Depth).Evaluation.Score + Aspiration_Window_Size);
         
            Search_Depth := Search_Depth + 1;
            
         end if;
         
         
         -- Once a checkmate is found, try to quickly detect any shorter way to
         -- checkmate by searching more moves at full depth without reductions
         -- and pruning. 
         if Principal_Variation (Zero_Depth).Evaluation.Game_Phase = Checkmate then
            Multi_Pv := Multi_Pv + 1;
         end if;
         
         exit when Time_Has_Up and then not Pondering;
         exit when Search_Depth = Clock.Search_Depth_Limit;
      end loop;
      
   end Run_Iterative_Deepening;

   
end Chess.Engine.Thinking;
