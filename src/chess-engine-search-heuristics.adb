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


package body Chess.Engine.Search.Heuristics is


   ----------------------------------
   -- Initialize_History_Heuristic --
   ----------------------------------

   procedure Initialize_History_Heuristic is
   begin
      --  History_Heuristic := (others => (others => 0));
      Failure := (others => (others => 0));
      Success := (others => (others => 0));
   end Initialize_History_Heuristic;


   ------------------------
   -- Register_Good_Move --
   ------------------------

   --  procedure Register_Good_Move (Move : in Move_Type; Depth : in Depth_Type) is
   --  begin
   --     History_Heuristic (Move.Piece, Move.To) := History_Heuristic (Move.Piece, Move.To) + Depth * Depth;
   --     if History_Heuristic (Move.Piece, Move.To) >= History_Threshold then
   --        Scale_History_Score;
   --     end if;
   --  end Register_Good_Move;

   ----------------------------
   -- Register_As_Successful --
   ----------------------------

   procedure Register_As_Successful (Move : in Move_Type) is
   begin
      Success (Move.Piece, Move.To) := Success (Move.Piece, Move.To) + 1;
      if Success (Move.Piece, Move.To) >= History_Threshold then
         Scale_History_Hit;
      end if;
   end Register_As_Successful;


   ------------------------------
   -- Register_As_Unsuccessful --
   ------------------------------

   procedure Register_As_Unsuccessful (Move : in Move_Type) is
   begin
      Failure (Move.Piece, Move.To) := Failure (Move.Piece, Move.To) + 1;
      if Failure (Move.Piece, Move.To) >= History_Threshold then
         Scale_History_Hit;
      end if;
   end Register_As_Unsuccessful;


   -------------------
   -- History_Score --
   -------------------

   --  function History_Score (Move : in Move_Type) return Heuristic_Score is
   --  begin
   --     return History_Heuristic (Move.Piece, Move.To);
   --  end History_Score;


   ---------------------
   -- History_Balance --
   ---------------------

   function History_Balance (Move : in Move_Type; Depth : in Depth_Type) return History_Balance_Type is
      Val : constant Integer := Depth * Success (Move.Piece, Move.To) - Failure (Move.Piece, Move.To);
   begin
      return (if Val < 0 then Negative elsif Val = 0 then Neutral else Positiv);
   end History_Balance;


   ---------------------
   -- History_Balance --
   ---------------------

   function History_Balance (Move : in Move_Type) return Heuristic_Score is
      Failures : constant Heuristic_Score := Failure (Move.Piece, Move.To) + 1; -- Avoid division by Zero
   begin
      return (Success(Move.Piece, Move.To) * History_Threshold / Failures) mod History_Threshold;
   end History_Balance;


   -------------------------
   -- Scale_History_Score --
   -------------------------

   --  procedure Scale_History_Score is
   --  begin
   --     for Piece in Piece_Type'Range loop
   --        for Destination_Square in Square_Type'Range loop
   --           History_Heuristic (Piece, Destination_Square) := (History_Heuristic (Piece, Destination_Square) + 1) / 2;
   --        end loop;
   --     end loop;
   --  end Scale_History_Score;


   -----------------------
   -- Scale_History_Hit --
   -----------------------

   procedure Scale_History_Hit is
   begin
      for Piece in Piece_Type'Range loop
         for Destination_Square in Square_Type'Range loop
            Success (Piece, Destination_Square) := (Success (Piece, Destination_Square) + 1) / 2;
            Failure (Piece, Destination_Square) := (Failure (Piece, Destination_Square) + 1) / 2;
         end loop;
      end loop;
   end Scale_History_Hit;

end Chess.Engine.Search.Heuristics;
