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
with Ada.Exceptions;

with Chess.IO;

package body Chess.Engine.See is
   
   
   --------------------------------------
   -- Static_Exchange_Evaluation_Score --
   --------------------------------------
   
   function Static_Exchange_Evaluation_Score (Chessboard : in out Chessboard_Type; Move : in Move_Type) return See_Score_Type is
    
      ----------------------------
      -- Attacker_Is_Not_Pinned --
      ----------------------------
      
      function Attacker_Is_Not_Pinned (Chessboard : in Chessboard_Type; Square : in Square_Type) return Boolean is
         Attacker : Piece_Type renames Chessboard.Square (Square);
      begin
         pragma Assert (Attacker not in King_Type, "Bug! King cannot be pinned");
         return Piece_Is_Absolute_Pinned (Chessboard => Chessboard, Square => Square) = No_Direction;
      end Attacker_Is_Not_Pinned;
      
      --------------------------
      -- Next_Weaker_Attacker --
      --------------------------
      
      function Next_Weaker_Attacker (Attack_Data : in out Attack_Collection_Type) return Attack_Type is
         Weakest_Pivot : Natural := 1; -- Index of weaker attacker
         Weakest_Attacker_Score : Score_Type := Infinity;
         Attacker_Score        : Score_Type;
         Weakest_Attacker       : Attack_Type;
      begin

         pragma Assert (Attack_Data.Number_Of_Attackers > 0, "No Attacker found to be processes in the capture sequece");
         
         -- Find the weakest attacker from the given attack collection data
         for I in 1 .. Attack_Data.Number_Of_Attackers loop
            Attacker_Score := Piece_Score (Attack_Data.Attacker (I).Piece);
            if Attacker_Score < Weakest_Attacker_Score then
               Weakest_Pivot := I;
               Weakest_Attacker_Score := Attacker_Score;
            end if;
            exit when Attack_Data.Attacker (I).Piece in Pawn_Type;
         end loop;
         
         -- Save the information about the weakest attacker, to be returned
         -- to the caller and processed as next capture in the sequence
         Weakest_Attacker := Attack_Data.Attacker (Weakest_Pivot);
         
         -- "Delete" the information from the data
         Attack_Data.Attacker (Weakest_Pivot) := Attack_Data.Attacker (Attack_Data.Number_Of_Attackers);
         Attack_Data.Attacker (Attack_Data.Number_Of_Attackers) := Attack_Type'(Origin => 0, Piece => Empty);
         Attack_Data.Number_Of_Attackers := Attack_Data.Number_Of_Attackers - 1;
         
         return Weakest_Attacker;
      end Next_Weaker_Attacker;
     
      ---------------
      -- See_Score --
      ----------------
      
      function See_Score (Capturing : Piece_Type; Side : in Color_Type) return Score_Type is
         Attack_Data     : Attack_Collection_Type;
         Attacker        : Attack_Type;
         Score                : Score_Type := 0;
         Opponent_Side        : constant Color_Type := not Side;
         Captured_Score       : constant Score_Type := Piece_Score (Capturing);
         Promotion_Score      : constant Score_Type := 900; -- Queen
         Pawn_Score           : constant Score_Type := 100;
      begin 
         
         -- Build a list of all the opponent pieces attacking the target square
         -- and sort them out from the weakest to the strongest. 
         
         Attack_Data := Defending_Square
           (Chessboard => Chessboard, Side => Side, Square => Move.To);
         
         while Attack_Data.Number_Of_Attackers > 0 loop
            Attacker := Next_Weaker_Attacker (Attack_Data => Attack_Data);
            if Attacker.Piece in King_Type or else Attacker_Is_Not_Pinned (Chessboard, Attacker.Origin) then
               Chessboard.Square (Attacker.Origin) := Empty;
               Score := Captured_Score - See_Score (Capturing => Attacker.Piece, Side => Opponent_Side);
               Chessboard.Square (Attacker.Origin) := Attacker.Piece;
               
               if Attacker.Piece = White_Pawn and then Rank(Move.To) in Rank_8 then
                  Score := Score + Promotion_Score - Pawn_Score;
               elsif Attacker.Piece = Black_Pawn and then Rank(Move.To) in Rank_1 then
                  Score := Score + Promotion_Score - Pawn_Score;
               end if;
               
               if Score < 0 and then Piece_Score (Attacker.Piece) >= Captured_Score then
                  -- We observe that, from this point on, static captures only
                  -- makes the static evaluation worse.
                  Score := 0;
               end if;
            
               exit when Piece_Score (Attacker.Piece) <= Captured_Score;
               exit when Attacker.Piece in King_Type;
            end if;
         end loop;
         
         return Score;
      end See_Score;
     
      Sequence_Score : See_Score_Type := 0;
      
      Side_To_Move  : constant Color_Type := (if Move.Piece in White_Piece_Type then White else Black);
      Opponent_Side : constant Color_Type := not Side_To_Move;
      
   begin
   
      -- Note: If a move delivers a discovery check, the moving piece cannot be
      -- taken back.
      if Move.Check in Discovery_Check | Double_Check | Checkmate then
         return Piece_Score (Move.Captured);
      end if;
      
      if Move.Captured /= Empty then        
         Sequence_Score := Piece_Score (Move.Captured);
      elsif Move.Flag = Capture_En_Passant then
         Sequence_Score := Piece_Score (White_Pawn);
      end if;
      
      if Move.Promotion /= Empty then
         return Sequence_Score + Piece_Score (Move.Promotion) - 100; -- Pawn is gone ;)
      end if;
     
      Chessboard.Square (Move.From) := Empty;
      Sequence_Score := Sequence_Score - See_Score (Capturing => Move.Piece, Side => Opponent_Side);
      Chessboard.Square (Move.From) := Move.Piece;
      
      return Sequence_Score;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Static_Exchange_Evaluation_Score;

  
   --------------------------------
   -- Static_Exchange_Evaluation --
   --------------------------------
   
   function Static_Exchange_Evaluation (Chessboard : in out Chessboard_Type; Move : in Move_Type) return Capture_Result_Type is
      Sequence_Score : constant Score_Type := Static_Exchange_Evaluation_Score (Chessboard, Move);
   begin
      return (if Sequence_Score = 0 then Equal elsif Sequence_Score > 0 then Winning else Losing);
   end Static_Exchange_Evaluation;
   
   
   ------------------------------------------
   -- Static_Recapture_Exchange_Evaluation --
   ------------------------------------------
   
   function Static_Recapture_Exchange_Evaluation (Chessboard : in out Chessboard_Type; Recapture_Move, Previous_Move : in Move_Type) return Score_Type is
      Score : constant Score_Type := Static_Exchange_Evaluation_Score (Chessboard, Recapture_Move);
   begin
      return Score - Piece_Score (Previous_Move.Captured);
   end Static_Recapture_Exchange_Evaluation;
   
   
   
end Chess.Engine.See;
