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


with Chess.Score; use Chess.Score;

package Chess.Engine.Evaluations is

   ------------------------
   -- Overall Evaluation --
   ------------------------

   type Match_Status_Type is
     (In_Progress, -- Game playing
      Checkmate,
      Draw_By_Stalemate,
      Draw_By_Fifty_Moves_Rule,
      Draw_By_Insufficient_Material,
      Draw_By_Threefold_Repetitions,
      Draw_By_Perpetual_Check) with Size => 3;

   type Evaluation_Type is
      record
         Score      : Score_Type := 0;
         Game_Phase : Match_Status_Type := In_Progress;
      end record;
   --  pragma Pack (Evaluation_Type);

   function "-" (Eval : in Evaluation_Type) return Evaluation_Type is
     (Evaluation_Type'(Score => -Eval.Score, Game_Phase => Eval.Game_Phase)) with Inline => True;


   ---------------------
   -- Game Phase data --
   ---------------------

   type Game_Phase_Type is (Opening, End_Game)
     with
       Size => 2;

   --  function "*"
   --    (Left : in Game_Phase_Type; Right : in Score_Type) return Score_Type
   --  is
   --    (Score_Type (Left) * Right);
   --
   --  function "*"
   --    (Left : in Score_Type; Right : in Game_Phase_Type) return Score_Type
   --  is
   --    (Left * Score_Type (Right);
   --
   --
   subtype Phased_Score_Type is Score_Type range 0 .. 100;
   --  Min_Phase : constant Score_Type := Phase_Score_Type'First;
   --  Max_Phase : constant Score_Type := Phase_Score_Type'Last;


   ------------------------
   -- Piece Square Table --
   ------------------------

   type Piece_Square_Table_Type is array (Board_Type'Range) of aliased Score_Type
     with
       Default_Component_Value => 0;

   ------------------------
   -- Tapered Score data --
   ------------------------

   type Tapered_Score_Type is array (Game_Phase_Type'Range) of aliased Score_Type
     with
       Default_Component_Value => 0;

   function "+"
     (Score : in Tapered_Score_Type; Bonus : in Tapered_Score_Type)
      return Tapered_Score_Type
   is
     (Opening  => Score (Opening) + Bonus (Opening),
      End_Game => Score (End_Game) + Bonus (End_Game));

   function "-"
     (Score : in Tapered_Score_Type; Penalty : in Tapered_Score_Type)
      return Tapered_Score_Type
   is
     (Opening  => Score (Opening) - Penalty (Opening),
      End_Game => Score (End_Game) - Penalty (End_Game));

   function "+"
     (Score : in Tapered_Score_Type; Bonus : in Score_Type)
      return Tapered_Score_Type
   is
     (Opening  => Score (Opening) + Bonus,
      End_Game => Score (End_Game) + Bonus);

   function "-"
     (Score : in Tapered_Score_Type; Penalty : in Score_Type)
      return Tapered_Score_Type
   is
     (Opening  => Score (Opening) - Penalty,
      End_Game => Score (End_Game) - Penalty);

   function "*"
     (Score : in Tapered_Score_Type; Amount : in Integer)
      return Tapered_Score_Type
   is
     (Opening  => Score (Opening) * Amount,
      End_Game => Score (End_Game) * Amount);

   function "*"
     (Amount : in Integer; Score : in Tapered_Score_Type)
      return Tapered_Score_Type
   is (Score * Amount);


   Zero : constant Tapered_Score_Type := (Opening => 0, End_Game => 0);


   function Image (S : in Tapered_Score_Type) return String is
     ("(Opening =>" & Score_Type'Image (S (Opening)) & ", End_Game =>" &  Score_Type'Image (S (End_Game)) & ")");

   ------------
   -- Resign --
   ------------

   Resign_Mode : Boolean := False;
   -- When the resign mode is activated, the engine will resign when the
   -- evaluation is sistematically negative and there's no hope to improve
   -- it.

   Resign_Threshold : constant Natural := 6;
   Resign_Threshold_Score : constant Score_Type := -800;

end Chess.Engine.Evaluations;
