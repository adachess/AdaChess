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


with Ada.Numerics.Discrete_Random;


package Chess.Score is

   subtype Score_Type is Integer range -1_000_000 .. 1_000_000;

   Infinity : constant Score_Type := 900_900;
   
   
   -----------------
   -- Game scores --
   -----------------
   
   Mate : constant Score_Type := 32767;
   Draw : constant Score_Type := 0;
   
   function Is_Winning_Mate 
     (Score : in Score_Type) return Boolean is (Score >= Mate - 128);
   
   function Is_Losing_Mate 
     (Score : in Score_Type) return Boolean is (Score <= -Mate + 128);
   
   function Is_Mate (Score : in Score_Type) return Boolean is
     (Is_Winning_Mate (Score) or else Is_Losing_Mate (Score));
      
      
   -----------------
   -- Random Mode --
   -----------------

   subtype Random_Score_Type is Score_Type range -4 .. 4;

   package Score_Random is new Ada.Numerics.Discrete_Random (Random_Score_Type);

   Score_Seed : Score_Random.Generator;
   -- This is the seed for the random score generator.
   
   Random_Mode : Boolean := False;
   -- This flag tells the evaluation whether a small random value shall be added
   -- to the final evaluation in order to vary its play

end Chess.Score;
