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


package Chess.Nodes is

   ----------------------------------------
   -- Type definitions and configuration --
   ----------------------------------------

   type Node_Type is mod 2 ** 64
     with 
       Size => 64;
   
   
   subtype Perft_Node_Type is Node_Type range 0 .. 2 ** 46; 
   -- Handle up to perft 10. This is needed for a pretty print of the perft and
   -- divide command. AdaChess will never overflow this count. The speed of the
   -- move generator in a moder laptop is around 20Mln nps: to count up to
   -- maximum node in perft, the engine has to run for 1000 hours (40 days).

   ----------------------
   -- Search Tree Node --
   ----------------------

   type Tree_Node_Type is (Pv_Node, Cut_Node, All_Node)
     with Default_Value => Pv_Node, Size => 2;
   
   function "not" (Node : Tree_Node_Type) return Tree_Node_Type
   is (case Node is 
          when Pv_Node => Pv_Node,
          when Cut_Node => All_Node,
          when All_Node => Cut_Node);
   

end Chess.Nodes;
