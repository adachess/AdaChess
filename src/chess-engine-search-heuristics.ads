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


package Chess.Engine.Search.Heuristics is

   subtype Heuristic_Score is Natural;
   

   procedure Initialize_History_Heuristic;
   -- Initialize the history heuristic. Set all the values to their own 0 (Zero)

   
   --  procedure Register_Good_Move (Move : in Move_Type; Depth : in Depth_Type)
   --    with
   --      Inline => True,
   --      Pre => Move /= Empty_Move and not Move_Is_Capture (Move);
   -- Update the history score for the given move, according also to the current
   -- depth. If the history score overflow a certain limit, the overall history
   -- will be halved. The history score limit is a guarantee for the ordering
   -- moves algorithm to ensure that captures are always searched first.
   --
   -- Agruments
   --    Move  : The current move
   --    Depth : The remaining search depth when the move caused a cutoff
   -- Aspects
   --    Inline        : True
   --    Preconditions : The move shall be a quiet move (a non-tactical)
   
   procedure Register_As_Successful (Move : in Move_Type)
     with 
       Inline => True,
       Pre => Move /= Empty_Move;
   -- Inform the heuristic that a move has been successfull and a score has to
   -- be given in order to improve the move ordering abilities.
   -- Note that this routine shall be called for non-capturing moves caused a
   -- beta cut-off in the main search.
   -- The score will be assigned to the move based on the depth and the current
   -- score already obtained, if any
   --
   -- Arguments
   --    Move  : The successful move
   -- Aspects
   --    Inline        : True
   --    Preconditions : The given move shall be a valid non-tactical move
     
   procedure Register_As_Unsuccessful (Move : in Move_Type)
     with 
       Inline => True,
       Pre => Move /= Empty_Move;
   -- Inform the heuristic that a move has been unsuccessful. This routine shall
   -- be called for non-capturing moves that failed to produce a beta cut-off
   -- at a search tree node where a beta cutoff has been found.
   --
   -- Arguments
   --    Move : The move that failed to produce a beta cut-off
   -- Aspects
   --    Inline        : True
   --    Preconditions : The given move shall be a valid non-tactical move
   
   --  function History_Score (Move : in Move_Type) return Heuristic_Score
   --    with Inline => True;
   -- Retrieve the score for the given move from the history table.
   --
   -- Arguments
   --    Move : The move whose score to look for
   -- Returns
   --    The score in the heuristic table for the given move
   -- Aspects
   --    Inline : True
   
   
   type History_Balance_Type is (Positiv, Neutral, Negative);
   -- Note: the word Positiv, on purpose written without the last letter, has
   -- been selected to avoid conflicts with the name Positive, which refers to
   -- the subtype of Natural.
   
   function History_Balance (Move : in Move_Type; Depth : in Depth_Type) return History_Balance_Type;
   -- Establish if the move has an history mostly successfull or unsuccessfull
   -- according to the the amount of beta-cutoffs occurred.
   --
   -- Arguments
   --    Move  : The move whose balance to look for
   --    Depth : The depth search to balance success and failure score
   -- Returns
   --    The relation between success and unsuccess data
   
   function History_Balance (Move : in Move_Type) return Heuristic_Score;
   -- Compute the Move value according to the history of failures and success
   --
   -- Arguments
   --    Move  : The move whose balance to look for
   -- Returns
   --    The score related to the success and failures found in the search
   
   
   History_Threshold : constant Heuristic_Score := 16384;
   -- This is a bound limit used to divide the quiet move and the capture moves
   -- while performing a move ordering. Any quiet move shall always be scored
   -- less that this threshold, and every capture move shall be always scored
   -- more than this threshold. The only exceptions are the PV move, hash moves
   -- and killer heuristic move.
   
   History_Bound : constant Heuristic_Score := (History_Threshold * 60) / 100;
   -- Represent a bound under which a late move can be searched in a reduced 
   -- search
   
   
private
   
   type History_Heuristic_Type is array (Piece_Type'Range, Square_Type'Range) of Heuristic_Score;
   
   --  History_Heuristic : History_Heuristic_Type;
   -- Handle a table with score for every move with the purpose of influencing
   -- the move ordering by selecting moves with higher score first and avoid
   -- moves proven to have bad score
   
   Success : History_Heuristic_Type;
   Failure : History_Heuristic_Type;
   
   
   --  procedure Scale_History_Score
   --    with
   --      Inline => True;
   -- Controls the history heuristic in order to keep it under a certain
   -- amount of value. The history heuristic shall never reach a value that
   -- is better than a checkmate or the PV value (infinite score).
   -- When the history reaches his limit, all the value in it will be halve.
   --
   -- Aspects
   --    Inline : True
   
   
   procedure Scale_History_Hit 
     with 
       Inline => True;
   -- Controls the history success/Failure in order to keep it under a certain
   -- amount of value. When the history reaches his limit, all the value in it 
   --will be halve.
   --
   -- Aspects
   --    Inline : True
   
   
end Chess.Engine.Search.Heuristics;
