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


with ada.Containers;
with Ada.Containers.Vectors;

with Chess.Moves; use Chess.Moves;
with Chess.Engine; use Chess.Engine;
with Chess.Engine.Evaluations; use Chess.Engine.Evaluations;


package Chess.Matches is
   
   use type Ada.Containers.Count_Type;
   
   subtype Count_Type is Ada.Containers.Count_Type;
   
   
   type Match_Type is tagged private;
   
   
   procedure Create_New (Match : out Match_Type; Start : in Count_Type := 1);
   -- Initialize the data to host a new match. The list of moves is reset and
   -- the game status is reset too.
   --
   -- Arguments
   --    Match : The new Match
   --    Start : The first move for this match (normally the 1st)
   
   procedure Create_New_From_Game (Match : out Match_Type; Chessboard : in Chessboard_Type);
   -- Create a new Match based on a game just played, by iterating over the
   -- move played
   --
   -- Arguments
   --    Match      : The new match
   --    Chessboard : The chess game 
   
   
   procedure Append (Match : in out Match_Type; Move : in Move_Type)
     with
       Inline => True,
       Pre => Move /= Empty_Move;
   -- Add a move into the given Match. Does not check for legality
   --
   -- Arguments
   --    Match : The match to update
   --    Move  : The move to add to a match
   -- Aspects
   --    Inline
   --    Preconditions : The move is a not-empty move
   
   procedure Delete_Last_Move (Match : in out Match_Type)
     with
       Inline => True;
   -- Remove the last move played from the given match. If the match does not
   -- contains any move, nothing will happen
   --
   -- Arguments
   --    Match : The match to handle
   -- Aspects
   --    Inline
   
   function Length (Match : in Match_Type) return Count_Type
     with
       Inline => True;
   -- Calculate amount of (semi)moves that a specified match has
   --
   -- Arguments
   --    Match : The match to handle
   -- Returns
   --    The number of total semi-moves in the match
   -- Aspects
   --    Inline
   
   function Get_Move_At_Index (Match : in Match_Type; Index : Count_Type) return Move_Type
     with
       Pre => Match.Length >= Index,
       Post => Get_Move_At_Index'Result /= Empty_Move;
   -- Extract a specific move from a match. 
   --
   -- Arguments
   --    Match : The match to handle
   --    Index : The move to extract
   -- Returns
   --    The move at the given index
   -- Aspects
   --    Preconditions : The match has more moves than the value specified
   --    Postcondition : The retrieved move is a valid move
   
   procedure Display (Match : in Match_Type; Format : in Boolean := True);
   -- Print a match into the console, formatted in pretty-print if requested.
   --
   -- Arguments
   --    Match  : The match to print
   --    Format : If true, the mathc will be pretty-printed, otherwise the match
   --             will be printed as a sequence of moves
   
   function "=" (Left, Right : Match_Type) return Boolean;
   function "<" (Left, Right : Match_Type) return Boolean; -- Find a subset
   
private
   
   package Chess_Match is new Ada.Containers.Vectors
     (Index_Type => Count_Type, Element_Type => Move_Type);
   
   type Match_Type is tagged record
      Moves  : Chess_Match.Vector;
      Status : Match_Status_Type;
      Start  : Count_Type := 1; -- When the game is loaded from FEN, can start from any number of ply
   end record;
   
   Empty_Match : Chess_Match.Vector renames Chess_Match.Empty_Vector;
   
end Chess.Matches;
