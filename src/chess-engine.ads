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


with Chess;
with Chess.Colors; use Chess.Colors;
with Chess.Pieces; use Chess.Pieces;
with Chess.Board;  use Chess.Board;
with Chess.Board.Directions; use Chess.Board.Directions;
with Chess.Moves;  use Chess.Moves;
with Chess.Pins;   use Chess.Pins;
with Chess.Nodes;    use Chess.Nodes;
with Chess.Depths;  use Chess.Depths;
with Chess.History; use Chess.History;
with Chess.Hashes;  use Chess.Hashes;


package Chess.Engine is

   -- Those are array for pieces. We must know the square where any piece are,
   -- then we can pick the right piece from that square in the ChessBoard. Then,
   -- to perform some fast operation while adding and deleting piece
   -- that comes on captures, promotions and other moves,
   -- we use an auxiliary board, caled Piece_Table.
   -- That board is populated with index of the array of the
   -- white and black pieces.

   -- AdaChess implements the board via a piece-square array. The piece_list
   -- contains the position of each piece of both sides. It is divided into 4
   -- logical slices, each slide is reserved of a specific type:
   -- Range  1 .. 16 => White Pieces
   -- Range 17 .. 32 => Black Pieces

   type Pieces_List_Type is array (Natural range 1 .. 32) of Square_Type;
   subtype White_Pieces_Range is Natural range 1 .. 16;
   subtype Black_Pieces_Range is Natural range 17 .. 32;
   Black_Pieces_Start_Index : constant Natural := 16;

   type Piece_Table_Type is array (Square_Type'Range) of Square_Type;


   type Castle_History_Type is array (History_Depth_Type'Range) of Boolean;
   -- Track the castling availabiliting situation for each step

   type Moves_History_List_Type is array (History_Depth_Type'Range) of History_Move_Type;

   type En_Passant_History_Type is array (History_Depth_Type'Range) of Square_Type;

   subtype Fifty_Counter_Type is Natural range 0 .. 100;

   ----------------
   -- Move Stack --
   ----------------

   -- The Stack is a concept seen in many other engine. The idea is to use a
   -- single dimension array and to add legal moves to it. At each ply, the
   -- stack tracks the index of the first move. The index of the last move can
   -- be easily retrieve as the previous value from the the index of the first
   -- move of the next ply (that value minus one...)
   -- The Stack is composed by 3 main parts:
   -- 1) The size of the array
   -- 2) The real stack containing moves
   -- 3) The "pointer", i.e, the index of the first move at each ply

   subtype Move_Stack_Range_Type is Natural range 0 .. 4096 - 1;
   -- Note: very high value are necessary for loading PGN books with long games
   -- The current range has been selected to handle very long games

   type Moves_Stack_Type is array (Move_Stack_Range_Type'Range) of Move_Type;
   -- Handle up to depth 80..

   type Moves_Stack_Pointer is array (Depth_Type'Range) of Natural;


   ----------------
   -- Chessboard --
   ----------------

   type Chessboard_Type is tagged
      record
         Square : Board_Type;

         Ply    : Depth_Type;

         Pieces_List    : Pieces_List_Type;
         Piece_Table    : Piece_Table_Type;

         White_Pieces_Counter : Piece_Counter_Type;
         White_King_Position  : Square_Type;

         Black_Pieces_Counter : Piece_Counter_Type;
         Black_King_Position  : Square_Type;

         Side_To_Move : Color_Type;

         White_Castle_Queenside : Castle_History_Type;
         White_Castle_Kingside  : Castle_History_Type;
         Black_Castle_Queenside : Castle_History_Type;
         Black_Castle_Kingside  : Castle_History_Type;

         En_Passant          : En_Passant_History_Type;

         Moves_Stack          : Moves_Stack_Type;
         Moves_Pointer        : Moves_Stack_Pointer;

         History_Ply         : History_Depth_Type;

         Moves_History       : Moves_History_List_Type;
         -- Moves History is our track for moves played during the game.
         -- We need an history for two reasons: to undo moves and to
         -- look back for 3-times repetitions rule.

         Fifty               : Fifty_Counter_Type;

         Force_Validity_Test : Boolean;
         -- The Validity_Test is a flag that shall be activate when it is necessary
         -- to verify if a move is legal or not. This happens when the move
         -- generator cannot guarantee that a move will not leave the King under
         -- check. The flag is always activated on moves that involve:
         -- 1) Castling
         -- 2) En-passant
         -- 3) Moves with absolute pinned piece (when sliding in direction other
         --    than the absolute pin direction)
         -- 4) King escapes

      end record;


   -------------------
   -- Moves Counter --
   -------------------

   function Moves_Counter
     (Chessboard : in Chessboard_Type; Ply : in Depth_Type)
      return Natural
     with
       Inline => True,
       Pre => Ply <= Horizon;
   -- Detect how many legal moves are available to a specific ply.
   --
   -- Arguments
   --    Ply : The depth where the function shall count the legal moves available
   -- Returns
   --    The number of legal moves at the specified ply
   -- Aspects
   --    Inline : The funcion is inlined
   --    Precondition : The specified Ply shall not exceed the horizon

   Search_Nodes               : Node_Type := 0;
   -- Nodes is a counter that is used for both performance measurement
   -- and for taking trace of Branching Factor while performing the
   -- Principal Variation Search. On Perft, nodes give us a proof that
   -- the move generator works well and without bugs.

   Qnodes                     : Node_Type := 0;

   ----------------------------------
   -- Tracking pieces on the board --
   ----------------------------------

   -- Add a piece to the white/black pieces list. The square is needed, the piece
   -- is obtained by looking that square in the ChessBoard
   procedure Add_White_Piece
     (Chessboard : in out Chessboard_Type;
      Square     : in Square_Type)
     with
       Inline => True;
   -- Add a white piece to the piece list in the chessboard
   --
   -- Arguments
   --    Square : The square where the piece is located
   -- Aspects
   --    Inline

   procedure Add_Black_Piece
     (Chessboard : in out Chessboard_Type;
      Square     : in Square_Type)
     with
       Inline => True;
   -- Inform the board that a black piece has beed added on a given square
   --
   -- Arguments
   --    Square : The square where the piece is located
   -- Aspects
   --    Inline

   procedure Delete_White_Piece
     (Chessboard : in out Chessboard_Type;
      Square     : in Square_Type)
     with
       Inline => True;
   -- Inform the board that a white piece has beed removed from a given square
   --
   -- Arguments
   --    Square : The square where the piece was located
   -- Aspects
   --    Inline

   procedure Delete_Black_Piece
     (Chessboard : in out Chessboard_Type;
      Square     : in Square_Type)
     with
       Inline => True;
   -- Inform the board that a black piece has beed removed from a given square
   --
   -- Arguments
   --    Square : The square where the piece was located
   -- Aspects
   --    Inline

   procedure Update_White_Piece
     (Chessboard : in out Chessboard_Type;
      From, To   : in Square_Type)
     with
       Inline => True;
   -- Inform the board that a white piece has beed moved to a given square
   --
   -- Arguments
   --    From : The origin square where the piece was located
   --    To   : The destination square where the pice has been moved
   -- Aspects
   --    Inline

   procedure Update_Black_Piece
     (Chessboard : in out Chessboard_Type;
      From, To   : in Square_Type)
     with
       Inline => True;
   -- Inform the board that a black piece has beed moved to a given square
   --
   -- Arguments
   --    From : The origin square where the piece was located
   --    To   : The destination square where the pice has been moved
   -- Aspects
   --    Inline


   procedure Print_Moves_List (Chessboard : in out Chessboard_Type);
   -- Print on console the legal move from the current side to move



   ---------------------------------------
   -- Zobrist Hash data and subprograms --
   ---------------------------------------

   type Hash_Pieces_Type is array (Piece_Type'Range, Board_Type'Range) of Hash_Type;
   Hash_Pieces          : Hash_Pieces_Type;

   type Hash_En_Passant_Type is array (Board_Type'Range) of Hash_Type;
   Hash_En_Passant      : Hash_En_Passant_Type;

   subtype Castle_Possibility is Natural range 1 .. 4;
   type Hash_Castle_Type is array (Castle_Possibility'Range) of Hash_Type;
   Hash_Castle          : Hash_Castle_Type;

   Hash_Side            : Hash_Type;

   Hash                 : Hash_Type;
   -- Hash is a variable that store the Hash value of the
   -- current chess board position. This global variable
   -- will be updated at any moves and used for both finding
   -- moves repetitions and Transposition Tables.


   procedure Initialize_Hash (Chessboard : in out Chessboard_Type);
   -- Initialize the hash value by writing some random hash data into the Hash
   -- containers. This is the standard procedure for the Zobrist algorithm

   procedure Update_Hash (Chessboard : in out Chessboard_Type);
   -- Recalculate the hash value for the current chess position.


   --------------------------
   -- Reset and Initialize --
   --------------------------

   procedure Reset (Chessboard : in out Chessboard_Type);
   -- Set all value to their own "zero", clean the board and every counter,
   -- prepare the Chessboard to be filled.

   procedure Initialize (Chessboard : in out Chessboard_Type);
   -- Initialize Chessboard, set white side to move, reset counters and so on.


   -----------------------
   -- Display functions --
   -----------------------

   procedure Display_Piece_Table (Chessboard : in out Chessboard_Type);
   -- Display the piece table on conole. This procedure can be used for debugging
   -- and testing purpose


   --------------------
   -- Move Generator --
   --------------------

   procedure Clear_Moves_List (Chessboard : in out Chessboard_Type)
     with
       Inline => True,
       Pre => (Chessboard.Ply <= Horizon);
   -- Clean eveny generated move for the current ply and reset the legal move
   -- counter for the current ply
   --
   -- Aspects
   --    Precondition : The current Ply does not exceed horizon

   procedure Generate_Moves (Chessboard : in out Chessboard_Type);
   -- Generate all the legal moves from the current position. The move generator
   -- will delegate to other routines for the legality test (if needed) and/or
   -- to generate legal moves for special cases - such as when the current side
   -- has the king in check

   procedure Generate_Check_Evasion (Chessboard : in out Chessboard_Type)
     with
       Pre => Last_Move_Made (Chessboard).Check /= No_Check;
   -- Generate all the legal moves from the current position, assuming that the
   -- side to move has the king in check. This routine perform special checks
   -- and it is optimized for these specific cases
   --
   -- Aspects
   --    Precondition : The side to move has the king in check

   function King_Has_Escapes
     (Chessboard : in out Chessboard_Type; Type_Of_Check : in Check_Type)
      return Boolean
     with
       Pre => Type_Of_Check /= No_Check;
   -- Lookup the current chessboard to find if the King of the side to move can
   -- escape from a check condition.
   --
   -- Arguments
   --    Type_Of_Check : The kind of check (such as direct check, discovery, ...
   --                    under which the King should escape
   -- Returns
   --    True if at least 1 legal move is available from the current positon,
   --    False otherwise
   -- Aspects
   --    Precondition : The King of the side to move is in check

   procedure Generate_Tactical_Moves (Chessboard : in out Chessboard_Type);
   -- Move generator for Quiescence search. This generator is a kind of clone
   -- of the "standard" legal move generator, with the only difference that this
   -- will generate only moves of tactical importance and check evasions

   procedure Generate_Captures (Chessboard : in out Chessboard_Type);
   -- Move generator that generates only captures (including en-passant) and
   -- captures that leads to promotions

   procedure Generate_See_Moves (Chessboard : in out Chessboard_Type; Target : in Square_Type)
     with
       Pre => Chessboard.Square (Target) /= Frame;
   -- Special move generator used for static exchange evaluation only
   --
   -- Arguments
   --    Target : The destination square to evaluate for statical exchange
   -- Aspects
   --    Precondition : The desintaion square is a valid square

   procedure Generate_See_Check_Evasion (Chessboard : in out Chessboard_Type; See_Target : in Square_Type)
     with
       Pre => Chessboard.Square (See_Target) /= Frame;
   -- Special move generator for check evasions to be used for static exchange
   -- evaluation only
   --
   -- Arguments
   --    See_Target : The destination square to evaluate for statical exchange
   -- Aspects
   --    Precondition : The desintaion square is a valid square

   function Move_Is_Tactical (Move : in Move_Type) return Boolean is
     (Move.Check /= No_Check
      or else Move.Captured /= Empty
      or else Move.Flag = Capture_En_Passant
      or else Move.Promotion /= Empty
      or else Move.Flag = Castle)
     with
       Pre => Move.Flag /= Null_Move;
   -- Sifts the current move data and decide whether shall be considered of
   -- tactical importance. Tactical moves, as opposite to quiet moves, are those
   -- moves involving a check, a caputre or a promotion. Castling moves, due to
   -- the importance of king-safety, are also of tactical importance
   --
   -- Arguments
   --    Move : The move to sifts through
   -- Returns
   --    True if the move is of tactical importance, False otherwise
   -- Aspects
   --    Precondition : The move does not represent a Null Move

   function Move_Is_Quiet (Move : in Move_Type) return Boolean is
     (not Move_Is_Tactical (Move))
       with
         Pre => Move.Flag /= Null_Move;
   -- Any move that is not tactical
   --
   -- Arguments
   --    Move : The move to sifts through
   -- Returns
   --    True if the move is NOT of tactical importance, False otherwise
   -- Aspects
   --    Precondition : The move does not represent a Null Move

   -------------------
   -- Register Move --
   -------------------

   procedure Register_Move
     (Chessboard : in out Chessboard_Type; From, To : in Square_Type)
     with
       Inline => True,
       Pre => Chessboard.Square (To) /= Frame;
   -- Create a move based on the starting and destination square specified and
   -- set other values to their default
   --
   -- Arguments
   --    From : The current, starting square of the Move
   --    To   : The destination square
   -- Aspects
   --    Inline
   --    Precondition : The destination square is a valid square

   procedure Register_Move
     (Chessboard : in out Chessboard_Type; From, To : in Square_Type; Flag : in Flag_Type)
     with
       Inline => True;
   -- Create a move based on the starting and destination square specified and
   -- with the given flag. Set the other values to their default.
   -- Note that, if the flag indicate that the move represent a pawn promotion,
   -- the procedure will generate four moves once for each promotion kind
   --
   -- Arguments
   --    From : The current, starting square of the Move
   --    To   : The destination square
   -- Aspects
   --    Inline
   --    Precondition : The destination square is a valid square

   procedure Register_Move
     (Chessboard : in out Chessboard_Type; Move : in Move_Type)
     with
       Inline => True;
   -- Save a move into the stack of legal moves. This function will perform the
   -- legality check - if needed - and add further information such as if the
   -- moves gives check (and which kind of check) and, in case the notation
   -- requires it, it will detect if a disambiguation is needed (for output
   -- purpose only)
   --
   -- Arguments
   --    From : The current, starting square of the Move
   --    To   : The destination square
   -- Aspects
   --    Inline
   --    Precondition : The destination square is a valid square

   procedure Register_Tactical_Move
     (Chessboard : in out Chessboard_Type; From, To : in Square_Type)
     with
       Inline => True,
       Pre => Chessboard.Square (To) /= Frame;
   -- Create a move based on the starting and destination square specified and
   -- set other values to their default. This procedure is called from the
   -- special generator for tactical moves, in order to filter and register
   -- only tactical moves
   --
   -- Arguments
   --    From : The current, starting square of the Move
   --    To   : The destination square
   -- Aspects
   --    Inline
   --    Precondition : The destination square is a valid square

   procedure Register_Tactical_Move
     (Chessboard : in out Chessboard_Type; From, To : in Square_Type; Flag : in Flag_Type)
     with
       Inline => True,
       Pre => Chessboard.Square (To) /= Frame;
   -- Create a move based on the starting and destination square specified and
   -- with the given flag. Set the other values to their default.
   -- Note that, if the flag indicate that the move represent a pawn promotion,
   -- the procedure will generate four moves once for each promotion kind.
   -- This procedure is called from the special generator for tactical moves
   -- with the purpose to filter and prevent the registration of non-tactical
   -- moves
   --
   -- Arguments
   --    From : The current, starting square of the Move
   --    To   : The destination square
   -- Aspects
   --    Inline
   --    Precondition : The destination square is a valid square

   procedure Register_Tactical_Move
     (Chessboard : in out Chessboard_Type; Move : in Move_Type)
     with
       Inline => True;
   -- Save a move into the stack of legal moves. This function will perform the
   -- legality check - if needed - and add further information such as if the
   -- moves gives check (and which kind of check) and, in case the notation
   -- requires it, it will detect if a disambiguation is needed (for output
   -- purpose only).
   -- This procedure is called from the special generator for tactical moves
   -- with the purpose to filter and prevent the registration of non-tactical
   -- moves
   --
   -- Arguments
   --    From : The current, starting square of the Move
   --    To   : The destination square
   -- Aspects
   --    Inline
   --    Precondition : The destination square is a valid square


   -------------------------
   -- Play and Undo Moves --
   -------------------------

   procedure Play (Chessboard : in out Chessboard_Type; Move : in Move_Type);
   -- Make a move in current Chessboard. The procedure save some data in order
   -- to undo the move and update the chessboard (such as castle flag, fifty
   -- moves counter and so on)
   --
   -- Arguments
   --    Move : The move to be played

   procedure Undo (Chessboard : in out Chessboard_Type);
   -- Take back the last move played on the chessboard and restore the situation
   -- as it was previous to it (sich as castle flag, fifty moves counter and so on
   -- Undoing a move is often call "takeback" in other engines

   procedure Play_See_Move (Chessboard : in out Chessboard_Type; Move : in Move_Type);
   -- This routine is an optimized and simplified version of the Play, meant to
   -- be used in the Static Exchange Evaluation.
   --
   -- Arguments
   --    Move : The move to statically evaluate

   procedure Undo_See_Move (Chessboard : in out Chessboard_Type);
   -- This routine is an optimized and simplified version of the Play_See_Move,
   -- meant to be used in the Static Exchange Evaluation

   procedure Play_Null_Move (Chessboard : in out Chessboard_Type)
     with
       Inline => True;
   -- Make a "Null Move"
   --
   -- Aspects
   --    Inline

   procedure Undo_Null_Move (Chessboard : in out Chessboard_Type)
     with
       Inline => True;
   -- Take back a "Null Move"
   --
   -- Aspects
   --    Inline

   -- While testing if a move that checks opponent king
   -- also does a checkmate, I use a lighter version
   -- of the make-move and to take it back
   procedure Play_Check_Move (Chessboard : in out Chessboard_Type; Move : in Move_Type)
     with
       Pre => Move.Check in Direct_Check | Discovery_Check | Double_Check | Unknown_Check;
   -- This routine is a slightly optimized version of the standard Play, meant
   -- to be used when a moves gives check. The goal is to investigate if the
   -- given check is actually a checkmate
   --
   -- Arguments
   --    Move : The move that gives check
   -- Aspects
   --    Precondition : The current move gives check

   procedure Undo_Check_Move (Chessboard : in out Chessboard_Type);
   -- This routine is a slightly optimized version of the standard Undo, meant
   -- to be used to take back a move that gave check.


   -------------------
   -- Last move made --
   --------------------

   function Last_Move_Made (Chessboard : in Chessboard_Type) return Move_Type
     with
       Inline => True;
   -- Track the last move that has been played on the chessboard
   --
   -- Returns
   --    The last move played, or an Empty_Move if no moves has been played yet
   -- Aspects
   --    Inline


   ---------------------
   -- Mirror position --
   ---------------------

   procedure Mirror_Position (Chessboard : in out Chessboard_Type);
   -- Mirror the current position.
   --
   -- Arguments
   --    Chessboard : the current chess game data

   ------------------
   -- Attacks data --
   ------------------

   type Attack_Type is
      record
         Origin : Square_Type; -- square where the attacks begins
         Piece  : Piece_Type;
      end record;
   --  pragma Pack (Attack_Type);

   type Attack_Array_Type is array (Natural range 1 .. 8) of Attack_Type;

   -- Collect all the attackers found --
   type Attack_Collection_Type is
      record
         Attacker            : Attack_Array_Type;
         Number_Of_Attackers : Natural;
      end record;
   --  pragma Pack (Attack_Collection_Type);

   -- Attacks functions answer this question:
   -- Does side "Side" attacks square "Square"?
   -- Returns True if the answer is Yes, false otherwise.
   function Attacks
     (Chessboard : in Chessboard_Type; Side : in Color_Type; Square : in Square_Type)
      return Boolean
     with
       Pre => Chessboard.Square (Square) /= Frame;
   -- Detect if the given side attacks the given square, i.e. if any piece of
   -- the given side can reach that square. Note that the piece attacking the
   -- square, if any, might also be absolutely pinned and still controlling the
   -- target square
   --
   -- Arguments
   --    Side   : The side to test if a piece attacks the target square
   --    Square : The target square, to look if it is attacked
   -- Returns
   --    True if any piece attacks/control the Square, False otherwise
   -- Aspects
   --    The Square shall be a valid square

   -- Lookup the square in which stands the attacking piece.
   -- Optionally, it can stop either if a first attacker has found
   -- or it can collect all the attackers on that square.
   function Attacking_Square
     (Chessboard : in Chessboard_Type;
      Side       : in Color_Type;
      Square     : in Square_Type;
      Only_One   : in Boolean)
      return Attack_Collection_Type
     with
       Pre => Chessboard.Square (Square) /= Frame;
   -- Scan the chessboard to collect any piece of the given side attacking a
   -- certain square. If requested, it will stop the scan once a first attack is
   -- found
   --
   -- Arguments
   --    Side     : The side to test if a piece attacks the target square
   --    Square   : The target square, too look if it is attacked
   --    Only_One : If True, it will stop searching after a first attacks is
   --               found. If False, it will collect all the attacks
   -- Returns
   --    The collection of attacks found

   function Attacking_Piece
     (Chessboard : in Chessboard_Type;
      Side       : in Color_Type;
      Square     : in Square_Type;
      Only_One   : in Boolean)
      return Attack_Collection_Type renames Attacking_Square;
   -- This routine is a renaming for Attacking_Square, used for readability.
   -- When a routine scan the board checking if a square is under attack, the
   -- Attacking_Square will be used. When a routine scan the board checking
   -- whether a specific Piece is threatend, this routing will be called.
   --
   -- Arguments
   --    Side     : The side to test if a piece attacks the target square
   --    Square   : The target square, too look if it is attacked
   --    Only_One : If True, it will stop searching after a first attacks is
   --               found. If False, it will collect all the attacks
   -- Returns
   --    The collection of attacks found

   function Defending_Square
     (Chessboard : in Chessboard_Type;
      Side       : in Color_Type;
      Square     : in Square_Type)
      return Attack_Collection_Type;
   -- Find all the attackers to a given square (to do this, delegates the job to
   -- the Attacking_Square routin) but keeps only attacks from a King and from
   -- pieces not under an absolute Pin.
   -- To be used in Static Exchange Evaluation
   --
   -- Arguments
   --    Side     : The side to test if a piece attacks the target square
   --    Square   : The target square, too look if it is attacked
   --    Only_One : If True, it will stop searching after a first attacks is
   --               found. If False, it will collect all the attacks
   -- Returns
   --    The collection of attacks found


   -------------------
   -- King in Check --
   -------------------

   function Has_King_In_Check
     (Chessboard : in Chessboard_Type;
      Side       : in Color_Type)
      return Boolean
     with
       Inline => True;
   -- Check whether the King of a specific Side is under check or not
   --
   -- Arguments
   --    Side : The Side to be checked whether the King is under check
   -- Returns
   --    True if Side has the King in check, False otherwhise
   -- Aspects
   --    Inline

   function White_Has_King_In_Check
     (Chessboard : in out Chessboard_Type)
      return Boolean
     with
       Inline => True;
   -- Check whether the White King is in check. This is essentially a shortcut
   -- for Has_King_In_Check with a White side selected
   --
   -- Returns
   --    True if White King is under check, False otherwise

   function Black_Has_King_In_Check
     (Chessboard : in out Chessboard_Type)
      return Boolean
     with
       Inline => True;
   -- Check whether the Black King is in check. This is essentially a shortcut
   -- for Has_King_In_Check with a Black side selected
   --
   -- Returns
   --    True if Black King is under check, False otherwise

   function Absolute_Pin_Direction (Chessboard : in Chessboard_Type; Square : in Square_Type) return Direction_Type
     with
       Inline => True,
       Pre => Chessboard.Square (Square) not in Frame | Empty | King_Type;
   -- Detect the direction where an absolute pin has origin, in reference to the
   -- given square. If the piece standing on the given Square is no pinned (or
   -- not with an absolute pin) the No_Direction will be returned.
   --
   -- Arguments
   --    Square : The square where the piece is located, to which check the
   --             absolute pin condition and find the absolute pin direction
   -- Returns
   --    The direction of the attack when an absolute pin subsist, or No_Direction
   -- Aspects
   --    Inline
   --    Precondition : The given square is a valid, not empty square and occupied
   --                   by any piece that is not the King

   function Piece_Is_Absolute_Pinned (Chessboard : in Chessboard_Type; Square : in Square_Type) return Direction_Type
     with
       Inline => True,
       Pre => Chessboard.Square (Square) not in Empty | Frame;
   -- Similar to Absolute_Pin_Direction, check the pin from the opponent side
   -- perspective


   function Move_Leaves_King_In_Check
     (Chessboard : in out Chessboard_Type;
      Move       : in Move_Type)
      return Boolean
     with
       Pre => Move.Piece /= Empty;
   -- Detect if the given move will leave the own King under check. This routine
   -- is part of the legality test in move generation. If the piece moving is
   -- the King itslef, the target square should not be attacked by the oppoent
   -- pieces. If the piece moving is any other, the routine will scan the board
   -- for attackers to the King
   --
   -- Arguments
   --    Move : The move
   -- Returns
   --    True if the move will leave the own King under check, False otherwise
   -- Aspects
   --    Precondition : The moved piece is a valid piece

   function Move_Checks_Opponent_King
     (Chessboard : in out Chessboard_Type; Move : in Move_Type)
      return Check_Type
     with
       Pre => Move.Piece not in Frame | Empty;
   -- Detect if a move will check the opponent King and, if so, investigate
   -- further to find which kind of check is it.
   --
   -- Arguments
   --    Move : The move
   -- Returns
   --    The type of check the move will give to the opponent King
   -- Aspects
   --    Preconditions : The piece moved is a valid piece

   ----------------
   -- Directions --
   ----------------

   function Find_Sliding_Direction
     (Origin, Destination : in Square_Type)
      return Direction_Type
     with
       Inline => True,
       Pre => (Origin /= Destination);
   -- Retrieve the sliding direction connecting two squares. The two squares are
   -- connected, from a sliding perspective, if they are on the same raw or on
   -- the same column, or diagonal or anti-diagonal. In one of these cases, the
   -- direction is retrieved (like north/south, east/west, and so on).
   --
   -- Arguments
   --    Origin      : The origin square
   --    Destination : The destination square
   -- Returns
   --    The sliding direction between the given square, if connected for
   --    sliding, or No_Direction if not.
   -- Aspects
   --    Inline
   --    Precondition : The two square are not the same square

   type Direction_Table_Type is array (Board_Type'Range, Board_Type'Range) of Direction_Type;
   Directions_Table : Direction_Table_Type;

   procedure Preload_Sliding_Direction;
   -- Preload all the possible sliding direction in the directions table. This
   -- table is fillend once at startup.


   --------------------------------
   -- Move parsing and notations --
   --------------------------------

   function Parse_Move (Chessboard : in out Chessboard_Type; Input : in String) return Move_Type;
   -- Read a move as input and try to interpretate it as a move. The input
   -- can be given in any of the notation that AdaChess understand.
   --
   -- Arguments:
   --    Input      : A string representing a move
   -- Returns:
   --    The move data, or an Empty_Move if no corrispondence is found

   function Detect_Ambiguous_Move_Notation
     (Chessboard : in out Chessboard_Type; Move : in Move_Type)
      return Ambiguous_Flag_Type;
   -- Resolve conflict in the Standard Algebraic notation, if any. This function
   -- will only detect if a conflict exists and, in case, which one.
   --
   -- Arguments:
   --    Move       : The move that must be verify
   -- Returns:
   --    The ambiguity found, if any


   ----------------
   -- Exceptions --
   ----------------

   Invalid_Castle_Move : exception;
   -- This exception occur if a move has the castle flag turned on but the move
   -- is not a castle


end Chess.Engine;
