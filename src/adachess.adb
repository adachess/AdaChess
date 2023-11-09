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
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Strings.Bounded;
with Ada.Containers.Vectors;
with Ada.Dispatching;

with Ada.Command_Line;      use Ada.Command_Line;

with Chess;
with Chess.Colors;        use Chess.Colors;
with Chess.Moves;         use Chess.Moves;
with Chess.Notations;     use Chess.Notations;
with Chess.Protocols;     use Chess.Protocols;
with Chess.Nodes;         use Chess.Nodes;
with Chess.Depths;        use Chess.Depths;
with Chess.Info;
with Chess.Score;         use Chess.Score;
with Chess.Clocks;        use Chess.Clocks;

with Chess.Engine;                     use Chess.Engine;
with Chess.Engine.Ponder;              use Chess.Engine.Ponder;
with Chess.Engine.Transposition_Table; use Chess.Engine.Transposition_Table;
with Chess.Engine.Thinking;            use Chess.Engine.Thinking;
with Chess.Engine.Search;              use Chess.Engine.Search;
with Chess.Engine.Search.PV;           use Chess.Engine.Search.PV;
with Chess.Engine.See;                 use Chess.Engine.See;
with Chess.Engine.PV;                  use Chess.Engine.PV;
with Chess.Engine.Evaluations;         use Chess.Engine.Evaluations;
with Chess.Engine.Evaluations.Static_Evaluations; use Chess.Engine.Evaluations.Static_Evaluations;

with Chess.Engine.Perfts;              use Chess.Engine.Perfts;

with Chess.IO;     use Chess.IO;
with Chess.IO.Fen;
with Chess.IO.Consoles;

with Chess.Books;          use Chess.Books;
with Chess.Books.Openings; use Chess.Books.Openings;


with ACMultiprocessor;

--  with Chess.Engine.Tuning; use Chess.Engine.Tuning;

with String_Lib;


procedure AdaChess is
   use String_Lib;

   -----------------
   -- System_Info --
   -----------------

   procedure System_Info is
      use ACMultiprocessor;
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Number of CPU detected:" & CPU_Range'Image (Number_Of_CPUs));
      Ada.Text_IO.New_Line;
   end System_Info;


   ------------
   -- Prompt --
   ------------

   procedure Prompt is
      function Version return String renames Chess.Info.Version;
   begin
      Ada.Text_IO.Flush;
      Ada.Text_IO.Put ("AdaChess v." & Version & " => ");
      Ada.Text_IO.Flush;
   end Prompt;


   -----------
   -- Usage --
   -----------

   procedure Usage is
      Name    : constant String := Chess.Info.Engine_Name;
      Author  : constant String := Chess.Info.Author;
      Version : constant String := Chess.Info.Version;
      Email   : constant String := Chess.Info.E_Mail;
      Site    : constant String := Chess.Info.Site;
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        (Name & " is a chess engine written by " & Author & "." & ASCII.LF
         & "Current release is n." & Version & ASCII.LF & ASCII.LF
         & "Visit " & Site & " for informations on how to connect " & Name & ASCII.LF
         & "to an external GUI (like Winboard or Arena)." & ASCII.LF
         & "Write to " & Email & " for comments and report bug reports!" & ASCII.LF & ASCII.LF
         & "Enjoy, share, have fun!");
      Ada.Text_IO.New_Line;
   end Usage;


   -----------------
   -- Engine_Info --
   -----------------

   function Engine_Info return String is
     (Chess.Info.Engine_Name & " " & Chess.Info.Version & " - " & Chess.Info.Engine_Motto);


   -----------------
   -- Resign data --
   -----------------

   Resign_Status : Natural := 0;


   Forcemode : Boolean;
   -- Forcemode is a command give from the xboard communication protocol to
   -- prevent the engine doing anything

   ------------
   -- Ponder --
   ------------

   Ponder            : Ponder_Mode := Off;
   Ponder_Move       : Move_Type := Empty_Move;
   Ponder_Hit        : Ponder_Guess := Miss;

   -----------------
   -- Console I/O --
   -----------------

   Command         : Protocol_Command_Type := Noop;
   Console_Input   : String (1 .. 128);
   Last            : Natural := 1;

   function Input     return String is (Console_Input (1 .. Last));

   ---------------------
   -- Extract_Command --
   ---------------------

   function Extract_Command (Source : in String) return String is
   begin
      return Extract_Token (Source => Source, Delimiter => Whitespace);
   end Extract_Command;

   ---------------
   -- Parameter --
   ---------------

   function Parameter return String is
      Source : constant String := Input;
      Tokens : constant Natural := Count_Tokens (Source, Whitespace);
      Index  : Natural;
   begin
      if Tokens > 1 then
         Index := Index_Of (Source, Whitespace);
         return Trim (Source (Index .. Source'Last), Whitespace, Both);
      end if;
      return Empty_String;
   end Parameter;



   ----------------
   -- Chessboard --
   ----------------

   Chessboard : Chessboard_Type;
   Ply : Depth_Type renames Chessboard.Ply;

   ------------
   -- Engine --
   ------------

   Engine_Side  : Color_Type := Black;
   Score        : Score_Type := 0;
   Move         : Move_Type;
   Memory_In_Mb : Natural := 256;

   -- AdaChess trigger the engine search via a separate thread. In this way, the
   -- main thread (this) can still receive commands and elaborate them in the
   -- meanwhile. Note that the clock is also implemented into a separate thread.

   task type Thinking_Task is
      entry Think (Chessboard : Chessboard_Type);
      entry Ponder (Chessboard : Chessboard_Type; Ponder_Move : Move_Type);
      entry Interrupt_Ponder;
      entry Answer (Selected_Move : out Move_Type);
   end Thinking_Task;

   task body Thinking_Task is
      Move          : Move_Type := Empty_Move;
      Current_Match : Chessboard_Type;
      Pondering     : Boolean;
   begin

      loop

         Pondering := False;

         select
            accept Think (Chessboard : Chessboard_Type) do
               Current_Match := Chessboard;
            end Think;

            Clock.Start;
            Move := Think (Current_Match);
         or
            accept Ponder (Chessboard : Chessboard_Type; Ponder_Move : Move_Type) do
               Current_Match := Chessboard;
               Move := Ponder_Move;
               Pondering := True;
               Wait_Pondering_Answer := True;
            end Ponder;

            --  Current_Match.Generate_Moves;
            Clock.Start;
            Current_Match.Generate_Moves;
            Current_Match.Play (Move);
            Ponder (Current_Match);
            Move := Principal_Variation (Zero_Depth).Main_Line (Zero_Depth).Move;
            Wait_Pondering_Answer := False;

         or
            accept Interrupt_Ponder;

         or
            accept Answer (Selected_Move : out Move_Type) do
               Selected_Move := Move;
            end Answer;
            Clock.Stop;

            if Pondering then
               Pondering := False;
            end if;
         or
            terminate;
         end select;
      end loop;
   end Thinking_Task;

   Engine : Thinking_Task;

   Print_Console_Logo : Boolean := False;

   Argc               : Natural;

begin

   Ada.Text_IO.Put_Line (Engine_Info);

   -----------------------
   -- Load sliding data --
   -----------------------

   Preload_Sliding_Direction;

   -----------------------------------
   -- Parse command line parameters --
   -----------------------------------

   if Argument_Count > 0 then
      Argc := 1;
      while Argc <= Argument_Count loop
         if Argument (Argc) in "-n" | "--notation" then
            if Argc + 1 <= Argument_Count then
               if Argument (Argc + 1) = "winboard" then
                  Default_Notation := Pure_Algebraic;
               elsif Argument (Argc + 1) = "san" then
                  Default_Notation := Standard_Algebraic;
               elsif Argument (Argc + 1) = "lan" then
                  Default_Notation := Long_Algebraic;
               elsif Argument (Argc + 1) = "iccf" then
                  Default_Notation := ICCF;
               else
                  Ada.Text_IO.Put_Line ("Notation " & Argument (Argc + 1)  & " is not available in AdaChess");
               end if;
               Argc := Argc + 1;
            end if;

         elsif Argument (Argc) in "-m" | "--hash" then
            if Argc + 1 <= Argument_Count then
               Ada.Text_IO.Put_Line ("Reserve memory for hashing:" & Integer'Image (Integer'Value (Argument (Argc + 1))));
               Argc := Argc + 1;
            end if;

         elsif Argument (Argc) = "--system-info" then
            System_Info;

         elsif Argument (Argc) = "--easter-egg" then
            Print_Console_Logo := True;

         elsif Argument (Argc) = "--opening-book" then
            if Argc + 1 <= Argument_Count then
               Ada.Text_IO.Put_Line ("Adding opening book: " & Argument (Argc + 1));
               Add_Opening_Book (Book_File_Name => Argument (Argc + 1));
               Argc := Argc + 1;
            end if;

         elsif Argument (Argc) = "--pgn-as-book" then
            if Argc + 1 <= Argument_Count then
               Load_All_Books (Argument (Argc + 1));
               Argc := Argc + 1;
            else
               Load_All_Books;
            end if;

         elsif Argument (Argc) = "--convert-book" then
            if Argc + 1 <= Argument_Count then
               Add_Opening_Book (Book_File_Name => Argument (Argc + 1));
               Load_All_Opening_Books;
               Save_As_Simple_Book (Argument (Argc + 1) & ".txt");
            end if;

         elsif Argument (Argc) = "--multi-pv" then
            if Argc + 1 <= Argument_Count then
               Multi_Pv_Command_Line_Block : declare
                  Amount : constant Positive := Positive'Value (Argument (Argc + 1));
               begin
                  Multi_Pv := Amount;
                  Ada.Text_IO.Put_Line ("Using" & Positive'Image (Multi_Pv) & " PV lines");
               end Multi_Pv_Command_Line_Block;
               Argc := Argc + 1;
            end if;

         elsif Argument (Argc) = "--resign" then
            Resign_Mode := True;

         end if;

         Argc := Argc + 1;
      end loop;
   end if;

   ------------------
   -- Console Logo --
   ------------------

   if Print_Console_Logo then
      Ada.Text_IO.New_Line (2);
      Ada.Text_IO.Put_Line (Chess.Info.Console_Logo);
      Ada.Text_IO.New_Line (2);
   end if;

   -------------------------
   -- Data Initialization --
   -------------------------

   Chessboard.Initialize;
   Initialize_Search_Engine;
   Initialize_Evaluation_Engine;


   ------------------------
   -- Load opening books --
   ------------------------

   Load_All_Opening_Books;
   if Book_Entries > 0 then
      Ada.Text_IO.Put_Line ("Total" & Natural'Image (Book_Entries) & " opening book entries");
   end if;


   Engine_Side := Black;
   Forcemode := True;


   --------------------------
   -- Initialize the Clock --
   --------------------------

   Clock.Set_Fixed_Time_Per_Move (7.0);


   while True loop

      Clear_Transposition_Table;

      if not Pondering then
         Clear_All_Euristics;
         Clear_Principal_Variation;
      end if;

      ---------------
      -- Pondering --
      ---------------

      if Chessboard.Side_To_Move = Engine_Side and then Forcemode = False then
         -- In case the move we are pondering is the right guess, continue
         -- to search until the time has expired

         if Resign_Mode and then Resign_Status >= Resign_Threshold then
            Ada.Text_IO.Put_Line ("(resign)");
            Resign_Status := 0;
         else

            if Ponder = On and then Pondering then
               Pondering := False;

               -- Ada.Text_IO.Put_Line ("Engine to move. Ponder = " & Ponder_Guess'Image (Ponder_Hit));

               while Wait_Pondering_Answer loop
                  Ada.Dispatching.Yield;
               end loop;

               if Ponder_Hit = Hit then
                  -- Ada.Text_IO.Put_Line ("Extract the move found in pondering");
                  Engine.Answer (Move);
                  -- Ada.Text_IO.Put_Line ("Found move " & Chess.IO.Move_To_String (Move));
               else
                  Clock.Interrupt;
                  Engine.Interrupt_Ponder;
                  Clear_Transposition_Table;
                  Clear_Principal_Variation;
                  Clear_All_Euristics;
                  -- Ada.Text_IO.Put_Line ("Run thinking again");
                  Engine.Think (Chessboard);
                  Engine.Answer (Move);
               end if;

            else
               -- Ada.Text_IO.Put_Line ("Run normal thinking");
               Engine.Think (Chessboard);
               Engine.Answer (Move);
            end if;
         end if;

         if Resign_Mode then
            if Principal_Variation (Zero_Depth).Evaluation.Score <= Resign_Threshold_Score then
               Resign_Status := Resign_Status + 1;
            else
               Resign_Status := 0;
            end if;
         end if;

         Chessboard.Play (Move);

         case Communication_Protocol is
            when Winboard =>
               Ada.Text_IO.Put ("move ");
               Print_Move (Move, Pure_Algebraic);
            when Universal_Chess_Interface =>
               raise Chess.Not_Implemented with "UCI Protocol is not yet supported in AdaChess";
            when No_Gui_Connection =>
               Ada.Text_IO.Put ("move ");
               Print_Move (Move, Default_Notation);
         end case;

         Ada.Text_IO.New_Line;
         Ada.Text_IO.Flush;

         Pondering := False;
         Ponder_Move := Empty_Move;

         if Ponder = On then
            -- Verify that the prediceted countermove is a valid move. This
            -- step is required because the aspiration window in search might
            -- fails and the pv change before the countermove is updated
            Chessboard.Generate_Moves;
            for I in Chessboard.Moves_Pointer (Ply) .. Chessboard.Moves_Pointer (Ply + 1) - 1 loop
               if Chessboard.Moves_Stack (I) = Principal_Variation (Zero_Depth).Predicted_Countermove then
                  Ponder_Move := Principal_Variation (Zero_Depth).Predicted_Countermove;
               end if;
               exit when Ponder_Move /= Empty_Move;
            end loop;
         end if;

         Clear_Principal_Variation;

         if Ponder_Move /= Empty_Move and then Ponder_Move.Check /= Checkmate then
            Pondering := True;
            Engine.Ponder (Chessboard => Chessboard, Ponder_Move => Ponder_Move);
         end if;

      end if;

      --------------------
      -- Generate Moves --
      --------------------

      Chessboard.Generate_Moves;

      ------------
      -- Prompt --
      ------------

      if Communication_Protocol = No_Gui_Connection then
         Prompt;
      end if;


      ----------------------------------
      -- Read user input and parse it --
      ----------------------------------

      Command := Noop;

      Ada.Text_IO.Get_Line (Console_Input, Last);
      if Trim (Console_Input (1 .. last))'Length > 0 then
         Command := Parse_Input (Extract_Command (Console_Input (1 .. Last)));
      end if;

      ---------------------------------------
      -- Scan input and trigger the engine --
      ---------------------------------------

      case Command is
         when Noop =>
            -- Sometimes Xboard sends a new line to the engine
            -- Just skip this command
            null;

         when Quit | Exit_Command =>
            Clock.Stop;

         when Xboard =>
            Communication_Protocol := Winboard;
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Flush;

         when Uci =>
            Communication_Protocol := Universal_Chess_Interface;
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Flush;

         when Question_Mark =>
            Forcemode := False;

         when Protover =>
            Ada.Text_IO.Put_Line ("feature myname=""AdaChess""");
            Ada.Text_IO.Put_Line ("feature memory=1");
            Ada.Text_IO.Put_Line ("feature colors=0");
            Ada.Text_IO.Put_Line ("feature setboard=1");
            Ada.Text_IO.Put_Line ("feature sigint=0");
            Ada.Text_IO.Put_Line ("feature sigterm=0");
            Ada.Text_IO.Put_Line ("feature usermove=0");
            Ada.Text_IO.Put_Line ("feature random=1");
            Ada.Text_IO.Put_Line ("feature option=""Resign -check 0""");
            Ada.Text_IO.Put_Line ("feature san=0"); -- AdaChess DOES support SAN but the Arena GUI has a bug and send wrong SAN when a king in under check
            Ada.Text_IO.Flush;
            Ada.Text_IO.Put_Line ("feature done=1");
            Ada.Text_IO.Flush;

         when Accepted =>
            null;

         when New_Game =>
            Chessboard.Initialize;
            Init_Transposition_Table (Memory_In_Mb);
            Clear_All_Euristics;
            Chessboard.Update_Hash;
            Engine_Side := Black;
            Forcemode := False;
            Random_Mode := False;
            Resign_Status := 0;
            Ponder := Off;
            Ponder_Move := Empty_Move;
            Open_Book;

         when Random =>
            Random_Mode := True;

         when Go =>
            Forcemode := False;
            Engine_Side := Chessboard.Side_To_Move;

         when White =>
            -- Note: This command is deprecated in Winboard protocol v.2
            Engine_Side := Black;

         when Black =>
            -- Note: This command is deprecated in Winboard protocol v.2
            Engine_Side := White;

         when Force =>
            Forcemode := True;

         when Easy =>
            Ponder := Off;

         when Hard =>
            Ponder := On;

         when Post =>
            Principal_Variation_Post := True;

         when Nopost =>
            Principal_Variation_Post := False;

         when Undo =>
            --  Ply := Ply + 1; -- Adjust for manual undo
            Chessboard.Undo;
            Forcemode := True;
            if Pondering then
               Pondering := False;
            end if;

         when Level =>
            Level_Block : declare
               Moves     : Natural;
               Minutes   : Natural;
               Increment : Natural;
               Level_Parameter : constant String := Parameter; -- Calculate Parameter once
            begin
               Moves     := Natural'Value (Extract_Token_At (Source => Level_Parameter, Token_Number => 1, Delimiter => ' '));
               Minutes   := Natural'Value (Extract_Token_At (Source => Level_Parameter, Token_Number => 2, Delimiter => ' '));
               Increment := Natural'Value (Extract_Token_At (Source => Level_Parameter, Token_Number => 3, Delimiter => ' '));
               Clock.Set_Level (Moves => Moves, Minutes => Minutes, Increment => Increment);
            exception
               when E : others =>
                  Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E));
                  Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
                  Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
            end Level_Block;

         when Time =>
            Time_Block    : declare
               Time_Left     : constant Duration := Duration'Value (Parameter) / 100.0;
               Moves_Played  : constant Natural := Chessboard.History_Ply / 2;
            begin
               Clock.Update_Time_Left
                 (Time_Left => Time_Left, Moves => Moves_Played);
            end Time_Block;

         when Otim =>
            Clock.Update_Opponent_Time_Left (Duration'Value (Parameter) / 100.0);

         when St =>
            Time_Per_Move_Block : declare
               Time_Per_Move : constant Duration := Duration'Value (Parameter);
            begin
               Clock.Set_Fixed_Time_Per_Move (Time_Per_Move);
            end Time_Per_Move_Block;

         when Sd =>
            Fixed_Search_Depth_Block    : declare
               Depth_Limit : constant Depth_Type := Depth_Type'Value (Parameter);
            begin
               Clock.Set_Max_Search_Depth (Depth_Limit => Depth_Limit);
            end Fixed_Search_Depth_Block;

         when Memory =>
            Memory_Block : begin
               Memory_In_Mb := Natural'Value (Parameter);
               if Memory_In_Mb > Megabyte'Last then
                  Memory_In_Mb := Megabyte'Last;
               end if;
            exception
               when others =>
                  Ada.Text_IO.Put_Line ("Invalid memory size given! Value of" & Natural'Image (Memory_In_Mb) & "MB will be used");
            end Memory_Block;
            Init_Transposition_Table (Size => Memory_In_Mb);

         when Setboard =>
            Forcemode := True;
            Chess.IO.Fen.From_String (Chessboard => Chessboard, Fen => Parameter);
            Close_Book;
            Resign_Status := 0;

         when Multipv =>
            Multi_Pv_Block : declare
               Amount : constant Positive := Positive'Value (Parameter);
            begin
               Multi_Pv := Amount;
               Ada.Text_IO.Put_Line ("Using" & Positive'Image (Multi_Pv) & " PV lines");
            end Multi_Pv_Block;

         when Perft =>
            Perft (Chessboard, Positive'Value (Parameter));

         when Divide =>
            Divide (Chessboard, Positive'Value (Parameter));

         when Bench | Benchmark =>
            declare
               Seconds : Duration := 30.0;
            begin
               if Parameter'Length > 0 then
                  Seconds := Duration'Value (Parameter);
               end if;

               Chess.IO.Consoles.Display_On_Console (Chessboard);
               --  Chessboard.Display_On_Console;
               Ada.Text_IO.Put_Line ("Benchmarking current displayed position for" &  Natural'Image (Natural (Seconds)) & " seconds");
               Ada.Text_IO.Flush;
               delay 1.0;
               Clock.Set_Fixed_Time_Per_Move (Seconds);
               Clock.Start;
               Move := Benchmark (Chessboard);
               Clock.Stop;
               Ada.Text_IO.Put_Line ("Total nodes :" & Node_Type'Image (Search_Nodes));
               Ada.Text_IO.Put_Line ("NPS:" & Node_Type'Image ((Search_Nodes / Node_Type (Seconds))));
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Flush;
            exception
               when others =>
                  Ada.Text_IO.Put_Line
                    ("Invalid Benchmark parameter. See usage");
            end;
            Open_Book;

         when Moves =>
            Chessboard.Generate_Moves;
            Moves_List_Block : declare
               First_Move : constant Natural := Chessboard.Moves_Pointer (Ply);
               Last_Move  : constant Natural := Chessboard.Moves_Pointer (Ply + 1) - 1;
            begin
               for I in First_Move .. Last_Move loop
                  Move := Chessboard.Moves_Stack (I);
                  Chess.IO.Print_Move (Move, Default_Notation);
                  if I < Last_Move then
                     Ada.Text_IO.Put (" ");
                  end if;
               end loop;
               Ada.Text_IO.New_Line;
            end Moves_List_Block;

         when Eval =>
            Score := Evaluate (Chessboard).Score;
            Ada.Text_IO.Put_Line ("Evaluation: " & Score_Type'Image (Score));

         when Evalall =>
            Chess.IO.Consoles.Display_On_Console (Chessboard);
            --  Chessboard.Display_On_Console;
            Generate_Moves (Chessboard);
            for I in Chessboard.Moves_Pointer (Ply) .. Chessboard.Moves_Pointer (Ply + 1) - 1 loop
               Move := Chessboard.Moves_Stack (I);
               Chessboard.Play (Move);
               Score := -Evaluate (Chessboard).Score;
               Chessboard.Undo;
               Ada.Text_IO.Put_Line (Move_To_String (Move) & ": score" & Score_Type'Image (Score));
            end loop;

         when Seetest =>
            declare
               See_Score : Score_Type;
            begin
               Chess.IO.Consoles.Display_On_Console (Chessboard);
               Chessboard.Generate_Moves;

               for I in Chessboard.Moves_Pointer (Ply) .. Chessboard.Moves_Pointer (Ply + 1) - 1 loop
                  Move := Chessboard.Moves_Stack (I);
                  Ada.Text_IO.Put ("Seeing " & Move_To_String (Move));
                  --  See := Static_Exchange_Evaluation (Chessboard, Move);
                  See_Score := Static_Exchange_Evaluation_Score (Chessboard, Move);
                  Ada.Text_IO.Put_Line (": " & Capture_Result_Type'Image ((if See_Score > 0 then Winning elsif See_Score = 0 then Equal else Losing)) & " capture with score" & (if See_Score >= 0 then "" else " ") & Score_Type'Image (See_Score));
                  Ada.Text_IO.Put_Line (Chess.IO.Fen.Fen_Save_To_String (Chessboard => Chessboard));

               end loop;
            end;

         when Mirror =>
            Chess.IO.Consoles.Display_On_Console (Chessboard);
            Ada.Text_IO.Put_Line (Chess.IO.Fen.Fen_Save_To_String (Chessboard => Chessboard));
            Chessboard.Mirror_Position;
            Chess.IO.Consoles.Display_On_Console (Chessboard);
            Ada.Text_IO.Put_Line (Chess.IO.Fen.Fen_Save_To_String (Chessboard => Chessboard));

         when Display =>
            Chess.IO.Consoles.Display_On_Console (Chessboard);

         when Fensave =>
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line (Chess.IO.Fen.Fen_Save_To_String (Chessboard => Chessboard));
            Ada.Text_IO.New_Line;

         when Invert =>
            if Chessboard.Side_To_Move = White then
               Chessboard.Side_To_Move := Black;
            else
               Chessboard.Side_To_Move := White;
            end if;

         when Usage =>
            Usage;

         when Usermove =>
            Move := Parse_Move (Chessboard, Input);
            Chessboard.Play (Move);
            if Pondering then
               Ponder_Hit := (if Ponder_Move = Move then Hit else Miss);
            end if;

         when Unknown =>
            -- Try if the input represent a valid move, by parsing it.
            Move := Parse_Move (Chessboard, Input);
            -- Note: The parser will try to detect input string as move by
            -- comparing it with any of the known notation. It doesn't matter
            -- which is the current notation set, the parser will still compare
            -- the input with all the available notation.
            -- If the notation support additional signs for checks, the input
            -- shall contains those signs. For example, if the input is Nd6 but
            -- the move is represented, internally, as Nd6+, the parser will not
            -- match it.
            if Move /= Empty_Move then
               Chessboard.Play (Move);

               if Pondering then
                  Ponder_Hit := (if Ponder_Move = Move then Hit else Miss);
               end if;
            end if;

         when others =>
            Ada.Text_IO.Put_Line ("Command " & Command'Image & " not implemented in AdaChess");

      end case;

      Ada.Text_IO.Flush;

      exit when Command = Quit;
   end loop;

   -- Should not be necessary, but in case, do it the hard way!
   abort Engine;

   Ada.Text_IO.Put_Line ("Thanks for playing with AdaChess!");

end AdaChess;
