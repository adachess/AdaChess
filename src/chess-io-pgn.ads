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


package Chess.IO.Pgn is

   Max_Match_Size : constant Natural := 16384;

   subtype Header_String is String (1 .. 128);
   subtype Match_String is String (1 .. Max_Match_Size); -- Can handle 300+ moves

   type Header_Info_Type is
     (Unknown, Event, Site, Date, Round, White, Black, Result);

   type Portable_Game_Notation_Match_Type is
      record
         Event          : Header_String := (others => ' ');
         ECO            : Header_String := (others => ' ');
         Moves_Sequence : Match_String := (others => ' ');
      end record;

   function To_Header (Source : in String) return Header_String;
   -- Normalize a generic string into a Header String valid for then PGN data.
   -- An input string that is shorter than an header will be filled with white
   -- spaces; an input string that is longer than an header will be trimmed.
   --
   -- Arguments
   --    Source : the string to be converted into a header
   -- Return
   --    An header with the given string

   function Is_Header (Source : in String) return Boolean
     with
       Pre => Source'Length > 0;
   -- Parse the given string and detect if it represent a header information
   -- inside a pgn file. It recognize any kind of header, no matter if AdaChess
   -- can process it or not.
   --
   -- Arguments
   --    Source : The string to parse
   -- Returns
   --    True if Source is an header information, False otherwise
   -- Aspects
   --    Precondition : The source is not empty

   function Extract_Header_Info_Type (Source : in String) return Header_Info_Type
     with
       Pre => Is_Header (Source);
   -- Read a string and try to detect which kind of header information are
   -- on it.
   --
   -- Arguments
   --    Source : The header string
   -- Aspects
   --    Precondition : The Source represent a PGN Header

   function Extract_Header (Source : in String) return Header_String
     with
       Pre => Is_Header (Source);
   -- Given a String representing a PGN header, this function will extract
   -- the information inside of it
   --
   -- Arguments
   --    Source : The header string
   -- Aspects
   --    Precondition : The Source represent a PGN Header

   procedure Format_Pgn_Match (Match : in out String);
   -- Format the PGN match
   --
   -- Arguments
   --    Match : A string containing the entire game match


   Dot        : constant Character := '.';

   Invalid_Pgn_Data : exception;

private

   Tokens : constant String := "[]{}*" & ASCII.Quotation; -- the '"'

   subtype Token_Type is Character;

   Token_Header_Start    : constant Token_Type := '[';
   Token_Header_End      : constant Token_Type := ']';
   Token_Comment_Start   : constant Token_Type := '{';
   Token_Commend_End     : constant Token_Type := '}';
   Token_Variation_Start : constant Token_Type := '(';
   Token_Variation_End   : constant Token_Type := ')';
   Token_Quote           : constant Token_Type := '"';

   Open_Tag  : constant Character := '[';
   Close_Tag : constant Character := ']';
   Quotes    : constant Character := '"';

end Chess.IO.Pgn;
