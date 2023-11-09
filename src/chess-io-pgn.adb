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


with Ada.Characters.Handling;

with String_Lib; use String_Lib;


package body Chess.IO.Pgn is

   ---------------
   -- To_Header --
   ---------------

   function To_Header (Source : in String) return Header_String is
      Header : Header_String := (others => Whitespace);
   begin
      if Source'Length <= Header'Length then
         Header (Header'First .. Header'First + Source'Length - 1) := Source;
      elsif Source'Length > Header'Length then
         Header := Source (Source'First .. Source'First + Header'Length - 1);
      end if;
      return Header;
   end To_Header;


   ---------------
   -- Is_Header --
   ---------------

   function Is_Header (Source : in String) return Boolean is

      function Parse_Token (Str : in String) return Character is
         S : constant String := Trim (Source => Str, Side => Left);
      begin
         if S'Length = 0 then
            return Whitespace;
         end if;
         return S (S'First);
      end Parse_Token;

      Token : constant Character := Parse_Token (Str => Source);

   begin
      return Token = Token_Header_Start;
   end Is_Header;


   ------------------------------
   -- Extract_Header_Info_Type --
   ------------------------------

   function Extract_Header_Info_Type (Source : in String) return Header_Info_Type is

      function Uppercase (Source : in String) return String renames
        Ada.Characters.Handling.To_Upper;

      First : constant Natural := Index_Of
        (Source => Source, Delimiter => Token_Header_Start);
      Last  : constant Natural := Index_Of
        (Source => Source (First + 1 .. Source'Last), Delimiter => Whitespace);

   begin
      for Info in Header_Info_Type loop
         if Uppercase (Source (First + 1 .. Last - 1)) = Uppercase (Info'Image) then
            return Info;
         end if;
      end loop;
      return Unknown;
   end Extract_Header_Info_Type;


   --------------------
   -- Extract_Header --
   --------------------

   function Extract_Header (Source : in String) return Header_String is
      First : constant Natural := Index_Of
        (Source => Source, Delimiter => Token_Quote);
      Last  : constant Natural := Index_Of
        (Source => Source (First + 1 .. Source'Last), Delimiter => Token_Quote);
   begin
      return To_Header (Source (First + 1 .. Last - 1)); -- Header is inside tokens
   end Extract_Header;


   ----------------------
   -- Format_Pgn_Match --
   ----------------------

   procedure Format_Pgn_Match (Match : in out String) is
      Comment_Zone_Start   : Natural := Match'First;
      Comment_Zone_End     : Natural := Match'First;
      Variation_Zone_Start : Natural := Match'First;
      Variation_Zone_End   : Natural := Match'First;
      Index                : Natural := Match'First;
      Nested_Variation     : Natural := 0;
   begin

      -- Remove any new line feed and/or signs
      for I in Match'Range loop
         if Match (I) = ASCII.LF or else Match (I) = ASCII.CR then
            Match (I) := Whitespace;
         end if;
         if Match (I) = '!' or else Match (I) = '?' then
            Match (I) := Whitespace;
         end if;
      end loop;

      -- Clear comment, if any
      loop
         exit when Index = Match'Last;
         if Match (Index) = Token_Comment_Start then
            Comment_Zone_Start := Index;
         end if;
         if Match (Index) = Token_Commend_End then
            Comment_Zone_End := Index;
            Match (Comment_Zone_Start .. Comment_Zone_End) := (others => Whitespace);
         end if;

         Index := Index + 1;

      end loop;

      Index := Match'First;

      -- Clean variation inside the pgn. Note that in a chess match variations
      -- can be nested. It is common to find them especially when a pgn is
      -- written with comments from GM.
      loop
         exit when Index = Match'Last;

         -- First case, a new variation is going to be started. We just sign
         -- the point where
         if Match (Index) = Token_Variation_Start then
            if Nested_Variation = 0 then
               Variation_Zone_Start := Index;
            end if;
            Nested_Variation := Nested_Variation + 1;
         end if;

         if Match (Index) = Token_Variation_End then
            Nested_Variation := Nested_Variation - 1;
            if Nested_Variation = 0 then
               Variation_Zone_End := Index;
               Match (Variation_Zone_Start .. Variation_Zone_End) := (others => Whitespace);
            end if;
         end if;

         Index := Index + 1;

      end loop;

      if Nested_Variation > 0 then
         raise Invalid_Pgn_Data;
      end if;

      -- Replace dots with spaces
      for Char of Match loop
         if Char = Dot then
            Char := Whitespace;
         end if;
      end loop;

   end Format_Pgn_Match;


end Chess.IO.Pgn;
