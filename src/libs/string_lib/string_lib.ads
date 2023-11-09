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


package String_Lib is

   
   --------------------------------
   -- Common useful placeholders --
   --------------------------------

   Whitespace : constant Character := ' ';
   Pipe       : constant Character := '|';
   Semicolon  : constant Character := ';';
   Dash       : constant Character := '-';
   
   Empty_String : constant String := "";
   

   function Count_Tokens (Source : in String; Delimiter : in Character) return Natural;
   -- Parse the give string and count how many token can be extracted. The
   -- tokens are separated by the given delimiter. Multiple delimiters will not
   -- be treated as a single one, i.e. will not result in multiple token count.
   --
   -- Arguments
   --    Source    : The string to parse
   --    Delimiter : The delimiter to use to separate tokens
   -- Returns
   --    The number of tokens found
   
   function Has_Token (Source : in String; Delimiter : in Character := Whitespace) return Boolean;
   -- Parse the given Source to find and return a token.
   --
   -- Arguments:
   --    Source    : The input string to parse
   --    Delimiter : The character to search in the Source
   -- Returns:
   --    True if a token exists, False if not


   --     function Count_Tokens (Source : in String; Delimiter : in Character) return Natural;
   -- Count the number of tokens in the given string. Multiple consecutive
   -- delimiters are treated as a single delimiter.
   -- The behavior consider only the slices between delimiters. An empty input
   -- will result in a 0 token count. Same for an input with only delimiters
   -- on it. An input with no delimiter is a single token. Other cases are
   -- counted as described.
   --
   -- Arguments:
   --    Source    : The given input data to count tokens
   --    Delimiter : The token separator to be used
   -- Returns:
   --    The number of tokens found.

   function Extract_Token_At (Source : in String; Token_Number : in Positive; Delimiter : in Character) return String;
   -- Extracht the Nth token from the Source string and return it. The tokens
   -- are delimited by a specified delimiter
   --
   -- Arguments
   --    Source       : The input string to be parsed
   --    Token_Number : The chunk of the item to be extract
   --    Delimiter    : The character that will split the input source into chunks
   -- Returns
   --    The Token at the specified position
   -- Exceptions
   --    Not_Enough_Tokens : raised if token number exceed the token count
   
   
   function Extract_Token (Source : in String; Delimiter : in Character := Whitespace) return String
     with
       Pre => Has_Token (Source),
       Post => Extract_Token'Result'Length > 0;
   -- Read a string as input and return the slice between the begining
   -- and the first delimiter found. If delimiter is not found, the entire
   -- Source is returned as the only Token.
   --
   -- Arguments
   --    Source    : The input string to parse
   --    Delimiter : The character that delimit the token
   -- Returns
   --    The slice of the string corresponding to the token
   -- Aspects
   --    Preconditions  : The source string contains at least one token
   --    Postconditions : The output token is not an empty string
   
   function Extract_Last_Token (Source : in String; Delimiter : in Character := Whitespace) return String
     with
       Pre => Has_Token (Source),
       Post => Extract_Last_Token'Result'Length > 0;
   -- Similar to Extract_Token, but works in a reverse way.
   --
   -- Arguments
   --    Source    : The input string to parse
   --    Delimiter : The character that delimit the token
   -- Returns
   --    The slice of the string corresponding to the token
   -- Aspects
   --    Preconditions  : The source string contains at least one token
   --    Postconditions : The output token is not an empty string
   

   ----------
   -- Trim --
   ----------

   type Trim_Side_Type is (Left, Right, Both);

   function Trim (Source    : in String;
                  Delimiter : in Character := Whitespace;
                  Side      : in Trim_Side_Type := Both) return String;
   -- Cut the side of the input string to remove any delimiter found.
   --
   -- Arguments:
   --    Source    : The input string to trim
   --    Delimiter : The character to isolate
   --    Side      : Information about which side to trim
   -- Returns:
   --     The slice of the string with trimmed side

   function Index_Of (Source : in String; Delimiter : in Character) return Natural;
   -- Scan a Source string to find an occurrence of the Delimiter. If the
   -- Delimiter is found, its index will be returned. If not, an exception will
   -- be raised. The function returns the first delimiter found.
   --
   -- Arguments:
   --    Source    : The input string to search the delimiter
   --    Delimiter : The character to search inside the Source string
   -- Returns:
   --    The index of the first Delimiter found.
   -- Exceptions:
   --     Item_Not_Found : raised if the given Source does not contain
   --                      the expected Delimiter
   -- Aspects
   --    
   
   procedure Replace (Source : in out String; Char : in Character; Replace_With : in Character);

   function Is_Empty (Source : in String) return Boolean
     with
       Inline => True;
   -- Test a string to check if it is an empty string. A string
   -- is considered empty if contains no character or contains only
   -- white spaces

   ----------------
   -- Exceptions --
   ----------------

   Delimiter_Not_Found : exception;
   Token_Not_Found     : exception;
   Not_Enough_Tokens   : exception;

private


   


   

   
   
   
end String_Lib;
