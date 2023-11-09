package body String_Lib is

   
   -------------------
   -- Extract_Token --
   -------------------
   
   function Extract_Token_At (Source : in String; Token_Number : in Positive; Delimiter : in Character) return String is
      Tokens   : Natural := 0;
      From, To : Natural;
   begin
      
      if Count_Tokens (Source => Source, Delimiter => Delimiter) < Token_Number then
         raise Not_Enough_Tokens;
      end if;
         
      From := Source'First;
      To := Source'Last;
      
      loop
         Slice_Block : declare
            S  : constant String := Trim 
              (Source => Source (From .. To), Delimiter => Delimiter, Side => Left);
         begin
            From := S'First;
            exit when To < From;
            From := From + Extract_Token (S, Delimiter)'Length;
            Tokens := Tokens + 1;
            if Tokens = Token_Number then
               return Extract_Token (S, Delimiter);
            end if;
         end Slice_Block;
      end loop;
      
      -- Should never pass from here, since we test for the token
      -- count in the beginning.
      raise Token_Not_Found;
      
   end Extract_Token_At;
   
   
   -------------------
   -- Extract_Token --
   -------------------
   
   function Extract_Token (Source : in String; Delimiter : in Character := Whitespace) return String is
      Last  : Natural := 0;
      Input : constant String := Trim (Source, Delimiter, Both);
   begin      
      Last := Index_Of (Input, Delimiter) - 1;
      return Input (Input'First .. Last);      
   exception
      when Delimiter_Not_Found =>
         -- When the Source has no delimiter, the entire Source is
         -- the Token itself. 
         return Input;
   end Extract_Token;
   
     
   --------------
   -- Index_Of --
   --------------
   
   function Index_Of (Source : in String; Delimiter : in Character) return Natural is
   begin
      for I in Source'Range loop
         if Source (I) = Delimiter then
            return I;
         end if;
      end loop;
      raise Delimiter_Not_Found;
   end Index_Of;
   
   
   -------------
   -- Replace --
   -------------
   
   procedure Replace (Source : in out String; Char : in Character; Replace_With : in Character) is
   begin
      for I in Source'Range loop
         if Source (I) = Char then
            Source (I) := Replace_With;
         end if;
      end loop;
   end Replace;
   

   --------------
   -- Is_Empty --
   --------------
   
   function Is_Empty (Source : in String) return Boolean is
   begin
      return Trim (Source, Whitespace, Both)'Length = 0;
   end Is_Empty;
   
   
   ---------------
   -- Has_Token --
   ---------------
   
   function Has_Token (Source : in String; Delimiter : in Character := Whitespace) return Boolean is
   begin
      return (for some C of Trim (Source, Delimiter, Both) => C /= Delimiter);
   end Has_Token;
   
   
   ------------------
   -- Count_Tokens --
   ------------------
   
   function Count_Tokens (Source : in String; Delimiter : in Character) return Natural is
      Tokens   : Natural := 0;
      From, To : Natural;      
   begin
      From := Source'First;
      To := Source'Last;
      
      loop
         Slice_Block : declare
            Input  : constant String := Trim (Source => Source (From .. To), Delimiter => Delimiter, Side => Left);
         begin
            From := Input'First; 
            exit when To < From;
            Tokens := Tokens + 1;
            From := Index_Of (Input, Delimiter);
         exception
            when Delimiter_Not_Found =>
               exit;
         end Slice_Block;
      end loop;
      
      return Tokens;
   end Count_Tokens;
   
   
   
   
   
   -----------------------
   -- Extract_Last_Token --
   ------------------------
   
   function Extract_Last_Token (Source : in String; Delimiter : in Character := Whitespace) return String is
      Input : constant String := Trim (Source, Delimiter, Both);
   begin
      for I in reverse Input'Range loop
         if Input (I) = Delimiter then
            return Input (I + 1 .. Input'Last);
         end if;
      end loop;
      
      -- If no delimiter is found, the input is one token that means that
      -- the entire Source is the only token.
      -- The precondition that a token exists ensure that the entire
      -- string can be used as a token

      return Input;
   end Extract_Last_Token;
   
   
   ----------
   -- Trim --
   ----------
   
   function Trim 
     (Source    : in String;
      Delimiter : in Character := Whitespace;
      Side      : in Trim_Side_Type := Both) return String
   is
      From : Natural := Source'First;
      To   : Natural := Source'Last;
   begin
      -- Trim left side
      if Side in Left | Both then
         loop
            exit when From > To or else Source (From) /= Delimiter;
            From := From + 1;
         end loop;
      end if;
      -- Trim right side
      if Side in Right | Both then
         loop
            exit when To < From or else Source (To) /= Delimiter;
            To := To - 1;
         end loop;
      end if;
      return Source (From .. To);
   end Trim;
   
end String_Lib;
