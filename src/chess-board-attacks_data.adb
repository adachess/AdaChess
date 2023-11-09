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


package body Chess.Board.Attacks_Data is

   
   ------------------------
   -- Attacks_From_North --
   ------------------------

   function Attacks_From_North
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Target : Square_Type;
      Piece  : Piece_Type;
   begin
      Target := Square + North;
      while Board (Target) = Empty loop
         Target := Target + North;
      end loop;
      Piece := Board (Target);
      if Side = White and then Piece in White_Rook | White_Queen then
         return True;
      elsif Side = Black and then Piece in Black_Rook | Black_Queen then
         return True;
      end if;
      return False;
   end Attacks_From_North;


   ------------------------
   -- Attacks_From_South --
   ------------------------

   function Attacks_From_South
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Target : Square_Type;
      Piece  : Piece_Type;
   begin
      Target := Square + South;
      while Board (Target) = Empty loop
         Target := Target + South;
      end loop;
      Piece := Board (Target);
      if Side = White and then Piece in White_Rook | White_Queen then
         return True;
      elsif Side = Black and then Piece in Black_Rook | Black_Queen then
         return True;
      end if;
      return False;
   end Attacks_From_South;


   -----------------------
   -- Attacks_From_East --
   -----------------------

   function Attacks_From_East
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Target : Square_Type;
      Piece  : Piece_Type;
   begin
      Target := Square + East;
      while Board (Target) = Empty loop
         Target := Target + East;
      end loop;
      Piece := Board (Target);
      if Side = White and then Piece in White_Rook | White_Queen then
         return True;
      elsif Side = Black and then Piece in Black_Rook | Black_Queen then
         return True;
      end if;
      return False;
   end Attacks_From_East;


   -----------------------
   -- Attacks_From_West --
   -----------------------

   function Attacks_From_West
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Target : Square_Type;
      Piece  : Piece_Type;
   begin
      Target := Square + West;
      while Board (Target) = Empty loop
         Target := Target + West;
      end loop;
      Piece := Board (Target);
      if Side = White and then Piece in White_Rook | White_Queen then
         return True;
      elsif Side = Black and then Piece in Black_Rook | Black_Queen then
         return True;
      end if;
      return False;
   end Attacks_From_West;


   -----------------------------
   -- Attacks_From_North_East --
   -----------------------------

   function Attacks_From_North_East
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Target : Square_Type;
      Piece  : Piece_Type;
   begin
      Target := Square + North_East;
      while Board (Target) = Empty loop
         Target := Target + North_East;
      end loop;
      Piece := Board (Target);
      if Side = White and then Piece in White_Bishop | White_Queen then
         return True;
      elsif Side = Black and then Piece in Black_Bishop | Black_Queen then
         return True;
      end if;
      return False;
   end Attacks_From_North_East;


   -----------------------------
   -- Attacks_From_North_West --
   ------------------------------

   function Attacks_From_North_West
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Target : Square_Type;
      Piece  : Piece_Type;
   begin
      Target := Square + North_West;
      while Board (Target) = Empty loop
         Target := Target + North_West;
      end loop;
      Piece := Board (Target);
      if Side = White and then Piece in White_Bishop | White_Queen then
         return True;
      elsif Side = Black and then Piece in Black_Bishop | Black_Queen then
         return True;
      end if;
      return False;
   end Attacks_From_North_West;


   -----------------------------
   -- Attacks_From_South_East --
   -----------------------------

   function Attacks_From_South_East
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Target : Square_Type;
      Piece  : Piece_Type;
   begin
      Target := Square + South_East;
      while Board (Target) = Empty loop
         Target := Target + South_East;
      end loop;
      Piece := Board (Target);
      if Side = White and then Piece in White_Bishop | White_Queen then
         return True;
      elsif Side = Black and then Piece in Black_Bishop | Black_Queen then
         return True;
      end if;
      return False;
   end Attacks_From_South_East;


   -----------------------------
   -- Attacks_From_South_West --
   -----------------------------

   function Attacks_From_South_West
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Target : Square_Type;
      Piece  : Piece_Type;
   begin
      Target := Square + South_West;
      while Board (Target) = Empty loop
         Target := Target + South_West;
      end loop;
      Piece := Board (Target);
      if Side = White and then Piece in White_Bishop | White_Queen then
         return True;
      elsif Side = Black and then Piece in Black_Bishop | Black_Queen then
         return True;
      end if;
      return False;
   end Attacks_From_South_West;


   -----------------------------------
   -- Attacks_From_North_North_East --
   -----------------------------------

   function Attacks_From_North_North_East
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Piece  : Piece_Type;
   begin
      Piece := Board (Square);
      if Side = White and then Piece = White_Knight then
         return True;
      elsif Side = Black and then Piece = Black_Knight then
         return True;
      end if;
      return False;
   end Attacks_From_North_North_East;


   ----------------------------------
   -- Attacks_From_North_East_East --
   ----------------------------------

   function Attacks_From_North_East_East
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Piece  : Piece_Type;
   begin
      Piece := Board (Square);
      if Side = White and then Piece = White_Knight then
         return True;
      elsif Side = Black and then Piece = Black_Knight then
         return True;
      end if;
      return False;
   end Attacks_From_North_East_East;


   ----------------------------------
   -- Attacks_From_South_East_East --
   ----------------------------------

   function Attacks_From_South_East_East
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Piece  : Piece_Type;
   begin
      Piece := Board (Square);
      if Side = White and then Piece = White_Knight then
         return True;
      elsif Side = Black and then Piece = Black_Knight then
         return True;
      end if;
      return False;
   end Attacks_From_South_East_East;


   -----------------------------------
   -- Attacks_From_South_South_East --
   -----------------------------------

   function Attacks_From_South_South_East
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Piece  : Piece_Type;
   begin
      Piece := Board (Square);
      if Side = White and then Piece = White_Knight then
         return True;
      elsif Side = Black and then Piece = Black_Knight then
         return True;
      end if;
      return False;
   end Attacks_From_South_South_East;


   -----------------------------------
   -- Attacks_From_South_South_West --
   -----------------------------------

   function Attacks_From_South_South_West
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Piece  : Piece_Type;
   begin
      Piece := Board (Square);
      if Side = White and then Piece = White_Knight then
         return True;
      elsif Side = Black and then Piece = Black_Knight then
         return True;
      end if;
      return False;
   end Attacks_From_South_South_West;


   ----------------------------------
   -- Attacks_From_South_West_West --
   ----------------------------------

   function Attacks_From_South_West_West
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Piece  : Piece_Type;
   begin
      Piece := Board (Square);
      if Side = White and then Piece = White_Knight then
         return True;
      elsif Side = Black and then Piece = Black_Knight then
         return True;
      end if;
      return False;
   end Attacks_From_South_West_West;


   ----------------------------------
   -- Attacks_From_North_West_West --
   ----------------------------------

   function Attacks_From_North_West_West
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Piece  : Piece_Type;
   begin
      Piece := Board (Square);
      if Side = White and then Piece = White_Knight then
         return True;
      elsif Side = Black and then Piece = Black_Knight then
         return True;
      end if;
      return False;
   end Attacks_From_North_West_West;


   -----------------------------------
   -- Attacks_From_North_North_West --
   -----------------------------------

   function Attacks_From_North_North_West
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      Piece  : Piece_Type;
   begin
      Piece := Board (Square);
      if Side = White and then Piece = White_Knight then
         return True;
      elsif Side = Black and then Piece = Black_Knight then
         return True;
      end if;
      return False;
   end Attacks_From_North_North_West;
   
   
   ------------------------------
   -- Attacks_From_Placeholder --
   ------------------------------
   
   function Attacks_From_Placeholder
     (Board : in Board_Type; Side : in Color_Type; Square : in Square_Type) return Boolean is
      pragma Unreferenced (Board);
      pragma Unreferenced (Side);
      pragma Unreferenced (Square);
   begin
      return False;
   end Attacks_From_Placeholder;
   

end Chess.Board.Attacks_Data;
