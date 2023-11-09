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


package Chess.Notations is

   -- AdaChess support standard notations many notations. The current Standard
   -- Algebraic, as well as the Long Algebraic, are compliant to the PGN
   -- standard and NOT the FIDE.
   type Notation_Type is
     (Standard_Algebraic, -- e4 Nf6 O-O Qxc3+ e8=Q...
      Long_Algebraic,     -- e2-e4 Ng8-f6 O-O Qa5xc3+ e7-e8=Q...
      Pure_Algebraic,     -- e2e4 g8f6 e1c1 a5c3
      ICCF)               -- 5254 7866 5171
     with Size => 2;

   Default_Notation : Notation_Type := Standard_Algebraic;

end Chess.Notations;
