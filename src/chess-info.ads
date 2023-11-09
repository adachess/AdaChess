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


package Chess.Info is

   -----------------
   -- Engine Name --
   -----------------
   
   function Engine_Name return String is ("AdaChess");
   -- Expose the name of the engine.
   --
   -- Returns:
   --    The pure name of the engine
   
   function Engine_Motto return String is ("Smart Chess Engine");
   -- Expose the current "motto" of the engine.
   --
   -- Returns:
   --    The motto of this engine
   

   --------------------
   -- Engine Version --
   --------------------

   function Version return String is ("3.9.0-dev");
   -- Export the current version of the engine version. The output is composed
   -- by three numbers separated by a dot. Respectively, they represent the
   -- major version, minor version and status.
   -- 
   -- Returns
   --    The version numer of the engine
   
   
   ------------
   -- Author --
   ------------
   
   function Author return String is ("Alessandro Iavicoli");
   -- Export the author of AdaChess, i.e. my name ;-)
   
   
   --------------------
   -- E-Mail Address --
   --------------------
   
   function E_Mail return String is ("adachess@gmail.com");
   -- Export the official email address of the engine. Feel free to contact me
   -- about AdaChess for any reason that you like. I appreciate if I receive
   -- comments, suggestions, tips, everything.
   
   
   -------------------
   -- Internet Site --
   -------------------
   
   function Site return String is ("https://www.adachess.com");
   -- Expose the official internet site of the engine
   
   
   ------------------
   -- Console_Logo --
   ------------------
   
   function Console_Logo return String is 
     (  "                                      .(@).                                     " & ASCII.LF &
        "                          (@@@@@@@@@@@@&&&@@@@@@@@@@@@,                         " & ASCII.LF &
        "                     @@@@@@                           @@@@@@                    " & ASCII.LF &
        "                 @@@@@                                     @@@@@                " & ASCII.LF &
        "              @@@@                                             @@@@             " & ASCII.LF &
        "           @@@@                       @@@@@                       @@@#          " & ASCII.LF &
        "         @@@@                  (      @@@@@      )                  @@@@        " & ASCII.LF &
        "       @@@@                    @@@@@@@@@@@@@@@@@@@                    @@@%      " & ASCII.LF &
        "      @@@                       .@@@@@@@@@@@@@@@                        @@@     " & ASCII.LF &
        "    (@@                           @@@@@@@@@@@@@                          ,@@    " & ASCII.LF &
        "   @@@                            @@@@@@@@@@@@                             @@%  " & ASCII.LF &
        "  @@@                              ,,,,,,,,,,,                              @@  " & ASCII.LF &
        "  @@                            @@@@@@@@@@@@@@@@@                           ,@@ " & ASCII.LF &
        " @@@                             #@@@@@@@@@@@@@*                             @@@" & ASCII.LF &
        " @@                                ,,,,,,,,,,,                               *@@" & ASCII.LF &
        " @@                                @@@@@@@@@@@                                @@" & ASCII.LF &
        "@@@                                @@@@@@@@@@@                                @@" & ASCII.LF &
        " @@                                @@@@@@@@@@@                                @@" & ASCII.LF &
        " @@                                @@@@@@@@@@@                               /@@" & ASCII.LF &
        " @@@                              @@@@@@@@@@@@%                              @@@" & ASCII.LF &
        "  @@                              @@@@@@@@@@@@@                             /@@ " & ASCII.LF &
        "  #@@                            *@@@@@@@@@@@@@                             @@  " & ASCII.LF &
        "   @@@                           @@@@@@@@@@@@@@@                           @@(  " & ASCII.LF &
        "    .@@                        @@@@@@@@@@@@@@@@@@@                       (@@    " & ASCII.LF &
        "      @@@                   @@@@@@@@@@@@@@@@@@@@@@@@@                   @@@     " & ASCII.LF &
        "       @@@@               @@@@@@@@@@@@@@@@@@@@@@@@@@@@@               @@@/      " & ASCII.LF &
        "         @@@@             @@@@@@@@@@@@@@@@@@@@@@@@@@@@@             @@@@        " & ASCII.LF &
        "           #@@@           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@           @@@,          " & ASCII.LF &
        "              @@@@                                             @@@@             " & ASCII.LF &
        "                 @@@@@                                     @@@@@                " & ASCII.LF &
        "                     @@@@@@,                         *@@@@@@                    " & ASCII.LF &
        "                           @@@@@@@@@@@@@@@@@@@@@@@@@@@                          " & ASCII.LF);
   -- Draw the AdaChess Logo as ASCII art
   
end Chess.Info;
