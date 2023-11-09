# AdaChess - Smart Chess Engine

AdaChess is an open source chess engine written entirely in the Ada programming language. The official page of the engine, with more detailed information, can be found in the [Chess Programming Wiki](https://www.chessprogramming.org/AdaChess). 

The main goal while developing the engine is to have fun and add new cool functionalities.

Starting from the version 4.0 the engine source code and an executable for windows are available on this repository. Feel free to download them and create new cool bugs.

AdaChess is a true original chess engine written completely from scratch as hobby project. The main goal are having a lot of fun and implementing cool, original ideas. 

## Download

The latest version of the engine is the release 4.0. If you wish to download just the binaries (executable for Windows), follow this link:
- [AdaChess v4.0](https://github.com/adachess/AdaChess/releases/download/v4.0/AdaChess_v4.0.zip)

or navigate in this repository to get the source, tags, and so on. 
Note: Although you can play AdaChess via console, I recommend to use an external GUI that support Winboard/Xboard protocol and setup the engine. 


## Some cool features

AdaChess is an intermediate/advanced engine that comes with many features, many of them unique. Here a list of the most cool:
- Legal move generator with check-investigation 
- Statistical data for perft and divide (see [Perft Results](https://www.chessprogramming.org/Perft_Results). In the moment I am writing, all the data except the node count were generated with AdaChess) 
- Move annotation in real time (new in release 4.0). The engine decide whether a move is brilliant/good/interesting and will print out this information in the pv. This feature is still a bit experimental, but the premises are very good.

## Compile and Run AdaChess

The fastest way to compile AdaChess is to download an Ada compiler and the Gnat Programming Studio via AdaCore (you can use alire for this) and open the gpr file (adachess.gpr). **Be sure that the scenario mode is on "release"** and compile it. The executable will be created on the same directory.

To use a PGN file as opening book, place the PGN in the same directory of the engine and run it with the --opening-book command line option, for example:
- adachess.exe --opening-book Kasparov.pgn
if you wish to use more opening books on the same time just add more command line options:
- adachess.exe --opening-book Kasparov.pgn --opening-book Carlsen.pgn --opening-book default.pgn

Note that parsing the PGN files will take some time and huge books can take minutes to be loaded.

## License

AdaChess is a GPL licenced software, it comes with source code and binaries for Windows (executable). Download, play and enjoy!
