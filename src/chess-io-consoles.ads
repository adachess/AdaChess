package Chess.IO.Consoles is
   
   package Render_Interface is
      
      type Render_Engine is interface;
      
      procedure Render 
        (Engine     : in Render_Engine; 
         Chessboard : in Chessboard_Type) is abstract;
      
   end Render_Interface;
   
   
   procedure Display_On_Console (Chessboard : in Chessboard_Type);
   
private
   
   
   
   type Console_Render_Engine is abstract new Render_Interface.Render_Engine with null record;

   procedure Render 
     (Engine     : in Console_Render_Engine;
      Chessboard : in Chessboard_Type) is abstract;
   
   
   type Simple_Console_Render_Engine is new Console_Render_Engine with null record;
   overriding procedure Render
     (Engine     : in Simple_Console_Render_Engine;
      Chessboard : in Chessboard_Type);
   
   Selected_Render_Engine : Render_Interface.Render_Engine'Class
     := Simple_Console_Render_Engine'(null record);
   
end Chess.IO.Consoles;
