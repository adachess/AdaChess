package Chess.Moves.Annotations is

   type Annotation_Type is
     (None,
      Blunder,
      Mistake, 
      Dobious,
      Interesting, -- A move that leads to a small improvement
      Good,        -- A move that brings a valuable advatages
      Brilliant)   -- Amazing move!
     with 
       Size => 3,
       Default_Value => None;
   
   type Annotated_Move_Type is record
      Move       : Move_Type;
      Annotation : Annotation_Type;
   end record;
   
   ---------
   -- "=" --
   ---------
   
   function "=" (Left, Right : in Annotated_Move_Type) return Boolean is
     (Left.Move = Right.Move);
   
   function "=" (Left : in Move_Type; Right : in Annotated_Move_Type) return Boolean is
     (Left = Right.Move);
   
   function "=" (Left : in Annotated_Move_Type; Right : in Move_Type) return Boolean is
     (Left.Move = Right);
   
   
   Empty_Annotated_Move : constant Annotated_Move_Type :=
     Annotated_Move_Type'(Move       => Empty_Move,
                          Annotation => None);
   

end Chess.Moves.Annotations;
