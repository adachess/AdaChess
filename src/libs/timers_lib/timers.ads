package Timers is
   
   pragma Pure (Timers);
   
private
   
   type Timer_Status_Type is
     (Not_Yet_Started, Started, Stopped)
     with 
       Default_Value => Not_Yet_Started,
       Size => 2;
   
end Timers;
