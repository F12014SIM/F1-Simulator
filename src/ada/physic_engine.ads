with Ada.Numerics.Elementary_Functions;
with Common;
with Competitor;
with Circuit;
use Circuit;
with Path_Handler;
use Path_Handler;
with Checkpoint_Handler;
use Checkpoint_Handler;
with Ada.Strings.Unbounded;

package Physic_Engine is
   type Float_Array is array (POSITIVE range <>) of Float;

   package String_Unb renames Ada.Strings.Unbounded;
   use type String_Unb.Unbounded_String;

   procedure Evaluate(F_Segment          : Checkpoint_Synch_Point;
                      Paths_2_Cross      : Crossing_Point;
                      Competitor_Id      : Integer;
                      Strategy_Style     : Common.Driving_Style;
                      Max_Speed          : Float;
                      Max_Acceleration   : Float;
                      Tyre_Type          : String_Unb.Unbounded_String;
                      Tyre_Usury         : in out Float;
                      Length_Path        : out Float;
                      Crossing_Time_Out  : out Float;
                      Speed_Out          : out Float;
                      Last_Speed_Reached : in out Float);
end Physic_Engine;
