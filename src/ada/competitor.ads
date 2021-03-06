with Circuit;
use Circuit;

with Input_Sources.File;
use Input_Sources.File;
with Sax.Readers; use Sax.Readers;
with DOM.Readers; use DOM.Readers;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Attrs; use DOM.Core.Attrs;
with Ada.IO_Exceptions;

with Competitor_Computer;
use Competitor_Computer;

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Unbounded;

with CompetitorRadio;
use CompetitorRadio;

with Common;
package Competitor is
   package Str renames Ada.Strings.Unbounded;
   use type Str.Unbounded_String;

   procedure Set_Laps( LapsQty : in Integer);

   -- Competitor components
   type VEL is array (POSITIVE range <>) of Float;
   type Driver is private;

   type Car is private;

   type Competitor_Details is private;

   type Competitor_Details_Point is Access Competitor_Details;

   -- Get Methods
   function Get_First_Name(Competitor_In : Competitor_Details_Point) return Str.Unbounded_String;
   function Get_Tyre_Usury(Competitor_In : Competitor_Details_Point) return Common.PERCENTAGE;
   function Get_Max_Acceleration(Competitor_In : Competitor_Details_Point) return Float;
   function Get_Last_Speed_Reached(Competitor_In : Competitor_Details_Point) return Float;
   function Get_Strategy_Style(Competitor_In : Competitor_Details_Point) return Common.Driving_Style;
   function Init_Competitor(Xml_File : STRING;
                            Current_Circuit_Race_Iterator : Racetrack_Iterator;
                            Id_In : Integer;
                            Laps_In : Integer;
                            BoxRadio_CorbaLoc : in STRING) return Competitor_Details_Point;

   procedure Configure_Car(Car_In : in out Car;
                           Max_Speed_In : Float;
                           Max_Acceleration_In : Float;
                           Engine_In : Str.Unbounded_String;
                           Tyre_Usury_In : Common.Percentage;
                           Mixture_In : Str.Unbounded_String;
                           Model_In : Str.Unbounded_String;
                           Tyre_Type_In : Str.Unbounded_String);

   task type Competitor_Task(Car_Driver_In : Competitor_Details_Point) is
      entry Start;
   end Competitor_Task;

   procedure Configure_Driver(Driver_In: in out Driver;
                              Team_In : Str.Unbounded_String;
                              First_Name_In : Str.Unbounded_String;
                              Last_Name_In : Str.Unbounded_String);

private

   type Car is record
      Max_Speed          : Float;
      Max_Acceleration   : Float;
      Engine             : Str.Unbounded_String := Str.Null_Unbounded_String;
      Tyre_Usury         : Common.Percentage;
      Mixture            : Str.Unbounded_String := Str.Null_Unbounded_String;
      Model              : Str.Unbounded_String := Str.Null_Unbounded_String;
      Tyre_Type          : Str.Unbounded_String := Str.Null_Unbounded_String;
      Last_Speed_Reached : Float := 0.0;
   end record;

   type Driver is record
      Team       : Str.Unbounded_String := Str.Null_Unbounded_String;
      First_Name : Str.Unbounded_String := Str.Null_Unbounded_String;
      Last_Name  : Str.Unbounded_String := Str.Null_Unbounded_String;
    end record;

   type Competitor_Details is tagged record
      Racing_Car   			: Car;
      Racing_Driver 			: Driver;
      Current_Strategy 			: Common.Strategy;
      Current_Circuit_Race_Iterator 	: Racetrack_Iterator;
      Id				: Integer;
      On_Board_Computer 		: Computer_Point := new Computer;
      Radio 				: Box_Connection;
   end record;

end Competitor;
