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

with OnBoardComputer;
use OnBoardComputer;

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

   procedure Set_Laps( LapsQty : in INTEGER);


   type VEL is array (POSITIVE range <>) of FLOAT;
   type DRIVER is record
      Team : Str.Unbounded_String := Str.Null_Unbounded_String;--STRING(1..7):="xxxxxxx";
      FirstName : Str.Unbounded_String := Str.Null_Unbounded_String;--STRING(1..8):="xxxxxxxx";
      LastName : Str.Unbounded_String := Str.Null_Unbounded_String;--STRING(1..6):="xxxxxx";
      Vel_In : FLOAT := 0.0;
    end record;

   type CAR is record -- macchina
      MaxSpeed : FLOAT;
      MaxAcceleration : FLOAT;
      GasTankCapacity : FLOAT;
      Engine : Str.Unbounded_String := Str.Null_Unbounded_String;--STRING(1..6):="xxxxxx";
      TyreUsury : Common.PERCENTAGE;
      GasolineLevel : FLOAT;
      -- private subtype TYRE is record
      Mixture : Str.Unbounded_String := Str.Null_Unbounded_String;-- : STRING(1..20):="xxxxxxxxxxxxxxxxxxxx";
      Model : Str.Unbounded_String := Str.Null_Unbounded_String;--STRING(1..20):="xxxxxxxxxxxxxxxxxxxx";
      Type_Tyre : Str.Unbounded_String := Str.Null_Unbounded_String;--STRING(1..20):="xxxxxxxxxxxxxxxxxxxx";

      --end record;

   end record;

   type CAR_DRIVER is tagged record
      auto : CAR;
      pilota : DRIVER;
      strategia : Common.STRATEGY;
      RaceIterator : RACETRACK_ITERATOR;
      Id: INTEGER;
      statsComputer : COMPUTER_POINT := new COMPUTER;
      Radio : BOX_CONNECTION;
   end record;

   type CAR_DRIVER_ACCESS is access CAR_DRIVER;
   function Init_Competitor(xml_file : STRING;
                            RaceIterator : RACETRACK_ITERATOR;
                            id_In : INTEGER;
                            laps_In : INTEGER;
                            BoxRadio_CorbaLoc : in STRING) return CAR_DRIVER_ACCESS;
  -- procedure Set_Id(Car_In : in out CAR_DRIVER_ACCESS; Id_In : INTEGER);
-- set and get function for tyre into car
   function Get_Mixture(Car_In : CAR_DRIVER_ACCESS) return Str.Unbounded_String;
   function Get_TypeTyre(Car_In : CAR_DRIVER_ACCESS) return Str.Unbounded_String;
   function Get_Model(Car_In : CAR_DRIVER_ACCESS) return Str.Unbounded_String;
   procedure Set_Mixture(Car_In : in out CAR_DRIVER_ACCESS;
                         Mixture_In : in Str.Unbounded_String);
   procedure Set_TypeTyre(Car_In : in out CAR_DRIVER_ACCESS;
                          TypeTyre_In: in Str.Unbounded_String);
   procedure Set_Model(Car_In : in out CAR_DRIVER_ACCESS;
                       Model_In : in Str.Unbounded_String);



   -- Set car values
   function Get_MaxSpeed(Car_In : CAR_DRIVER_ACCESS) return FLOAT;
   function Get_MaxAcceleration(Car_In : CAR_DRIVER_ACCESS) return FLOAT;
   function Get_GasTankCapacity(Car_In : CAR_DRIVER_ACCESS) return FLOAT;
   function Get_Engine(Car_In : CAR_DRIVER_ACCESS) return Str.Unbounded_String;

   procedure Configure_Car(Car_In : in out CAR;
                           MaxSpeed_In : FLOAT;
                           MaxAcceleration_In : FLOAT;
                           GasTankCapacity_In : FLOAT;
                           Engine_In : Str.Unbounded_String;
                           TyreUsury_In : Common.PERCENTAGE;
                           GasolineLevel_In : FLOAT;
                           Mixture_In : Str.Unbounded_String;
                           Model_In : Str.Unbounded_String;
                           Type_Tyre_In : Str.Unbounded_String);

   procedure Set_Usury(Car_In : in out CAR_DRIVER_ACCESS;
                       Usury_In : Common.PERCENTAGE);
   procedure Set_GasLevel(Car_In : in out CAR_DRIVER_ACCESS;
                          GasLevel_In : FLOAT);
   function Get_Usury(Car_In : CAR_DRIVER_ACCESS) return Common.PERCENTAGE;
   function Get_GasLevel(Car_In : CAR_DRIVER_ACCESS) return FLOAT;
   function Calculate_Status(infoLastSeg : in CAR_DRIVER_ACCESS) return BOOLEAN;
   -- procedure Calculate_Status(infoLastSeg);
   -- questo metodo controlla tyre usury e gasoline level
   -- se sono sotto una soglia critica richiede l'intervento dei box
   -- per ora metto parametri a caso (cio� bisogna definire di preciso
   -- quale dev'essere il limite per richiamare i box, bisogna evitare che
   --la macchina non riesca pi� a girare nel caso il box non sia tempestivo
   --  nella risposta quindi bisogna che la soglia permetta ancora qualche giro,
   -- almeno 2 direi.
   procedure Get_Status(Car_In : CAR_DRIVER_ACCESS; Usury_Out : out FLOAT; Level_Out : out FLOAT );
   procedure Set_Vel_In(Competitor_In : in out CAR_DRIVER_ACCESS; PVel_In : in FLOAT);
   function Get_Vel_In(Competitor_In : CAR_DRIVER_ACCESS) return FLOAT;
   function Get_Team(Competitor_In :CAR_DRIVER_ACCESS) return Str.Unbounded_String;
   function Get_FirstName(Competitor_In : CAR_DRIVER_ACCESS) return Str.Unbounded_String;
   function Get_LastName(Competitor_In : CAR_DRIVER_ACCESS) return Str.Unbounded_String;
   procedure Set_Team(Competitor_In: in out CAR_DRIVER_ACCESS;
                      Team_In : in Str.Unbounded_String);
   procedure Set_FirstName(Competitor_In: in out CAR_DRIVER_ACCESS;
                           FirstName_In : in Str.Unbounded_String);
   procedure Set_LastName(Competitor_In: in out CAR_DRIVER_ACCESS;
                          LastName_In : in Str.Unbounded_String);

   --type CAR_DRIVER_ACCESS is access CAR_DRIVER;
--task TASKCOMPETITOR(Car_In : CAR_DRIVER_ACCESS);

   task type TASKCOMPETITOR(carDriver_In : CAR_DRIVER_ACCESS) is
      entry Start;
   end TASKCOMPETITOR;
   --type taskdebug is access TASKCOMPETITOR;
--procedure Set_endWait(temp : in taskdebug);
   -- subtype str is Strategy.STRATEGY;

   procedure Configure_Driver(Car_In: in out DRIVER;
                              Team_In : Str.Unbounded_String;
                              FirstName_In : Str.Unbounded_String;
                              LastName_In : Str.Unbounded_String;
                              Vel_In : FLOAT);

   --function Evaluate(driver : CAR_DRIVER_ACCESS ; F_Segment : CHECKPOINT_SYNCH_POINT;
     --                Paths2Cross : CROSSING_POINT) return FLOAT;
   --function CalculateCrossingTime(CarDriver : CAR_DRIVER_ACCESS; PathsCollection_Index : INTEGER;
    --                              F_Segment : CHECKPOINT_SYNCH_POINT ; Vel_In : FLOAT;
    --                              Paths2Cross : CROSSING_POINT) return FLOAT;
   function Get_StrategyGasLevel(str_In : CAR_DRIVER_ACCESS) return FLOAT;

   function Get_StrategyPitstopLaps(str_In : CAR_DRIVER_ACCESS) return INTEGER;

   function Get_StrategyTyreType (str_In : CAR_DRIVER_ACCESS) return Str.Unbounded_String;

   function Get_StrategyPitStopDelay (str_In : CAR_DRIVER_ACCESS) return FLOAT;

   function Get_StrategyStyle (str_In : CAR_DRIVER_ACCESS) return Common.DRIVING_STYLE;

end Competitor;
