with Common;
use Common;

with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

package Box is

   package Unbounded_String renames Ada.Strings.Unbounded;
   use type Unbounded_String.Unbounded_String;

   type INFO_NODE is private;
   type INFO_NODE_POINT is access INFO_NODE;

   --Temporary public type. It has to be private DEL
   --   type BOX_STRATEGY is private;
   type BOX_STRATEGY is record
      Type_Tyre : Unbounded_String.Unbounded_String := Unbounded_String.Null_Unbounded_String;
      Style : DRIVING_STYLE;
      GasLevel : PERCENTAGE;
      PitStopLap : INTEGER;
      PitStopDelay : FLOAT;
   end record;
   --type COMPETITION_UPDATE is private;
   type COMPETITION_UPDATE( competitor_qty : INTEGER) is record
      GasLevel : PERCENTAGE;
      TyreUsury : PERCENTAGE;
      MeanSpeed : FLOAT; -- km/h
      MeanGasConsumption : FLOAT; -- l/h
      Time : FLOAT;
      Lap : INTEGER;
      Sector : INTEGER;
      Classific : access COMPETITOR_LIST := new COMPETITOR_LIST(1..competitor_qty);
   end record;


   type STRATEGY_HISTORY is array(POSITIVE range <>) of BOX_STRATEGY;

   Interval : FLOAT; -- set after competition joining
   Sector_Qty : INTEGER := 3; --It's fixed in the f1 competitions
   Competitor_Id : INTEGER;

   protected type SYNCH_COMPETITION_UPDATES is
      procedure Add_Data(CompetitionUpdate_In : access COMPETITION_UPDATE);
      entry Wait(NewInfo : out COMPETITION_UPDATE;
                 Num : in INTEGER);
      entry Get_Update( NewInfo : out COMPETITION_UPDATE;
                       Num : INTEGER );
      function IsUpdated return BOOLEAN;
   private
      Updates_Current : Info_Node_Point;
      Updates_Last : Info_Node_Point;
      Updated : BOOLEAN := False;
   end SYNCH_COMPETITION_UPDATES;

   -- This task is the responsible of getting the competition updates from the
   --+ remote server and putting them into the updated buffer shared with
   --+ the strategy updater
   task type MONITOR(SharedBuffer : access SYNCH_COMPETITION_UPDATES;
                     MonitorRadio_CorbaLOC : access Unbounded_String.Unbounded_String) is
   end MONITOR;

   --The resource handle the mutually exclusive access to the
   --+ strategy history. The resource is written by the
   --+ strategy updated that add new strategies to the history
   --+ everytime it calculates a new one.
   --+ The task responsible for the remote interface of the Box
   --+ (used by the competitor to ask for a new strategy) uses
   --+ this resources to search for the up-to-date strategy.

   protected type SYNCH_STRATEGY_HISTORY is

      procedure Init( Lap_Qty : in INTEGER );

      procedure AddStrategy( Strategy : in BOX_STRATEGY );

      entry Get_Strategy( NewStrategy : out BOX_STRATEGY;
                         Lap : in INTEGER);

   private
      history : access STRATEGY_HISTORY;
      history_size : INTEGER := 0;
      Updated : BOOLEAN := false;
   end SYNCH_STRATEGY_HISTORY;

   -- The strategy updater takes new information about the competition
   --+ whenever they are available in the update buffer. Then it uses
   --+ them to compute the new startegy lap by lap.
   task type STRATEGY_UPDATER ( SharedBuffer : access SYNCH_COMPETITION_UPDATES;
                               SharedHistory : access SYNCH_STRATEGY_HISTORY) is
   end STRATEGY_UPDATER;

   -- BOX RADIO TYPES AND METHODS DEFINITION --
   type BOX_RADIO is private;

   --Temporary test function DEL
   function BoxStrategyToXML(strategy : BOX_STRATEGY) return STRING;
   function CompetitionUpdateToXML(update : COMPETITION_UPDATE) return STRING;
   -- Local methods --

private
--     type COMPETITION_UPDATE is record
--        GasLevel : PERCENTAGE;
--        TyreUsury : PERCENTAGE;
--        MeanSpeed : FLOAT; -- km/h
--        MeanGasConsumption : FLOAT; -- l/h
--        Time : FLOAT;
--        Lap : INTEGER;
--        Sector : INTEGER;
--        -- Classific : decidere come esprimerla;
--     end record;

   type Info_Node is record
      Index : INTEGER;
      Previous : INFO_NODE_POINT;
      Next : INFO_NODE_POINT;
      This : access COMPETITION_UPDATE;
   end record;

--     type BOX_STRATEGY is record
--        Type_Tyre : STRING(1..20);
--        Style : DRIVING_STYLE;
--        GasLevel : PERCENTAGE;
--        PitStopLap : INTEGER;
--        PitStopDelay : FLOAT;
--     end record;

   type BOX_RADIO is record
      CompetitionAddress : IP_ADDRESS;
      -- TODO: add the IOR or whatelse is needed to connect to the competition
   end record;

end Box;
