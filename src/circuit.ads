with Common;
with Queue; use Queue;
with Input_Sources.File;
use Input_Sources.File;
with Sax.Readers; use Sax.Readers;
with DOM.Readers; use DOM.Readers;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Attrs; use DOM.Core.Attrs;
with Ada.IO_Exceptions;

with Ada.Text_IO;
use Ada.Text_IO;


package Circuit is

   subtype ANGLE_GRADE is FLOAT range 0.0..360.00;
   subtype DIFFICULTY_RANGE is FLOAT range 0.0..10.0;
   Checkpoints_Qty : POSITIVE := 2;
   MaxCompetitors_Qty : POSITIVE := 4;--4;

   procedure Set_CheckpointsQty (Qty_In : POSITIVE);
   procedure Set_MaxCompetitorsQty ( Qty_In : POSITIVE);

   -- PATH Structure delaration
   --+ A PATH is one of the possible ways the competitor can take
   --+ in a segment of the circuit.
   type PATH is private;

   -- In a segment the paths can have different lengths. For instance
   --+ in a bend the innermost path is shorter than the others.
   function Get_Length(Path_In : PATH) return FLOAT;
   -- The angle of the bending angle of the path
   function Get_Angle(Path_In : PATH) return FLOAT;
   -- The grip of the path
   function Get_Grip(Path_In : PATH) return FLOAT;
   -- Another parameter the characterise the path and it may depend
   --+ on different features.
   function Get_Difficulty(Path_In : PATH) return FLOAT;

   --TODO: PATHS sould be private
   --This array represents the set of paths a segment of the
   --+ circuit has.
   type PATHS is array(INTEGER range <>) of PATH;
   type POINT_PATHS is access PATHS;

   --Checkpoint Structure delcaration.
   type Checkpoint is tagged private;
   type POINT_Checkpoint is access Checkpoint;

   --Init Segment.
   --TODO: Probably it should be private
   procedure Set_Values(Checkpoint_In : in out POINT_Checkpoint;
                        SectorID_In : INTEGER;
                        IsGoal_In : BOOLEAN;
                        Length_In : FLOAT;
                        Angle_In : ANGLE_GRADE;
                        PathsQty_In : POSITIVE;
                        Competitors_Qty : POSITIVE);

   --procedure Set_Next(Checkpoint_In : in out POINT_Checkpoint;
   --                   NextCheckpoint_In : POINT_Checkpoint);

   --function Get_Path(Checkpoint_In : POINT_Checkpoint;
   --                  Path_Num : INTEGER ) return PATH;
   --function Get_Next_Checkpoint(Checkpoint_In : POINT_Checkpoint) return POINT_Checkpoint;
   --function Get_Length(Checkpoint_In : POINT_Checkpoint) return FLOAT;
   function Get_Time(Checkpoint_In : POINT_Checkpoint;
                     CompetitorID_In : INTEGER) return FLOAT;


   protected type CROSSING(Paths_In : POINT_PATHS) is
      procedure Update_Time(Time_In : in FLOAT;
                            PathIndex : in INTEGER);
      function Get_Size return INTEGER;
      function Get_Length(PathIndex : INTEGER) return FLOAT;
      function Get_Angle(PathIndex : INTEGER) return FLOAT;
      function Get_Grip(PathIndex : INTEGER) return FLOAT;
      function Get_Difficulty(PathIndex : INTEGER) return FLOAT;
      function Get_PathTime(PathIndex : INTEGER) return FLOAT;

   private
      F_Paths : POINT_PATHS := Paths_In;
   end CROSSING;

   type CROSSING_POINT is access CROSSING;

   protected type CHECKPOINT_SYNCH(Checkpoint_In : POINT_Checkpoint) is

      procedure Signal_Arrival(CompetitorID_In : INTEGER;
                               Paths2Cross : out CROSSING_POINT);
      procedure Signal_Leaving(CompetitorID_In : INTEGER);
      procedure Set_ArrivalTime(CompetitorID_In : INTEGER;
                                Time_In : FLOAT);
      procedure Remove_Competitor(CompetitorID_In : INTEGER);
      procedure Set_Competitors(Competitors : Common.COMPETITORS_LIST;
                                Times : Common.FLOAT_LIST);
      function Get_Time(CompetitorID_In : INTEGER) return FLOAT;


      function Get_SectorID return INTEGER;

      function getChanged return Boolean;

      entry Wait(CompetitorID_In : INTEGER;
                 Paths2Cross : out CROSSING_POINT);

   private
      F_Checkpoint : POINT_Checkpoint := Checkpoint_In;
      Changed : BOOLEAN := false;
      Sincronizzazione : BOOLEAN := false;
      ContaConcorrenti : INTEGER := 0;
   end CHECKPOINT_SYNCH;

   type CHECKPOINT_SYNCH_POINT is access CHECKPOINT_SYNCH;
   function getChanged (temp : in CHECKPOINT_SYNCH_POINT) return Boolean;
   type RACETRACK is array(POSITIVE range <>) of CHECKPOINT_SYNCH_POINT;
   type RACETRACK_POINT is access RACETRACK;

   --The iterator is supposed to be used by the Competitor, in order
   --to allow him to move in the "correct direction"
   type RACETRACK_ITERATOR is private;
   procedure Init_Racetrack(Racetrack_In : in out RACETRACK_POINT;
                            Document_In : DOCUMENT);
   procedure Set_Checkpoint(Racetrack_In : in out RACETRACK;
                         Checkpoint_In : CHECKPOINT_SYNCH_POINT;
                            Position_In : POSITIVE);
   procedure Set_Competitors(Racetrack_In : in out RACETRACK_POINT;
                             Competitors : in Common.COMPETITORS_LIST);
   function Get_Iterator(Racetrack_In : RACETRACK_POINT) return RACETRACK_ITERATOR;
   procedure Get_CurrentCheckpoint(RaceIterator : in out RACETRACK_ITERATOR;
                               CurrentCheckpoint : out CHECKPOINT_SYNCH_POINT);
   procedure Get_NextCheckpoint(RaceIterator : in out RACETRACK_ITERATOR;
                               NextCheckpoint : out CHECKPOINT_SYNCH_POINT);
   procedure Get_PreviousCheckpoint(RaceIterator : in out RACETRACK_ITERATOR;
                                    PreviousCheckpoint : out CHECKPOINT_SYNCH_POINT);
   function Get_RaceLength(RaceIterator : RACETRACK_ITERATOR) return INTEGER;
   function Get_Position(RaceIterator : RACETRACK_ITERATOR) return POSITIVE;
   function Get_IsFinished(RaceIterator : RACETRACK_ITERATOR) return BOOLEAN;
   function Get_Racetrack(Racetrack_File : STRING) return RACETRACK_POINT;
   function Get_Checkpoint(Racetrack_In : RACETRACK;
                           Position : POSITIVE) return CHECKPOINT_SYNCH_POINT;
   function Print_Racetrack(Racetrack_In : RACETRACK) return INTEGER;

private

   type Checkpoint is tagged record
      Queue : access SORTED_QUEUE;
      SectorID : INTEGER;
      IsGoal : BOOLEAN;
      Multiplicity : POSITIVE;
      PathsCollection : CROSSING_POINT;
   end record;

   type PATH is record
      Length : FLOAT;
      Grip : FLOAT;
      Difficulty : FLOAT range 0.0..10.0;
      Angle : FLOAT range 0.0..360.0;
      LastTime : FLOAT;
   end record;

   type RACETRACK_ITERATOR is record
      Race_Point : RACETRACK_POINT;
      Position : POSITIVE := 1;
   end record;


end Circuit;
