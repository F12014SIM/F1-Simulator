with Circuit;
use Circuit;
with Competitor;
use Competitor;
with Competitor;

with Ada.Calendar;
use Ada.Calendar;
with Ada.Real_Time;
use Ada.Real_Time;

package Strategy is
   subtype comp is Competitor.CAR;
   function evaluate(segm : SEGMENT;
                     driver : comp) return Ada.Calendar.TIME;
   type STRATEGY is
      record
         pitstopGasolineLevel : INTEGER;
         pitstopLaps : INTEGER;
         pitstopCondition : BOOLEAN; -- correggere il tipo
         trim : INTEGER; -- correggere il tipo
         pitstop : BOOLEAN;
      end record;

end Strategy;
