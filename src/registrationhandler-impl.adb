with Ada.Text_IO;

with RegistrationHandler.Skel;
pragma Warnings (Off, RegistrationHandler.Skel);

pragma Warnings (Off);

with CORBA;

package body RegistrationHandler.impl is

   Comp : Competition.SYNCH_COMPETITION_POINT;

   procedure Init(Competition_In : in Competition.SYNCH_COMPETITION_POINT) is
   begin
      Comp := Competition_In;
   end Init;

   procedure Join_Competition(Self : access Object;
                              CompetitorDescriptor : in CORBA.STRING;
                              BoxCorbaLOC : in CORBA.STRING;
                              MonitorCorbaLOC : out CORBA.STRING;
                              Competitor_ID : out CORBA.Short;
                              CircuitLength : out CORBA.Float;
                              Laps : out CORBA.Short) is
      Competitor_ID_INT : INTEGER;

   begin
      Ada.Text_IO.Put_Line("registering....");
      Comp.Register_NewCompetitor(CORBA.To_Standard_String(CompetitorDescriptor),
                                  CORBA.To_Standard_String(BoxCorbaLOC),
                                  Competitor_ID_INT);

      Laps := CORBA.Short(Comp.Get_Laps);
      CircuitLength := CORBA.Float(Comp.Get_CircuitLength);
      Competitor_ID := Corba.SHORT(Competitor_ID_INT);

      -- MonitorCorbaLOC := Comp.GetMonitor;
   end Join_Competition;

   -- When the box joining request has been accepted, the box wait that
   --+ all the other participants join the competition. After that
   --+ it's possible to have all the information about the competition
   --+ (like the total number of competitors, that could be less
   --+ than the maximum number choosen at the beginning).

   function Wait_Ready(Self : access Object;
                       CompetitorID : CORBA.Short) return CORBA.String is
   begin
      return CORBA.To_CORBA_String("No configuration yet");
   end Wait_Ready;

end RegistrationHandler.impl;
