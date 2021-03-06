pragma Style_Checks ("NM32766");
---------------------------------------------------
--  This file has been generated automatically from
--  ../idl/radio.idl
--  by IAC (IDL to Ada Compiler) GPL 2009-20090519 (rev. 144248).
---------------------------------------------------
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------
with CORBA.Object;
with PolyORB.Std;
with CORBA;
pragma Elaborate_All (CORBA);

package broker.radio.Box_Monitor_Radio is

   type Ref is
     new CORBA.Object.Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:broker/radio/Box_Monitor_Radio:1.0";

   procedure GetUpdate
     (Self : Ref;
      num : CORBA.Short;
      time : out CORBA.Float;
      metres : out CORBA.Float;
      Returns : out CORBA.String);

   GetUpdate_Repository_Id : constant PolyORB.Std.String :=
     "IDL:broker/radio/Box_Monitor_Radio/GetUpdate:1.0";

   procedure Force_Pitstop
     (Self : Ref;
      force : CORBA.Boolean);

   Force_Pitstop_Repository_Id : constant PolyORB.Std.String :=
     "IDL:broker/radio/Box_Monitor_Radio/Force_Pitstop:1.0";

   function Is_A
     (Self : Ref;
      Logical_Type_Id : PolyORB.Std.String)
     return CORBA.Boolean;

private
   function Is_A
     (Logical_Type_Id : PolyORB.Std.String)
     return CORBA.Boolean;

end broker.radio.Box_Monitor_Radio;
