pragma Style_Checks ("NM32766");
---------------------------------------------------
--  This file has been generated automatically from
--  ../idl/init.idl
--  by IAC (IDL to Ada Compiler) GPL 2009-20090519 (rev. 144248).
---------------------------------------------------
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------
with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Object;

package broker.init.CompetitionConfigurator.Helper is

   TC_CompetitionConfigurator : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return broker.init.CompetitionConfigurator.Ref;

   function To_Any
     (Item : broker.init.CompetitionConfigurator.Ref)
     return CORBA.Any;

   function Unchecked_To_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return broker.init.CompetitionConfigurator.Ref;

   function To_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return broker.init.CompetitionConfigurator.Ref;

   
   package Internals is

      procedure Initialize_CompetitionConfigurator;

   end Internals;

end broker.init.CompetitionConfigurator.Helper;
