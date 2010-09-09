pragma Style_Checks ("NM32766");
---------------------------------------------------
--  This file has been generated automatically from
--  boxConfigurator.idl
--  by IAC (IDL to Ada Compiler) GPL 2009-20090519 (rev. 144248).
---------------------------------------------------
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------
with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Object;

package BoxConfigurator.Helper is

   TC_BoxConfigurator : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return BoxConfigurator.Ref;

   function To_Any
     (Item : BoxConfigurator.Ref)
     return CORBA.Any;

   function Unchecked_To_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return BoxConfigurator.Ref;

   function To_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return BoxConfigurator.Ref;

   
   package Internals is

      procedure Initialize_BoxConfigurator;

   end Internals;

end BoxConfigurator.Helper;
