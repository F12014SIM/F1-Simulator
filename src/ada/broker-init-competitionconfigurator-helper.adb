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
with PolyORB.Std;
with PolyORB.Any;
with CORBA.Object.Helper;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body broker.init.CompetitionConfigurator.Helper is

   
   package body Internals is

      CompetitionConfigurator_Initialized : PolyORB.Std.Boolean :=
        False;

      ----------------------------------------
      -- Initialize_CompetitionConfigurator --
      ----------------------------------------

      procedure Initialize_CompetitionConfigurator is
         Name_� : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("CompetitionConfigurator");
         Id_� : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:broker/init/CompetitionConfigurator:1.0");
      begin
         if not CompetitionConfigurator_Initialized
         then
            CompetitionConfigurator_Initialized :=
              True;
            broker.init.CompetitionConfigurator.Helper.TC_CompetitionConfigurator :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.TC_Object);
            CORBA.Internals.Add_Parameter
              (TC_CompetitionConfigurator,
               CORBA.To_Any
                 (Name_�));
            CORBA.Internals.Add_Parameter
              (TC_CompetitionConfigurator,
               CORBA.To_Any
                 (Id_�));
            CORBA.TypeCode.Internals.Disable_Reference_Counting
              (broker.init.CompetitionConfigurator.Helper.TC_CompetitionConfigurator);
         end if;
      end Initialize_CompetitionConfigurator;

   end Internals;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return broker.init.CompetitionConfigurator.Ref
   is
   begin
      return To_Ref
        (CORBA.Object.Helper.From_Any
           (Item));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : broker.init.CompetitionConfigurator.Ref)
     return CORBA.Any
   is
      A : CORBA.Any :=
        CORBA.Object.Helper.To_Any
           (CORBA.Object.Ref
              (Item));
   begin
      CORBA.Internals.Set_Type
        (A,
         TC_CompetitionConfigurator);
      return A;
   end To_Any;

   ----------------------
   -- Unchecked_To_Ref --
   ----------------------

   function Unchecked_To_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return broker.init.CompetitionConfigurator.Ref
   is
      Result : broker.init.CompetitionConfigurator.Ref;
   begin
      Set
        (Result,
         CORBA.Object.Object_Of
           (The_Ref));
      return Result;
   end Unchecked_To_Ref;

   ------------
   -- To_Ref --
   ------------

   function To_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return broker.init.CompetitionConfigurator.Ref
   is
   begin
      if (CORBA.Object.Is_Nil
        (The_Ref)
         or else CORBA.Object.Is_A
           (The_Ref,
            Repository_Id))
      then
         return Unchecked_To_Ref
           (The_Ref);
      end if;
      CORBA.Raise_Bad_Param
        (CORBA.Default_Sys_Member);
   end To_Ref;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      broker.init.CompetitionConfigurator.Helper.Internals.Initialize_CompetitionConfigurator;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"broker.init.CompetitionConfigurator.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end broker.init.CompetitionConfigurator.Helper;
