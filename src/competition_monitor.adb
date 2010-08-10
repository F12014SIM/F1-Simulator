pragma Style_Checks ("NM32766");
---------------------------------------------------
--  This file has been generated automatically from
--  competition_monitor.idl
--  by IAC (IDL to Ada Compiler) GPL 2009-20090519 (rev. 144248).
---------------------------------------------------
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------
with PolyORB.Any.NVList;
with PolyORB.Any;
with PolyORB.Requests;
with PolyORB.Types;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Exceptions;

package body Competition_Monitor is

   getClassific_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   ---------------------------
   -- getClassific_Result_� --
   ---------------------------

   function getClassific_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getClassific_Result_�);
   begin
      return (Name => getClassific_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getClassific_Result_�;

   ------------------
   -- getClassific --
   ------------------

   function getClassific
     (Self : Ref)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getClassific_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      --  Create the Argument list
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      --  Setting the result value
      PolyORB.Any.Set_Value
        (PolyORB.Any.Get_Container
           (Result_Nv_�.Argument).all,
         Arg_CC_Result_�_�'Unrestricted_Access);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => "getClassific",
         Arg_List => Argument_List_�,
         Result => Result_Nv_�,
         Req => Request_�);
      --  Invoking the request (synchronously or asynchronously)
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      --  Raise exception, if needed
      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence
        (Request_�);
      PolyORB.Requests.Destroy_Request
        (Request_�);
      --  Return value
      return Result_�;
   end getClassific;

   getBestLap_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   -------------------------
   -- getBestLap_Result_� --
   -------------------------

   function getBestLap_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getBestLap_Result_�);
   begin
      return (Name => getBestLap_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getBestLap_Result_�;

   ----------------
   -- getBestLap --
   ----------------

   function getBestLap
     (Self : Ref)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getBestLap_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      --  Create the Argument list
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      --  Setting the result value
      PolyORB.Any.Set_Value
        (PolyORB.Any.Get_Container
           (Result_Nv_�.Argument).all,
         Arg_CC_Result_�_�'Unrestricted_Access);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => "getBestLap",
         Arg_List => Argument_List_�,
         Result => Result_Nv_�,
         Req => Request_�);
      --  Invoking the request (synchronously or asynchronously)
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      --  Raise exception, if needed
      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence
        (Request_�);
      PolyORB.Requests.Destroy_Request
        (Request_�);
      --  Return value
      return Result_�;
   end getBestLap;

   getBestSector_Arg_Name_index_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("index");

   getBestSector_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   ----------------------------
   -- getBestSector_Result_� --
   ----------------------------

   function getBestSector_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getBestSector_Result_�);
   begin
      return (Name => getBestSector_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getBestSector_Result_�;

   -------------------
   -- getBestSector --
   -------------------

   function getBestSector
     (Self : Ref;
      index : CORBA.Short)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Arg_CC_index_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (index'Unrestricted_Access);
      Arg_Any_index_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_index_�'Unchecked_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getBestSector_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      --  Create the Argument list
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      --  Fill the Argument list
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getBestSector_Arg_Name_index_�,
         PolyORB.Any.Any
           (Arg_Any_index_�),
         PolyORB.Any.ARG_IN);
      --  Setting the result value
      PolyORB.Any.Set_Value
        (PolyORB.Any.Get_Container
           (Result_Nv_�.Argument).all,
         Arg_CC_Result_�_�'Unrestricted_Access);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => "getBestSector",
         Arg_List => Argument_List_�,
         Result => Result_Nv_�,
         Req => Request_�);
      --  Invoking the request (synchronously or asynchronously)
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      --  Raise exception, if needed
      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence
        (Request_�);
      PolyORB.Requests.Destroy_Request
        (Request_�);
      --  Return value
      return Result_�;
   end getBestSector;

   getCondCar_Arg_Name_competitorID_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("competitorID");

   getCondCar_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   -------------------------
   -- getCondCar_Result_� --
   -------------------------

   function getCondCar_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getCondCar_Result_�);
   begin
      return (Name => getCondCar_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getCondCar_Result_�;

   ----------------
   -- getCondCar --
   ----------------

   function getCondCar
     (Self : Ref;
      competitorID : CORBA.Short)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Arg_CC_competitorID_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (competitorID'Unrestricted_Access);
      Arg_Any_competitorID_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_competitorID_�'Unchecked_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getCondCar_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      --  Create the Argument list
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      --  Fill the Argument list
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getCondCar_Arg_Name_competitorID_�,
         PolyORB.Any.Any
           (Arg_Any_competitorID_�),
         PolyORB.Any.ARG_IN);
      --  Setting the result value
      PolyORB.Any.Set_Value
        (PolyORB.Any.Get_Container
           (Result_Nv_�.Argument).all,
         Arg_CC_Result_�_�'Unrestricted_Access);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => "getCondCar",
         Arg_List => Argument_List_�,
         Result => Result_Nv_�,
         Req => Request_�);
      --  Invoking the request (synchronously or asynchronously)
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      --  Raise exception, if needed
      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence
        (Request_�);
      PolyORB.Requests.Destroy_Request
        (Request_�);
      --  Return value
      return Result_�;
   end getCondCar;

   getCompetitor_Arg_Name_competitorID_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("competitorID");

   getCompetitor_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   ----------------------------
   -- getCompetitor_Result_� --
   ----------------------------

   function getCompetitor_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getCompetitor_Result_�);
   begin
      return (Name => getCompetitor_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getCompetitor_Result_�;

   -------------------
   -- getCompetitor --
   -------------------

   function getCompetitor
     (Self : Ref;
      competitorID : CORBA.Short)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Arg_CC_competitorID_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (competitorID'Unrestricted_Access);
      Arg_Any_competitorID_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_competitorID_�'Unchecked_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getCompetitor_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      --  Create the Argument list
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      --  Fill the Argument list
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getCompetitor_Arg_Name_competitorID_�,
         PolyORB.Any.Any
           (Arg_Any_competitorID_�),
         PolyORB.Any.ARG_IN);
      --  Setting the result value
      PolyORB.Any.Set_Value
        (PolyORB.Any.Get_Container
           (Result_Nv_�.Argument).all,
         Arg_CC_Result_�_�'Unrestricted_Access);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => "getCompetitor",
         Arg_List => Argument_List_�,
         Result => Result_Nv_�,
         Req => Request_�);
      --  Invoking the request (synchronously or asynchronously)
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      --  Raise exception, if needed
      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence
        (Request_�);
      PolyORB.Requests.Destroy_Request
        (Request_�);
      --  Return value
      return Result_�;
   end getCompetitor;

   getCompetitorTimeSector_Arg_Name_competitorID_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("competitorID");

   getCompetitorTimeSector_Arg_Name_sectorIn_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("sectorIn");

   getCompetitorTimeSector_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   --------------------------------------
   -- getCompetitorTimeSector_Result_� --
   --------------------------------------

   function getCompetitorTimeSector_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getCompetitorTimeSector_Result_�);
   begin
      return (Name => getCompetitorTimeSector_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getCompetitorTimeSector_Result_�;

   -----------------------------
   -- getCompetitorTimeSector --
   -----------------------------

   function getCompetitorTimeSector
     (Self : Ref;
      competitorID : CORBA.Short;
      sectorIn : CORBA.Short)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Arg_CC_competitorID_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (competitorID'Unrestricted_Access);
      Arg_Any_competitorID_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_competitorID_�'Unchecked_Access);
      Arg_CC_sectorIn_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (sectorIn'Unrestricted_Access);
      Arg_Any_sectorIn_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_sectorIn_�'Unchecked_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getCompetitorTimeSector_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      --  Create the Argument list
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      --  Fill the Argument list
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getCompetitorTimeSector_Arg_Name_competitorID_�,
         PolyORB.Any.Any
           (Arg_Any_competitorID_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getCompetitorTimeSector_Arg_Name_sectorIn_�,
         PolyORB.Any.Any
           (Arg_Any_sectorIn_�),
         PolyORB.Any.ARG_IN);
      --  Setting the result value
      PolyORB.Any.Set_Value
        (PolyORB.Any.Get_Container
           (Result_Nv_�.Argument).all,
         Arg_CC_Result_�_�'Unrestricted_Access);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => "getCompetitorTimeSector",
         Arg_List => Argument_List_�,
         Result => Result_Nv_�,
         Req => Request_�);
      --  Invoking the request (synchronously or asynchronously)
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      --  Raise exception, if needed
      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence
        (Request_�);
      PolyORB.Requests.Destroy_Request
        (Request_�);
      --  Return value
      return Result_�;
   end getCompetitorTimeSector;

   getCompetitorTimeLap_Arg_Name_competitorID_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("competitorID");

   getCompetitorTimeLap_Arg_Name_lapIn_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("lapIn");

   getCompetitorTimeLap_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   -----------------------------------
   -- getCompetitorTimeLap_Result_� --
   -----------------------------------

   function getCompetitorTimeLap_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getCompetitorTimeLap_Result_�);
   begin
      return (Name => getCompetitorTimeLap_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getCompetitorTimeLap_Result_�;

   --------------------------
   -- getCompetitorTimeLap --
   --------------------------

   function getCompetitorTimeLap
     (Self : Ref;
      competitorID : CORBA.Short;
      lapIn : CORBA.Short)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Arg_CC_competitorID_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (competitorID'Unrestricted_Access);
      Arg_Any_competitorID_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_competitorID_�'Unchecked_Access);
      Arg_CC_lapIn_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (lapIn'Unrestricted_Access);
      Arg_Any_lapIn_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_lapIn_�'Unchecked_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getCompetitorTimeLap_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      --  Create the Argument list
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      --  Fill the Argument list
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getCompetitorTimeLap_Arg_Name_competitorID_�,
         PolyORB.Any.Any
           (Arg_Any_competitorID_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getCompetitorTimeLap_Arg_Name_lapIn_�,
         PolyORB.Any.Any
           (Arg_Any_lapIn_�),
         PolyORB.Any.ARG_IN);
      --  Setting the result value
      PolyORB.Any.Set_Value
        (PolyORB.Any.Get_Container
           (Result_Nv_�.Argument).all,
         Arg_CC_Result_�_�'Unrestricted_Access);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => "getCompetitorTimeLap",
         Arg_List => Argument_List_�,
         Result => Result_Nv_�,
         Req => Request_�);
      --  Invoking the request (synchronously or asynchronously)
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      --  Raise exception, if needed
      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence
        (Request_�);
      PolyORB.Requests.Destroy_Request
        (Request_�);
      --  Return value
      return Result_�;
   end getCompetitorTimeLap;

   getCompetitorTimeCheck_Arg_Name_competitorID_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("competitorID");

   getCompetitorTimeCheck_Arg_Name_checkpoint_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("checkpoint");

   getCompetitorTimeCheck_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   -------------------------------------
   -- getCompetitorTimeCheck_Result_� --
   -------------------------------------

   function getCompetitorTimeCheck_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getCompetitorTimeCheck_Result_�);
   begin
      return (Name => getCompetitorTimeCheck_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getCompetitorTimeCheck_Result_�;

   ----------------------------
   -- getCompetitorTimeCheck --
   ----------------------------

   function getCompetitorTimeCheck
     (Self : Ref;
      competitorID : CORBA.Short;
      checkpoint : CORBA.Short)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Arg_CC_competitorID_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (competitorID'Unrestricted_Access);
      Arg_Any_competitorID_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_competitorID_�'Unchecked_Access);
      Arg_CC_checkpoint_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (checkpoint'Unrestricted_Access);
      Arg_Any_checkpoint_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_checkpoint_�'Unchecked_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getCompetitorTimeCheck_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      --  Create the Argument list
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      --  Fill the Argument list
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getCompetitorTimeCheck_Arg_Name_competitorID_�,
         PolyORB.Any.Any
           (Arg_Any_competitorID_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getCompetitorTimeCheck_Arg_Name_checkpoint_�,
         PolyORB.Any.Any
           (Arg_Any_checkpoint_�),
         PolyORB.Any.ARG_IN);
      --  Setting the result value
      PolyORB.Any.Set_Value
        (PolyORB.Any.Get_Container
           (Result_Nv_�.Argument).all,
         Arg_CC_Result_�_�'Unrestricted_Access);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => "getCompetitorTimeCheck",
         Arg_List => Argument_List_�,
         Result => Result_Nv_�,
         Req => Request_�);
      --  Invoking the request (synchronously or asynchronously)
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      --  Raise exception, if needed
      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence
        (Request_�);
      PolyORB.Requests.Destroy_Request
        (Request_�);
      --  Return value
      return Result_�;
   end getCompetitorTimeCheck;

   getGas_Arg_Name_Competitor_Id_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Competitor_Id");

   getGas_Arg_Name_Sector_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Sector");

   getGas_Arg_Name_Lap_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Lap");

   getGas_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   ---------------------
   -- getGas_Result_� --
   ---------------------

   function getGas_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getGas_Result_�);
   begin
      return (Name => getGas_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getGas_Result_�;

   ------------
   -- getGas --
   ------------

   function getGas
     (Self : Ref;
      Competitor_Id : CORBA.Short;
      Sector : CORBA.Short;
      Lap : CORBA.Short)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Arg_CC_Competitor_Id_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Competitor_Id'Unrestricted_Access);
      Arg_Any_Competitor_Id_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_Competitor_Id_�'Unchecked_Access);
      Arg_CC_Sector_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Sector'Unrestricted_Access);
      Arg_Any_Sector_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_Sector_�'Unchecked_Access);
      Arg_CC_Lap_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Lap'Unrestricted_Access);
      Arg_Any_Lap_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_Lap_�'Unchecked_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getGas_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      --  Create the Argument list
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      --  Fill the Argument list
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getGas_Arg_Name_Competitor_Id_�,
         PolyORB.Any.Any
           (Arg_Any_Competitor_Id_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getGas_Arg_Name_Sector_�,
         PolyORB.Any.Any
           (Arg_Any_Sector_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getGas_Arg_Name_Lap_�,
         PolyORB.Any.Any
           (Arg_Any_Lap_�),
         PolyORB.Any.ARG_IN);
      --  Setting the result value
      PolyORB.Any.Set_Value
        (PolyORB.Any.Get_Container
           (Result_Nv_�.Argument).all,
         Arg_CC_Result_�_�'Unrestricted_Access);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => "getGas",
         Arg_List => Argument_List_�,
         Result => Result_Nv_�,
         Req => Request_�);
      --  Invoking the request (synchronously or asynchronously)
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      --  Raise exception, if needed
      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence
        (Request_�);
      PolyORB.Requests.Destroy_Request
        (Request_�);
      --  Return value
      return Result_�;
   end getGas;

   getTyreUsury_Arg_Name_Competitor_Id_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Competitor_Id");

   getTyreUsury_Arg_Name_Sector_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Sector");

   getTyreUsury_Arg_Name_Lap_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Lap");

   getTyreUsury_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   ---------------------------
   -- getTyreUsury_Result_� --
   ---------------------------

   function getTyreUsury_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getTyreUsury_Result_�);
   begin
      return (Name => getTyreUsury_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getTyreUsury_Result_�;

   ------------------
   -- getTyreUsury --
   ------------------

   function getTyreUsury
     (Self : Ref;
      Competitor_Id : CORBA.Short;
      Sector : CORBA.Short;
      Lap : CORBA.Short)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Arg_CC_Competitor_Id_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Competitor_Id'Unrestricted_Access);
      Arg_Any_Competitor_Id_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_Competitor_Id_�'Unchecked_Access);
      Arg_CC_Sector_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Sector'Unrestricted_Access);
      Arg_Any_Sector_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_Sector_�'Unchecked_Access);
      Arg_CC_Lap_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Lap'Unrestricted_Access);
      Arg_Any_Lap_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_Lap_�'Unchecked_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getTyreUsury_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      --  Create the Argument list
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      --  Fill the Argument list
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getTyreUsury_Arg_Name_Competitor_Id_�,
         PolyORB.Any.Any
           (Arg_Any_Competitor_Id_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getTyreUsury_Arg_Name_Sector_�,
         PolyORB.Any.Any
           (Arg_Any_Sector_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getTyreUsury_Arg_Name_Lap_�,
         PolyORB.Any.Any
           (Arg_Any_Lap_�),
         PolyORB.Any.ARG_IN);
      --  Setting the result value
      PolyORB.Any.Set_Value
        (PolyORB.Any.Get_Container
           (Result_Nv_�.Argument).all,
         Arg_CC_Result_�_�'Unrestricted_Access);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => "getTyreUsury",
         Arg_List => Argument_List_�,
         Result => Result_Nv_�,
         Req => Request_�);
      --  Invoking the request (synchronously or asynchronously)
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      --  Raise exception, if needed
      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence
        (Request_�);
      PolyORB.Requests.Destroy_Request
        (Request_�);
      --  Return value
      return Result_�;
   end getTyreUsury;

   getMeanSpeed_Arg_Name_Competitor_Id_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Competitor_Id");

   getMeanSpeed_Arg_Name_Sector_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Sector");

   getMeanSpeed_Arg_Name_Lap_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Lap");

   getMeanSpeed_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   ---------------------------
   -- getMeanSpeed_Result_� --
   ---------------------------

   function getMeanSpeed_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getMeanSpeed_Result_�);
   begin
      return (Name => getMeanSpeed_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getMeanSpeed_Result_�;

   ------------------
   -- getMeanSpeed --
   ------------------

   function getMeanSpeed
     (Self : Ref;
      Competitor_Id : CORBA.Short;
      Sector : CORBA.Short;
      Lap : CORBA.Short)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Arg_CC_Competitor_Id_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Competitor_Id'Unrestricted_Access);
      Arg_Any_Competitor_Id_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_Competitor_Id_�'Unchecked_Access);
      Arg_CC_Sector_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Sector'Unrestricted_Access);
      Arg_Any_Sector_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_Sector_�'Unchecked_Access);
      Arg_CC_Lap_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Lap'Unrestricted_Access);
      Arg_Any_Lap_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_Lap_�'Unchecked_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getMeanSpeed_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      --  Create the Argument list
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      --  Fill the Argument list
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getMeanSpeed_Arg_Name_Competitor_Id_�,
         PolyORB.Any.Any
           (Arg_Any_Competitor_Id_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getMeanSpeed_Arg_Name_Sector_�,
         PolyORB.Any.Any
           (Arg_Any_Sector_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getMeanSpeed_Arg_Name_Lap_�,
         PolyORB.Any.Any
           (Arg_Any_Lap_�),
         PolyORB.Any.ARG_IN);
      --  Setting the result value
      PolyORB.Any.Set_Value
        (PolyORB.Any.Get_Container
           (Result_Nv_�.Argument).all,
         Arg_CC_Result_�_�'Unrestricted_Access);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => "getMeanSpeed",
         Arg_List => Argument_List_�,
         Result => Result_Nv_�,
         Req => Request_�);
      --  Invoking the request (synchronously or asynchronously)
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      --  Raise exception, if needed
      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence
        (Request_�);
      PolyORB.Requests.Destroy_Request
        (Request_�);
      --  Return value
      return Result_�;
   end getMeanSpeed;

   getTime_Arg_Name_Competitor_Id_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Competitor_Id");

   getTime_Arg_Name_Sector_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Sector");

   getTime_Arg_Name_Lap_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Lap");

   getTime_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   ----------------------
   -- getTime_Result_� --
   ----------------------

   function getTime_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getTime_Result_�);
   begin
      return (Name => getTime_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getTime_Result_�;

   -------------
   -- getTime --
   -------------

   function getTime
     (Self : Ref;
      Competitor_Id : CORBA.Short;
      Sector : CORBA.Short;
      Lap : CORBA.Short)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Arg_CC_Competitor_Id_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Competitor_Id'Unrestricted_Access);
      Arg_Any_Competitor_Id_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_Competitor_Id_�'Unchecked_Access);
      Arg_CC_Sector_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Sector'Unrestricted_Access);
      Arg_Any_Sector_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_Sector_�'Unchecked_Access);
      Arg_CC_Lap_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Lap'Unrestricted_Access);
      Arg_Any_Lap_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_Lap_�'Unchecked_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getTime_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      --  Create the Argument list
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      --  Fill the Argument list
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getTime_Arg_Name_Competitor_Id_�,
         PolyORB.Any.Any
           (Arg_Any_Competitor_Id_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getTime_Arg_Name_Sector_�,
         PolyORB.Any.Any
           (Arg_Any_Sector_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getTime_Arg_Name_Lap_�,
         PolyORB.Any.Any
           (Arg_Any_Lap_�),
         PolyORB.Any.ARG_IN);
      --  Setting the result value
      PolyORB.Any.Set_Value
        (PolyORB.Any.Get_Container
           (Result_Nv_�.Argument).all,
         Arg_CC_Result_�_�'Unrestricted_Access);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => "getTime",
         Arg_List => Argument_List_�,
         Result => Result_Nv_�,
         Req => Request_�);
      --  Invoking the request (synchronously or asynchronously)
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      --  Raise exception, if needed
      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence
        (Request_�);
      PolyORB.Requests.Destroy_Request
        (Request_�);
      --  Return value
      return Result_�;
   end getTime;

   getMeanGasConsumption_Arg_Name_Competitor_Id_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Competitor_Id");

   getMeanGasConsumption_Arg_Name_Sector_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Sector");

   getMeanGasConsumption_Arg_Name_Lap_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Lap");

   getMeanGasConsumption_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   ------------------------------------
   -- getMeanGasConsumption_Result_� --
   ------------------------------------

   function getMeanGasConsumption_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getMeanGasConsumption_Result_�);
   begin
      return (Name => getMeanGasConsumption_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getMeanGasConsumption_Result_�;

   ---------------------------
   -- getMeanGasConsumption --
   ---------------------------

   function getMeanGasConsumption
     (Self : Ref;
      Competitor_Id : CORBA.Short;
      Sector : CORBA.Short;
      Lap : CORBA.Short)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Arg_CC_Competitor_Id_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Competitor_Id'Unrestricted_Access);
      Arg_Any_Competitor_Id_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_Competitor_Id_�'Unchecked_Access);
      Arg_CC_Sector_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Sector'Unrestricted_Access);
      Arg_Any_Sector_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_Sector_�'Unchecked_Access);
      Arg_CC_Lap_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Lap'Unrestricted_Access);
      Arg_Any_Lap_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_Lap_�'Unchecked_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getMeanGasConsumption_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      --  Create the Argument list
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      --  Fill the Argument list
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getMeanGasConsumption_Arg_Name_Competitor_Id_�,
         PolyORB.Any.Any
           (Arg_Any_Competitor_Id_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getMeanGasConsumption_Arg_Name_Sector_�,
         PolyORB.Any.Any
           (Arg_Any_Sector_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getMeanGasConsumption_Arg_Name_Lap_�,
         PolyORB.Any.Any
           (Arg_Any_Lap_�),
         PolyORB.Any.ARG_IN);
      --  Setting the result value
      PolyORB.Any.Set_Value
        (PolyORB.Any.Get_Container
           (Result_Nv_�.Argument).all,
         Arg_CC_Result_�_�'Unrestricted_Access);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => "getMeanGasConsumption",
         Arg_List => Argument_List_�,
         Result => Result_Nv_�,
         Req => Request_�);
      --  Invoking the request (synchronously or asynchronously)
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      --  Raise exception, if needed
      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence
        (Request_�);
      PolyORB.Requests.Destroy_Request
        (Request_�);
      --  Return value
      return Result_�;
   end getMeanGasConsumption;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self : Ref;
      Logical_Type_Id : PolyORB.Std.String)
     return CORBA.Boolean
   is
   begin
      return (False
         or else (Is_A
           (Logical_Type_Id)
            or else CORBA.Object.Is_A
              (CORBA.Object.Ref
                 (Self),
               Logical_Type_Id)));
   end Is_A;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Logical_Type_Id : PolyORB.Std.String)
     return CORBA.Boolean
   is
   begin
      return ((CORBA.Is_Equivalent
        (Logical_Type_Id,
         Competition_Monitor.Repository_Id)
         or else CORBA.Is_Equivalent
           (Logical_Type_Id,
            "IDL:omg.org/CORBA/Object:1.0"))
         or else False);
   end Is_A;

end Competition_Monitor;
