pragma Style_Checks ("NM32766");
---------------------------------------------------
--  This file has been generated automatically from
--  monitorSystem.idl
--  by IAC (IDL to Ada Compiler) GPL 2009-20090519 (rev. 144248).
---------------------------------------------------
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------
with PolyORB.Any.NVList;
with PolyORB.Any;
with PolyORB.Types;
with PolyORB.Requests;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Exceptions;

package body MonitorSystem is

   getCompetitorInfo_Arg_Name_competitorId_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("competitorId");

   getCompetitorInfo_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   --------------------------------
   -- getCompetitorInfo_Result_� --
   --------------------------------

   function getCompetitorInfo_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getCompetitorInfo_Result_�);
   begin
      return (Name => getCompetitorInfo_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getCompetitorInfo_Result_�;

   -----------------------
   -- getCompetitorInfo --
   -----------------------

   function getCompetitorInfo
     (Self : Ref;
      competitorId : CORBA.Short)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Arg_CC_competitorId_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (competitorId'Unrestricted_Access);
      Arg_Any_competitorId_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_competitorId_�'Unchecked_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getCompetitorInfo_Result_�;
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
         getCompetitorInfo_Arg_Name_competitorId_�,
         PolyORB.Any.Any
           (Arg_Any_competitorId_�),
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
         Operation => "getCompetitorInfo",
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
   end getCompetitorInfo;

   getCompetitionInfo_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   ---------------------------------
   -- getCompetitionInfo_Result_� --
   ---------------------------------

   function getCompetitionInfo_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getCompetitionInfo_Result_�);
   begin
      return (Name => getCompetitionInfo_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getCompetitionInfo_Result_�;

   ------------------------
   -- getCompetitionInfo --
   ------------------------

   function getCompetitionInfo
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
        getCompetitionInfo_Result_�;
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
         Operation => "getCompetitionInfo",
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
   end getCompetitionInfo;

   getEverything_Arg_Name_competitorId_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("competitorId");

   getEverything_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   ----------------------------
   -- getEverything_Result_� --
   ----------------------------

   function getEverything_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getEverything_Result_�);
   begin
      return (Name => getEverything_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getEverything_Result_�;

   -------------------
   -- getEverything --
   -------------------

   function getEverything
     (Self : Ref;
      competitorId : CORBA.Short)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Arg_CC_competitorId_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (competitorId'Unrestricted_Access);
      Arg_Any_competitorId_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_competitorId_�'Unchecked_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getEverything_Result_�;
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
         getEverything_Arg_Name_competitorId_�,
         PolyORB.Any.Any
           (Arg_Any_competitorId_�),
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
         Operation => "getEverything",
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
   end getEverything;

   getClassification_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   --------------------------------
   -- getClassification_Result_� --
   --------------------------------

   function getClassification_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getClassification_Result_�);
   begin
      return (Name => getClassification_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getClassification_Result_�;

   -----------------------
   -- getClassification --
   -----------------------

   function getClassification
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
        getClassification_Result_�;
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
         Operation => "getClassification",
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
   end getClassification;

   getStatsBySect_Arg_Name_compId_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("compId");

   getStatsBySect_Arg_Name_sector_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("sector");

   getStatsBySect_Arg_Name_lap_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("lap");

   getStatsBySect_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   -----------------------------
   -- getStatsBySect_Result_� --
   -----------------------------

   function getStatsBySect_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (getStatsBySect_Result_�);
   begin
      return (Name => getStatsBySect_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end getStatsBySect_Result_�;

   --------------------
   -- getStatsBySect --
   --------------------

   function getStatsBySect
     (Self : Ref;
      compId : CORBA.Short;
      sector : CORBA.Short;
      lap : CORBA.Short)
     return CORBA.String
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.String;
      pragma Warnings (Off, Result_�);
      Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (Result_�'Unrestricted_Access);
      Arg_CC_compId_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (compId'Unrestricted_Access);
      Arg_Any_compId_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_compId_�'Unchecked_Access);
      Arg_CC_sector_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (sector'Unrestricted_Access);
      Arg_Any_sector_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_sector_�'Unchecked_Access);
      Arg_CC_lap_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (lap'Unrestricted_Access);
      Arg_Any_lap_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_lap_�'Unchecked_Access);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        getStatsBySect_Result_�;
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
         getStatsBySect_Arg_Name_compId_�,
         PolyORB.Any.Any
           (Arg_Any_compId_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getStatsBySect_Arg_Name_sector_�,
         PolyORB.Any.Any
           (Arg_Any_sector_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         getStatsBySect_Arg_Name_lap_�,
         PolyORB.Any.Any
           (Arg_Any_lap_�),
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
         Operation => "getStatsBySect",
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
   end getStatsBySect;

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
         MonitorSystem.Repository_Id)
         or else CORBA.Is_Equivalent
           (Logical_Type_Id,
            "IDL:omg.org/CORBA/Object:1.0"))
         or else False);
   end Is_A;

end MonitorSystem;
