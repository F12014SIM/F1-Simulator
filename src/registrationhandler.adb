pragma Style_Checks ("NM32766");
---------------------------------------------------
--  This file has been generated automatically from
--  registrationHandler.idl
--  by IAC (IDL to Ada Compiler) GPL 2009-20090519 (rev. 144248).
---------------------------------------------------
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------
with PolyORB.Any.NVList;
with PolyORB.Types;
with PolyORB.Any;
with PolyORB.Requests;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Exceptions;

package body RegistrationHandler is

   Remote_Join_Arg_Name_competitorDescriptor_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("competitorDescriptor");

   Remote_Join_Arg_Name_address_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("address");

   Remote_Join_Arg_Name_radioAddress_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("radioAddress");

   Remote_Join_Arg_Name_compId_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("compId");

   Remote_Join_Arg_Name_monitorSystemAddress_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("monitorSystemAddress");

   Remote_Join_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   --------------------------
   -- Remote_Join_Result_� --
   --------------------------

   function Remote_Join_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (Remote_Join_Result_�);
   begin
      return (Name => Remote_Join_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_Void),
      Arg_Modes => 0);
   end Remote_Join_Result_�;

   -----------------
   -- Remote_Join --
   -----------------

   procedure Remote_Join
     (Self : Ref;
      competitorDescriptor : CORBA.String;
      address : CORBA.String;
      radioAddress : out CORBA.String;
      compId : out CORBA.Short;
      monitorSystemAddress : out CORBA.String)
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Arg_CC_competitorDescriptor_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (competitorDescriptor'Unrestricted_Access);
      Arg_Any_competitorDescriptor_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_String,
            Arg_CC_competitorDescriptor_�'Unchecked_Access);
      Arg_CC_address_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (address'Unrestricted_Access);
      Arg_Any_address_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_String,
            Arg_CC_address_�'Unchecked_Access);
      Arg_CC_radioAddress_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (radioAddress'Unrestricted_Access);
      Arg_Any_radioAddress_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_String,
            Arg_CC_radioAddress_�'Unchecked_Access);
      pragma Warnings (Off, radioAddress);
      Arg_CC_compId_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (compId'Unrestricted_Access);
      Arg_Any_compId_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_compId_�'Unchecked_Access);
      pragma Warnings (Off, compId);
      Arg_CC_monitorSystemAddress_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (monitorSystemAddress'Unrestricted_Access);
      Arg_Any_monitorSystemAddress_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_String,
            Arg_CC_monitorSystemAddress_�'Unchecked_Access);
      pragma Warnings (Off, monitorSystemAddress);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        Remote_Join_Result_�;
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
         Remote_Join_Arg_Name_competitorDescriptor_�,
         PolyORB.Any.Any
           (Arg_Any_competitorDescriptor_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         Remote_Join_Arg_Name_address_�,
         PolyORB.Any.Any
           (Arg_Any_address_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         Remote_Join_Arg_Name_radioAddress_�,
         PolyORB.Any.Any
           (Arg_Any_radioAddress_�),
         PolyORB.Any.ARG_OUT);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         Remote_Join_Arg_Name_compId_�,
         PolyORB.Any.Any
           (Arg_Any_compId_�),
         PolyORB.Any.ARG_OUT);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         Remote_Join_Arg_Name_monitorSystemAddress_�,
         PolyORB.Any.Any
           (Arg_Any_monitorSystemAddress_�),
         PolyORB.Any.ARG_OUT);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => "Remote_Join",
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
   end Remote_Join;

   Remote_Ready_Arg_Name_compId_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("compId");

   Remote_Ready_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   ---------------------------
   -- Remote_Ready_Result_� --
   ---------------------------

   function Remote_Ready_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (Remote_Ready_Result_�);
   begin
      return (Name => Remote_Ready_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_Boolean),
      Arg_Modes => 0);
   end Remote_Ready_Result_�;

   ------------------
   -- Remote_Ready --
   ------------------

   function Remote_Ready
     (Self : Ref;
      compId : CORBA.Short)
     return CORBA.Boolean
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Result_� : CORBA.Boolean;
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
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        Remote_Ready_Result_�;
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
         Remote_Ready_Arg_Name_compId_�,
         PolyORB.Any.Any
           (Arg_Any_compId_�),
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
         Operation => "Remote_Ready",
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
   end Remote_Ready;

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
         RegistrationHandler.Repository_Id)
         or else CORBA.Is_Equivalent
           (Logical_Type_Id,
            "IDL:omg.org/CORBA/Object:1.0"))
         or else False);
   end Is_A;

end RegistrationHandler;
