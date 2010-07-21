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

   Join_Competition_Arg_Name_competitorDescriptorFile_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("competitorDescriptorFile");

   Join_Competition_Arg_Name_boxCorbaLoc_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("boxCorbaLoc");

   Join_Competition_Arg_Name_monitorCorbaLoc_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("monitorCorbaLoc");

   Join_Competition_Arg_Name_competitorId_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("competitorId");

   Join_Competition_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   -------------------------------
   -- Join_Competition_Result_� --
   -------------------------------

   function Join_Competition_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (Join_Competition_Result_�);
   begin
      return (Name => Join_Competition_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_Void),
      Arg_Modes => 0);
   end Join_Competition_Result_�;

   ----------------------
   -- Join_Competition --
   ----------------------

   procedure Join_Competition
     (Self : Ref;
      competitorDescriptorFile : CORBA.String;
      boxCorbaLoc : CORBA.String;
      monitorCorbaLoc : out CORBA.String;
      competitorId : out CORBA.Short)
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Arg_CC_competitorDescriptorFile_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (competitorDescriptorFile'Unrestricted_Access);
      Arg_Any_competitorDescriptorFile_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_String,
            Arg_CC_competitorDescriptorFile_�'Unchecked_Access);
      Arg_CC_boxCorbaLoc_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (boxCorbaLoc'Unrestricted_Access);
      Arg_Any_boxCorbaLoc_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_String,
            Arg_CC_boxCorbaLoc_�'Unchecked_Access);
      Arg_CC_monitorCorbaLoc_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (monitorCorbaLoc'Unrestricted_Access);
      Arg_Any_monitorCorbaLoc_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_String,
            Arg_CC_monitorCorbaLoc_�'Unchecked_Access);
      pragma Warnings (Off, monitorCorbaLoc);
      Arg_CC_competitorId_� : aliased PolyORB.Any.Content'Class :=
        CORBA.Wrap
           (competitorId'Unrestricted_Access);
      Arg_Any_competitorId_� : constant CORBA.Any :=
        CORBA.Internals.Get_Wrapper_Any
           (CORBA.TC_Short,
            Arg_CC_competitorId_�'Unchecked_Access);
      pragma Warnings (Off, competitorId);
      Request_� : PolyORB.Requests.Request_Access;
      Result_Nv_� : PolyORB.Any.NamedValue :=
        Join_Competition_Result_�;
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
         Join_Competition_Arg_Name_competitorDescriptorFile_�,
         PolyORB.Any.Any
           (Arg_Any_competitorDescriptorFile_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         Join_Competition_Arg_Name_boxCorbaLoc_�,
         PolyORB.Any.Any
           (Arg_Any_boxCorbaLoc_�),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         Join_Competition_Arg_Name_monitorCorbaLoc_�,
         PolyORB.Any.Any
           (Arg_Any_monitorCorbaLoc_�),
         PolyORB.Any.ARG_OUT);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         Join_Competition_Arg_Name_competitorId_�,
         PolyORB.Any.Any
           (Arg_Any_competitorId_�),
         PolyORB.Any.ARG_OUT);
      --  Creating the request
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => "Join_Competition",
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
   end Join_Competition;

   Wait_Ready_Arg_Name_competitorId_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("competitorId");

   Wait_Ready_Result_Name_� : constant PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("Result");

   -------------------------
   -- Wait_Ready_Result_� --
   -------------------------

   function Wait_Ready_Result_� return PolyORB.Any.NamedValue is
      pragma Inline (Wait_Ready_Result_�);
   begin
      return (Name => Wait_Ready_Result_Name_�,
      Argument => CORBA.Internals.Get_Empty_Any
        (CORBA.TC_String),
      Arg_Modes => 0);
   end Wait_Ready_Result_�;

   ----------------
   -- Wait_Ready --
   ----------------

   function Wait_Ready
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
        Wait_Ready_Result_�;
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
         Wait_Ready_Arg_Name_competitorId_�,
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
         Operation => "Wait_Ready",
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
   end Wait_Ready;

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
