pragma Style_Checks ("NM32766");
---------------------------------------------------
--  This file has been generated automatically from
--  box_monitor_radio.idl
--  by IAC (IDL to Ada Compiler) GPL 2009-20090519 (rev. 144248).
---------------------------------------------------
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------
with Box_Monitor_Radio.Impl;
with CORBA;
pragma Elaborate_All (CORBA);
with PolyORB.Any;
with CORBA.NVList;
with CORBA.ServerRequest;
with PolyORB.CORBA_P.IR_Hooks;
with CORBA.Object;
with CORBA.Object.Helper;
with PolyORB.CORBA_P.Domain_Management;
with PortableServer;
with PolyORB.Std;
with CORBA.ORB;
with PolyORB.CORBA_P.Exceptions;
with PolyORB.Qos.Exception_Informations;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body Box_Monitor_Radio.Skel is

   GetUpdate_Arg_Name_num_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("num");

   GetUpdate_Arg_Name_time_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("time");

   procedure Invoke
     (Self : PortableServer.Servant;
      Request : CORBA.ServerRequest.Object_Ptr);

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Self : PortableServer.Servant;
      Request : CORBA.ServerRequest.Object_Ptr)
   is
      Operation_� : constant PolyORB.Std.String :=
        CORBA.To_Standard_String
           (CORBA.ServerRequest.Operation
              (Request.all));
      Argument_List_� : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_�);
      begin
         if (Operation_�
            = "_is_a")
         then
            declare
               Type_Id_� : CORBA.String;
               Arg_Name_Type_Id_� : constant CORBA.Identifier :=
                 CORBA.To_CORBA_String
                    ("Type_Id_�");
               Argument_Type_Id_� : constant CORBA.Any :=
                 CORBA.To_Any
                    (Type_Id_�);
               Result_� : CORBA.Boolean;
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  Arg_Name_Type_Id_�,
                  Argument_Type_Id_�,
                  CORBA.ARG_IN);
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               Type_Id_� :=
                 CORBA.From_Any
                    (Argument_Type_Id_�);
               Result_� :=
                 Is_A
                    (CORBA.To_Standard_String
                       (Type_Id_�));
               CORBA.ServerRequest.Set_Result
                 (Request,
                  CORBA.To_Any
                    (Result_�));
            end;
         elsif (Operation_�
            = "GetUpdate")
         then
            declare
               Argument_num_� : CORBA.Short;
               pragma Warnings (Off, Argument_num_�);
               Arg_CC_num_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_num_�'Unrestricted_Access);
               Arg_Any_num_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_num_�'Unchecked_Access);
               Argument_time_� : CORBA.Float;
               pragma Warnings (Off, Argument_time_�);
               Arg_CC_time_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_time_�'Unrestricted_Access);
               Arg_Any_time_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Float,
                     Arg_CC_time_�'Unchecked_Access);
               Result_� : CORBA.String;
               pragma Warnings (Off, Result_�);
               Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Result_�'Unrestricted_Access);
               Arg_Any_Result_�_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_String,
                     Arg_CC_Result_�_�'Unchecked_Access);
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  GetUpdate_Arg_Name_num_�,
                  Arg_Any_num_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  GetUpdate_Arg_Name_time_�,
                  Arg_Any_time_�,
                  CORBA.ARG_OUT);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Box_Monitor_Radio.Impl.GetUpdate
                 (Box_Monitor_Radio.Impl.Object'Class
                    (Self.all)'Access,
                  Argument_num_�,
                  Argument_time_�,
                  Result_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "_interface")
         then
            CORBA.ServerRequest.Arguments
              (Request,
               Argument_List_�);
            CORBA.ServerRequest.Set_Result
              (Request,
               CORBA.Object.Helper.To_Any
                 (CORBA.Object.Ref
                    (PolyORB.CORBA_P.IR_Hooks.Get_Interface_Definition
                       (CORBA.To_CORBA_String
                          (Repository_Id)))));

         elsif (Operation_�
            = "_domain_managers")
         then
            CORBA.ServerRequest.Arguments
              (Request,
               Argument_List_�);
            CORBA.ServerRequest.Set_Result
              (Request,
               PolyORB.CORBA_P.Domain_Management.Get_Domain_Managers
                 (Self));

         elsif ((Operation_�
            = "_non_existent")
            or else (Operation_�
               = "_non_existent"))
         then
            CORBA.ServerRequest.Arguments
              (Request,
               Argument_List_�);
            CORBA.ServerRequest.Set_Result
              (Request,
               CORBA.To_Any
                 (CORBA.Boolean'
                    (False)));

         else
            CORBA.Raise_Bad_Operation
              (CORBA.Default_Sys_Member);
         end if;
      exception
         when E : others =>
            CORBA.ServerRequest.Set_Exception
              (Request,
               PolyORB.CORBA_P.Exceptions.System_Exception_To_Any
                 (E));
            PolyORB.Qos.Exception_Informations.Set_Exception_Information
              (Request,
               E);
      end;
   end Invoke;

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean;

   ------------------
   -- Servant_Is_A --
   ------------------

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean
   is
   begin
      return (Obj.all
         in Box_Monitor_Radio.Impl.Object'Class);
   end Servant_Is_A;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      PortableServer.Internals.Register_Skeleton
        (Box_Monitor_Radio.Repository_Id,
         Servant_Is_A'Access,
         Is_A'Access,
         Invoke'Access);
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"Box_Monitor_Radio.Skel",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => PolyORB.Utils.Strings.Lists.Empty,
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end Box_Monitor_Radio.Skel;
