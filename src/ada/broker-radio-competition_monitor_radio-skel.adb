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
with broker.radio.Competition_Monitor_Radio.Impl;
with CORBA;
pragma Elaborate_All (CORBA);
with PolyORB.Any;
with CORBA.NVList;
with CORBA.ServerRequest;
with broker.radio.Competition_Monitor_Radio.Helper;
with broker.radio.Competition_Monitor_Radio;
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

package body broker.radio.Competition_Monitor_Radio.Skel is

   Get_CompetitorInfo_Arg_Name_lap_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("lap");

   Get_CompetitorInfo_Arg_Name_sector_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("sector");

   Get_CompetitorInfo_Arg_Name_id_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("id");

   Get_CompetitorInfo_Arg_Name_time_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("time");

   Get_CompetitorInfo_Arg_Name_metres_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("metres");

   Get_CompetitionInfo_Arg_Name_timeInstant_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("timeInstant");

   Get_CompetitionInfo_Arg_Name_xmlInfo_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("xmlInfo");

   ready_Arg_Name_competitorId_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorId");

   Get_CompetitionConfiguration_Arg_Name_xmlConf_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("xmlConf");

   Get_CompetitorConfiguration_Arg_Name_id_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("id");

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
            = "Get_CompetitorInfo")
         then
            declare
               Argument_lap_� : CORBA.Short;
               pragma Warnings (Off, Argument_lap_�);
               Arg_CC_lap_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_lap_�'Unrestricted_Access);
               Arg_Any_lap_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_lap_�'Unchecked_Access);
               Argument_sector_� : CORBA.Short;
               pragma Warnings (Off, Argument_sector_�);
               Arg_CC_sector_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_sector_�'Unrestricted_Access);
               Arg_Any_sector_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_sector_�'Unchecked_Access);
               Argument_id_� : CORBA.Short;
               pragma Warnings (Off, Argument_id_�);
               Arg_CC_id_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_id_�'Unrestricted_Access);
               Arg_Any_id_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_id_�'Unchecked_Access);
               Argument_time_� : CORBA.Float;
               pragma Warnings (Off, Argument_time_�);
               Arg_CC_time_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_time_�'Unrestricted_Access);
               Arg_Any_time_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Float,
                     Arg_CC_time_�'Unchecked_Access);
               Argument_metres_� : CORBA.Float;
               pragma Warnings (Off, Argument_metres_�);
               Arg_CC_metres_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_metres_�'Unrestricted_Access);
               Arg_Any_metres_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Float,
                     Arg_CC_metres_�'Unchecked_Access);
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
                  Get_CompetitorInfo_Arg_Name_lap_�,
                  Arg_Any_lap_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  Get_CompetitorInfo_Arg_Name_sector_�,
                  Arg_Any_sector_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  Get_CompetitorInfo_Arg_Name_id_�,
                  Arg_Any_id_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  Get_CompetitorInfo_Arg_Name_time_�,
                  Arg_Any_time_�,
                  CORBA.ARG_OUT);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  Get_CompetitorInfo_Arg_Name_metres_�,
                  Arg_Any_metres_�,
                  CORBA.ARG_OUT);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               broker.radio.Competition_Monitor_Radio.Impl.Get_CompetitorInfo
                 (broker.radio.Competition_Monitor_Radio.Impl.Object'Class
                    (Self.all)'Access,
                  Argument_lap_�,
                  Argument_sector_�,
                  Argument_id_�,
                  Argument_time_�,
                  Argument_metres_�,
                  Result_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "Get_CompetitionInfo")
         then
            declare
               Argument_timeInstant_� : CORBA.Float;
               pragma Warnings (Off, Argument_timeInstant_�);
               Arg_CC_timeInstant_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_timeInstant_�'Unrestricted_Access);
               Arg_Any_timeInstant_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Float,
                     Arg_CC_timeInstant_�'Unchecked_Access);
               Argument_xmlInfo_� : CORBA.String;
               pragma Warnings (Off, Argument_xmlInfo_�);
               Arg_CC_xmlInfo_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_xmlInfo_�'Unrestricted_Access);
               Arg_Any_xmlInfo_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_String,
                     Arg_CC_xmlInfo_�'Unchecked_Access);
               Result_� : broker.radio.Competition_Monitor_Radio.float_sequence;
               pragma Warnings (Off, Result_�);
               Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
                 broker.radio.Competition_Monitor_Radio.Helper.Internals.Wrap
                    (broker.radio.Competition_Monitor_Radio.IDL_SEQUENCE_float.Sequence
                       (Result_�)'Unrestricted_Access);
               Arg_Any_Result_�_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (broker.radio.Competition_Monitor_Radio.Helper.TC_float_sequence,
                     Arg_CC_Result_�_�'Unchecked_Access);
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  Get_CompetitionInfo_Arg_Name_timeInstant_�,
                  Arg_Any_timeInstant_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  Get_CompetitionInfo_Arg_Name_xmlInfo_�,
                  Arg_Any_xmlInfo_�,
                  CORBA.ARG_OUT);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               broker.radio.Competition_Monitor_Radio.Impl.Get_CompetitionInfo
                 (broker.radio.Competition_Monitor_Radio.Impl.Object'Class
                    (Self.all)'Access,
                  Argument_timeInstant_�,
                  Argument_xmlInfo_�,
                  Result_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "ready")
         then
            declare
               Argument_competitorId_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorId_�);
               Arg_CC_competitorId_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorId_�'Unrestricted_Access);
               Arg_Any_competitorId_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorId_�'Unchecked_Access);
               Result_� : CORBA.Boolean;
               pragma Warnings (Off, Result_�);
               Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Result_�'Unrestricted_Access);
               Arg_Any_Result_�_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Boolean,
                     Arg_CC_Result_�_�'Unchecked_Access);
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  ready_Arg_Name_competitorId_�,
                  Arg_Any_competitorId_�,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 broker.radio.Competition_Monitor_Radio.Impl.ready
                    (broker.radio.Competition_Monitor_Radio.Impl.Object'Class
                       (Self.all)'Access,
                     Argument_competitorId_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "Get_CompetitionConfiguration")
         then
            declare
               Argument_xmlConf_� : CORBA.String;
               pragma Warnings (Off, Argument_xmlConf_�);
               Arg_CC_xmlConf_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_xmlConf_�'Unrestricted_Access);
               Arg_Any_xmlConf_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_String,
                     Arg_CC_xmlConf_�'Unchecked_Access);
               Result_� : CORBA.Float;
               pragma Warnings (Off, Result_�);
               Arg_CC_Result_�_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Result_�'Unrestricted_Access);
               Arg_Any_Result_�_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Float,
                     Arg_CC_Result_�_�'Unchecked_Access);
            begin
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  Get_CompetitionConfiguration_Arg_Name_xmlConf_�,
                  Arg_Any_xmlConf_�,
                  CORBA.ARG_INOUT);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               broker.radio.Competition_Monitor_Radio.Impl.Get_CompetitionConfiguration
                 (broker.radio.Competition_Monitor_Radio.Impl.Object'Class
                    (Self.all)'Access,
                  Argument_xmlConf_�,
                  Result_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "Get_CompetitorConfiguration")
         then
            declare
               Argument_id_� : CORBA.Short;
               pragma Warnings (Off, Argument_id_�);
               Arg_CC_id_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_id_�'Unrestricted_Access);
               Arg_Any_id_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_id_�'Unchecked_Access);
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
                  Get_CompetitorConfiguration_Arg_Name_id_�,
                  Arg_Any_id_�,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 broker.radio.Competition_Monitor_Radio.Impl.Get_CompetitorConfiguration
                    (broker.radio.Competition_Monitor_Radio.Impl.Object'Class
                       (Self.all)'Access,
                     Argument_id_�);
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
         in broker.radio.Competition_Monitor_Radio.Impl.Object'Class);
   end Servant_Is_A;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      PortableServer.Internals.Register_Skeleton
        (broker.radio.Competition_Monitor_Radio.Repository_Id,
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
           (Name => +"broker.radio.Competition_Monitor_Radio.Skel",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => PolyORB.Utils.Strings.Lists.Empty,
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end broker.radio.Competition_Monitor_Radio.Skel;
