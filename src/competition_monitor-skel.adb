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
with Competition_Monitor.Impl;
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

package body Competition_Monitor.Skel is

   getClassific_Arg_Name_idComp_In_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("idComp_In");

   getInfo_Arg_Name_lap_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("lap");

   getInfo_Arg_Name_sector_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("sector");

   getInfo_Arg_Name_id_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("id");

   getBestSector_Arg_Name_index_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("index");

   getCondCar_Arg_Name_competitorIdIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorIdIn");

   getCompetitor_Arg_Name_competitorIdIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorIdIn");

   getCompetitorTimeSector_Arg_Name_competitorIdIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorIdIn");

   getCompetitorTimeSector_Arg_Name_sectorIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("sectorIn");

   getCompetitorTimeLap_Arg_Name_competitorIdIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorIdIn");

   getCompetitorTimeLap_Arg_Name_lapIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("lapIn");

   getCompetitorTimeCheck_Arg_Name_competitorIdIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorIdIn");

   getCompetitorTimeCheck_Arg_Name_checkpointIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("checkpointIn");

   getGas_Arg_Name_competitorIdIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorIdIn");

   getGas_Arg_Name_Sector_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Sector");

   getGas_Arg_Name_lapIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("lapIn");

   getTyreUsury_Arg_Name_competitorIdIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorIdIn");

   getTyreUsury_Arg_Name_sectorIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("sectorIn");

   getTyreUsury_Arg_Name_lapIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("lapIn");

   getMeanSpeed_Arg_Name_competitorIdIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorIdIn");

   getMeanSpeed_Arg_Name_sectorIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("sectorIn");

   getMeanSpeed_Arg_Name_lapIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("lapIn");

   getTime_Arg_Name_competitorIdIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorIdIn");

   getTime_Arg_Name_sectorIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("sectorIn");

   getTime_Arg_Name_lapIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("lapIn");

   getMeanGasConsumption_Arg_Name_competitorIdIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorIdIn");

   getMeanGasConsumption_Arg_Name_sectorIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("sectorIn");

   getMeanGasConsumption_Arg_Name_lapIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("lapIn");

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
            = "getClassific")
         then
            declare
               Argument_idComp_In_� : CORBA.Short;
               pragma Warnings (Off, Argument_idComp_In_�);
               Arg_CC_idComp_In_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_idComp_In_�'Unrestricted_Access);
               Arg_Any_idComp_In_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_idComp_In_�'Unchecked_Access);
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
                  getClassific_Arg_Name_idComp_In_�,
                  Arg_Any_idComp_In_�,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 Competition_Monitor.Impl.getClassific
                    (Competition_Monitor.Impl.Object'Class
                       (Self.all)'Access,
                     Argument_idComp_In_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "getInfo")
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
                  getInfo_Arg_Name_lap_�,
                  Arg_Any_lap_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getInfo_Arg_Name_sector_�,
                  Arg_Any_sector_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getInfo_Arg_Name_id_�,
                  Arg_Any_id_�,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 Competition_Monitor.Impl.getInfo
                    (Competition_Monitor.Impl.Object'Class
                       (Self.all)'Access,
                     Argument_lap_�,
                     Argument_sector_�,
                     Argument_id_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "getBestLap")
         then
            declare
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
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 Competition_Monitor.Impl.getBestLap
                    (Competition_Monitor.Impl.Object'Class
                       (Self.all)'Access);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "getBestSector")
         then
            declare
               Argument_index_� : CORBA.Short;
               pragma Warnings (Off, Argument_index_�);
               Arg_CC_index_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_index_�'Unrestricted_Access);
               Arg_Any_index_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_index_�'Unchecked_Access);
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
                  getBestSector_Arg_Name_index_�,
                  Arg_Any_index_�,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 Competition_Monitor.Impl.getBestSector
                    (Competition_Monitor.Impl.Object'Class
                       (Self.all)'Access,
                     Argument_index_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "getCondCar")
         then
            declare
               Argument_competitorIdIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorIdIn_�);
               Arg_CC_competitorIdIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorIdIn_�'Unrestricted_Access);
               Arg_Any_competitorIdIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorIdIn_�'Unchecked_Access);
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
                  getCondCar_Arg_Name_competitorIdIn_�,
                  Arg_Any_competitorIdIn_�,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 Competition_Monitor.Impl.getCondCar
                    (Competition_Monitor.Impl.Object'Class
                       (Self.all)'Access,
                     Argument_competitorIdIn_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "getCompetitor")
         then
            declare
               Argument_competitorIdIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorIdIn_�);
               Arg_CC_competitorIdIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorIdIn_�'Unrestricted_Access);
               Arg_Any_competitorIdIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorIdIn_�'Unchecked_Access);
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
                  getCompetitor_Arg_Name_competitorIdIn_�,
                  Arg_Any_competitorIdIn_�,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 Competition_Monitor.Impl.getCompetitor
                    (Competition_Monitor.Impl.Object'Class
                       (Self.all)'Access,
                     Argument_competitorIdIn_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "getCompetitorTimeSector")
         then
            declare
               Argument_competitorIdIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorIdIn_�);
               Arg_CC_competitorIdIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorIdIn_�'Unrestricted_Access);
               Arg_Any_competitorIdIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorIdIn_�'Unchecked_Access);
               Argument_sectorIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_sectorIn_�);
               Arg_CC_sectorIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_sectorIn_�'Unrestricted_Access);
               Arg_Any_sectorIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_sectorIn_�'Unchecked_Access);
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
                  getCompetitorTimeSector_Arg_Name_competitorIdIn_�,
                  Arg_Any_competitorIdIn_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getCompetitorTimeSector_Arg_Name_sectorIn_�,
                  Arg_Any_sectorIn_�,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 Competition_Monitor.Impl.getCompetitorTimeSector
                    (Competition_Monitor.Impl.Object'Class
                       (Self.all)'Access,
                     Argument_competitorIdIn_�,
                     Argument_sectorIn_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "getCompetitorTimeLap")
         then
            declare
               Argument_competitorIdIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorIdIn_�);
               Arg_CC_competitorIdIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorIdIn_�'Unrestricted_Access);
               Arg_Any_competitorIdIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorIdIn_�'Unchecked_Access);
               Argument_lapIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_lapIn_�);
               Arg_CC_lapIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_lapIn_�'Unrestricted_Access);
               Arg_Any_lapIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_lapIn_�'Unchecked_Access);
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
                  getCompetitorTimeLap_Arg_Name_competitorIdIn_�,
                  Arg_Any_competitorIdIn_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getCompetitorTimeLap_Arg_Name_lapIn_�,
                  Arg_Any_lapIn_�,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 Competition_Monitor.Impl.getCompetitorTimeLap
                    (Competition_Monitor.Impl.Object'Class
                       (Self.all)'Access,
                     Argument_competitorIdIn_�,
                     Argument_lapIn_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "getCompetitorTimeCheck")
         then
            declare
               Argument_competitorIdIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorIdIn_�);
               Arg_CC_competitorIdIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorIdIn_�'Unrestricted_Access);
               Arg_Any_competitorIdIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorIdIn_�'Unchecked_Access);
               Argument_checkpointIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_checkpointIn_�);
               Arg_CC_checkpointIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_checkpointIn_�'Unrestricted_Access);
               Arg_Any_checkpointIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_checkpointIn_�'Unchecked_Access);
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
                  getCompetitorTimeCheck_Arg_Name_competitorIdIn_�,
                  Arg_Any_competitorIdIn_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getCompetitorTimeCheck_Arg_Name_checkpointIn_�,
                  Arg_Any_checkpointIn_�,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 Competition_Monitor.Impl.getCompetitorTimeCheck
                    (Competition_Monitor.Impl.Object'Class
                       (Self.all)'Access,
                     Argument_competitorIdIn_�,
                     Argument_checkpointIn_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "getGas")
         then
            declare
               Argument_competitorIdIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorIdIn_�);
               Arg_CC_competitorIdIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorIdIn_�'Unrestricted_Access);
               Arg_Any_competitorIdIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorIdIn_�'Unchecked_Access);
               Argument_Sector_� : CORBA.Short;
               pragma Warnings (Off, Argument_Sector_�);
               Arg_CC_Sector_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Sector_�'Unrestricted_Access);
               Arg_Any_Sector_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Sector_�'Unchecked_Access);
               Argument_lapIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_lapIn_�);
               Arg_CC_lapIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_lapIn_�'Unrestricted_Access);
               Arg_Any_lapIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_lapIn_�'Unchecked_Access);
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
                  getGas_Arg_Name_competitorIdIn_�,
                  Arg_Any_competitorIdIn_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getGas_Arg_Name_Sector_�,
                  Arg_Any_Sector_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getGas_Arg_Name_lapIn_�,
                  Arg_Any_lapIn_�,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 Competition_Monitor.Impl.getGas
                    (Competition_Monitor.Impl.Object'Class
                       (Self.all)'Access,
                     Argument_competitorIdIn_�,
                     Argument_Sector_�,
                     Argument_lapIn_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "getTyreUsury")
         then
            declare
               Argument_competitorIdIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorIdIn_�);
               Arg_CC_competitorIdIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorIdIn_�'Unrestricted_Access);
               Arg_Any_competitorIdIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorIdIn_�'Unchecked_Access);
               Argument_sectorIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_sectorIn_�);
               Arg_CC_sectorIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_sectorIn_�'Unrestricted_Access);
               Arg_Any_sectorIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_sectorIn_�'Unchecked_Access);
               Argument_lapIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_lapIn_�);
               Arg_CC_lapIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_lapIn_�'Unrestricted_Access);
               Arg_Any_lapIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_lapIn_�'Unchecked_Access);
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
                  getTyreUsury_Arg_Name_competitorIdIn_�,
                  Arg_Any_competitorIdIn_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getTyreUsury_Arg_Name_sectorIn_�,
                  Arg_Any_sectorIn_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getTyreUsury_Arg_Name_lapIn_�,
                  Arg_Any_lapIn_�,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 Competition_Monitor.Impl.getTyreUsury
                    (Competition_Monitor.Impl.Object'Class
                       (Self.all)'Access,
                     Argument_competitorIdIn_�,
                     Argument_sectorIn_�,
                     Argument_lapIn_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "getMeanSpeed")
         then
            declare
               Argument_competitorIdIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorIdIn_�);
               Arg_CC_competitorIdIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorIdIn_�'Unrestricted_Access);
               Arg_Any_competitorIdIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorIdIn_�'Unchecked_Access);
               Argument_sectorIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_sectorIn_�);
               Arg_CC_sectorIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_sectorIn_�'Unrestricted_Access);
               Arg_Any_sectorIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_sectorIn_�'Unchecked_Access);
               Argument_lapIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_lapIn_�);
               Arg_CC_lapIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_lapIn_�'Unrestricted_Access);
               Arg_Any_lapIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_lapIn_�'Unchecked_Access);
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
                  getMeanSpeed_Arg_Name_competitorIdIn_�,
                  Arg_Any_competitorIdIn_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getMeanSpeed_Arg_Name_sectorIn_�,
                  Arg_Any_sectorIn_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getMeanSpeed_Arg_Name_lapIn_�,
                  Arg_Any_lapIn_�,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 Competition_Monitor.Impl.getMeanSpeed
                    (Competition_Monitor.Impl.Object'Class
                       (Self.all)'Access,
                     Argument_competitorIdIn_�,
                     Argument_sectorIn_�,
                     Argument_lapIn_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "getTime")
         then
            declare
               Argument_competitorIdIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorIdIn_�);
               Arg_CC_competitorIdIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorIdIn_�'Unrestricted_Access);
               Arg_Any_competitorIdIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorIdIn_�'Unchecked_Access);
               Argument_sectorIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_sectorIn_�);
               Arg_CC_sectorIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_sectorIn_�'Unrestricted_Access);
               Arg_Any_sectorIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_sectorIn_�'Unchecked_Access);
               Argument_lapIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_lapIn_�);
               Arg_CC_lapIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_lapIn_�'Unrestricted_Access);
               Arg_Any_lapIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_lapIn_�'Unchecked_Access);
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
                  getTime_Arg_Name_competitorIdIn_�,
                  Arg_Any_competitorIdIn_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getTime_Arg_Name_sectorIn_�,
                  Arg_Any_sectorIn_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getTime_Arg_Name_lapIn_�,
                  Arg_Any_lapIn_�,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 Competition_Monitor.Impl.getTime
                    (Competition_Monitor.Impl.Object'Class
                       (Self.all)'Access,
                     Argument_competitorIdIn_�,
                     Argument_sectorIn_�,
                     Argument_lapIn_�);
               --  Setting the result
               CORBA.ServerRequest.Set_Result
                 (Request,
                  Arg_Any_Result_�_�);
               CORBA.NVList.Internals.Clone_Out_Args
                 (Argument_List_�);
            end;
         elsif (Operation_�
            = "getMeanGasConsumption")
         then
            declare
               Argument_competitorIdIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorIdIn_�);
               Arg_CC_competitorIdIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorIdIn_�'Unrestricted_Access);
               Arg_Any_competitorIdIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorIdIn_�'Unchecked_Access);
               Argument_sectorIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_sectorIn_�);
               Arg_CC_sectorIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_sectorIn_�'Unrestricted_Access);
               Arg_Any_sectorIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_sectorIn_�'Unchecked_Access);
               Argument_lapIn_� : CORBA.Short;
               pragma Warnings (Off, Argument_lapIn_�);
               Arg_CC_lapIn_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_lapIn_�'Unrestricted_Access);
               Arg_Any_lapIn_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_lapIn_�'Unchecked_Access);
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
                  getMeanGasConsumption_Arg_Name_competitorIdIn_�,
                  Arg_Any_competitorIdIn_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getMeanGasConsumption_Arg_Name_sectorIn_�,
                  Arg_Any_sectorIn_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getMeanGasConsumption_Arg_Name_lapIn_�,
                  Arg_Any_lapIn_�,
                  CORBA.ARG_IN);
               --  Processing request
               CORBA.ServerRequest.Arguments
                 (Request,
                  Argument_List_�);
               --  Call Implementation
               Result_� :=
                 Competition_Monitor.Impl.getMeanGasConsumption
                    (Competition_Monitor.Impl.Object'Class
                       (Self.all)'Access,
                     Argument_competitorIdIn_�,
                     Argument_sectorIn_�,
                     Argument_lapIn_�);
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
         in Competition_Monitor.Impl.Object'Class);
   end Servant_Is_A;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      PortableServer.Internals.Register_Skeleton
        (Competition_Monitor.Repository_Id,
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
           (Name => +"Competition_Monitor.Skel",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => PolyORB.Utils.Strings.Lists.Empty,
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end Competition_Monitor.Skel;
