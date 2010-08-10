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
with CORBA.ServerRequest;
with CORBA.NVList;
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

   getBestSector_Arg_Name_index_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("index");

   getCondCar_Arg_Name_competitorID_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorID");

   getCompetitor_Arg_Name_competitorID_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorID");

   getCompetitorTimeSector_Arg_Name_competitorID_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorID");

   getCompetitorTimeSector_Arg_Name_sectorIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("sectorIn");

   getCompetitorTimeLap_Arg_Name_competitorID_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorID");

   getCompetitorTimeLap_Arg_Name_lapIn_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("lapIn");

   getCompetitorTimeCheck_Arg_Name_competitorID_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("competitorID");

   getCompetitorTimeCheck_Arg_Name_checkpoint_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("checkpoint");

   getGas_Arg_Name_Competitor_Id_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Competitor_Id");

   getGas_Arg_Name_Sector_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Sector");

   getGas_Arg_Name_Lap_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Lap");

   getTyreUsury_Arg_Name_Competitor_Id_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Competitor_Id");

   getTyreUsury_Arg_Name_Sector_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Sector");

   getTyreUsury_Arg_Name_Lap_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Lap");

   getMeanSpeed_Arg_Name_Competitor_Id_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Competitor_Id");

   getMeanSpeed_Arg_Name_Sector_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Sector");

   getMeanSpeed_Arg_Name_Lap_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Lap");

   getTime_Arg_Name_Competitor_Id_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Competitor_Id");

   getTime_Arg_Name_Sector_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Sector");

   getTime_Arg_Name_Lap_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Lap");

   getMeanGasConsumption_Arg_Name_Competitor_Id_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Competitor_Id");

   getMeanGasConsumption_Arg_Name_Sector_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Sector");

   getMeanGasConsumption_Arg_Name_Lap_� : constant CORBA.Identifier :=
     CORBA.To_CORBA_String
        ("Lap");

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
                 Competition_Monitor.Impl.getClassific
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
               Argument_competitorID_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorID_�);
               Arg_CC_competitorID_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorID_�'Unrestricted_Access);
               Arg_Any_competitorID_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorID_�'Unchecked_Access);
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
                  getCondCar_Arg_Name_competitorID_�,
                  Arg_Any_competitorID_�,
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
                     Argument_competitorID_�);
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
               Argument_competitorID_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorID_�);
               Arg_CC_competitorID_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorID_�'Unrestricted_Access);
               Arg_Any_competitorID_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorID_�'Unchecked_Access);
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
                  getCompetitor_Arg_Name_competitorID_�,
                  Arg_Any_competitorID_�,
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
                     Argument_competitorID_�);
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
               Argument_competitorID_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorID_�);
               Arg_CC_competitorID_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorID_�'Unrestricted_Access);
               Arg_Any_competitorID_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorID_�'Unchecked_Access);
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
                  getCompetitorTimeSector_Arg_Name_competitorID_�,
                  Arg_Any_competitorID_�,
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
                     Argument_competitorID_�,
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
               Argument_competitorID_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorID_�);
               Arg_CC_competitorID_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorID_�'Unrestricted_Access);
               Arg_Any_competitorID_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorID_�'Unchecked_Access);
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
                  getCompetitorTimeLap_Arg_Name_competitorID_�,
                  Arg_Any_competitorID_�,
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
                     Argument_competitorID_�,
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
               Argument_competitorID_� : CORBA.Short;
               pragma Warnings (Off, Argument_competitorID_�);
               Arg_CC_competitorID_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_competitorID_�'Unrestricted_Access);
               Arg_Any_competitorID_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_competitorID_�'Unchecked_Access);
               Argument_checkpoint_� : CORBA.Short;
               pragma Warnings (Off, Argument_checkpoint_�);
               Arg_CC_checkpoint_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_checkpoint_�'Unrestricted_Access);
               Arg_Any_checkpoint_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_checkpoint_�'Unchecked_Access);
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
                  getCompetitorTimeCheck_Arg_Name_competitorID_�,
                  Arg_Any_competitorID_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getCompetitorTimeCheck_Arg_Name_checkpoint_�,
                  Arg_Any_checkpoint_�,
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
                     Argument_competitorID_�,
                     Argument_checkpoint_�);
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
               Argument_Competitor_Id_� : CORBA.Short;
               pragma Warnings (Off, Argument_Competitor_Id_�);
               Arg_CC_Competitor_Id_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Competitor_Id_�'Unrestricted_Access);
               Arg_Any_Competitor_Id_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Competitor_Id_�'Unchecked_Access);
               Argument_Sector_� : CORBA.Short;
               pragma Warnings (Off, Argument_Sector_�);
               Arg_CC_Sector_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Sector_�'Unrestricted_Access);
               Arg_Any_Sector_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Sector_�'Unchecked_Access);
               Argument_Lap_� : CORBA.Short;
               pragma Warnings (Off, Argument_Lap_�);
               Arg_CC_Lap_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Lap_�'Unrestricted_Access);
               Arg_Any_Lap_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Lap_�'Unchecked_Access);
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
                  getGas_Arg_Name_Competitor_Id_�,
                  Arg_Any_Competitor_Id_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getGas_Arg_Name_Sector_�,
                  Arg_Any_Sector_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getGas_Arg_Name_Lap_�,
                  Arg_Any_Lap_�,
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
                     Argument_Competitor_Id_�,
                     Argument_Sector_�,
                     Argument_Lap_�);
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
               Argument_Competitor_Id_� : CORBA.Short;
               pragma Warnings (Off, Argument_Competitor_Id_�);
               Arg_CC_Competitor_Id_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Competitor_Id_�'Unrestricted_Access);
               Arg_Any_Competitor_Id_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Competitor_Id_�'Unchecked_Access);
               Argument_Sector_� : CORBA.Short;
               pragma Warnings (Off, Argument_Sector_�);
               Arg_CC_Sector_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Sector_�'Unrestricted_Access);
               Arg_Any_Sector_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Sector_�'Unchecked_Access);
               Argument_Lap_� : CORBA.Short;
               pragma Warnings (Off, Argument_Lap_�);
               Arg_CC_Lap_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Lap_�'Unrestricted_Access);
               Arg_Any_Lap_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Lap_�'Unchecked_Access);
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
                  getTyreUsury_Arg_Name_Competitor_Id_�,
                  Arg_Any_Competitor_Id_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getTyreUsury_Arg_Name_Sector_�,
                  Arg_Any_Sector_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getTyreUsury_Arg_Name_Lap_�,
                  Arg_Any_Lap_�,
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
                     Argument_Competitor_Id_�,
                     Argument_Sector_�,
                     Argument_Lap_�);
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
               Argument_Competitor_Id_� : CORBA.Short;
               pragma Warnings (Off, Argument_Competitor_Id_�);
               Arg_CC_Competitor_Id_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Competitor_Id_�'Unrestricted_Access);
               Arg_Any_Competitor_Id_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Competitor_Id_�'Unchecked_Access);
               Argument_Sector_� : CORBA.Short;
               pragma Warnings (Off, Argument_Sector_�);
               Arg_CC_Sector_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Sector_�'Unrestricted_Access);
               Arg_Any_Sector_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Sector_�'Unchecked_Access);
               Argument_Lap_� : CORBA.Short;
               pragma Warnings (Off, Argument_Lap_�);
               Arg_CC_Lap_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Lap_�'Unrestricted_Access);
               Arg_Any_Lap_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Lap_�'Unchecked_Access);
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
                  getMeanSpeed_Arg_Name_Competitor_Id_�,
                  Arg_Any_Competitor_Id_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getMeanSpeed_Arg_Name_Sector_�,
                  Arg_Any_Sector_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getMeanSpeed_Arg_Name_Lap_�,
                  Arg_Any_Lap_�,
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
                     Argument_Competitor_Id_�,
                     Argument_Sector_�,
                     Argument_Lap_�);
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
               Argument_Competitor_Id_� : CORBA.Short;
               pragma Warnings (Off, Argument_Competitor_Id_�);
               Arg_CC_Competitor_Id_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Competitor_Id_�'Unrestricted_Access);
               Arg_Any_Competitor_Id_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Competitor_Id_�'Unchecked_Access);
               Argument_Sector_� : CORBA.Short;
               pragma Warnings (Off, Argument_Sector_�);
               Arg_CC_Sector_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Sector_�'Unrestricted_Access);
               Arg_Any_Sector_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Sector_�'Unchecked_Access);
               Argument_Lap_� : CORBA.Short;
               pragma Warnings (Off, Argument_Lap_�);
               Arg_CC_Lap_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Lap_�'Unrestricted_Access);
               Arg_Any_Lap_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Lap_�'Unchecked_Access);
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
                  getTime_Arg_Name_Competitor_Id_�,
                  Arg_Any_Competitor_Id_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getTime_Arg_Name_Sector_�,
                  Arg_Any_Sector_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getTime_Arg_Name_Lap_�,
                  Arg_Any_Lap_�,
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
                     Argument_Competitor_Id_�,
                     Argument_Sector_�,
                     Argument_Lap_�);
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
               Argument_Competitor_Id_� : CORBA.Short;
               pragma Warnings (Off, Argument_Competitor_Id_�);
               Arg_CC_Competitor_Id_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Competitor_Id_�'Unrestricted_Access);
               Arg_Any_Competitor_Id_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Competitor_Id_�'Unchecked_Access);
               Argument_Sector_� : CORBA.Short;
               pragma Warnings (Off, Argument_Sector_�);
               Arg_CC_Sector_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Sector_�'Unrestricted_Access);
               Arg_Any_Sector_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Sector_�'Unchecked_Access);
               Argument_Lap_� : CORBA.Short;
               pragma Warnings (Off, Argument_Lap_�);
               Arg_CC_Lap_� : aliased PolyORB.Any.Content'Class :=
                 CORBA.Wrap
                    (Argument_Lap_�'Unrestricted_Access);
               Arg_Any_Lap_� : constant CORBA.Any :=
                 CORBA.Internals.Get_Wrapper_Any
                    (CORBA.TC_Short,
                     Arg_CC_Lap_�'Unchecked_Access);
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
                  getMeanGasConsumption_Arg_Name_Competitor_Id_�,
                  Arg_Any_Competitor_Id_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getMeanGasConsumption_Arg_Name_Sector_�,
                  Arg_Any_Sector_�,
                  CORBA.ARG_IN);
               CORBA.NVList.Add_Item
                 (Argument_List_�,
                  getMeanGasConsumption_Arg_Name_Lap_�,
                  Arg_Any_Lap_�,
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
                     Argument_Competitor_Id_�,
                     Argument_Sector_�,
                     Argument_Lap_�);
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
