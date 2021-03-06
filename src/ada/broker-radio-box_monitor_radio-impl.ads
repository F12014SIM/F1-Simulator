with CORBA;
with PortableServer;

with Broker.Radio.Box_Monitor_Radio;

package Broker.Radio.Box_Monitor_Radio.impl is

   type Object is new PortableServer.Servant_Base with null record;

   type Object_Acc is access Object;

   procedure Force_Pitstop (Self : access Object;
                            Force : Corba.Boolean);

   procedure GetUpdate(Self : access Object;
                       num : in CORBA.Short;
                       time : out CORBA.Float;
                       metres : out CORBA.Float;
                       Returns : out CORBA.String);

end Broker.Radio.Box_Monitor_Radio.impl;
