package broker.radio;

/**
* broker/radio/Competition_Monitor_RadioHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from ../idl/radio.idl
* Sunday, September 26, 2010 2:23:33 PM CEST
*/

public final class Competition_Monitor_RadioHolder implements org.omg.CORBA.portable.Streamable
{
  public broker.radio.Competition_Monitor_Radio value = null;

  public Competition_Monitor_RadioHolder ()
  {
  }

  public Competition_Monitor_RadioHolder (broker.radio.Competition_Monitor_Radio initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = broker.radio.Competition_Monitor_RadioHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    broker.radio.Competition_Monitor_RadioHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return broker.radio.Competition_Monitor_RadioHelper.type ();
  }

}
