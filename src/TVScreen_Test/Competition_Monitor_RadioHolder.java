
/**
* Competition_Monitor_RadioHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from competition_monitor_radio.idl
* Friday, August 27, 2010 7:23:49 PM CEST
*/

public final class Competition_Monitor_RadioHolder implements org.omg.CORBA.portable.Streamable
{
  public Competition_Monitor_Radio value = null;

  public Competition_Monitor_RadioHolder ()
  {
  }

  public Competition_Monitor_RadioHolder (Competition_Monitor_Radio initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = Competition_Monitor_RadioHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    Competition_Monitor_RadioHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return Competition_Monitor_RadioHelper.type ();
  }

}
