
/**
* RegistrationHandlerHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from registrationHandler.idl
* Tuesday, August 17, 2010 12:39:16 AM CEST
*/

public final class RegistrationHandlerHolder implements org.omg.CORBA.portable.Streamable
{
  public RegistrationHandler value = null;

  public RegistrationHandlerHolder ()
  {
  }

  public RegistrationHandlerHolder (RegistrationHandler initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = RegistrationHandlerHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    RegistrationHandlerHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return RegistrationHandlerHelper.type ();
  }

}
