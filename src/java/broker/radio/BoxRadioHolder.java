package broker.radio;

/**
* broker/radio/BoxRadioHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from ../idl/radio.idl
* lunedì 11 ottobre 2010 16.12.43 CEST
*/

public final class BoxRadioHolder implements org.omg.CORBA.portable.Streamable
{
  public broker.radio.BoxRadio value = null;

  public BoxRadioHolder ()
  {
  }

  public BoxRadioHolder (broker.radio.BoxRadio initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = broker.radio.BoxRadioHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    broker.radio.BoxRadioHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return broker.radio.BoxRadioHelper.type ();
  }

}
