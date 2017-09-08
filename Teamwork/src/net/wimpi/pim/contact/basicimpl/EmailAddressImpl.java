/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.basicimpl;

import net.wimpi.pim.contact.model.EmailAddress;
import net.wimpi.pim.util.AbstractIdentifiable;

/**
 * A basic and simple implementation of an
 * {@link net.wimpi.pim.contact.model.EmailAddress}.
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class EmailAddressImpl
    extends AbstractIdentifiable
    implements EmailAddress {

  static final long serialVersionUID = -1098179650039438919L;

  //instance attributes
  protected String m_Address;
  protected String m_Type = TYPE_INTERNET;		//defaults to Internet

  public String getAddress() {
    return m_Address;
  }//getAddress

  public void setAddress(String addr) {
    m_Address = addr;
  }//setAddress

  public String getType() {
    return m_Type;
  }//getType

  public void setType(String type) {
    m_Type = type;
  }//setType

  public boolean isType(String type) {
    return m_Type.equals(type);
  }//isType

}//class EmailAddressImpl
