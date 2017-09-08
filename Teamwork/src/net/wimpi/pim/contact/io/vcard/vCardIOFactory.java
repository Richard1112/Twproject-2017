/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.io.vcard;

import net.wimpi.pim.contact.io.ContactMarshaller;
import net.wimpi.pim.contact.io.ContactUnmarshaller;
import net.wimpi.pim.factory.ContactIOFactory;

/**
 * Class representing a <tt>ContactIOFactory</tt>, which
 * can be used to instantiate vCard marshaller and unmarshaller
 * instances.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class vCardIOFactory implements ContactIOFactory {

  public ContactMarshaller createContactMarshaller() {
    return new vCardMarshaller();
  }//createContactMarshaller

  public ContactUnmarshaller createContactUnmarshaller() {
    return new vCardUnmarshaller();
  }//createContactUnmarshaller

}//class vCardIOFactory
