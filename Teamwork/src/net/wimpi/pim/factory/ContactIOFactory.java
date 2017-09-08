/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.factory;

import net.wimpi.pim.contact.io.ContactMarshaller;
import net.wimpi.pim.contact.io.ContactUnmarshaller;

/**
 * Interface for a <tt>ContactIOFactory</tt>, which
 * can be used to instantiate marshaller and unmarshaller
 * instances.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public interface ContactIOFactory {


  /**
   * Returns a new <tt>ContactMarshaller</tt> instance.
   *
   * @return the newly created <tt>ContactMarshaller</tt> instance.
   */
  public ContactMarshaller createContactMarshaller();

  /**
   * Returns a new <tt>ContactUnmarshaller</tt> instance.
   *
   * @return the newly created <tt>ContactUnmarshaller</tt> instance.
   */
  public ContactUnmarshaller createContactUnmarshaller();

}//interface ContactIOFactory
