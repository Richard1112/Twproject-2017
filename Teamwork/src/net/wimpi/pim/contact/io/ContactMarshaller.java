/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.io;

import net.wimpi.pim.contact.model.Contact;

import java.io.OutputStream;

/**
 * Interface modeling a <tt>ContactMarshaller</tt>.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public interface ContactMarshaller {

  /**
   * Sets the encoding used by this <tt>ContactMarshaller</tt>.
   *
   * @param enc the encoding as <tt>String</tt>.
   */
  public void setEncoding(String enc);

  /**
   * Marshalls a contact to the given <tt>OutputStream</tt>.
   *
   * @param out the <tt>OutputStream</tt> to write to.
   * @param contact the <tt>Contact</tt> to be marshalled.
   */
  public void marshallContact(OutputStream out, Contact contact);

  /**
   * Marshalls a group of contacts to the given
   * <tt>OutputStream</tt>.
   *
   * @param out the <tt>OutputStream</tt> to write to.
   * @param contacts the <tt>Contact[]</tt> to be marshalled.
   */
  public void marshallContacts(OutputStream out, Contact[] contacts);

}//interface ContactMarshaller
