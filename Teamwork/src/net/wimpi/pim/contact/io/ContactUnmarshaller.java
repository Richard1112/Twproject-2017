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

import java.io.InputStream;

/**
 * Interface modeling a <tt>ContactMarshaller</tt>.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public interface ContactUnmarshaller {

  /**
   * Sets the encoding used by this <tt>ContactUnmarshaller</tt>.
   *
   * @param enc the encoding as <tt>String</tt>.
   */
  public void setEncoding(String enc);

  /**
   * Unmarshalls a contact from the given <tt>InpuStream</tt>.
   *
   * @param in the <tt>InputStream</tt> to read from.
   */
  public Contact unmarshallContact(InputStream in);

  /**
   * Unmarshalls a group of contacts from the given
   * <tt>InputStream</tt>.
   *
   * @param in the <tt>InputStream</tt> to read from.
   */
  public Contact[] unmarshallContacts(InputStream in);

  /**
   * Sets the strict flag for the unmarshalling
   * mechanism.
   * <p>
   * If this flag is set, then the unmarshalling should fail
   * for input data that does not comply to the specfication.
   * If it is not set, then the implementation should make
   * efforts to recover at least all parts that comply to the
   * standard.
   * <p>
   * Note that the default setting for this flag is supposed
   * to be <tt>true</tt>.
   *
   * @param b true for setting the flag, false otherwise.
   */
  public void setStrict(boolean b);

  /**
   * Tests if this <tt>ContactUnmarshaller</tt> flag for
   * strict mode is set.
   *
   * @return true if flag for strict set, false otherwise.
   */
  public boolean isStrict();

}//interface ContactUnmarshaller
