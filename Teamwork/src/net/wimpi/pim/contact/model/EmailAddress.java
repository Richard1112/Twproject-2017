/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.model;

import net.wimpi.pim.util.Identifiable;

import java.io.Serializable;

/**
 * An interface modeling an email address based on the
 * types and information of the vCard Mime directory
 * profile standard specification.
 * <p>
 * For reference see RFC 2426:<br>
 * 3.3.2 EMAIL Type Definition<br>
 * <br>
 * Note that for a standard conformant implementation
 * you have to observe that the default type is
 * <tt>INTERNET</tt>.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public interface EmailAddress
    extends Identifiable, Serializable {

  /**
   * Returns the address of this <tt>EmailAddress</tt>.
   *
   * @return the address as <tt>String</tt>.
   */
  public String getAddress();

  /**
   * Sets the address of this <tt>EmailAddress</tt>.
   *
   * @param addr the address as <tt>String</tt>.
   */
  public void setAddress(String addr);

  /**
   * Returns the type of this <tt>EmailAddress</tt>.
   *
   * @return the type as <tt<String</tt>.
   */
  public String getType();

  /**
   * Sets the type of this <tt>EmailAddress</tt>.
   *
   * @param type the type as <tt>String</tt>.
   */
  public void setType(String type);

  /**
   * Tests if this <tt>EmailAddress</tt> is
   * of a given type.
   * Common types are defined as constants of
   * the <tt>EmailAddress</tt>  interface.
   *
   * @return true if of given type, false otherwise.
   */
  public boolean isType(String TYPE);

  /**
   * Indicates an internet address type.
   */
  public static final String TYPE_INTERNET = "INTERNET";

  /**
   * Indicates an X400 address type.
   */
  public static final String TYPE_X400 = "X400";

}//interface EmailAddress
