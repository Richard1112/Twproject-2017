/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.model;

import java.io.Serializable;

/**
 * An interface modeling an extension
 * based on the types and information defined
 * by the vCard Mime directory profile standard.
 * <p>
 * For reference see RFC 2426:<br>
 * 3.8 EXTENDED TYPES<br>
 * <p>
 * @author Dieter Wimberger (wimpi)
 * @version 0.1 (22/07/2003)
 */
public interface Extension
    extends Serializable {

  /**
   * Returns the identifier for this <tt>Extension</tt>.
   *
   * @return the identifier as <tt>String</tt>.
   */
  public String getIdentifier();


}//interface Extension
