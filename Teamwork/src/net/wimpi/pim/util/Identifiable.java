/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.util;

/**
 * An interface for classes producing
 * unique identifiable instances.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public interface Identifiable {

  /**
   * Returns the unique identifier of
   * this <tt>Identifiable</tt> instance.
   *
   * @return the unique identifier as <tt>String</tt>.
   */
  public String getUID();

}//interface Identifiable
