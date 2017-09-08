/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.db;

import net.wimpi.pim.contact.model.Contact;

/**
 * Interface modeling a <tt>ContactFilter</tt>.
 * <p>
 * @author Dieter Wimberger (wimpi)
 * @version 0.1 (22/07/2003)
 */
public interface ContactFilter {

  /**
   * Tests if a given contact passes through
   * this <tt>ContactFilter</tt>.
   *
   * @param contact a <tt>Contact</tt> instance.
   * @return true if passes, false otherwise.
   */
  public boolean passes(Contact contact);

  /**
   * Sets a child filter, which will be applied by the parent.
   * <p>
   * Note that this semantics allow to construct chains of
   * different filters.
   *
   * @param filter a <tt>ContactFilter</tt> instance.
   */
  public void setChildFilter(ContactFilter filter);

}//interface ContactFilter
