/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.db;

/**
 * Interface modeling a <tt>ContactGroupFilter</tt>.
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public interface ContactGroupFilter {

  /**
   * Tests if a given contact passes through
   * this <tt>ContactGroupFilter</tt>.
   *
   * @param group a <tt>ContactGroup</tt> instance.
   * @return true if passes, false otherwise.
   */
  public boolean passes(ContactGroup group);

  /**
   * Sets a child filter, which will be applied by the parent.
   * <p>
   * Note that this semantics allow to construct chains of
   * different filters.
   *
   * @param filter a <tt>ContactGroupFilter</tt> instance.
   */
  public void setChildFilter(ContactGroupFilter filter);

}//interface ContactGroupFilter
