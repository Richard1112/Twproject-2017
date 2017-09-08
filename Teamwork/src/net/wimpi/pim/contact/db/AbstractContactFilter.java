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
 * Class representing an abstract <tt>ContactFilter</tt>.
 * to be extended for simplifying implementations.
 *
 * <p>
 * @author Dieter Wimberger (wimpi)
 * @version 0.1 (22/07/2003)
 */
public abstract class AbstractContactFilter
    implements ContactFilter {

  protected ContactFilter m_ChildFilter;

  public abstract boolean passes(Contact contact);

  /**
   * Tests if the given <tt>Contact</tt> instance passes
   * this <tt>AbstractContactFilter</tt>'s child filter.
   * <p>
   * If this <tt>AbstractContactFilter</tt> has no child filter
   * set, this method will always return true.
   *
   * @param contact a <tt>Contact</tt> instance.
   * @return true if passes (or no child filter set), false otherwise.
   */
  public boolean passesChildFilter(Contact contact) {
    if (m_ChildFilter == null) {
      return true;
    } else {
      return m_ChildFilter.passes(contact);
    }
  }//passesChildFilter

  public void setChildFilter(ContactFilter filter) {
    m_ChildFilter = filter;
  }//setChildFilter

  /**
   * Returns the child filter of this <tt>AbstractContactFilter</tt>.
   *
   * @return the child filter as <tt>ContactFilter</tt>.
   */
  public ContactFilter getChildFilter() {
    return m_ChildFilter;
  }//getChildFilter

}//class AbstractContactFilter
