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
import net.wimpi.pim.util.Identifiable;

/**
 * Interface modeling a <tt>ContactDatabase</tt>.
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public interface ContactDatabase
    extends Identifiable {

  /**
   * Returns the owner of this <tt>ContactDatabase</tt>.
   *
   * @return the owner as <tt>Contact</tt>.
   */
  public Contact getOwner();

  /**
   * Sets the owner of this <tt>ContactDatabase</tt>.
   *
   * @param contact the owner as <tt>Contact</tt>.
   */
  public void setOwner(Contact contact);

  /**
   * Returns the collection of contacts stored in
   * this <tt>ContactDatabase</tt>.
   *
   * @return the collection of contacts as <tt>ContactCollection</tt>.
   */
  public ContactCollection getContactCollection();

  /**
   * Returns the collection of contact groups
   * stored in this <tt>ContactDatabase</tt>.
   *
   * @return the collection of groups as <tt>ContactGroupCollection</tt>.
   */
  public ContactGroupCollection getContactGroupCollection();

  /**
   * Creates and returns a new <tt>ContactGroup</tt> instance.
   *
   * @return a new <tt>ContactGroup</tt> instace.
   */
  public ContactGroup createContactGroup();

}//interface ContactDatabase
