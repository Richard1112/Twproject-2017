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

import java.util.Iterator;

/**
 * An interface modeling a contact group.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public interface ContactGroup
    extends Identifiable {

  /**
   * Returns the name of this <tt>ContactGroup</tt>.
   *
   * @return the name as <tt>String</tt>.
   */
  public String getName();

  /**
   * Sets the name of this <tt>ContactGroup</tt>.
   *
   * @param name the name as <tt>String</tt>.
   */
  public void setName(String name);

  /**
   * Returns all contacts associated
   * with this <tt>ContactGroup</tt>.
   *
   * @return an <tt>Iterator</tt> over all
   *         <tt>Contact</tt> instances.
   */
  public Iterator getContacts();

  /**
   * Returns all contacts associated
   * with this <tt>ContactGroup</tt>.
   *
   * @return an array of <tt>Contact<tt> instances.
   */
  public Contact[] listContacts();

  /**
   * Adds a contact to this <tt>ContactGroup</tt>.
   *
   * @param contact the contact to be added as <tt>Contact</tt>.
   */
  public void addContact(Contact contact);

  /**
   * Removes a contact from this <tt>ContactGroup</tt>.
   *
   * @param contact the contact to be removed from
   *        this <tt>ContactGroup</tt>.
   */
  public void removeContact(Contact contact);

  /**
   * Tests if this <tt>ContactGroup</tt> contains
   * a given contact.
   *
   * @param UID the unique identifier of the contact as <tt>String</tt>.
   * @return true if contains contact, false otherwise.
   */
  public boolean contains(String UID);

  /**
   * Tests if this <tt>ContactGroup</tt> contains
   * a given contact.
   *
   * @param contact the contact as <tt>Contact</tt>.
   * @return true if contains contact, false otherwise.
   */
  public boolean contains(Contact contact);

  /**
   * Returns the number of contacts in
   * this <tt>ContactGroup</tt>.
   *
   * @return the number of contacts as <tt>int</tt>.
   */
  public int size();

  /**
   * Returns the description of this <tt>ContactGroup</tt>.
   *
   * @return the description as <tt>String</tt>.
   */
  public String getDescription();

  /**
   * Sets the description of this <tt>ContactGroup</tt>.
   *
   * @param description the description as <tt>String</tt>.
   */
  public void setDescription(String description);

}//ContactGroup
