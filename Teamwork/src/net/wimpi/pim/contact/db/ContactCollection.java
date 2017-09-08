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

import java.util.Iterator;

/**
 * Interface modeling a <tt>ContactCollection</tt>.
 * <p>
 * @author Dieter Wimberger (wimpi)
 * @version 0.1 (22/07/2003)
 */
public interface ContactCollection {

  /**
   * Tests if this <tt>ContactCollection</tt> contains
   * a given contact.
   *
   * @param UID the unique identifier of the contact as <tt>String</tt>.
   * @return true if contains contact, false otherwise.
   */
  public boolean contains(String UID);

  /**
   * Tests if this <tt>ContactCollection</tt> contains
   * a given contact.
   *
   * @param contact the contact as <tt>Contact</tt>.
   * @return true if contains contact, false otherwise.
   */
  public boolean contains(Contact contact);


  /**
   * Tests if this <tt>ContactCollection</tt> contains
   * a contact with the given name.
   *
   * @param name the formatted name of the contact as <tt>String</tt>.
   * @return true if contains contact, false otherwise.
   */
  public boolean containsByName(String name);

  /**
   * Returns the <tt>Contact</tt> instance for
   * a given unique identifier.
   *
   * @param UID the unique identifier of a contact as <tt>String</tt>.
   * @return the contact as <tt>Contact</tt>.
   */
  public Contact get(String UID);

  /**
   * Returns the <tt>Contact</tt> instance with a
   * given name.
   *
   * @param name the formatted name of the contact as <tt>String</tt>.
   * @return the contact as <tt>Contact</tt>.
   */
  public Contact getByName(String name);

  /**
   * Returns all contacts in this <tt>ContactCollection</tt>.
   *
   * @return the list of contacts as <tt>Contact[]</tt>.
   */
  public Contact[] toArray();

  /**
   * Returns all contacts in this <tt>ContactCollection</tt>,
   * that pass the given filter.
   *
   * @param filter the filter as <tt>ContactFilter</tt>.
   * @return the list of contacts as <tt>Contact[]</tt>.
   */
  public Contact[] toArray(ContactFilter filter);

  /**
   * Adds a given contact to this <tt>ContactCollection</tt>.
   *
   * @param contact the contact as <tt>Contact</tt>.
   */
  public void add(Contact contact);

  /**
   * Removes a given contact from this <tt>ContactCollection</tt>.
   *
   * @param UID the unique identifier of a contact as <tt>String</tt>.
   * @return the removed <tt>Contact</tt> instance, or null otherwise.
   */
  public Contact remove(String UID);

  /**
   * Removes a given contact from this <tt>ContactCollection</tt>.
   *
   * @param contact the contact as <tt>Contact</tt>.
   */
  public void remove(Contact contact);

  /**
   * Returns an iterator over all contacts in
   * this <tt>ContactCollection</tt>.
   *
   * @return an <tt>Iterator</tt> instance.
   */
  public Iterator iterator();

  /**
   * Returns an iterator over all contacts in
   * this <tt>ContactCollection</tt>, that pass
   * the given filter.
   *
   * @param filter the filter as <tt>ContactFilter</tt>.
   * @return an <tt>Iterator</tt> instance.
   */
  public Iterator iterator(ContactFilter filter);

  /**
   * Returns the number of contacts in
   * this <tt>ContactCollection</tt>.
   *
   * @return the number of contacts as <tt>int</tt>.
   */
  public int size();

}//interface ContactCollection
