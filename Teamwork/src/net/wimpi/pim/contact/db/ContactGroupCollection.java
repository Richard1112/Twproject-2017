/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.db;

import java.util.Iterator;

/**
 * Interface modeling a <tt>ContactGroupCollection</tt>.
 * <p>
 * @author Dieter Wimberger (wimpi)
 * @version 0.1 (22/07/2003)
 */
public interface ContactGroupCollection {

  /**
   * Tests if this <tt>ContactGroupCollection</tt> contains
   * a given contact group.
   *
   * @param UID the unique identifier of the group as <tt>String</tt>.
   * @return true if contains group, false otherwise.
   */
  public boolean contains(String UID);

  /**
   * Tests if this <tt>ContactGroupCollection</tt> contains
   * a given contact group.
   *
   * @param group the group as <tt>ContactGroup</tt>.
   * @return true if contains group, false otherwise.
   */
  public boolean contains(ContactGroup group);

  /**
   * Tests if this <tt>ContactGroupCollection</tt> contains
   * a contact group with the given name.
   *
   * @param name the unique name of the group as <tt>String</tt>.
   * @return true if contains group, false otherwise.
   */
  public boolean containsByName(String name);

  /**
   * Returns the <tt>ContactGroup</tt> instance for
   * a given unique identifier.
   *
   * @param UID the unique identifier of a group as <tt>String</tt>.
   * @return the group as <tt>ContactGroup</tt>.
   */
  public ContactGroup get(String UID);

  /**
   * Returns the <tt>ContactGroup</tt> instance with a
   * given name.
   *
   * @param name the name of the group as <tt>String</tt>.
   * @return the group as <tt>ContactGroup</tt>.
   */
  public ContactGroup getByName(String name);

  /**
   * Returns all contact groups in this <tt>ContactGroupCollection</tt>.
   *
   * @return the list of groups as <tt>ContactGroup[]</tt>.
   */
  public ContactGroup[] toArray();

  /**
   * Returns all contact groups in this <tt>ContactGroupCollection</tt>,
   * that pass the given filter.
   *
   * @param filter the filter as <tt>ContactGroupFilter</tt>.
   * @return the list of contacts as <tt>ContactGroup[]</tt>.
   */
  public ContactGroup[] toArray(ContactGroupFilter filter);

  /**
   * Adds a given contact group to this <tt>ContactGroupCollection</tt>.
   * Contact groups should have unique names, thus, when adding a
   * group with an existing name, this method should
   * not add the group and return false.
   *
   *
   * @param group the contact group as <tt>ContactGroup</tt>.
   * @return true if successful, false otherwise.
   */
  public boolean add(ContactGroup group);

  /**
   * Removes a given contact group from this <tt>ContactGroupCollection</tt>.
   *
   * @return the <tt>ContactGroup</tt> instance removed, or null otherwise.
   * @param UID the unique identifier of a group as <tt>String</tt>.
   */
  public ContactGroup remove(String UID);

  /**
   * Removes a given contact group from this <tt>ContactGroupCollection</tt>.
   *
   * @param group the contact group as <tt>ContactGroup</tt>.
   */
  public void remove(ContactGroup group);


  /**
   * Returns an iterator over all groups in
   * this <tt>ContactGroupCollection</tt>.
   *
   * @return an ,tt>Iterator</tt> instance.
   */
  public Iterator iterator();

  /**
   * Returns an iterator over all contact groups in
   * this <tt>ContactGroupCollection</tt>, that pass
   * the given filter.
   *
   * @param filter the filter as <tt>ContactGroupFilter</tt>.
   * @return an <tt>Iterator</tt> instance.
   */
  public Iterator iterator(ContactGroupFilter filter);

  /**
   * Returns the number of groups in
   * this <tt>ContactGroupCollection</tt>.
   *
   * @return the number of groups as <tt>int</tt>.
   */
  public int size();

}//interface ContactGroupCollection
