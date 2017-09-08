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
import java.util.Date;
import java.util.Iterator;

/**
 * An interface modeling contact based on the
 * types and information of the vCard Mime directory
 * profile standard specification.
 * <p>
 * For reference see RFC 2426:<br>
 * 3.6 EXPLANATORY TYPES<br>
 * 3.6.1 CATEGORIES Type Definition<br>
 * 3.6.2 NOTE Type Definition<br>
 * 3.6.4 REV Type Definition<br>
 * 3.6.7 UID Type Definition<br>
 * 3.6.8 URL Type Definition<br>
 * 3.7.1 CLASS Type Definition<br>
 * 3.7.2 KEY Type Definition<br>
 * <br>
 * Note that a flag for frequently used
 * contacts has been added, which represents
 * an extension to the standard.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 *
 * @see net.wimpi.pim.contact.model.PersonalIdentity
 * @see net.wimpi.pim.contact.model.OrganizationalIdentity
 * @see net.wimpi.pim.contact.model.Address
 * @see net.wimpi.pim.contact.model.Communications
 * @see net.wimpi.pim.contact.model.GeographicalInformation
 * @see net.wimpi.pim.contact.model.Key
 */
public interface Contact
    extends Identifiable, Serializable {

  /**
   * Returns the <tt>PersonalIdentity</tt> of
   * this <tt>Contact</tt>.
   *
   * @return a <tt>PersonalIdentity</tt> instance.
   */
  public PersonalIdentity getPersonalIdentity();

  /**
   * Sets the <tt>PersonalIdentity</tt> of
   * this <tt>Contact</tt>.
   *
   * @param identity a <tt>PersonalIdentity</tt> instance.
   */
  public void setPersonalIdentity(PersonalIdentity identity);

  /**
   * Tests if this <tt>Contact</tt> has a
   * <tt>PersonalIdentity</tt>.
   *
   * @return true if it has, false otherwise.
   */
  public boolean hasPersonalIdentity();

  /**
   * Returns the <tt>OrganizationalIdentity</tt> of
   * this <tt>Contact</tt>.
   *
   * @return an <tt>OrganizationalIdentity</tt> instance.
   */
  public OrganizationalIdentity getOrganizationalIdentity();

  /**
   * Sets the <tt>OrganizationalIdentity</tt> of
   * this <tt>Contact</tt>.
   *
   * @param identity an <tt>OrganizationalIdentity</tt> instance.
   */
  public void setOrganizationalIdentity(OrganizationalIdentity identity);

  /**
   * Tests if this <tt>Contact</tt> has a
   * <tt>OrganizationalIdentity</tt>.
   *
   * @return true if it has, false otherwise.
   */
  public boolean hasOrganizationalIdentity();

  /**
   * Returns all addresses associated with
   * this <tt>Contact</tt>.
   *
   * @return an <tt>Iterator</tt> over all
   *         <tt>Address</tt> instances.
   */
  public Iterator getAddresses();

  /**
   * Returns all addresses associated with
   * this <tt>Contact</tt>.
   *
   * @return the addresses as <tt>Address[]</tt>.
   */
  public Address[] listAddresses();

  /**
   * Returns the address at the given index.
   *
   * @param uid the unique identifier as <tt>String</tt>.
   * @return the <tt>Address</tt> instance.
   */
  public Address getAddress(String uid);

  /**
   * Returns the last added address. This method
   * will return null, if no address was added.
   *
   * @return an <tt>Address</tt> instance.
   */
  public Address getLastAddedAddress();

  /**
   * Adds an address to this <tt>Contact</tt>.
   *
   * @param addr the <tt>Address</tt> to be added.
   */
  public void addAddress(Address addr);

  /**
   * Removes and returns the <tt>Address</tt>
   * at the given index.
   *
   * @param addr the <tt>Address</tt> instance to be removed.
   */
  public void removeAddress(Address addr);

  /**
   * Returns the preferred address
   * of this <tt>Contact</tt> instance.
   *
   * @return the preferred address as <tt>Address</tt>.
   */
  public Address getPreferredAddress();

  /**
   * Sets the preferred address
   * of this <tt>Contact</tt> instance.
   *
   * @param address the preferred address as <tt>Address</tt>.
   */
  public void setPreferredAddress(Address address);

  /**
   * Tests if the given address is the preferred one
   * of this <tt>Contact</tt>.
   *
   * @param address the address to be tested as <tt>Address</tt>.
   * @return true if preferred, false otherwise.
   */
  public boolean isPreferredAddress(Address address);

  /**
   * Returns the addresses of a given type
   * of this <tt>Contact</tt> instance.
   *
   * @return int the type as <tt>int</tt>.
   * @return the addresses as <tt>Address[]</tt>.
   * @see net.wimpi.pim.contact.model.Address
   */
  public Address[] listAddressesByType(int TYPE);

  /**
   * Returns the number of addresses associated
   * with this <tt>Contact</tt>.
   *
   * @return the number as <tt>int</tt>.
   */
  public int getAddressCount();

  /**
   * Returns the <tt>Communications</tt> instance
   * associated with this <tt>Contact</tt>.
   *
   * @return the <tt>Communications</tt> instance.
   */
  public Communications getCommunications();

  /**
   * Sets the <tt>Communications</tt> instance
   * associated with this <tt>Contact</tt>.
   *
   * @param comm the <tt>Communications</tt> instance.
   */
  public void setCommunications(Communications comm);

  /**
   * Tests if this <tt>Contact</tt> has a
   * <tt>Communications</tt> instance.
   *
   * @return true if it has, false otherwise.
   */
  public boolean hasCommunications();

  /**
   * Returns the <tt>GeographicalInformation</tt> instance
   * associated with this <tt>Contact</tt>.
   *
   * @return the <tt>GeographicalInformation</tt> instance.
   */
  public GeographicalInformation getGeographicalInformation();

  /**
   * Sets the <tt>GeographicalInformation</tt> instance
   * associated with this <tt>Contact</tt>.
   *
   * @param geoinfo the <tt>GeographicalInformation</tt> instance.
   */
  public void setGeographicalInformation(
      GeographicalInformation geoinfo);

  /**
   * Tests if this <tt>Contact</tt> has a
   * <tt>GeographicalInformation</tt> instance.
   *
   * @return true if it has, false otherwise.
   */
  public boolean hasGeographicalInformation();

  /**
   * Returns all categories of this
   * this <tt>Contact</tt>.
   *
   * @return the categories as <tt>String[]</tt>.
   */
  public String[] listCategories();

  /**
   * Returns the category at the given index.
   *
   * @param index the index as <tt>int</tt>.
   * @return the category as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String getCategory(int index)
      throws IndexOutOfBoundsException;

  /**
   * Sets the category at the given index,
   * returning the replaced one.
   *
   * @param index the index as <tt>int</tt>.
   * @param category the category as <tt>String</tt>.
   * @return the category as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String setCategory(int index, String category)
      throws IndexOutOfBoundsException;

  /**
   * Adds a category to this <tt>Contact</tt>.
   *
   * @param category the category as <tt>String</tt>.
   */
  public void addCategory(String category);

  /**
   * Removes and returns the category at
   * the given index.
   *
   * @param index the index as <tt>int</tt>.
   * @return the removed category as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String removeCategory(int index)
      throws IndexOutOfBoundsException;

  /**
   * Removes all categories.
   */
  public void removeAllCategories();

  /**
   * Returns the number of categories associated
   * with this <tt>Contact</tt>.
   *
   * @return the number as <tt>int</tt>.
   */
  public int getCategoryCount();

  /**
   * Returns an URL associated with
   * this <tt>Contact</tt>.
   *
   * @return the URL as <tt>String</tt>.
   */
  public String getURL();

  /**
   * Sets the URL associated with
   * this <tt>Contact</tt>.
   *
   * @param url the URL as <tt>String</tt>.
   */
  public void setURL(String url);

  /**
   * Tests if this <tt>Contact</tt> is frequently
   * used.
   *
   * @return true if frequent, false otherwise.
   */
  public boolean isFrequent();

  /**
   * Sets the frequent flag of this <tt>Contact</tt>.
   *
   * @param b true if frequent, false otherwise.
   */
  public void setFrequent(boolean b);

  /**
   * Returns the note associated with
   * this <tt>Contact</tt>.
   *
   * @return the note as <tt>String</tt>.
   */
  public String getNote();

  /**
   * Sets the note associated with
   * this <tt>Contact</tt>.
   *
   * @param note the note as <tt>String</tt>.
   */
  public void setNote(String note);

  /**
   * Returns the current revision date
   * of this <tt>Contact</tt>.
   *
   * @return the revision date as <tt>Date</tt>.
   */
  public Date getCurrentRevisionDate();

  /**
   * Sets the current revision date
   * of this <tt>Contact</tt>.
   *
   * @param date the revision date as <tt>Date</tt>.
   */
  public void setCurrentRevisionDate(Date date);

  /**
   * Returns the access classification of
   * this <tt>Contact</tt>.
   *
   * @return the access classification as <tt>String</tt>.
   */
  public String getAccessClassification();

  /**
   * Sets the access classification of
   * this <tt>Contact</tt>.
   *
   * @param ac the access classification as <tt>String</tt>.
   */
  public void setAccessClassification(String ac);

  /**
   * Returns the public key associated
   * with this <tt>Contact</tt>.
   *
   * @return the public key as <tt>Key</tt>.
   */
  public Key getPublicKey();

  /**
   * Sets the public key associated with
   * this <tt>Contact</tt>.
   *
   * @param key the public key as <tt>Key</tt>.
   */
  public void setPublicKey(Key key);

  /**
   * Tests if this <tt>Contact</tt> has
   * a public key.
   *
   * @return true if there is a key associated with
   *         this <tt>Contact</tt>, false otherwise.
   */
  public boolean hasPublicKey();

  /**
   * Returns the sound associated
   * with this <tt>Contact</tt>.
   *
   * @return the sound as <tt>Sound</tt>.
   */
  public Sound getSound();

  /**
   * Sets the sound associated with
   * this <tt>Contact</tt>.
   *
   * @param sound the sound as <tt>Sound</tt>.
   */
  public void setSound(Sound sound);

  /**
   * Tests if this <tt>Contact</tt> has
   * a sound associated with it.
   *
   * @return true if there is a sound associated with
   *         this <tt>Contact</tt>, false otherwise.
   */
  public boolean hasSound();

  /**
   * Returns the extensions associated with
   * this <tt>Contact</tt>.
   *
   * @return the extensions as <tt>Extensions</tt> instance.
   */
  public Extensions getExtensions();

  /**
   * Sets the extensions associated with
   * this <tt>Contact</tt>.
   *
   * @param extensions the extensions as <tt>Extensions</tt> instance.
   */
  public void setExtensions(Extensions extensions);

  /**
   * Tests if this <tt>Contact</tt> has extensions.
   *
   * @return true if it has extensions, false otherwise.
   */
  public boolean hasExtensions();

}//interface Contact
