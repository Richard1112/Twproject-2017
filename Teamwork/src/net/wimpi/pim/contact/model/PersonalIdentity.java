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
import java.util.Date;

/**
 * An interface modeling the personal
 * identity of a contact based on the types
 * and information defined by the vCard Mime
 * directory profile standard.
 * <p>
 * For reference see RFC 2426:<br>
 * 3.1 Identification Types<br>
 * 3.1.1 FN Type Definition<br>
 * 3.1.2 N Type Definition<br>
 * 3.1.3 NICKNAME Type Definition<br>
 * 3.1.4 PHOTO Type Definition<br>
 * 3.1.5 BDAY Type Definition<br>
 * 3.6.5 SORT-STRING Type Definition<br>
 * <br>
 * Note that the formatted name is based on the
 * semantics of the X.520 Common Name attribute
 * and represents a required field for vCards.
 * <br>
 * The N type has been split up into
 * Last (Family) Name, First (Given) Name,
 * Additional Names, (Honorific) Prefixes, and
 * (Honorific) Suffixes (with their
 * respective accessor and mutator methods).
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 *
 * @see net.wimpi.pim.contact.model.Image
 */
public interface PersonalIdentity
    extends Serializable {

  /**
   * Returns the firstname of this <tt>PersonalIdentity</tt>.
   * The firstname is also referred to as given name.
   *
   * @return the firstname as <tt>String</tt>.
   */
  public String getFirstname();

  /**
   * Sets the firstname of this <tt>PersonalIdentity</tt>.
   * The firstname is also referred to as given name.
   *
   * @param name the firstname as <tt>String</tt>.
   */
  public void setFirstname(String name);

  /**
   * Returns the lastname of this <tt>PersonalIdentity</tt>.
   * The lastname is also referred to as family name.
   *
   * @return the lastname as <tt>String</tt>.
   */
  public String getLastname();

  /**
   * Sets the lastname of this <tt>PersonalIdentity</tt>.
   * The lastname is also referred to as family name.
   *
   * @param name the firstname as <tt>String</tt>.
   */
  public void setLastname(String name);

  /**
   * Returns all additional names associated
   * with this <tt>PersonalIdentity</tt>.
   *
   * @return the names as <tt>String[]</tt>.
   */
  public String[] listAdditionalNames();

  /**
   * Returns the additional name at the given
   * index.
   *
   * @param index the index as <tt>int</tt>.
   * @return the name as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String getAdditionalName(int index)
      throws IndexOutOfBoundsException;

  /**
   * Sets the additional name at the given
   * index, returning the replaced one.
   *
   * @param index the index as <tt>int</tt>.
   * @param name the name to be set as <tt>String</tt>.
   * @return the replaced name as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String setAdditionalName(int index, String name)
      throws IndexOutOfBoundsException;

  /**
   * Adds an additional name.
   *
   * @param name the name as <tt>String</tt>.
   */
  public void addAdditionalName(String name);

  /**
   * Removes and returns an additional name at a given
   * index.
   *
   * @param index the index as <tt>int</tt>.
   * @return the removed name as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String removeAdditionalName(int index)
      throws IndexOutOfBoundsException;

  /**
   * Removes all additional names.
   */
  public void removeAllAdditionalNames();

  /**
   * Returns the number of additional names
   * set for this <tt>PersonalIdentity</tt>.
   *
   * @return the number as <tt>int</tt>.
   */
  public int getAdditionalNameCount();

  /**
   * Returns all nicknames associated
   * with this <tt>PersonalIdentity</tt>.
   *
   * @return the names as <tt>String[]</tt>.
   */
  public String[] listNicknames();

  /**
   * Returns the nickname at the given
   * index.
   *
   * @param index the index as <tt>int</tt>.
   * @return the name as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String getNickname(int index)
      throws IndexOutOfBoundsException;

  /**
   * Sets the nickname at the given
   * index, returning the replaced one.
   *
   * @param index the index as <tt>int</tt>.
   * @param name the name to be set as <tt>String</tt>.
   * @return the replaced name as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String setNickname(int index, String name)
      throws IndexOutOfBoundsException;

  /**
   * Adds a nickname.
   *
   * @param name the name as <tt>String</tt>.
   */
  public void addNickname(String name);

  /**
   * Removes and returns a nickname at a given
   * index.
   *
   * @param index the index as <tt>int</tt>.
   * @return the removed name as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String removeNickname(int index)
      throws IndexOutOfBoundsException;

  /**
   * Removes all nicknames.
   */
  public void removeAllNicknames();

  /**
   * Returns the number of nicknames
   * set for this <tt>PersonalIdentity</tt>.
   *
   * @return the number as <tt>int</tt>.
   */
  public int getNicknameCount();

  /**
   * Returns all prefixes associated
   * with this <tt>PersonalIdentity</tt>.
   *
   * @return the prefixes as <tt>String[]</tt>.
   */
  public String[] listPrefixes();

  /**
   * Returns the prefix at the given
   * index.
   *
   * @param index the index as <tt>int</tt>.
   * @return the prefix as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String getPrefix(int index)
      throws IndexOutOfBoundsException;

  /**
   * Sets the prefix at the given
   * index, returning the replaced one.
   *
   * @param index the index as <tt>int</tt>.
   * @param prefix the prefix to be set as <tt>String</tt>.
   * @return the replaced prefix as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String setPrefix(int index, String prefix)
      throws IndexOutOfBoundsException;

  /**
   * Adds a prefix.
   *
   * @param prefix the prefix as <tt>String</tt>.
   */
  public void addPrefix(String prefix);

  /**
   * Removes and returns a prefix at a given
   * index.
   *
   * @param index the index as <tt>int</tt>.
   * @return the removed prefix as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String removePrefix(int index)
      throws IndexOutOfBoundsException;

  /**
   * Removes all prefixes.
   */
  public void removeAllPrefixes();

  /**
   * Returns all suffixes associated
   * with this <tt>PersonalIdentity</tt>.
   *
   * @return the suffixes as <tt>String[]</tt>.
   */
  public String[] listSuffixes();

  /**
   * Returns the suffix at the given
   * index.
   *
   * @param index the index as <tt>int</tt>.
   * @return the suffix as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String getSuffix(int index)
      throws IndexOutOfBoundsException;

  /**
   * Sets the suffix at the given
   * index, returning the replaced one.
   *
   * @param index the index as <tt>int</tt>.
   * @param suffix the suffix to be set as <tt>String</tt>.
   * @return the replaced suffix as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String setSuffix(int index, String suffix)
      throws IndexOutOfBoundsException;

  /**
   * Adds a suffix.
   *
   * @param suffix the suffix as <tt>String</tt>.
   */
  public void addSuffix(String suffix);

  /**
   * Removes and returns a suffix at a given
   * index.
   *
   * @param index the index as <tt>int</tt>.
   * @return the removed suffix as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String removeSuffix(int index)
      throws IndexOutOfBoundsException;

  /**
   * Removes all suffixes.
   */
  public void removeAllSuffixes();

  /**
   * Returns the birth date.
   *
   * @return the birth date as <tt>Date</tt>.
   */
  public Date getBirthDate();

  /**
   * Sets the birth date.
   *
   * @param birthdate the birth date as <tt>Date</tt>.
   */
  public void setBirthDate(Date birthdate);

  /**
   * Returns the formatted name.
   *
   * @return the formatted name as <tt>String</tt>.
   */
  public String getFormattedName();

  /**
   * Sets the formatted name.
   *
   * @param fn the formatted name as <tt>String</tt>.
   */
  public void setFormattedName(String fn);

  /**
   * Returns the sort string.
   *
   * @return the sort string as <tt>String</tt>.
   */
  public String getSortString();

  /**
   * Sets the sort string.
   *
   * @param sortstr the sort string as <tt>String</tt>.
   */
  public void setSortString(String sortstr);

  /**
   * Returns the photo associated with
   * this <tt>PersonalIdentity</tt>.
   *
   * @return the photo as <tt>Image</tt>.
   */
  public Image getPhoto();

  /**
   * Sets the photo associated with
   * this <tt>PersonalIdentity</tt>.
   *
   * @param photo the photo as <tt>Image</tt>.
   */
  public void setPhoto(Image photo);

  /**
   * Tests if this <tt>PersonalIdentity</tt> has
   * a photo associated.
   *
   * @return true if it has a photo, false otherwise.
   */
  public boolean hasPhoto();

}//interface PersonalIdentity
