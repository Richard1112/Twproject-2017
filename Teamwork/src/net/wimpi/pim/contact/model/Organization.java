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


/**
 * An interface modeling an organization
 * based on the types and information defined
 * by the vCard Mime directory profile standard.
 * <p>
 * For reference see RFC 2426:<br>
 * 3.5 Organizational Types<br>
 * 3.5.5 ORG Type Definition<br>
 * 3.5.3 LOGO Type Definition<br>
 * <br>
 * Note that the ORG type from the specification
 * has been split into name and units (with their
 * respective accessor and mutator methods).<br>
 * Also note that an URL field has been added to
 * the organization for convenience.<br>
 * Units are used to specify a certain part of
 * an organization.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 *
 * @see net.wimpi.pim.contact.model.Image
 */
public interface Organization
    extends Serializable {

  /**
   * Returns the name of this <tt>Organization</tt>.
   *
   * @return the name as <tt>String</tt>.
   */
  public String getName();

  /**
   * Sets the name of this <tt>Organization</tt>.
   *
   * @param name the name as <tt>String</tt>.
   */
  public void setName(String name);

  /**
   * Returns all units of this <tt>Organization</tt>.
   *
   * @return the units as <tt>String[]</tt>.
   */
  public String[] listUnits();

  /**
   * Returns the unit at the given index.
   *
   * @param index the index as <tt>int</tt>.
   * @return the unit as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index
   *         is out of bounds.
   */
  public String getUnit(int index)
      throws IndexOutOfBoundsException;

  /**
   * Sets the unit at the given index, returning the
   * replaced one.
   *
   * @param index the index as <tt>int</tt>.
   * @param unit the unit to be set as <tt>String</tt>.
   * @return the former unit at the index as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index
   *         is out of bounds.
   */
  public String setUnit(int index, String unit)
      throws IndexOutOfBoundsException;

  /**
   * Adds a given unit to this <tt>Organization</tt>.
   *
   * @param unit the unit as <tt>String</tt>.
   */
  public void addUnit(String unit);

  /**
   * Removes the unit at the given index.
   *
   * @param index the index as <tt>int</tt>.
   * @return the removed unit as <tt>String</tt>.
   * @throws IndexOutOfBoundsException if the index
   *         is out of bounds.
   */
  public String removeUnit(int index)
      throws IndexOutOfBoundsException;

  /**
   * Returns the number of set units.
   *
   * @return the number of units as <tt>int</tt>.
   */
  public int getUnitCount();

  /**
   * Returns the URL associated with
   * this <tt>Organization</tt>.
   *
   * @return the URL as <tt>String</tt>.
   */
  public String getURL();

  /**
   * Sets the URL associated with
   * this <tt>Organization</tt>.
   *
   * @param url the URL as <tt>String</tt>.
   */
  public void setURL(String url);

  /**
   * Returns the logo associated with
   * this <tt>Organization</tt>.
   *
   * @return the logo as <tt>Image</tt>.
   */
  public Image getLogo();

  /**
   * Sets the logo associated with
   * this <tt>Organization</tt>.
   *
   * @param logo the logo as <tt>Image</tt>.
   */
  public void setLogo(Image logo);

  /**
   * Tests if this <tt>Organization</tt> has
   * a logo associated.
   *
   * @return true if it has a logo, false otherwise.
   */
  public boolean hasLogo();

}//interface Organization
