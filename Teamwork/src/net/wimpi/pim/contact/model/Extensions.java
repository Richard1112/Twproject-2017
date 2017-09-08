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
import java.util.Iterator;

/**
 * Interface modeling a collection of extensions.
 * <p>
 *
 * @author Dieter Wimberger (wimpi)
 * @version (created Mar 18, 2003)
 */
public interface Extensions
    extends Serializable {

  /**
   * Returns a list identifiers of all
   * extensions in this <tt>Extensions</tt>
   * collection.
   *
   * @return the list of identifiers as <tt>String[]</tt>.
   */
  public String[] listIdentifiers();

  /**
   * Returns an iterator over all elements
   * of this <tt>Extensions</tt> collection with a
   * given identifier.
   *
   * @param xidentifier an identifier as <tt>String</tt>.
   * @return an <tt>Iterator</tt> over all
   *         <tt>Extension</tt> instances.
   */
  public Iterator iterator(String xidentifier);

  /**
   * Returns a list of extensions with the given
   * identifier.
   *
   * @param xidentifier an identifier as <tt>String</tt>.
   * @return the list as <tt>Extension[]</tt>.
   */
  public Extension[] list(String xidentifier);

  /**
   * Returns the extension with the given identifier.
   * Note that this is a convenience method, which will
   * return the first extension with the given identifier
   * from the collection.
   *
   * @param xidentifier an identifier as <tt>String</tt>.
   * @return the <tt>Extension</tt> instance.
   */
  public Extension get(String xidentifier);

  /**
   * Returns the extension with the given identifier
   * and the given index.
   *
   * @param xidentifier an identifier as <tt>String</tt>.
   * @param idx the index as <tt>int</tt>.
   * @return the <tt>Extension</tt> instance.
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public Extension get(String xidentifier, int idx)
      throws IndexOutOfBoundsException;

  /**
   * Adds an extension to this <tt>Extensions</tt>
   * collection.
   *
   * @param ext the <tt>Extension</tt> to be added.
   */
  public void add(Extension ext);

  /**
   * Removes the given extension from
   * this <tt>Extensions</tt> collection.
   *
   * @param ext the extension as <tt>Extension</tt> instance.
   */
  public void remove(Extension ext);

  /**
   * Removes <b>all</b> extensions with the given identifier
   * from this <tt>Extensions</tt> collection.
   *
   * @param xidentifier an identifier as <tt>String</tt>.
   */
  public void remove(String xidentifier);

  /**
   * Removes the extension with the given identifier and
   * index from this <tt>Extensions</tt> collection.
   *
   * @param xidentifier an identifier as <tt>String</tt>.
   * @param idx the index as <tt>int</tt>.
   *
   * @throws IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public Extension remove(String xidentifier, int idx)
      throws IndexOutOfBoundsException;

  /**
   * Returns the total number of extensions associated
   * with this <tt>Extensions</tt> collection.
   *
   * @return the number as <tt>int</tt>.
   */
  public int size();

  /**
   * Returns the number of extensions associated
   * with this <tt>Extensions</tt> collection with a given
   * identifier.
   *
   * @param xidentifier an identifier as <tt>String</tt>.
   *
   * @return the number as <tt>int</tt>.
   */
  public int size(String xidentifier);

}//interface Extensions
