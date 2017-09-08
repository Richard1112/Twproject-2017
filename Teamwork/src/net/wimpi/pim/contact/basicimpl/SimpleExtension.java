/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.basicimpl;

import net.wimpi.pim.util.StringUtil;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Class implementing a simple {@link net.wimpi.pim.contact.model.Extension}.
 * <p>
 * This class can be used to handle simple
 * extensions of the type:<br>
 * X-KIDS: Mary;Joe;Tom
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class SimpleExtension
    extends GenericExtension
    implements Serializable {

  static final long serialVersionUID = -378985030891875659L;

  //instance attributes
  protected List m_Values;

  public SimpleExtension(String identifier) {
    super(identifier);
    m_Values = new ArrayList(10);
  }//SimpleExtension

  public void setValue(String value) {
    String[] values = StringUtil.split(value, ';');
    m_Values.clear();
    for (int i = 0; i < values.length; i++) {
      addValue(values[i]);
    }
  }//setValue

  public String getValue() {
    return StringUtil.joinList(listValues(), ';');
  }//getValue

  /**
   * Returns all values of this
   * this <tt>SimpleExtension</tt>.
   *
   * @return the values as <tt>String[]</tt>.
   */
  public String[] listValues() {
    return StringUtil.listToStringArray(m_Values);
  }//listValues

  /**
   * Returns the Value at the given index.
   *
   * @param index the index as <tt>int</tt>.
   * @return the value as <tt>String</tt>.
   * @throws java.lang.IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String getValue(int index)
      throws IndexOutOfBoundsException {
    return (String) m_Values.get(index);
  }//getValue

  /**
   * Sets the value at the given index,
   * returning the replaced one.
   *
   * @param index the index as <tt>int</tt>.
   * @param value the value as <tt>String</tt>.
   * @return the replaced value as <tt>String</tt>.
   * @throws java.lang.IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String setValue(int index, String value)
      throws IndexOutOfBoundsException {
    return (String) m_Values.set(index, value);
  }//setValue

  /**
   * Adds a value to this <tt>SimpleExtension</tt>.
   *
   * @param value the value as <tt>String</tt>.
   */
  public void addValue(String value) {
    m_Values.add(value);
  }//addValue

  /**
   * Removes and returns the value at
   * the given index.
   *
   * @param index the index as <tt>int</tt>.
   * @return the removed value as <tt>String</tt>.
   * @throws java.lang.IndexOutOfBoundsException if the index is
   *         out of bounds.
   */
  public String removeValue(int index)
      throws IndexOutOfBoundsException {
    return (String) m_Values.remove(index);
  }//removeValue

  /**
   * Returns the number of values associated
   * with this <tt>SimpleExtension</tt>.
   *
   * @return the number as <tt>int</tt>.
   */
  public int getValueCount() {
    return m_Values.size();
  }//getValueCount

  /**
   * Returns a new SimpleExtension with the same identifier
   * as this <tt>SimpleExtension</tt> instance.
   *
   * @return a new <tt>SimpleExtension</tt> instance.
   */
  public GenericExtension createExtension() {
    return new SimpleExtension(this.getIdentifier());
  }//createExtension

}//class SimpleExtension
