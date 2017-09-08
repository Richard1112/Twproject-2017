/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.basicimpl;

import net.wimpi.pim.contact.model.Extension;

import java.util.Map;
import java.util.Set;

/**
 * Class implementing a generic {@link net.wimpi.pim.contact.model.Extension}.
 * <p>
 * This class can be derived to simplify implementation of extensions
 * for the developer, as it is not required write an ItemHandler for
 * this type of extensions.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public abstract class GenericExtension
    implements Extension {

  static final long serialVersionUID = -1586945227560000224L;

  //instance attributes
  protected String m_Identifier;
  protected String m_Value;
  protected Map m_Parameters;

  /**
   * Creates a new <tt>GenericExtension</tt> instance.
   *
   * @param identifier the identifier for the extension.
   */
  public GenericExtension(String identifier) {
    m_Identifier = identifier;
  }//SimpleExtension

  public String getIdentifier() {
    return m_Identifier;
  }//getIdentifier

  /**
   * Returns the value of this <tt>GenericExtension</tt>.
   *
   * @return the value as <tt>String</tt>.
   */
  public String getValue() {
    return m_Value;
  }//getValue

  /**
   * Sets the value of this <tt>GenericExtension</tt>.
   *
   * @param value a value as <tt>String</tt>.
   */
  public void setValue(String value) {
    m_Value = value;
  }//setValue

  /**
   * Returns the parameters of this <tt>GenericExtension</tt>.
   *
   * @return the parameters as <tt>Map</tt>.
   */
  public Map getParameters() {
    return m_Parameters;
  }//getParameters

  /**
   * Sets the parameters of this <tt>GenericExtension</tt>.
   *
   * @param parameters the parameters as <tt>Map</tt>.
   */
  public void setParameters(Map parameters) {
    m_Parameters = parameters;
  }//setParameters

  /**
   * Returns the parameter values of the parameter
   * with the given name.
   *
   * @param name a parameter name.
   * @return the parameter values as <tt>String[]</tt>.
   */
  public String[] getParameterValues(String name) {
    if (m_Parameters != null) {
      Object o = m_Parameters.get(name);
      if (o instanceof String) {
        String[] retval = new String[1];
        retval[0] = (String) o;
        return retval;
      } else {
        return (String[]) o;
      }
    } else {
      return EMPTY_ARRAY;
    }
  }//getParameter

  /**
   * Returns a list of all parameter names.
   *
   * @return the parameter names as <tt>String[]</tt>.
   */
  public String[] listParameters() {
    if (m_Parameters != null && m_Parameters.size() > 0) {
      Set set = m_Parameters.keySet();
      String[] strs = new String[set.size()];
      return (String[]) set.toArray(strs);
    } else {
      return EMPTY_ARRAY;
    }
  }//listParameters

  /**
   * Returns a new <tt>GenericExtension</tt>.
   *
   * @return a new instance of the implementing class.
   */
  public abstract GenericExtension createExtension();

  /**
   * Defines an empty array of <tt>String</tt>.
   */
  private static String[] EMPTY_ARRAY = new String[0];

}//class GenericExtension
