/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.util.versitio;

import net.wimpi.pim.util.EncodingUtility;
import net.wimpi.pim.util.StringUtil;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

public class versitItem {

  private String m_Identifier;
  private String m_Group;
  private String m_unprocessedParameters;
  private String m_Value;
  private HashMap m_Params;

  public versitItem(String token) {
    m_Params = new HashMap();
    setIdentifier(token);
  }//constructor

  public versitItem(String token, String value) {
    m_Params = new HashMap();
    setIdentifier(token);
    setValue(value);
  }//constructor

  private versitItem() {

  }//constructor

  public void setIdentifier(String id) {
    m_Identifier = id;
  }//setIdentifier

  public String getIdentifier() {
    return m_Identifier;
  }//getIdentifier

  public String getGroup() {
    return m_Group;
  }

  public void setGroup(String group) {
    m_Group = group;
  }//setGroup

  public boolean hasGroup() {
    return (m_Group != null);
  }//hasGroup

  protected void setUnprocessedParameters(String params) {
    m_unprocessedParameters = params;
  }//setUnprocessedParameters

  public String getUnprocessedParameters() {
    return m_unprocessedParameters;
  }//getUnprocessedParameters

  public String[] getParameter(String name) {
    if (m_Params != null) {
      Object o = m_Params.get(name);
      if (o instanceof String) {
        String[] retval = new String[1];
        retval[0] = (String) o;
        return retval;
      } else {
        return (String[]) o;
      }
    } else {
      return null;
    }
  }//getParameter

  /**
   * Returns a map containing the parameters.
   *
   * @return the parameters as <tt>Map</tt> instance.
   */
  public Map getParameters() {
    return m_Params;
  }//getParameters

  /**
   * Tests if this <tt>versitItem</tt> has
   * parameters.
   *
   * @return true if it has parameters, false otherwise.
   */
  public boolean hasParameters() {
    return (m_Params != null);
  }//hasParameters

  /**
   * Adds a parameter and mapped value to the parameters collection.
   * If the parameter already exists, it adds another value to
   * an already existing parameter by extending the array of strings.
   *
   * @param name the name of the parameter as <code>String</code>.
   * @param value the value of the parameter as <code>String</code>.
   */
  public void addParameter(String name, String value) {
    String values[];
    //System.out.println("DEBUG:"+name+"="+value);
    if (m_Params.containsKey(name)) {
      String oldValues[] = (String[]) m_Params.get(name);
      values = new String[oldValues.length + 1];
      for (int i = 0; i < oldValues.length; i++) {
        values[i] = oldValues[i];
      }
      values[oldValues.length] = value;
    } else {
      values = new String[1];
      values[0] = value;
    }
    m_Params.put(name, values);
  }//addParameter

  public void setValue(String value) {
    m_Value = value;
  }//setValue

  public String getValue() {
    return m_Value;
  }//getValue

  public String getDecodedValue() {
    //Check 2.1 quoted printable encoding
    String[] params = this.getParameter(versitToken.ENCODING);
    if (params != null && params.length > 0) {
      if (versitToken.ENCODING_QUOTEDPRINTABLE.equals(params[0])) {
        return EncodingUtility.decodeQP(m_Value);
      } else if (versitToken.ENCODING_BASE64.equals(params[0])) {
        return new String(EncodingUtility.decodeBase64(
            EncodingUtility.removeWhiteSpace(
                this.getValue().getBytes()
            )
        ));
      } else {
        return m_Value;
      }
    } else {
      return m_Value;
    }
  }//getDecodedValue


  private void setGroupAndId(String str) {
    //check group
    int splitpos = str.indexOf('.');
    if (splitpos != -1) {
      //split of group
      m_Group = str.substring(0, splitpos);
      m_Identifier = str.substring(splitpos + 1, str.length());
    } else {
      m_Identifier = str.toUpperCase();
    }
  }//setGroupAndId

  private void processParameters() {
    /**
     * Split up, put into hashtable, name=value, with default as
     * name if none given.
     */
    String[] params =
        StringUtil.split(getUnprocessedParameters(), ";");
    m_Params = new HashMap((int) (params.length * 1.35));
    String[] keyvalue = null;
    String[] paramlist = null;
    for (int i = 0; i < params.length; i++) {
      keyvalue = StringUtil.split(params[i], "=");
      if (keyvalue.length == 1) {
        addParameter("default".toUpperCase(), keyvalue[0]);
      } else {
        paramlist = StringUtil.splitList(keyvalue[1]);
        if (paramlist.length == 1) {
          addParameter(keyvalue[0].toUpperCase(), keyvalue[1]);
        } else {
          for (int n = 0; n < paramlist.length; n++) {
            addParameter(keyvalue[0].toUpperCase(), paramlist[n]);
          }
        }
      }
    }
  }//processParameters

/*
private static int count = 0;

  private void printParamDebug() {
    if (m_Params != null) {
      int m = 1;
      for (Iterator enum = m_Params.keySet().iterator(); enum.hasNext(); m++) {
        String key = (String) enum.next();
        String[] values = getParameter(key);
        System.out.println("    [" + m + "]" + key + "=" + StringUtil.joinList(values));
      }
    }
  }//printParamDebug

*/
  public static versitItem createItem(String line)
      throws versitException {

    //count++;
    //System.out.println(line);
    versitItem item = new versitItem();
    //String line = new String(entry);
    String prefix = "";		//group, id, params

    int splitpos = line.indexOf(':');
    if (splitpos != -1) {
      prefix = line.substring(0, splitpos);
      //finish value
      item.setValue(line.substring(splitpos + 1, line.length()));
      //split id and params
      splitpos = prefix.indexOf(';');
      if (splitpos != -1) {
        //finish group and identifier
        item.setGroupAndId(prefix.substring(0, splitpos));
        //process params
        item.setUnprocessedParameters(prefix.substring(splitpos + 1, prefix.length()));
        item.processParameters();
      } else {
        //no params
        item.setGroupAndId(prefix);
      }

    } else {

      throw new versitException("versit format seems invalid.");
    }
    return item;
  }//createItem

  public String toString() {
    StringBuffer sbuf = new StringBuffer();

    //Group
    if (m_Group != null && m_Group.length() > 0) {
      sbuf.append(m_Group).append(".");
    }
    sbuf.append(m_Identifier);
    //Parameters
    Set keys = m_Params.keySet();
    int i = 0;
    //add separator to identifier
    if (keys.size() > 0) {
      sbuf.append(";");
    }
    for (Iterator iter = keys.iterator(); iter.hasNext(); i++) {
      String key = (String) iter.next();
      String[] params = getParameter(key);
      if (params != null) {
        sbuf.append(key).append("=")
            .append(StringUtil.joinList(params));
        if (iter.hasNext()) {
          sbuf.append(";");
        }
      }
    }
    //Separator
    sbuf.append(":");

    //Folded Value
    sbuf.append(fold(sbuf.length(), m_Value));

    return sbuf.toString();
  }//toString

  private String fold(int start, String str) {
    StringBuffer sbuf = new StringBuffer();

    if ((start + str.length()) > 75) {
      int counter = 0;
      int breakcounter = start;

      while (counter < str.length()) {
        sbuf.append(str.charAt(counter));
        breakcounter++;
        counter++;
        if (breakcounter == 75) {
          breakcounter = 0;
          //folding
          sbuf.append("\r\n\t");
        }

      }
      return sbuf.toString();
    } else {
      return str;
    }
  }//fold


}//class versitItem
