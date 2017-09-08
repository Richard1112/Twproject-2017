/***
 * jpim Java PIM Library
 * Copyright (c) 2001 Dieter Wimberger
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.basicimpl;

import net.wimpi.pim.contact.model.Image;
import net.wimpi.pim.contact.model.Organization;
import net.wimpi.pim.util.StringUtil;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A basic and simple implementation of an
 * {@link net.wimpi.pim.contact.model.Organization}.
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class OrganizationImpl
    implements Organization {

  static final long serialVersionUID = -5164693796281574866L;

  //instance attributes
  protected String m_Name;
  protected String m_Url;
  protected List m_Units;

  //instance associations
  private Image m_Logo;

  public OrganizationImpl() {
    m_Units =
        Collections.synchronizedList(new ArrayList(3));
  }//OrganizationImpl

  public String getName() {
    return m_Name;
  }//getName

  public void setName(String name) {
    m_Name = name;
  }//setName

  public String getURL() {
    return m_Url;
  }//getURL

  public void setURL(String url) {
    m_Url = url;
  }//setURL

  public String getUnitsList() {
    return StringUtil.joinList(listUnits());
  }//getUnitsList

  public void setUnitsList(String list) {
    String[] units = StringUtil.splitList(list);
    m_Units.clear();
    for (int i = 0; i < units.length; i++) {
      addUnit(units[i]);
    }
  }//setUnitsList

  public String getUnit(int index)
      throws IndexOutOfBoundsException {

    return (String) m_Units.get(index);
  }//getUnit

  public String setUnit(int index, String unit) {
    return (String) m_Units.set(index, unit);
  }//setUnit

  public String[] listUnits() {
    String[] units = new String[m_Units.size()];
    return (String[]) m_Units.toArray(units);
  }//listUnits

  public void addUnit(String unit) {
    m_Units.add(unit);
  }//addUnit

  public String removeUnit(int index)
      throws IndexOutOfBoundsException {
    return (String) m_Units.remove(index);
  }//removeUnit

  public int getUnitCount() {
    return m_Units.size();
  }//getUnitCount

  public Image getLogo() {
    return m_Logo;
  }//getLogo

  public void setLogo(Image logo) {
    m_Logo = logo;
  }//setLogo

  public boolean hasLogo() {
    return (m_Logo != null);
  }//hasLogo

}//class OrganizationImpl
