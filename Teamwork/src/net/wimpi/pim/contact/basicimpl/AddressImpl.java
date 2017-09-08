/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.basicimpl;

import net.wimpi.pim.contact.model.Address;
import net.wimpi.pim.util.AbstractIdentifiable;

/**
 * A basic and simple implementation of an {@link net.wimpi.pim.contact.model.Address}.
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class AddressImpl
    extends AbstractIdentifiable
    implements Address {

  static final long serialVersionUID = 4049170718832434337L;

  //instance attributes
  protected String m_PostBox;
  protected String m_Extended;
  protected String m_Street;
  protected String m_City;
  protected String m_Region;
  protected String m_PostalCode;
  protected String m_Country;
  protected String m_Label = "";
  protected boolean m_Domestic;
  protected boolean m_International;
  protected boolean m_Work;
  protected boolean m_Home;
  protected boolean m_Postal;
  protected boolean m_Parcel;
  protected int m_Index;

  public int getIndex() {
    return m_Index;
  }//getIndex

  public void setIndex(int index) {
    m_Index = index;
  }//setIndex

  public String getPostBox() {
    return m_PostBox;
  }//getPostBox

  public void setPostBox(String pobox) {
    m_PostBox = pobox;
  }//setPostBox

  public String getExtended() {
    return m_Extended;
  }//getExtended

  public void setExtended(String extended) {
    m_Extended = extended;
  }//setExtended

  public String getStreet() {
    return m_Street;
  }//getStreet;

  public void setStreet(String street) {
    m_Street = street;
  }//setStreet

  public String getCity() {
    return m_City;
  }//getCity

  public void setCity(String city) {
    m_City = city;
  }//setCity

  public String getRegion() {
    return m_Region;
  }//getRegion

  public void setRegion(String region) {
    m_Region = region;
  }//setRegion

  public String getPostalCode() {
    return m_PostalCode;
  }//getPostalCode

  public void setPostalCode(String postalcode) {
    m_PostalCode = postalcode;
  }//setPostalCode

  public String getCountry() {
    return m_Country;
  }//getCountry

  public void setCountry(String country) {
    m_Country = country;
  }//setCountry

  public String getLabel() {
    return m_Label;
  }//getLabel

  public void setLabel(String label) {
    m_Label = label;
  }//setLabel

  public boolean isDomestic() {
    return m_Domestic;
  }//isDomestic

  public boolean isInternational() {
    return m_International;
  }//isInternational

  public void setDomestic(boolean b) {
    m_Domestic = b;
  }//setDomestic

  public void setInternational(boolean b) {
    m_International = b;
  }//setInternational

  public boolean isParcel() {
    return m_Parcel;
  }//isParcel

  public void setParcel(boolean b) {
    m_Parcel = b;
  }//setParcel

  public boolean isPostal() {
    return m_Postal;
  }//isPostal

  public void setPostal(boolean b) {
    m_Postal = b;
  }//setPostal

  public boolean isHome() {
    return m_Home;
  }//isHome

  public boolean isWork() {
    return m_Work;
  }//isWork

  public void setHome(boolean b) {
    m_Home = b;
  }//setHome;

  public void setWork(boolean b) {
    m_Work = b;
  }//setWork;

  public boolean isType(int TYPE) {
    switch (TYPE) {
      case TYPE_HOME:
        return isHome();
      case TYPE_WORK:
        return isWork();
      case TYPE_POSTAL:
        return isPostal();
      case TYPE_PARCEL:
        return isParcel();
      case TYPE_DOMESTIC:
        return isDomestic();
      case TYPE_INTERNATIONAL:
        return isInternational();
      default:
        return false;
    }
  }//isType

}//class AddressImpl
