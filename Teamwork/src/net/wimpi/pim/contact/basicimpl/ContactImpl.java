/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.basicimpl;

import net.wimpi.pim.contact.model.*;
import net.wimpi.pim.util.AbstractIdentifiable;
import net.wimpi.pim.util.StringUtil;

import java.util.*;

/**
 * A basic and simple implementation of a
 * {@link net.wimpi.pim.contact.model.Contact}.
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class ContactImpl
    extends AbstractIdentifiable
    implements Contact {

  static final long serialVersionUID = 5552536829541983668L;

  //instance attributes
  protected String m_Url;
  protected boolean m_Frequent = false;
  protected String m_Note;
  protected Date m_Revision;
  protected String m_Classification;
  protected List m_Addresses;
  protected List m_Categories;
  protected Address m_PreferredAddress;
  protected Address m_LastAddedAddress;

  //instance associations
  protected PersonalIdentity m_PersonalIdentity;
  protected OrganizationalIdentity m_OrganizationalIdentity;
  protected Communications m_Communications;
  protected GeographicalInformation m_GeoInfo;
  protected Key m_Key;
  protected Sound m_Sound;
  protected Extensions m_Extensions;

  public ContactImpl() {
    super();
    m_Addresses =
        Collections.synchronizedList(new ArrayList(5));
    m_Categories =
        Collections.synchronizedList(new ArrayList(5));
  }//constructor

  public PersonalIdentity getPersonalIdentity() {
    return m_PersonalIdentity;
  }//getPersonalIdentity

  public void setPersonalIdentity(PersonalIdentity identity) {
    m_PersonalIdentity = identity;
  }//setPersonalIdentity

  public boolean hasPersonalIdentity() {
    return (m_PersonalIdentity != null);
  }//hasPersonalIdentity

  public OrganizationalIdentity getOrganizationalIdentity() {
    return m_OrganizationalIdentity;
  }//getOrganizationalIdentity

  public void setOrganizationalIdentity(OrganizationalIdentity identity) {
    m_OrganizationalIdentity = identity;
  }//setOrganizationalIdentity

  public boolean hasOrganizationalIdentity() {
    return (m_OrganizationalIdentity != null);
  }//hasOrganizationalIdentity

  public Iterator getAddresses() {
    return m_Addresses.listIterator();
  }//getAddresses

  public Address[] listAddresses() {
    Address[] addr = new Address[m_Addresses.size()];
    return (Address[]) m_Addresses.toArray(addr);
  }//listAddresses

  public Address getAddress(String uid) {
    for (Iterator iter = m_Addresses.iterator(); iter.hasNext();) {
      Address addr = (Address) iter.next();
      if (addr.equals(uid)) {
        return addr;
      }
    }
    return null;
  }//getAddress

  public Address getLastAddedAddress() {
    return m_LastAddedAddress;
  }//getLastAddedAddress

  public void addAddress(Address addr) {
    m_Addresses.add(addr);
    m_LastAddedAddress = addr;
  }//addAddress

  public void removeAddress(Address addr)
      throws IndexOutOfBoundsException {
    if (isPreferredAddress(addr)) {
      m_PreferredAddress = null;
    }
    m_Addresses.remove(addr);
  }//removeAddress

  public Address getPreferredAddress() {
    return m_PreferredAddress;
  }//getPreferredAddress

  public void setPreferredAddress(Address address) {
    if (m_Addresses.contains(address)) {
      m_PreferredAddress = address;
    }
  }//setPreferredAddress

  public boolean isPreferredAddress(Address address) {
    return (address.equals(m_PreferredAddress));
  }//isPreferredAddress

  public int getAddressCount() {
    return m_Addresses.size();
  }//getAddressCount

  public Address[] listAddressesByType(int TYPE) {
    ArrayList list = new ArrayList(m_Addresses.size());
    for (Iterator iterator = m_Addresses.iterator(); iterator.hasNext();) {
      Address addr = (Address) iterator.next();
      if (addr.isType(TYPE)) {
        list.add(addr);
      }
    }
    //return array
    Address[] addrs = new Address[list.size()];
    return (Address[]) list.toArray(addrs);
  }//listAddressesByType

  public Communications getCommunications() {
    return m_Communications;
  }//getCommunications

  public void setCommunications(Communications communications) {
    m_Communications = communications;
  }//setCommunications

  public boolean hasCommunications() {
    return (m_Communications != null);
  }//hasCommunications

  public GeographicalInformation getGeographicalInformation() {
    return m_GeoInfo;
  }//getGeographicalInformation

  public void setGeographicalInformation(GeographicalInformation geoinfo) {
    m_GeoInfo = geoinfo;
  }//SetGeographicalInformation

  public boolean hasGeographicalInformation() {
    return (m_GeoInfo != null);
  }//hasGeographicalInformation

  public String getCategoriesList() {
    return StringUtil.joinList(listCategories());
  }//getCategoriesList

  public void setCategoriesList(String list) {
    String[] cats = StringUtil.splitList(list);
    m_Categories.clear();
    for (int i = 0; i < cats.length; i++) {
      addCategory(cats[i]);
    }
  }//setCategoriesList

  public String[] listCategories() {
    String[] cats = new String[m_Categories.size()];
    return (String[]) m_Categories.toArray(cats);
  }//listCategories

  public String getCategory(int index)
      throws IndexOutOfBoundsException {

    return (String) m_Categories.get(index);
  }//getCategory

  public String setCategory(int index, String cat) {
    return (String) m_Categories.set(index, cat);
  }//setCategory

  public void addCategory(String category) {
    m_Categories.add(category);
  }//addCategory

  public String removeCategory(int index)
      throws IndexOutOfBoundsException {
    return (String) m_Categories.remove(index);
  }//removeCategory

  public void removeAllCategories() {
    m_Categories.clear();
  }//removeAllCategories

  public int getCategoryCount() {
    return m_Categories.size();
  }//getCategoryCount

  public String getURL() {
    return m_Url;
  }//getURL

  public void setURL(String url) {
    m_Url = url;
  }//setURL

  public boolean isFrequent() {
    return m_Frequent;
  }//isFrequent

  public void setFrequent(boolean b) {
    m_Frequent = b;
  }//setFrequent

  public String getNote() {
    return m_Note;
  }//getNote

  public void setNote(String note) {
    m_Note = note;
  }//setNote

  public Date getCurrentRevisionDate() {
    return m_Revision;
  }//getCurrentRevisionDate

  public void setCurrentRevisionDate(Date revision) {
    m_Revision = revision;
  }//setCurrentRevisionDate

  public String getAccessClassification() {
    return m_Classification;
  }//getAccessClassification

  public void setAccessClassification(String classification) {
    m_Classification = classification;
  }//setAccessClassification

  public Key getPublicKey() {
    return m_Key;
  }//getPublicKey

  public void setPublicKey(Key key) {
    m_Key = key;
  }//setPublicKey

  public boolean hasPublicKey() {
    return (m_Key != null);
  }//hasPublicKey

  public Sound getSound() {
    return m_Sound;
  }//getSound

  public void setSound(Sound sound) {
    m_Sound = sound;
  }//setSound

  public boolean hasSound() {
    return (m_Sound != null);
  }//hasSound

  public Extensions getExtensions() {
    return m_Extensions;
  }//getExtensions

  public void setExtensions(Extensions extensions) {
    m_Extensions = extensions;
  }//setExtensions

  public boolean hasExtensions() {
    return (m_Extensions != null);
  }//hasExtensions

}//class ContactImpl
