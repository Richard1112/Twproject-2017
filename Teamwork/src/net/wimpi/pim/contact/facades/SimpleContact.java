/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.facades;

import net.wimpi.pim.Pim;
import net.wimpi.pim.contact.model.*;
import net.wimpi.pim.factory.ContactModelFactory;

import java.util.Date;
import java.util.Iterator;

/**
 * A simple contact class implemented as a
 * facade for the more complex contact model.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class SimpleContact {

  private transient ContactModelFactory m_CFM = Pim.getContactModelFactory();
  private transient Contact m_Contact;
  private transient PersonalIdentity m_PID;
  private transient OrganizationalIdentity m_OID;
  private transient Organization m_Org;
  private transient Communications m_COMM;
  private transient PhoneNumber m_HomeNumber;
  private transient PhoneNumber m_WorkNumber;
  private transient PhoneNumber m_PagerNumber;
  private transient PhoneNumber m_MobileNumber;
  private transient PhoneNumber m_FaxNumber;
  private transient Address m_WorkAddress;
  private transient Address m_HomeAddress;
  private transient EmailAddress m_Email;

  public SimpleContact() {
    m_Contact = m_CFM.createContact();
  }//constructor

  public SimpleContact(Contact ct) {
    m_Contact = ct;
    //pick direct references
    if (ct.hasPersonalIdentity()) {
      m_PID = ct.getPersonalIdentity();
    }
    if (ct.hasOrganizationalIdentity()) {
      m_OID = ct.getOrganizationalIdentity();
      if (m_OID.hasOrganization()) {
        m_Org = m_OID.getOrganization();
      }
    }
    if (ct.hasCommunications()) {
      m_COMM = ct.getCommunications();
      Iterator iter = m_COMM.getPhoneNumbers();
      while (iter.hasNext()) {
        PhoneNumber num = (PhoneNumber) iter.next();
        if (num.isHome()) {
          m_HomeNumber = num;
        } else if (num.isWork()) {
          m_WorkNumber = num;
        } else if (num.isPager()) {
          m_PagerNumber = num;
        } else if (num.isFax()) {
          m_FaxNumber = num;
        } else if (num.isCellular()) {
          m_MobileNumber = num;
        }
      }
      if (m_COMM.getEmailAddressCount() > 0) {
        m_Email = m_COMM.getPreferredEmailAddress();
        if (m_Email == null) {
          m_Email = m_COMM.listEmailAddresses()[0];
        }
      }
    }
    Iterator iter = m_Contact.getAddresses();
    while (iter.hasNext()) {
      Address a = (Address) iter.next();
      if (a.isHome()) {
        m_HomeAddress = a;
      } else if (a.isWork()) {
        m_WorkAddress = a;
      }
    }
  }//SimpleContact

  public String getUID() {
    return m_Contact.getUID();
  }//getUID

  public String getCategory() {
    try {
      return m_Contact.getCategory(0);
    } catch (IndexOutOfBoundsException ex) {
      return EMPTY;
    }
  }//getCategory

  public void setCategory(String category) {
    m_Contact.removeAllCategories();
    if (category != null && category.length() > 0) {
      m_Contact.addCategory(category);
    }
  }//setCategory

  public String getNickname() {
    if (m_PID != null && m_PID.getNicknameCount() > 0) {
      return m_PID.getNickname(0);
    } else {
      return EMPTY;
    }
  }//getNickname

  public void setNickname(String nickname) {
    if (nickname != null && nickname.length() > 0) {
      lazyCreatePID();
      m_PID.removeAllNicknames();
      m_PID.addNickname(nickname);
    } else {
      if (m_PID != null) {
        m_PID.removeAllNicknames();
      }
    }
  }//setNickname

  public String getFirstname() {
    if (m_PID != null) {
      return m_PID.getFirstname();
    } else {
      return EMPTY;
    }
  }//getFirstname

  public void setFirstname(String name) {
    lazyCreatePID();
    m_PID.setFirstname(name);
  }//getFirstname

  public String getLastname() {
    if (m_PID != null) {
      return m_PID.getLastname();
    } else {
      return EMPTY;
    }
  }//getLastname

  public void setLastname(String name) {
    lazyCreatePID();
    m_PID.setLastname(name);
  }//getLastname

  public String getMiddlename() {
    if (m_PID != null && m_PID.getNicknameCount() > 0) {
      return m_PID.getAdditionalName(0);
    } else {
      return EMPTY;
    }
  }//getMiddlename

  public void setMiddlename(String name) {
    if (name != null && name.length() > 0) {
      lazyCreatePID();
      m_PID.removeAllAdditionalNames();
      m_PID.addAdditionalName(name);
    } else {
      if (m_PID != null) {
        m_PID.removeAllAdditionalNames();
      }
    }
  }//setMiddlename

  public String getCompany() {
    if (m_Org != null) {
      return m_Org.getName();
    } else {
      return EMPTY;
    }
  }//getCompany

  public void setCompany(String name) {
    lazyCreateOrg();
    m_Org.setName(name);
  }//setCompany

  public String getTitle() {
    if (m_OID != null) {
      return m_OID.getTitle();
    } else {
      return EMPTY;
    }
  }//getTitle

  public void setTitle(String title) {
    lazyCreateOID();
    m_OID.setTitle(title);
  }//setTitle

  public String getRole() {
    if (m_OID != null) {
      return m_OID.getRole();
    } else {
      return EMPTY;
    }
  }//getRole

  public void setRole(String role) {
    lazyCreateOID();
    m_OID.setRole(role);
  }//setRole

  public String getHomePhoneNumber() {
    if (m_HomeNumber != null) {
      return m_HomeNumber.getNumber();
    } else {
      return EMPTY;
    }
  }//getHomePhoneNumber

  public void setHomePhoneNumber(String number) {
    lazyCreateHomeNumber();
    m_HomeNumber.setNumber(number);
  }//setHomePhoneNumber

  public String getWorkPhoneNumber() {
    if (m_WorkNumber != null) {
      return m_WorkNumber.getNumber();
    } else {
      return EMPTY;
    }
  }//getWorkPhoneNumber

  public void setWorkPhoneNumber(String number) {
    lazyCreateWorkNumber();
    m_WorkNumber.setNumber(number);
  }//setWorkPhoneNumber

  public String getPagerNumber() {
    if (m_PagerNumber != null) {
      return m_PagerNumber.getNumber();
    } else {
      return EMPTY;
    }
  }//getPagerNumber

  public void setPagerNumber(String number) {
    lazyCreatePagerNumber();
    m_PagerNumber.setNumber(number);
  }//setPagerNumber

  public String getFaxNumber() {
    if (m_FaxNumber != null) {
      return m_FaxNumber.getNumber();
    } else {
      return EMPTY;
    }
  }//getFaxNumber

  public void setFaxNumber(String number) {
    lazyCreateFaxNumber();
    m_FaxNumber.setNumber(number);
  }//setFaxNumber

  public String getMobileNumber() {
    if (m_MobileNumber != null) {
      return m_MobileNumber.getNumber();
    } else {
      return EMPTY;
    }
  }//getMobileNumber

  public void setMobileNumber(String number) {
    lazyCreateMobileNumber();
    m_MobileNumber.setNumber(number);
  }//setMobileNumber

  public String getWorkStreet() {
    if (m_WorkAddress != null) {
      return m_WorkAddress.getStreet();
    } else {
      return EMPTY;
    }
  }//getWorkStreet

  public void setWorkStreet(String street) {
    lazyCreateWorkAddress();
    m_WorkAddress.setStreet(street);
  }//setWorkStreet

  public String getWorkCity() {
    if (m_WorkAddress != null) {
      return m_WorkAddress.getCity();
    } else {
      return EMPTY;
    }
  }//getWorkCity

  public void setWorkCity(String city) {
    lazyCreateWorkAddress();
    m_WorkAddress.setCity(city);
  }//setWorkCity

  public String getWorkRegion() {
    if (m_WorkAddress != null) {
      return m_WorkAddress.getRegion();
    } else {
      return EMPTY;
    }
  }//getWorkRegion

  public void setWorkRegion(String region) {
    lazyCreateWorkAddress();
    m_WorkAddress.setRegion(region);
  }//setWorkRegion

  public String getWorkCountry() {
    if (m_WorkAddress != null) {
      return m_WorkAddress.getCountry();
    } else {
      return EMPTY;
    }
  }//getWorkCountry

  public void setWorkCountry(String country) {
    lazyCreateWorkAddress();
    m_WorkAddress.setCountry(country);
  }//setWorkCountry

  public String getWorkZIP() {
    if (m_WorkAddress != null) {
      return m_WorkAddress.getPostalCode();
    } else {
      return EMPTY;
    }
  }//getWorkZIP

  public void setWorkZIP(String zip) {
    lazyCreateWorkAddress();
    m_WorkAddress.setPostalCode(zip);
  }//setWorkZIP

  public String getHomeStreet() {
    if (m_HomeAddress != null) {
      return m_HomeAddress.getStreet();
    } else {
      return EMPTY;
    }
  }//getHomeStreet

  public void setHomeStreet(String street) {
    lazyCreateHomeAddress();
    m_HomeAddress.setStreet(street);
  }//setHomeStreet

  public String getHomeCity() {
    if (m_HomeAddress != null) {
      return m_HomeAddress.getCity();
    } else {
      return EMPTY;
    }
  }//getHomeCity

  public void setHomeCity(String city) {
    lazyCreateHomeAddress();
    m_HomeAddress.setCity(city);
  }//setHomeCity

  public String getHomeRegion() {
    if (m_HomeAddress != null) {
      return m_HomeAddress.getRegion();
    } else {
      return EMPTY;
    }
  }//getHomeRegion

  public void setHomeRegion(String region) {
    lazyCreateHomeAddress();
    m_HomeAddress.setRegion(region);
  }//setHomeRegion

  public String getHomeCountry() {
    if (m_HomeAddress != null) {
      return m_HomeAddress.getCountry();
    } else {
      return EMPTY;
    }
  }//getHomeCountry

  public void setHomeCountry(String country) {
    lazyCreateHomeAddress();
    m_HomeAddress.setCountry(country);
  }//setHomeCountry

  public String getHomeZIP() {
    if (m_HomeAddress != null) {
      return m_HomeAddress.getPostalCode();
    } else {
      return EMPTY;
    }
  }//getHomeZIP

  public void setHomeZIP(String zip) {
    lazyCreateHomeAddress();
    m_HomeAddress.setPostalCode(zip);
  }//setHomeZIP

  public String getEmail() {
    if (m_Email != null) {
      return m_Email.getAddress();
    } else {
      return EMPTY;
    }
  }//getEmail

  public String getFullEmail() {
    if (m_Email == null || m_Email.getAddress() == null
        || m_Email.getAddress().length() == 0) {
      return EMPTY;
    }
    StringBuffer sbuf = new StringBuffer();
    if (m_PID != null) {
      sbuf.append(m_PID.getFirstname())
          .append(' ')
          .append(m_PID.getLastname())
          .append(' ');
    }
    sbuf.append('<')
        .append(m_Email.getAddress())
        .append('>');
    return sbuf.toString();
  }//getFullEmail

  public void setEmail(String email) {
    lazyCreateEmail();
    m_Email.setAddress(email);
  }//setEmail


  public String getURL() {
    return m_Contact.getURL();
  }//getURL

  public void setURL(String url) {
    m_Contact.setURL(url);
  }//setURL

  public String getCompanyURL() {
    if (m_Org != null) {
      return m_Org.getURL();
    } else {
      return EMPTY;
    }
  }//getCompanyUrl

  public void setCompanyURL(String url) {
    lazyCreateOrg();
    m_Org.setURL(url);
  }//setCompanyUrl

  public String getComments() {
    return ((m_Contact.getNote() != null) ? m_Contact.getNote() : EMPTY);
  }//getComments

  public void setComments(String comments) {
    if (comments == null) {
      comments = EMPTY;
    }
    m_Contact.setNote(comments);
  }//setComments

  public Date getBirthDate() {
    if (m_PID != null) {
      return m_PID.getBirthDate();
    } else {
      return null;
    }
  }//getBirthDate

  public void setBirthDate(Date date) {
    if (date != null) {
      lazyCreatePID();
      m_PID.setBirthDate(date);
    }
  }//setBirthDate

  public boolean isFrequentRecipient() {
    return m_Contact.isFrequent();
  }//isFrequentRecipient

  public void setFrequentRecipient(boolean b) {
    m_Contact.setFrequent(b);
  }//setFrequentRecipient

  public Contact getContact() {
    return m_Contact;
  }//getContact

  public boolean equals(Object o) {
    return m_Contact.equals(o);
  }//equals

  private final static String EMPTY = "";

  private final void lazyCreatePID() {
    if (m_PID == null) {
      m_PID = m_CFM.createPersonalIdentity();
      m_Contact.setPersonalIdentity(m_PID);
    }
  }//lazyCreatePID

  private final void lazyCreateOID() {
    if (m_OID == null) {
      m_OID = m_CFM.createOrganizationalIdentity();
      m_Contact.setOrganizationalIdentity(m_OID);
    }
  }//lazyCreateOID

  private final void lazyCreateOrg() {
    if (m_Org == null) {
      lazyCreateOID();
      m_Org = m_CFM.createOrganization();
      m_OID.setOrganization(m_Org);
    }
  }//lazyCreateOrg

  private final void lazyCreateCOMM() {
    if (m_COMM == null) {
      m_COMM = m_CFM.createCommunications();
      m_Contact.setCommunications(m_COMM);
    }
  }//lazyCreateCOMM

  private final void lazyCreateHomeNumber() {
    if (m_HomeNumber == null) {
      lazyCreateCOMM();
      m_HomeNumber = m_CFM.createPhoneNumber();
      m_HomeNumber.setHome(true);
      m_COMM.addPhoneNumber(m_HomeNumber);
    }
  }//lazyCreateHomeNumber

  private final void lazyCreateWorkNumber() {
    if (m_WorkNumber == null) {
      lazyCreateCOMM();
      m_WorkNumber = m_CFM.createPhoneNumber();
      m_WorkNumber.setHome(false);
      m_WorkNumber.setWork(true);
      m_COMM.addPhoneNumber(m_WorkNumber);
    }
  }//lazyCreateHomeNumber

  private final void lazyCreatePagerNumber() {
    if (m_PagerNumber == null) {
      lazyCreateCOMM();
      m_PagerNumber = m_CFM.createPhoneNumber();
      m_PagerNumber.setPager(true);
      m_COMM.addPhoneNumber(m_PagerNumber);
    }
  }//lazyCreatePagerNumber

  private final void lazyCreateFaxNumber() {
    if (m_FaxNumber == null) {
      lazyCreateCOMM();
      m_FaxNumber = m_CFM.createPhoneNumber();
      m_FaxNumber.setFax(true);
      m_COMM.addPhoneNumber(m_FaxNumber);
    }
  }//lazyCreateFaxNumber

  private final void lazyCreateMobileNumber() {
    if (m_MobileNumber == null) {
      lazyCreateCOMM();
      m_MobileNumber = m_CFM.createPhoneNumber();
      m_MobileNumber.setCellular(true);
      m_COMM.addPhoneNumber(m_MobileNumber);
    }
  }//lazyCreateMobileNumber

  private final void lazyCreateWorkAddress() {
    if (m_WorkAddress == null) {
      m_WorkAddress = m_CFM.createAddress();
      m_WorkAddress.setWork(true);
      m_Contact.addAddress(m_WorkAddress);
    }
  }//lazyCreateWorkAddress

  private final void lazyCreateHomeAddress() {
    if (m_HomeAddress == null) {
      m_HomeAddress = m_CFM.createAddress();
      m_HomeAddress.setHome(true);
      m_Contact.addAddress(m_HomeAddress);
    }
  }//lazyCreateHomeAddress

  private final void lazyCreateEmail() {
    if (m_Email == null) {
      lazyCreateCOMM();
      m_Email = m_CFM.createEmailAddress();
      m_COMM.addEmailAddress(m_Email);
      m_COMM.setPreferredEmailAddress(m_Email);
    }
  }//lazyCreateEMail

}//class SimpleContact
