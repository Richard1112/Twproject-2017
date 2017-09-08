/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.basicimpl;

import net.wimpi.pim.contact.model.Communications;
import net.wimpi.pim.contact.model.EmailAddress;
import net.wimpi.pim.contact.model.PhoneNumber;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * A basic and simple implementation of the
 * {@link net.wimpi.pim.contact.model.Communications}.
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class CommunicationsImpl
    implements Communications {

  static final long serialVersionUID = 323095380120358187L;

  //instance attributes
  protected List m_PhoneNumbers;
  protected List m_EmailAddresses;
  protected String m_Mailer;
  protected EmailAddress m_PreferredEmail;
  protected PhoneNumber m_PreferredNumber;

  public CommunicationsImpl() {
    m_PhoneNumbers =
        Collections.synchronizedList(new ArrayList(5));
    m_EmailAddresses =
        Collections.synchronizedList(new ArrayList(5));
  }//constructor

  public Iterator getPhoneNumbers() {
    return m_PhoneNumbers.listIterator();
  }//getPhoneNumbers

  public PhoneNumber[] listPhoneNumbers() {
    PhoneNumber[] numbers = new PhoneNumber[m_PhoneNumbers.size()];
    return (PhoneNumber[]) m_PhoneNumbers.toArray(numbers);
  }//listPhoneNumbers

  public PhoneNumber getPhoneNumber(String uid) {
    for (Iterator iter = m_PhoneNumbers.iterator(); iter.hasNext();) {
      PhoneNumber number = (PhoneNumber) iter.next();
      if (number.equals(uid)) {
        return number;
      }
    }
    return null;
  }//getPhoneNumber

  public void addPhoneNumber(PhoneNumber number) {
    m_PhoneNumbers.add(number);
  }//addPhoneNumber

  public void removePhoneNumber(PhoneNumber number) {
    if (isPreferredPhoneNumber(number)) {
      m_PreferredNumber = null;
    }
    m_PhoneNumbers.remove(number);
  }//removePhoneNumber

  public PhoneNumber getPreferredPhoneNumber() {
    return m_PreferredNumber;
  }//getPreferredPhoneNumber

  public void setPreferredPhoneNumber(PhoneNumber number) {
    if (m_PhoneNumbers.contains(number)) {
      m_PreferredNumber = number;
    }
  }//setPreferredPhoneNumber

  public boolean isPreferredPhoneNumber(PhoneNumber number) {
    return (number.equals(m_PreferredNumber));
  }//isPreferredPhoneNumber

  public PhoneNumber[] listPhoneNumbersByType(int TYPE) {
    ArrayList list = new ArrayList(m_PhoneNumbers.size());
    for (Iterator iterator = m_PhoneNumbers.iterator(); iterator.hasNext();) {
      PhoneNumber num = (PhoneNumber) iterator.next();
      if (num.isType(TYPE)) {
        list.add(num);
      }
    }
    //return array
    PhoneNumber[] numbers = new PhoneNumber[list.size()];
    return (PhoneNumber[]) list.toArray(numbers);
  }//listPhoneNumbersByType

  public int getPhoneNumberCount() {
    return m_PhoneNumbers.size();
  }//getPhoneNumberCount

  public Iterator getEmailAddresses() {
    return m_EmailAddresses.listIterator();
  }//getEmailAddresses

  public EmailAddress[] listEmailAddresses() {
    EmailAddress[] emails = new EmailAddress[m_EmailAddresses.size()];
    return (EmailAddress[]) m_EmailAddresses.toArray(emails);
  }//listEmailAddresses

  public EmailAddress getEmailAddress(String uid) {

    for (Iterator iter = m_EmailAddresses.iterator(); iter.hasNext();) {
      EmailAddress addr = (EmailAddress) iter.next();
      if (addr.equals(uid)) {
        return addr;
      }
    }
    return null;
  }//getEmailAddress

  public void addEmailAddress(EmailAddress email) {
    m_EmailAddresses.add(email);
  }//addEmailAddress

  public void removeEmailAddress(EmailAddress email) {
    m_EmailAddresses.remove(email);
  }//removeEmailAddress

  public EmailAddress getPreferredEmailAddress() {
    return m_PreferredEmail;
  }//getPreferredEmailAddress

  public void setPreferredEmailAddress(EmailAddress email) {
    if (m_EmailAddresses.contains(email)) {
      m_PreferredEmail = email;
    }
  }//setPreferredAddress

  public boolean isPreferredEmailAddress(EmailAddress email) {
    return (email.equals(m_PreferredEmail));
  }//isPreferredEmailAddress

  public int getEmailAddressCount() {
    return m_EmailAddresses.size();
  }//getEmailAddressCount

  public String getMailer() {
    return m_Mailer;
  }//getMailer

  public void setMailer(String mailer) {
    m_Mailer = mailer;
  }//setMailer

}//class CommunicationsImpl
