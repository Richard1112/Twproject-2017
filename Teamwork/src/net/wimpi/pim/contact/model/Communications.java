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
 * An interface modeling a container for
 * telecommunications information based on the
 * types and information of the vCard Mime directory
 * profile standard specification.
 * <p>
 * For reference see RFC 2426:<br>
 * 3.3 Telecommunications Addressing Types<br>
 * 3.3.1 TEL Type Definition<br>
 * 3.3.2 EMAIL Type Definition<br>
 * 3.3.3 MAILER Type Definition<br>
 * <br>
 * Note that an implementation has to be able
 * to maintain order of the collections, as well
 * as manage the preferred number
 * and email address.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 *
 * @see net.wimpi.pim.contact.model.EmailAddress
 * @see net.wimpi.pim.contact.model.PhoneNumber
 */
public interface Communications
    extends Serializable {

  /**
   * Returns all phone numbers associated
   * with this <tt>Communications</tt>.
   *
   * @return an <tt>Iterator</tt> over all
   *         <tt>PhoneNumber</tt> instances.
   */
  public Iterator getPhoneNumbers();

  /**
   * Returns all phone numbers associated
   * with this <tt>Communications</tt>.
   *
   * @return an array of <tt>PhoneNumber<tt> instances.
   */
  public PhoneNumber[] listPhoneNumbers();

  /**
   * Returns the <tt>PhoneNumber</tt> instance
   * with the given identifier.
   *
   * @param uid a unique identifier as <tt>String</tt>.
   * @return the <tt>PhoneNumber</tt>.
   */
  public PhoneNumber getPhoneNumber(String uid);

  /**
   * Adds the given <tt>PhoneNumber</tt> instance
   * to this <tt>Communications</tt>.
   *
   * @param phone the <tt>PhoneNumber</tt> to be added.
   */
  public void addPhoneNumber(PhoneNumber phone);

  /**
   * Removes the given <tt>PhoneNumber</tt>
   * instance from this <tt>Communications</tt>.
   *
   * @param phone the <tt>PhoneNumber</tt> to be removed.
   */
  public void removePhoneNumber(PhoneNumber phone);

  /**
   * Returns the preferred the phone number of
   * this <tt>Communications</tt> instance.
   *
   * @return the preferred phone number as <tt>PhoneNumber</tt>.
   */
  public PhoneNumber getPreferredPhoneNumber();

  /**
   * Sets the preferred phone number of this <tt>Communications</tt>
   * instance.
   *
   * @param phone the preferred phone number as <tt>PhoneNumber</tt>.
   */
  public void setPreferredPhoneNumber(PhoneNumber phone);

  /**
   * Tests if the given phone number is the preferred one.
   *
   * @param phone the phone number to be tested as <tt>PhoneNumber</tt>.
   * @return true if preferred, false otherwise.
   */
  public boolean isPreferredPhoneNumber(PhoneNumber phone);

  /**
   * Returns the phone numbers of a given type
   * of this <tt>Communications</tt> instance.
   *
   * @return int the type as <tt>int</tt>.
   * @return the addresses as <tt>PhoneNumber[]</tt>.
   * @see net.wimpi.pim.contact.model.PhoneNumber
   */
  public PhoneNumber[] listPhoneNumbersByType(int TYPE);

  /**
   * Returns the number of <tt>PhoneNumber</tt> instances
   * associated with this <tt>Communications</tt>.
   *
   * @return the number of phone numbers as <tt>int</tt>.
   */
  public int getPhoneNumberCount();

  /**
   * Returns all email addresses associated
   * with this <tt>Communications</tt>.
   *
   * @return an <tt>Iterator</tt> over all
   *         <tt>EmailAddress</tt> instances.
   */
  public Iterator getEmailAddresses();

  /**
   * Returns all email addresses associated
   * with this <tt>Communications</tt>.
   *
   * @return an array of <tt>EmailAddress<tt> instances.
   */
  public EmailAddress[] listEmailAddresses();

  /**
   * Returns the <tt>EmailAddress</tt> instance
   * with the given identifier.
   *
   * @param uid a unique identifier as <tt>String</tt>.
   * @return the <tt>EmailAddress</tt> instance.
   */
  public EmailAddress getEmailAddress(String uid);

  /**
   * Adds the given <tt>EmailAddress</tt> instance
   * to this <tt>Communications</tt>.
   *
   * @param email the <tt>EmailAddress</tt> to be added.
   */
  public void addEmailAddress(EmailAddress email);

  /**
   * Removes the given <tt>EmailAddress</tt>
   * instance from this <tt>Communications</tt>.
   *
   * @param email the <tt>EmailAddress</tt> to be removed.
   */
  public void removeEmailAddress(EmailAddress email);

  /**
   * Returns the preferred email address
   * of this <tt>Communications</tt> instance.
   *
   * @return the preferred email address as <tt>EmailAddress</tt>.
   */
  public EmailAddress getPreferredEmailAddress();

  /**
   * Sets the preferred email address
   * of this <tt>Communications</tt> instance.
   *
   * @param email the preferred email address as <tt>EmailAddress</tt>.
   */
  public void setPreferredEmailAddress(EmailAddress email);

  /**
   * Tests if the given email address is the preferred one
   * of this <tt>Communications</tt> instance.
   *
   * @param email the address to be tested as <tt>EmailAddress</tt>.
   * @return true if preferred, false otherwise.
   */
  public boolean isPreferredEmailAddress(EmailAddress email);

  /**
   * Returns the number of <tt>EmailAddress</tt> instances
   * associated with this <tt>Communications</tt>.
   *
   * @return the number of email addresses as <tt>int</tt>.
   */
  public int getEmailAddressCount();

  /**
   * Returns the mailer identification.
   * Mailer refers to an electronic mail software
   * package.
   *
   * @return the mailer identification as <tt>String</tt>.
   */
  public String getMailer();

  /**
   * Sets the mailer identification.
   * Mailer refers to an electronic mail software
   * package.
   *
   * @param mailer the mailer identification as <tt>String</tt>.
   */
  public void setMailer(String mailer);

}//interface Communications
