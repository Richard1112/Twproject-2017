/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.factory;

import net.wimpi.pim.contact.model.*;

/**
 * Interface for a <tt>ContactModelFactory</tt>, which
 * can be used to instantiate model instances.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public interface ContactModelFactory {

  /**
   * Creates a new <tt>Address</tt> instance.
   *
   * @return a new <tt>Address</tt> instance.
   */
  public Address createAddress();

  /**
   * Creates a new <tt>Communications</tt> instance.
   *
   * @return a new <tt>Communications</tt> instance.
   */
  public Communications createCommunications();

  /**
   * Creates a new <tt>Contact</tt> instance.
   *
   * @return a new <tt>Contact</tt> instance.
   */
  public Contact createContact();

  /**
   * Creates a new <tt>EmailAddress</tt> instance.
   *
   * @return a new <tt>EmailAddress</tt> instance.
   */
  public EmailAddress createEmailAddress();

  /**
   * Creates a new <tt>GeographicalInformation</tt> instance.
   *
   * @return a new <tt>GeographicalInformation</tt> instance.
   */
  public GeographicalInformation createGeographicalInformation();

  /**
   * Creates a new <tt>Image</tt> instance.
   *
   * @return a new <tt>Image</tt> instance.
   */
  public Image createImage();

  /**
   * Creates a new <tt>Key</tt> instance.
   *
   * @return a new <tt>Key</tt> instance.
   */
  public Key createKey();

  /**
   * Creates a new <tt>Organization</tt> instance.
   *
   * @return a new <tt>Organization</tt> instance.
   */
  public Organization createOrganization();

  /**
   * Creates a new <tt>OrganizationalIdentity</tt> instance.
   *
   * @return a new <tt>OrganizationalIdentity</tt> instance.
   */
  public OrganizationalIdentity createOrganizationalIdentity();

  /**
   * Creates a new <tt>PersonalIdentity</tt> instance.
   *
   * @return a new <tt>PersonalIdentity</tt> instance.
   */
  public PersonalIdentity createPersonalIdentity();

  /**
   * Creates a new <tt>PhoneNumber</tt> instance.
   *
   * @return a new <tt>PhoneNumber</tt> instance.
   */
  public PhoneNumber createPhoneNumber();

  /**
   * Creates a new <tt>Sound</tt> instance.
   *
   * @return a new <tt>Sound</tt> instance.
   */
  public Sound createSound();

  /**
   * Creates a new <tt>Extensions</tt> instance with
   * the given type.
   *
   * @return a new <tt>Extensions</tt> instance.
   */
  public Extensions createExtensions();

}//class ContactModelFactory
