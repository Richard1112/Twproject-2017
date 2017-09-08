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
import net.wimpi.pim.factory.ContactModelFactory;

/**
 * Class implementing a standard {@link net.wimpi.pim.factory.ContactModelFactory},
 * returning instances of the standard model implementations.
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class BasicContactModelFactory
    implements ContactModelFactory {

  /**
   * Returns a new {@link AddressImpl} instance.
   *
   * @return an <tt>Address</tt> instance.
   */
  public Address createAddress() {
    return new AddressImpl();
  }//createAddress

  /**
   * Returns a new {@link CommunicationsImpl} instance.
   *
   * @return a <tt>Communications</tt> instance.
   */
  public Communications createCommunications() {
    return new CommunicationsImpl();
  }//createCommunications

  /**
   * Returns a new {@link ContactImpl} instance.
   *
   * @return a <tt>Contact</tt> instance.
   */
  public Contact createContact() {
    return new ContactImpl();
  }//createContact

  /**
   * Returns a new {@link EmailAddressImpl} instance.
   *
   * @return an <tt>EmailAddress</tt> instance.
   */
  public EmailAddress createEmailAddress() {
    return new EmailAddressImpl();
  }//createemailAddress

  /**
   * Returns a new {@link GeographicalInformationImpl} instance.
   *
   * @return a <tt>GeographicalInformation</tt> instance.
   */
  public GeographicalInformation createGeographicalInformation() {
    return new GeographicalInformationImpl();
  }//createGeographicalInformation

  /**
   * Returns a new {@link DataContainer} instance.
   *
   * @return an <tt>Image</tt> instance.
   */
  public Image createImage() {
    return new DataContainer();
  }//createImage

  /**
   *Returns a new {@link DataContainer} instance.
   *
   * @return a <tt>Key</tt> instance.
   */
  public Key createKey() {
    return new DataContainer();
  }//createKey

  /**
   * Returns a new {@link OrganizationImpl} instance.
   *
   * @return an <tt>Organization</tt> instance.
   */
  public Organization createOrganization() {
    return new OrganizationImpl();
  }//createOrganization

  /**
   * Returns a new {@link OrganizationalIdentityImpl} instance.
   *
   * @return an <tt>OrganizationalIdentity</tt> instance.
   */
  public OrganizationalIdentity createOrganizationalIdentity() {
    return new OrganizationalIdentityImpl();
  }//createOrganizationalIdentity

  /**
   * Returns a new {@link PersonalIdentityImpl} instance.
   *
   * @return a <tt>PersonalIdentity</tt> instance.
   */
  public PersonalIdentity createPersonalIdentity() {
    return new PersonalIdentityImpl();
  }//createPersonalIdentity

  /**
   * Returns a new {@link PhoneNumberImpl} instance.
   *
   * @return a <tt>PhoneNumber</tt> instance.
   */
  public PhoneNumber createPhoneNumber() {
    return new PhoneNumberImpl();
  }//createPhoneNumber

  /**
   * Returns a new {@link DataContainer} instance.
   *
   * @return a <tt>Sound</tt> instance.
   */
  public Sound createSound() {
    return new DataContainer();
  }//createDataContainer

  /**
   * Returns a new {@link ExtensionsImpl} instance.
   *
   * @return an <tt>Extensions</tt> instance.
   */
  public Extensions createExtensions() {
    return new ExtensionsImpl();
  }//createExtension

}//class BasicContactModelFactory
