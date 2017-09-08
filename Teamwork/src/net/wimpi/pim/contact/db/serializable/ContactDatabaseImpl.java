/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.db.serializable;

import net.wimpi.pim.contact.db.ContactCollection;
import net.wimpi.pim.contact.db.ContactDatabase;
import net.wimpi.pim.contact.db.ContactGroup;
import net.wimpi.pim.contact.db.ContactGroupCollection;
import net.wimpi.pim.contact.model.Contact;
import net.wimpi.pim.util.AbstractIdentifiable;

import java.io.Serializable;

/**
 * Class representing a serializable <tt>ContactDatabase</tt>.
 * implementation.
 *
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class ContactDatabaseImpl
    extends AbstractIdentifiable
    implements ContactDatabase, Serializable {

  static final long serialVersionUID = -462364707073284234L;

  protected ContactCollection m_ContactCollection;
  protected ContactGroupCollection m_ContactGroupCollection;
  protected Contact m_Owner;

  public Contact getOwner() {
    return m_Owner;
  }//getOwner

  public void setOwner(Contact contact) {
    m_Owner = contact;
  }//setOwner

  public ContactCollection getContactCollection() {
    if (m_ContactCollection == null) {
      m_ContactCollection = new ContactCollectionImpl();
    }
    return m_ContactCollection;
  }//getContactCollection

  public ContactGroupCollection getContactGroupCollection() {
    if (m_ContactGroupCollection == null) {
      m_ContactGroupCollection = new ContactGroupCollectionImpl();
    }
    return m_ContactGroupCollection;
  }//getContactGroupCollection

  public ContactGroup createContactGroup() {
    return ((ContactCollectionImpl) getContactCollection()).createContactGroup();
  }//createContactGroup


}//class ContactDatabaseImpl
