/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.db.serializable;

import net.wimpi.pim.contact.db.AbstractContactFilter;
import net.wimpi.pim.contact.db.ContactCollection;
import net.wimpi.pim.contact.db.ContactFilter;
import net.wimpi.pim.contact.db.ContactGroup;
import net.wimpi.pim.contact.model.Contact;
import net.wimpi.pim.util.AbstractIdentifiable;

import java.io.Serializable;
import java.util.*;

/**
 * Class representing a serializable <tt>ContactCollection</tt>.
 * implementation.
 *
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class ContactCollectionImpl
    implements ContactCollection, Serializable {

  static final long serialVersionUID = -7518133802753769769L;

  protected Map m_Contacts;
  protected Map m_NameIndex;
  transient protected int m_ModCount = 0;

  public ContactCollectionImpl() {
    m_Contacts = new HashMap(50);
    m_NameIndex = new HashMap(50);
  }//constructor

  public boolean contains(String UID) {
    return m_Contacts.keySet().contains(UID);
  }//contains

  public boolean contains(Contact contact) {
    return contains(contact.getUID());
  }//contains

  public boolean containsByName(String name) {
    return m_NameIndex.keySet().contains(name);
  }//containsByName

  public Contact get(String UID) {
    return (Contact) m_Contacts.get(UID);
  }//get

  public Contact getByName(String name) {
    return get((String) m_NameIndex.get(name));
  }//getByName

  public Contact[] toArray() {
    Contact[] contacts = new Contact[m_Contacts.size()];
    Iterator iter = iterator();
    for (int i = 0; i < m_Contacts.size(); i++) {
      contacts[i] = (Contact) iter.next();
    }
    return contacts;
  }//toArray

  public Contact[] toArray(ContactFilter filter) {
    ContactIterator iter = new ContactIterator(filter);
    Contact[] contacts = new Contact[iter.size()];
    for (int i = 0; i < iter.size(); i++) {
      contacts[i] = (Contact) iter.next();
    }
    return contacts;
  }//toArray

  public void add(Contact contact) {
    if (!m_Contacts.keySet().contains(contact.getUID())) {
      //add to collection
      int size = size();
      m_Contacts.put(contact.getUID(), contact);
      addToNameIndex(contact);
      if (size != size()) {
        if (m_ModCount == Integer.MAX_VALUE) {
          m_ModCount = 0;
        } else {
          m_ModCount++;
        }
      }
    }
  }//add

  public Contact remove(String UID) {
    Contact ct = (Contact) m_Contacts.remove(UID);
    if (ct != null) {
      removeFromNameIndex(ct);
      if (m_ModCount == Integer.MAX_VALUE) {
        m_ModCount = 0;
      } else {
        m_ModCount++;
      }
    }
    return ct;
  }//remove

  public void remove(Contact contact) {
    remove(contact.getUID());
  }//remove

  public Iterator iterator() {
    return new ContactIterator();
  }//iterator

  public Iterator iterator(ContactFilter filter) {
    return new ContactIterator(filter);
  }//iterator

  public int size() {
    return m_Contacts.size();
  }//size

  private void addToNameIndex(Contact contact) {
    if (contact == null) {
      return;
    }
    if (contact.hasPersonalIdentity()) {
      String fn = contact.getPersonalIdentity().getFormattedName();
      if (fn != null && fn.length() > 0) {
        //add to index
        m_NameIndex.put(fn, contact.getUID());
      }
    }
  }//addToNameIndex

  private void removeFromNameIndex(Contact contact) {
    if (contact == null) {
      return;
    }
    if (contact.hasPersonalIdentity()) {
      String fn = contact.getPersonalIdentity().getFormattedName();
      if (fn != null && fn.length() > 0) {
        //add to index
        m_NameIndex.remove(fn);
      }
    }
  }//removeFromNameIndex

  private boolean modified(int modcount) {
    return (modcount != m_ModCount);
  }//modified

  ContactGroup createContactGroup() {
    return new ContactGroupImpl();
  }//createContactGroup

  class ContactIterator
      implements Iterator {

    private ArrayList m_Entries;
    private int m_Next;
    private int m_Last = -1;
    private int m_ExpectedModCount = ContactCollectionImpl.this.m_ModCount;

    public ContactIterator() {
      prepare(null);
    }//constructor

    public ContactIterator(ContactFilter filter) {
      prepare(filter);
    }//constructor

    private void prepare(ContactFilter filter) {
      int size = ContactCollectionImpl.this.size();
      m_Entries = new ArrayList(size);
      Iterator iter = ContactCollectionImpl.this.m_Contacts.values().iterator();
      for (int i = 0; i < size; i++) {
        Contact ct = (Contact) iter.next();
        if (filter != null) {
          if (filter.passes(ct)) {
            m_Entries.add(ct);
          }
        } else {
          m_Entries.add(ct);
        }
      }
      m_Next = 0;
    }//prepare

    public boolean hasNext() {
      return (m_Next < m_Entries.size());
    }//hasNext

    public Object next() {
      if (m_Next >= m_Entries.size()) {
        throw new NoSuchElementException();
      }
      if (ContactCollectionImpl.this.modified(m_ExpectedModCount)) {
        throw new ConcurrentModificationException();
      }
      Object rv = m_Entries.get(m_Next);
      m_Last = m_Next;
      m_Next++;
      return rv;
    }//next

    public void remove() {
      if (m_Last == -1) {
        throw new IllegalStateException();
      } else {
        //remove
        ContactCollectionImpl.this.remove((Contact) m_Entries.get(m_Last));
        m_Entries.remove(m_Last);
        //reset last, adjust next pointer, and tree size
        m_Last = -1;
        m_Next--;
        m_ExpectedModCount++;
      }
    }//remove

    public int size() {
      return m_Entries.size();
    }//size

    public List getEntries() {
      return m_Entries;
    }//getEntries

  }//inner class ContactIterator

  protected class ContactGroupImpl
      extends AbstractIdentifiable
      implements ContactGroup, Serializable {

    static final long serialVersionUID = 1294968173858663287L;

    //instance attributes
    protected String m_Name;
    protected String m_Description;
    protected List m_ContactIDs;

    public ContactGroupImpl() {
      m_ContactIDs = new ArrayList(10);
    }//constructor

    public String getName() {
      return m_Name;
    }//getName

    public void setName(String name) {
      m_Name = name;
    }//setName

    public String getDescription() {
      return m_Description;
    }//getDescription

    public void setDescription(String description) {
      m_Description = description;
    }//setDescription

    public Contact[] listContacts() {
      ContactIterator iter = (ContactIterator) getContacts();
      Contact[] contacts = new Contact[iter.size()];
      for (int i = 0; i < iter.size(); i++) {
        contacts[i] = (Contact) iter.next();
      }
      return contacts;
    }//listContacts

    public Iterator getContacts() {
      return new ContactIterator(
          new AbstractContactFilter() {
            public boolean passes(Contact contact) {
              return ContactGroupImpl.this.contains(contact.getUID());
            }//passes
          }
      );
    }//getContacts

    public void addContact(Contact contact) {
      if (ContactCollectionImpl.this.contains(contact)) {
        m_ContactIDs.add(contact.getUID());
      } else {
        throw new IllegalArgumentException();
      }
    }//addContact

    public void removeContact(Contact contact) {
      m_ContactIDs.remove(contact.getUID());
    }//removeContact

    public boolean contains(String UID) {
      return m_ContactIDs.contains(UID);
    }//contains

    public boolean contains(Contact contact) {
      return ContactGroupImpl.this.contains(contact.getUID());
    }//contains

    public int size() {
      return m_ContactIDs.size();
    }//size

    /**
     * Remove ID's pointing to contacts which have been removed
     * from the database.
     *
     */
    public void cleanup() {
      Iterator iter = m_ContactIDs.iterator();
      while (iter.hasNext()) {
        if (!ContactCollectionImpl.this.contains((String) iter.next())) {
          iter.remove();
        }
      }
    }//cleanup

    private synchronized void writeObject(java.io.ObjectOutputStream s)
        throws java.io.IOException {

      //clean up id's
      cleanup();

      //write
      s.defaultWriteObject();
    }//writeObject

  }//ContactGroupImpl

}//class ContactCollectionImpl
