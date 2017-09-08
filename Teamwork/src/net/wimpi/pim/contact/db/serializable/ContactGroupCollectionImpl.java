/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.db.serializable;

import net.wimpi.pim.contact.db.ContactGroup;
import net.wimpi.pim.contact.db.ContactGroupCollection;
import net.wimpi.pim.contact.db.ContactGroupFilter;

import java.io.Serializable;
import java.util.*;

/**
 * Class representing a serializable <tt>ContactGroupCollection</tt>.
 * implementation.
 *
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class ContactGroupCollectionImpl
    implements ContactGroupCollection, Serializable {

  static final long serialVersionUID = 7926133701871079538L;

  //instance attributes
  protected Map m_Groups;
  protected Map m_NameIndex;
  transient protected int m_ModCount = 0;

  public ContactGroupCollectionImpl() {
    m_Groups = new HashMap(50);
    m_NameIndex = new HashMap(50);
  }//constructor

  public boolean contains(String UID) {
    return m_Groups.keySet().contains(UID);
  }//contains

  public boolean contains(ContactGroup group) {
    return contains(group.getUID());
  }//contains

  public boolean containsByName(String name) {
    return m_NameIndex.keySet().contains(name);
  }//containsByName

  public ContactGroup get(String UID) {
    return (ContactGroup) m_Groups.get(UID);
  }//get

  public ContactGroup getByName(String name) {
    return get((String) m_NameIndex.get(name));
  }//getByName

  public ContactGroup[] toArray() {
    ContactGroup[] groups = new ContactGroup[m_Groups.size()];
    Iterator iter = iterator();
    for (int i = 0; i < m_Groups.size(); i++) {
      groups[i] = (ContactGroup) iter.next();
    }
    return groups;
  }//toArray

  public ContactGroup[] toArray(ContactGroupFilter filter) {
    GroupIterator iter = new GroupIterator(filter);
    ContactGroup[] groups = new ContactGroup[iter.size()];
    for (int i = 0; i < iter.size(); i++) {
      groups[i] = (ContactGroup) iter.next();
    }
    return groups;
  }//toArray

  public boolean add(ContactGroup group) {
    if (containsByName(group.getName())) {
      return false;
    } else {
      //add to collection
      int size = size();
      m_Groups.put(group.getUID(), group);
      addToNameIndex(group);
      if (size != size()) {
        if (m_ModCount == Integer.MAX_VALUE) {
          m_ModCount = 0;
        } else {
          m_ModCount++;
        }
      }
      return true;
    }
  }//add

  public ContactGroup remove(String UID) {
    ContactGroup group = (ContactGroup) m_Groups.remove(UID);
    if (group != null) {
      removeFromNameIndex(group);
      if (m_ModCount == Integer.MAX_VALUE) {
        m_ModCount = 0;
      } else {
        m_ModCount++;
      }
    }
    return group;
  }//remove

  public void remove(ContactGroup group) {
    remove(group.getUID());
  }//remove

  public Iterator iterator() {
    return new GroupIterator();
  }//iterator

  public Iterator iterator(ContactGroupFilter filter) {
    return new GroupIterator(filter);
  }//iterator

  public int size() {
    return m_Groups.size();
  }//size

  private void addToNameIndex(ContactGroup group) {
    if (group == null) {
      return;
    }
    String name = group.getName();
    if (name != null && name.length() > 0) {
      //add to index
      m_NameIndex.put(name, group.getUID());
    }
  }//addToNameIndex

  private void removeFromNameIndex(ContactGroup group) {
    if (group == null) {
      return;
    }
    String name = group.getName();
    if (name != null && name.length() > 0) {
      //add to index
      m_NameIndex.remove(name);
    }
  }//removeFromNameIndex

  private boolean modified(int modcount) {
    return (modcount != m_ModCount);
  }//modified

  class GroupIterator
      implements Iterator {

    private ArrayList m_Entries;
    private int m_Next;
    private int m_Last = -1;
    private int m_ExpectedModCount = ContactGroupCollectionImpl.this.m_ModCount;

    public GroupIterator() {
      prepare(null);
    }//constructor

    public GroupIterator(ContactGroupFilter filter) {
      prepare(filter);
    }//constructor

    private void prepare(ContactGroupFilter filter) {
      int size = ContactGroupCollectionImpl.this.size();
      m_Entries = new ArrayList(size);
      Iterator iter = ContactGroupCollectionImpl.this.m_Groups.values().iterator();
      for (int i = 0; i < size; i++) {
        ContactGroup group = (ContactGroup) iter.next();
        if (filter != null) {
          if (filter.passes(group)) {
            m_Entries.add(group);
          }
        } else {
          m_Entries.add(group);
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
      if (ContactGroupCollectionImpl.this.modified(m_ExpectedModCount)) {
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
        ContactGroupCollectionImpl.this.remove((ContactGroup) m_Entries.get(m_Last));
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

  }//inner class GroupIterator

}//class ContactGroupCollection
