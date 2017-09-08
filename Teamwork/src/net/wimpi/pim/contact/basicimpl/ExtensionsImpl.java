/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.basicimpl;

import net.wimpi.pim.contact.model.Extension;
import net.wimpi.pim.contact.model.Extensions;

import java.util.*;


/**
 * A basic and simple implementation of the
 * {@link net.wimpi.pim.contact.model.Extensions}.
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class ExtensionsImpl
    implements Extensions {

  static final long serialVersionUID = -2386296030270876018L;

  protected Map m_Extensions;

  public ExtensionsImpl() {
    m_Extensions = Collections.synchronizedMap(new HashMap(10));
  }//constructor

  public String[] listIdentifiers() {
    Set keys = m_Extensions.keySet();
    String[] strs = new String[keys.size()];
    return (String[]) keys.toArray(strs);
  }//listIdentifiers

  public Extension[] list(String xidentifier) {
    if (m_Extensions.containsKey(xidentifier)) {
      return (Extension[]) ((List) m_Extensions.get(xidentifier)).toArray();
    } else {
      return new Extension[0];
    }
  }//list

  public Iterator iterator(String xidentifier) {
    if (m_Extensions.containsKey(xidentifier)) {
      return ((List) m_Extensions.get(xidentifier)).iterator();
    } else {
      //return empty iterator
      return EMPTY_ITERATOR;
    }
  }//get

  public Extension get(String xidentifier) {
    if (m_Extensions.containsKey(xidentifier)) {
      return (Extension) ((List) m_Extensions.get(xidentifier)).get(0);
    } else {
      return null;
    }
  }//get

  public Extension get(String xidentifier, int idx)
      throws IndexOutOfBoundsException {
    if (m_Extensions.containsKey(xidentifier)) {
      return (Extension) ((List) m_Extensions.get(xidentifier)).get(idx);
    } else {
      return null;
    }
  }//get

  public void add(Extension ext) {
    String xidentifier = ext.getIdentifier();
    if (m_Extensions.containsKey(xidentifier)) {
      ((List) m_Extensions.get(xidentifier)).add(ext);
    } else {
      List list = Collections.synchronizedList(new ArrayList(5));
      list.add(ext);
      m_Extensions.put(xidentifier, list);
    }
  }//add

  public void remove(Extension ext) {
    String xidentifier = ext.getIdentifier();
    if (m_Extensions.containsKey(xidentifier)) {
      ((List) m_Extensions.get(xidentifier)).remove(ext);
    }
  }//remove

  public void remove(String xidentifier) {
    m_Extensions.remove(xidentifier);
  }//remove

  public Extension remove(String xidentifier, int idx)
      throws IndexOutOfBoundsException {
    if (m_Extensions.containsKey(xidentifier)) {
      return (Extension) ((List) m_Extensions.get(xidentifier)).remove(idx);
    } else {
      return null;
    }
  }//remove

  public int size() {
    int count = 0;
    for (Iterator iterator = m_Extensions.keySet().iterator(); iterator.hasNext();) {
      count += size(iterator.next().toString());
      ;
    }
    return count;
  }//size

  public int size(String xidentifier) {
    if (m_Extensions.containsKey(xidentifier)) {
      return ((List) m_Extensions.get(xidentifier)).size();
    } else {
      return 0;
    }
  }//size

  private static Iterator EMPTY_ITERATOR = (
      new Iterator() {
        public boolean hasNext() {
          return false;
        }//hasNext

        public Object next() {
          return null;
        }//next

        public void remove() {
          throw new UnsupportedOperationException();
        }//remove

      }
      );

}//class ExtensionsImpl
