/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.util;

import java.io.Serializable;

/**
 * Abstract class implementing the
 * <tt>Identifiable</tt> interface.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public abstract class AbstractIdentifiable
    implements Identifiable, Serializable {

  static final long serialVersionUID = -4619681291249212932L;

  //instance attributes
  private String m_UID;

  public AbstractIdentifiable() {
    //ensure a unique identifier to be set
    setUID("");
  }//constructor

  public String getUID() {
    return m_UID;
  }//getUID

  /**
   * Sets the unique identifier.
   *
   * @param uid the unique identifer as <tt>String</tt>.
   */
  public void setUID(String uid) {
    if (uid == null || uid.length() == 0 || uid.indexOf(UID_PREFIX) == -1) {
      m_UID = UID_PREFIX + UIDGenerator.getUID();
    } else {
      m_UID = uid;
    }
  }//setUID

  public boolean equals(Object o) {
    if (o == null) return false;
    String oid = "";
    if (o instanceof Identifiable) {
      oid = ((Identifiable) o).getUID();
    } else if (o instanceof String) {
      oid = (String) o;
    } else {
      oid = o.toString();
    }
    return m_UID.equals(oid);
  }//equals

  private static final String UID_PREFIX = "jpim-";

}//interface Identifiable
