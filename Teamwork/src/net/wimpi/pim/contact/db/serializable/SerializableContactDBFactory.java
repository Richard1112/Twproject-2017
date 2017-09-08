/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.db.serializable;

import net.wimpi.pim.contact.db.ContactDatabase;
import net.wimpi.pim.factory.ContactDBFactory;

/**
 * Class representing a <tt>ContactDBFactory</tt>, which
 * can be used to instantiate serializable <tt>ContactDatabase</tt>
 * instances.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class SerializableContactDBFactory
    implements ContactDBFactory {

  public ContactDatabase createContactDatabase() {
    return new ContactDatabaseImpl();
  }//createContactDatabase

}//class SerializedContactDBFactory
