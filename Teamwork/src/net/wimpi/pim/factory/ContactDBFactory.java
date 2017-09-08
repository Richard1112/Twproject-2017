/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.factory;

import net.wimpi.pim.contact.db.ContactDatabase;

/**
 * Interface for a <tt>ContactDBFactory</tt>, which
 * can be used to instantiate contact database
 * instances.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public interface ContactDBFactory {

  /**
   * Returns a new <tt>ContactDatabase</tt> instance.
   *
   * @return the newly created <tt>ContactDatabase</tt> instance.
   */
  public ContactDatabase createContactDatabase();

}//interface ContactIOFactory
