/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.io.vcard;

import net.wimpi.pim.contact.model.Contact;
import net.wimpi.pim.util.versitio.versitItem;

/**
 * Interface modeling an <tt>ItemHandler</tt>
 * for vcard related <tt>versitItem</tt>'s.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public interface ItemHandler {

  /**
   * Handle the given item adding information to
   * the given <tt>Contact</tt> instance.
   *
   * @param ct the <tt>Contact</tt> instance.
   * @param item the <tt>versitItem</tt> to be handled.
   */
  public void handleItem(Contact ct, versitItem item);

  /**
   * Creates <tt>versitItem</tt> instances,
   * extracting information from the given
   * <tt>Contact</tt> instance.
   *
   * @param ct the <tt>Contact</tt> instance.
   * @return a <tt>versitItem[]</tt>.
   */
  public versitItem[] createItems(Contact ct);

}//ItemHandler
