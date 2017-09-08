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
import net.wimpi.pim.util.StringUtil;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitToken;

/**
 * Class implementing a handler for the <tt>CLASS</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.7.1 CLASS Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class CLASSItemHandler
    implements ItemHandler {

  public void handleItem(Contact ct, versitItem item) {
    ct.setAccessClassification(item.getDecodedValue());
  }//handleItem

  public versitItem[] createItems(Contact ct) {
    String ac = ct.getAccessClassification();
    if (StringUtil.isValidString(ac)) {
      versitItem[] items = new versitItem[1];
      items[0] = new versitItem(versitToken.CLASS, ac);
      return items;
    } else {
      return new versitItem[0];
    }
  }//createItems

}//CLASSItemHandler
