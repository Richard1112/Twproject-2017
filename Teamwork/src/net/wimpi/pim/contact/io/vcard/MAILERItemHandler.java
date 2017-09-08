/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.io.vcard;

import net.wimpi.pim.Pim;
import net.wimpi.pim.contact.model.Communications;
import net.wimpi.pim.contact.model.Contact;
import net.wimpi.pim.util.StringUtil;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitToken;

/**
 * Class implementing a handler for the <tt>LOGO</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.3.3 MAILER Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class MAILERItemHandler
    implements ItemHandler {

  public void handleItem(Contact ct, versitItem item) {
    Communications comm = ct.getCommunications();
    if (comm == null) {
      comm = Pim.getContactModelFactory().createCommunications();
      ct.setCommunications(comm);
    }
    comm.setMailer(item.getDecodedValue());
  }//handleItem

  public versitItem[] createItems(Contact ct) {
    if (!ct.hasCommunications()) {
      return new versitItem[0];
    }
    versitItem[] items = new versitItem[1];
    String mailer = ct.getCommunications().getMailer();
    if (!StringUtil.isValidString(mailer)) {
      return new versitItem[0];
    }
    versitItem item = new versitItem(versitToken.MAILER);
    item.setValue(mailer);
    items[0] = item;
    return items;
  }//createItems


}//MAILERItemHandler
