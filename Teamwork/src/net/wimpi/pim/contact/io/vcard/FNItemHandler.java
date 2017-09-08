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
import net.wimpi.pim.contact.model.Contact;
import net.wimpi.pim.contact.model.PersonalIdentity;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitToken;

/**
 * Class implementing a handler for the <tt>FN</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.1.1 FN Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class FNItemHandler
    implements ItemHandler {

  public void handleItem(Contact ct, versitItem item) {
    PersonalIdentity pid = ct.getPersonalIdentity();
    if (pid == null) {
      pid = Pim.getContactModelFactory().createPersonalIdentity();
      ct.setPersonalIdentity(pid);
    }
    pid.setFormattedName(item.getDecodedValue());
  }//handleItem

  public versitItem[] createItems(Contact ct) {
    //FIXME: vcard requires FN attribute
    if (!ct.hasPersonalIdentity()) {
      return new versitItem[0];
    }
    versitItem[] items = new versitItem[1];
    String fname = ct.getPersonalIdentity().getFormattedName();
    if (fname != null && fname.length() != 0) {
      items[0] = new versitItem(versitToken.FN, fname);
      return items;
    } else {
      return new versitItem[0];
    }
  }//createItem

}//FNItemHandler
