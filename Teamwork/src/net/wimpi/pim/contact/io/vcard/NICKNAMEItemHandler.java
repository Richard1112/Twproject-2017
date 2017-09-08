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
import net.wimpi.pim.util.StringUtil;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitToken;

/**
 * Class implementing a handler for the <tt>NICKNAME</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.1.3 NICKNAME Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class NICKNAMEItemHandler
    implements ItemHandler {

  public void handleItem(Contact ct, versitItem item) {
    PersonalIdentity pid = ct.getPersonalIdentity();
    if (pid == null) {
      pid = Pim.getContactModelFactory().createPersonalIdentity();
      ct.setPersonalIdentity(pid);
    }
    String[] multi = StringUtil.split(item.getDecodedValue(), ",");
    for (int n = 0; n < multi.length; n++) {
      pid.addNickname(multi[n]);
    }
  }//handleItem

  public versitItem[] createItems(Contact ct) {
    if (!ct.hasPersonalIdentity()) {
      return new versitItem[0];
    }
    versitItem[] items = new versitItem[1];
    String[] nicknames = ct.getPersonalIdentity().listNicknames();
    if (StringUtil.isValidStringArray(nicknames)) {
      versitItem item = new versitItem(versitToken.NICKNAME);
      item.setValue(StringUtil.joinList(nicknames));
      items[0] = item;
      return items;
    } else {
      return new versitItem[0];
    }

  }//createItems

}//NICKNAMEItemHandler
