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
 * Class implementing a handler for the <tt>N</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.1.2 N Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class NItemHandler
    implements ItemHandler {

  public void handleItem(Contact ct, versitItem item) {
    PersonalIdentity pid = ct.getPersonalIdentity();
    if (pid == null) {
      pid = Pim.getContactModelFactory().createPersonalIdentity();
      ct.setPersonalIdentity(pid);
    }
    String[] names = StringUtil.split(item.getDecodedValue(), ";");
    String[] multi = null;
    for (int i = 0; i < names.length; i++) {
      switch (i) {
        case 0:
          pid.setLastname(names[0]);
          break;
        case 1:
          pid.setFirstname(names[1]);
          break;
        case 2:
          multi = StringUtil.split(names[2], ",");
          for (int n = 0; n < multi.length; n++) {
            pid.addAdditionalName(multi[n]);
          }
          break;
        case 3:
          multi = StringUtil.split(names[3], ",");
          for (int n = 0; n < multi.length; n++) {
            pid.addPrefix(multi[n]);
          }
          break;
        case 4:
          multi = StringUtil.split(names[4], ",");
          for (int n = 0; n < multi.length; n++) {
            pid.addSuffix(multi[n]);
          }
          break;
      }
    }
  }//handleItem

  public versitItem[] createItems(Contact ct) {
    if (!ct.hasPersonalIdentity()) {
      return new versitItem[0];
    }
    versitItem[] items = new versitItem[1];
    //Extract data for item
    PersonalIdentity pid = ct.getPersonalIdentity();
    String firstname = pid.getFirstname();
    String lastname = pid.getLastname();
    String[] addnames = pid.listAdditionalNames();
    String[] suffixes = pid.listSuffixes();
    String[] prefixes = pid.listPrefixes();

    //check validity (should at least have first and lastname)
    if (pid == null
        || !StringUtil.isValidString(firstname)
        || !StringUtil.isValidString(lastname)) {

      return new versitItem[0];
    }
    //create & fill item
    versitItem item = new versitItem(versitToken.N);
    StringBuffer sbuf = new StringBuffer();
    sbuf.append(lastname).append(";");
    sbuf.append(firstname).append(";");

    if (StringUtil.isValidStringArray(addnames)) {
      sbuf.append(StringUtil.joinList(
          addnames)
      );
    }
    //the semi-colon has to be added anyway
    sbuf.append(";");

    if (StringUtil.isValidStringArray(prefixes)) {
      sbuf.append(StringUtil.joinList(
          pid.listPrefixes())
      );
    }
    //the semi-colon has to be added anyway
    sbuf.append(";");

    if (StringUtil.isValidStringArray(suffixes)) {
      sbuf.append(StringUtil.joinList(
          pid.listSuffixes())
      );
    }

    item.setValue(sbuf.toString());
    items[0] = item;
    return items;
  }//createItems

}//NItemHandler
