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
import net.wimpi.pim.contact.model.Organization;
import net.wimpi.pim.contact.model.OrganizationalIdentity;
import net.wimpi.pim.util.StringUtil;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitToken;

/**
 * Class implementing a handler for the <tt>ORG</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.5.5 ORG Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class ORGItemHandler
    implements ItemHandler {

  public void handleItem(Contact ct, versitItem item) {
    OrganizationalIdentity orgid = ct.getOrganizationalIdentity();
    if (orgid == null) {
      orgid = Pim.getContactModelFactory().createOrganizationalIdentity();
      ct.setOrganizationalIdentity(orgid);
    }
    Organization org = orgid.getOrganization();
    if (org == null) {
      org = Pim.getContactModelFactory().createOrganization();
      orgid.setOrganization(org);
    }
    String[] components = StringUtil.split(item.getDecodedValue(), ";");
    for (int i = 0; i < components.length; i++) {
      switch (i) {
        case 0:
          org.setName(components[0]);
          break;
        default:
          if (components[i].length() > 0) {
            org.addUnit(components[i]);
          }
      }
    }
  }//handleItem

  public versitItem[] createItems(Contact ct) {
    OrganizationalIdentity orgid = ct.getOrganizationalIdentity();
    if (orgid == null) {
      return new versitItem[0];
    }
    versitItem[] items = new versitItem[1];
    Organization org = orgid.getOrganization();
    if (org == null) {
      return new versitItem[0];
    }
    String name = org.getName();
    String[] units = org.listUnits();

    if (!StringUtil.isValidString(name)) {
      return new versitItem[0];
    }
    versitItem item = new versitItem(versitToken.ORG);

    StringBuffer sbuf = new StringBuffer();
    sbuf.append(org.getName()).append(";");
    if (StringUtil.isValidStringArray(units)) {
      for (int i = 0; i < units.length; i++) {
        sbuf.append(units[i]);
        if (i < units.length - 1) {
          sbuf.append(";");
        }
      }
    } else {
      sbuf.append(";");
    }
    item.setValue(sbuf.toString());
    items[0] = item;
    return items;
  }//createItems

}//ORGItemHandler
