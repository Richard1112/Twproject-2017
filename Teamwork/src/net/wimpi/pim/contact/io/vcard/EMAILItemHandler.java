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
import net.wimpi.pim.contact.model.EmailAddress;
import net.wimpi.pim.util.StringUtil;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitToken;

import java.util.Iterator;

/**
 * Class implementing a handler for the <tt>EMAIL</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.3.2 EMAIL Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class EMAILItemHandler
    implements ItemHandler {

  public void handleItem(Contact ct, versitItem item) {
    boolean preferred = false;
    EmailAddress email = Pim.getContactModelFactory().createEmailAddress();
    //1.handle data
    email.setAddress(item.getDecodedValue());
    //2.handle params
    String[] components = item.getParameter(versitToken.TYPE);
    if (components == null) {
      components = item.getParameter(versitToken.DEFAULT);
    }
    if (components != null && components.length > 0) {
      for (int i = 0; i < components.length; i++) {
        if (components[i].equals(versitToken.TYPE_PREF)) {
          preferred = true;
        } else {
          //overwrites, but just the type should come in
          email.setType(components[i]);
        }
      }
    } else {
      //set default
      email.setType(versitToken.TYPE_INTERNET);
    }
    Communications comm = ct.getCommunications();
    if (comm == null) {
      comm = Pim.getContactModelFactory().createCommunications();
      ct.setCommunications(comm);
    }
    comm.addEmailAddress(email);
    if (preferred) {
      comm.setPreferredEmailAddress(email);
    }
  }//handleItem

  public versitItem[] createItems(Contact ct) {


    if (!ct.hasCommunications()) {
      return new versitItem[0];
    }

    Communications comm = ct.getCommunications();
    versitItem[] items = new versitItem[comm.getEmailAddressCount()];
    int i = 0;
    for (Iterator iter = comm.getEmailAddresses(); iter.hasNext(); i++) {
      EmailAddress email = (EmailAddress) iter.next();
      String address = email.getAddress();
      String type = email.getType();
      if (!StringUtil.isValidString(address)) {
        return null;
      }
      versitItem item = new versitItem(versitToken.EMAIL);

      //1. handle value
      item.setValue(address);
      //2. handle params
      if (comm.isPreferredEmailAddress(email)) {
        item.addParameter(versitToken.TYPE, versitToken.TYPE_PREF);
      }
      if (StringUtil.isValidString(type)) {
        item.addParameter(versitToken.TYPE, type);
      }
      items[i] = item;
    }
    return items;
  }//createItems

}//EMAILItemHandler
