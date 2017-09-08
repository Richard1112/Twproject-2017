/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.io.vcard;

import net.wimpi.pim.contact.model.Address;
import net.wimpi.pim.contact.model.Contact;
import net.wimpi.pim.util.EncodingUtility;
import net.wimpi.pim.util.StringUtil;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitToken;

import java.util.Iterator;

/**
 * Class implementing a handler for the <tt>LABEL</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.2.2 LABEL Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class LABELItemHandler
    implements ItemHandler {

  public void handleItem(Contact ct, versitItem item) {
    //FIXME: based on assumption that the Label
    //corresponds to the last address added.
    String label = item.getDecodedValue();
    Address addr = ct.getLastAddedAddress();
    if (addr == null) {
      return;
    } else {
      addr.setLabel(label);
    }
  }//handleItem

  public versitItem[] createItems(Contact ct) {

    versitItem[] items = new versitItem[ct.getAddressCount()];
    int i = 0;
    for (Iterator iter = ct.getAddresses(); iter.hasNext(); i++) {
      Address adr = (Address) iter.next();

      if (adr == null) {
        return new versitItem[0];
      }
      String label = adr.getLabel();
      if (!StringUtil.isValidString(label)) {
        return new versitItem[0];
      }

      versitItem item = new versitItem(versitToken.LABEL);
      //set parameters, belongs to address anyway ;)
      if (adr.isDomestic()) {
        item.addParameter(versitToken.TYPE, versitToken.TYPE_DOM);
      }
      if (adr.isInternational()) {
        item.addParameter(versitToken.TYPE, versitToken.TYPE_INTL);
      }
      if (adr.isPostal()) {
        item.addParameter(versitToken.TYPE, versitToken.TYPE_POSTAL);
      }
      if (adr.isParcel()) {
        item.addParameter(versitToken.TYPE, versitToken.TYPE_PARCEL);
      }
      if (adr.isHome()) {
        item.addParameter(versitToken.TYPE, versitToken.TYPE_HOME);
      }
      if (adr.isWork()) {
        item.addParameter(versitToken.TYPE, versitToken.TYPE_WORK);
      }
      if (ct.isPreferredAddress(adr)) {
        item.addParameter(versitToken.TYPE, versitToken.TYPE_PREF);
      }

      item.setValue(new String(
          EncodingUtility.encodeLineBreaks(label.getBytes())));
      items[i] = item;
    }
    return items;
  }//createItems

}//LABELItemHandler
