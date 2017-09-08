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
import net.wimpi.pim.contact.model.GeographicalInformation;
import net.wimpi.pim.util.StringUtil;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitToken;

/**
 * Class implementing a handler for the <tt>GEO</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.4.2 GEO Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class GEOItemHandler
    implements ItemHandler {

  public void handleItem(Contact ct, versitItem item) {
    GeographicalInformation geo = ct.getGeographicalInformation();
    if (geo == null) {
      geo = Pim.getContactModelFactory().createGeographicalInformation();
      ct.setGeographicalInformation(geo);
    }
    String[] comps = StringUtil.split(item.getDecodedValue(), ",");
    geo.setLatitude(Double.parseDouble(comps[0]));
    geo.setLongitude(Double.parseDouble(comps[1]));
  }//handleItem

  public versitItem[] createItems(Contact ct) {
    GeographicalInformation geo = ct.getGeographicalInformation();
    if (geo == null) {
      return new versitItem[0];
    }
    versitItem[] items = new versitItem[1];
    double longitude = geo.getLongitude();
    double latitude = geo.getLatitude();
    if (longitude == 0 && latitude == 0) {
      return new versitItem[0];
    }
    versitItem item = new versitItem(versitToken.GEO);
    item.setValue(latitude + "," + longitude);
    items[0] = item;
    return items;
  }//createItems

}//GEOItemHandler
