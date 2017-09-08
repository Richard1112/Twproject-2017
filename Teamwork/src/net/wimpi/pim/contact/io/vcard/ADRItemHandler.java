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
import net.wimpi.pim.contact.model.Address;
import net.wimpi.pim.contact.model.Contact;
import net.wimpi.pim.util.EncodingUtility;
import net.wimpi.pim.util.StringUtil;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitToken;

import java.util.Iterator;

/**
 * Class implementing a handler for the <tt>ADR</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.2.1 ADR Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class ADRItemHandler
    implements ItemHandler {

  public void handleItem(Contact ct, versitItem item) {

    boolean preferred = false;
    String[] components = StringUtil.split(item.getDecodedValue(), ";");
    //System.out.println("DEBUG::"+StringUtil.joinList(components));
    //String[] multi=null;
    Address adr = Pim.getContactModelFactory().createAddress();

    //1. handle data
    for (int i = 0; i < components.length; i++) {
      switch (i) {
        case 0:
          adr.setPostBox(components[0]);
          break;
        case 1:
          adr.setExtended(components[1]);
          break;
        case 2:
          adr.setStreet(components[2]);
          break;
        case 3:
          adr.setCity(components[3]);
          break;
        case 4:
          adr.setRegion(components[4]);
          break;
        case 5:
          adr.setPostalCode(components[5]);
          break;
        case 6:
          adr.setCountry(components[6]);
          break;
      }
    }

    //2. handle params type or default!
    components = item.getParameter(versitToken.TYPE);
    if (components == null) {
      components = item.getParameter(versitToken.DEFAULT);
    }

    if (components != null) {
      for (int i = 0; i < components.length; i++) {
        String type = components[i].toUpperCase();
        if (type.equals(versitToken.TYPE_DOM)) {
          adr.setDomestic(true);
        } else if (type.equals(versitToken.TYPE_INTL)) {
          adr.setInternational(true);
        } else if (type.equals(versitToken.TYPE_POSTAL)) {
          adr.setPostal(true);
        } else if (type.equals(versitToken.TYPE_PARCEL)) {
          adr.setParcel(true);
        } else if (type.equals(versitToken.TYPE_HOME)) {
          adr.setHome(true);
        } else if (type.equals(versitToken.TYPE_WORK)) {
          adr.setWork(true);
        } else if (type.equals(versitToken.TYPE_PREF)) {
          preferred = true;
        }
      }
    } else {
      //set default as per standard
      adr.setInternational(true);
      adr.setPostal(true);
      adr.setParcel(true);
      adr.setWork(true);
    }
    ct.addAddress(adr);
    if (preferred) {
      ct.setPreferredAddress(adr);
    }
  }//handleItem

  public versitItem[] createItems(Contact ct) {

    versitItem[] items = new versitItem[ct.getAddressCount()];
    int i = 0;
    for (Iterator iter = ct.getAddresses(); iter.hasNext(); i++) {
      Address adr = (Address) iter.next();
      if (adr == null) {
        return null;
      }
      String pobox = adr.getPostBox();
      String ext = adr.getExtended();
      String street = adr.getStreet();
      String region = adr.getRegion();
      String city = adr.getCity();
      String zip = adr.getPostalCode();
      String country = adr.getCountry();

      StringBuffer sbuf = new StringBuffer();
      versitItem item = new versitItem(versitToken.ADR);

      //set parameters
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

      if (StringUtil.isValidString(pobox)) {
        sbuf.append(pobox);
      }
      sbuf.append(";");

      if (StringUtil.isValidString(ext)) {
        sbuf.append(EncodingUtility.encodeLineBreaks(ext));
      }
      sbuf.append(";");

      if (StringUtil.isValidString(street)) {
        sbuf.append(EncodingUtility.encodeLineBreaks(street));
      }
      sbuf.append(";");

      if (StringUtil.isValidString(city)) {
        sbuf.append(city);
      }
      sbuf.append(";");

      if (StringUtil.isValidString(region)) {
        sbuf.append(region);
      }
      sbuf.append(";");

      if (StringUtil.isValidString(zip)) {
        sbuf.append(zip);
      }
      sbuf.append(";");

      if (StringUtil.isValidString(country)) {
        sbuf.append(country);
      }

      item.setValue(sbuf.toString());
      items[i] = item;
    }
    return items;
  }//createItem

}//ADRItemHandler
