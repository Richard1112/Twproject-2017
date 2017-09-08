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

import java.util.SimpleTimeZone;
import java.util.TimeZone;

/**
 * Class implementing a handler for the <tt>TZ</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.4.1 TZ Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class TZItemHandler
    implements ItemHandler {

  public void handleItem(Contact ct, versitItem item) {
    GeographicalInformation geoinfo = ct.getGeographicalInformation();
    if (geoinfo == null) {
      geoinfo = Pim.getContactModelFactory().createGeographicalInformation();
      ct.setGeographicalInformation(geoinfo);
    }
    String t = item.getDecodedValue();

    if (!StringUtil.isValidString(t)) {
      return;
    }
    //System.out.println("DEBUG::"+t);
    String[] tz = processTZ(t);
    geoinfo.setTimeZone(
        new SimpleTimeZone(
            getOffset(tz),
            getStdID(tz)
        )
    );
  }//handleItem

  public versitItem[] createItems(Contact ct) {
    GeographicalInformation geo = ct.getGeographicalInformation();
    if (geo == null) {
      return new versitItem[0];
    }

    TimeZone timez = geo.getTimeZone();
    if (timez == null) {
      return new versitItem[0];
    }
    //Workaround for formatting to GMT offset
    String tzid = timez.getID();
    timez.setID("none");
    String tz = timez.getDisplayName();
    timez.setID(tzid);

    if (!StringUtil.isValidString(tz)) {
      return new versitItem[0];
    }
    versitItem[] items = new versitItem[1];
    versitItem item = new versitItem(versitToken.TZ);
    //GMT is not necessary
    item.setValue(tz.substring(3, tz.length()));
    items[0] = item;
    return items;
  }//createItems

  private int getOffset(String[] tz) {
    int offset = 0;
    try {

      //hours as mins
      offset = 60 * Integer.parseInt(tz[1]);
      //add minutes
      offset += Integer.parseInt(tz[2]);
      //correct east(-) or west(+) from GMT
      if (tz[0].equals("-")) {
        offset = (-1 * offset);
      }


    } catch (NumberFormatException numex) {
      return -1;
    }
    //calc millis from minutes
    offset = offset * 60 * 1000;
    return offset;
  }//getOffset

  private String getStdID(String[] tz) {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append(tz[0]);
    sbuf.append(tz[1]);
    if (tz[2] != null && tz[2].length() > 0) {
      sbuf.append(":").append(tz[2]);
    }
    return sbuf.toString();
  }//getStdID

  private String[] processTZ(String value) {
    String[] retval = new String[3];
    //plus or minus
    retval[0] = value.substring(0, 1);
    //check for first colon
    int colon = value.indexOf(':');
    if (colon > 0) {
      //has colon, cut off everything more then 6
      if (value.length() > 6) {
        value = value.substring(0, 6);
      }
      //place hours
      retval[1] = value.substring(1, 3);
      //place mins
      retval[2] = value.substring(4, 6);
    } else {
      //split off anything that is larger then 5
      if (value.length() > 5) {
        value = value.substring(0, 6);
      }
      //place hours
      retval[1] = value.substring(1, 3);
      if (value.length() > 3) {
        //place mins
        retval[2] = value.substring(3, 5);
      } else {
        retval[2] = "";
      }
    }
    return retval;
  }//processTZ


}//TZItemHandler
