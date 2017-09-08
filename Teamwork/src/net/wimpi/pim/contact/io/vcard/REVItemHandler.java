/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.io.vcard;

import net.wimpi.pim.contact.model.Contact;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitToken;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Class implementing a handler for the <tt>REV</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.6.4 REV Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class REVItemHandler
    implements ItemHandler {

  private DateFormat m_SimpleDateFormat;
  private DateFormat m_ExtendedDateFormat;
  private DateFormat m_SimpleDateTimeFormat;
  private DateFormat m_ExtendedDateTimeFormat;

  public REVItemHandler() {
    m_SimpleDateFormat = new SimpleDateFormat("yyyyMMdd");
    m_ExtendedDateFormat = new SimpleDateFormat("yyyy-MM-dd");
    m_SimpleDateTimeFormat = new SimpleDateFormat("yyyyMMdd'T'HHmmss");
    //System.out.println(m_SimpleDateTimeFormat.format(new Date()));
    m_ExtendedDateTimeFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
  }//constructor

  public void handleItem(Contact ct, versitItem item) {
    String datestr = item.getDecodedValue();
    int pos = datestr.indexOf("Z");
    if (pos != -1) {
      //cut off that thingy
      datestr = datestr.substring(0, pos);
    }
    //System.out.println("DEBUG::"+datestr);
    Date rev = null;

    try {

      if (datestr.indexOf("T") == -1) {
        if (datestr.indexOf("-") == -1) {
          //simple format
          rev = m_SimpleDateFormat.parse(datestr);
        } else {
          //extended format
          rev = m_ExtendedDateFormat.parse(datestr);
        }
      } else {
        if (datestr.indexOf("-") == -1) {
          //simple format
          rev = m_SimpleDateTimeFormat.parse(datestr);
        } else {
          //extended format
          rev = m_ExtendedDateTimeFormat.parse(datestr);
        }

      }
    } catch (ParseException pex) {
      //handle
      //pex.printStackTrace();
    }
    ct.setCurrentRevisionDate(rev);
  }//handleItem

  public versitItem[] createItems(Contact ct) {
    Date revdate = ct.getCurrentRevisionDate();
    if (revdate == null) {
      return new versitItem[0];
    }
    versitItem[] items = new versitItem[1];
    versitItem item = new versitItem(versitToken.REV);
    item.setValue(m_ExtendedDateTimeFormat.format(revdate));
    items[0] = item;
    return items;
  }//createItem

}//REVItemHandler
