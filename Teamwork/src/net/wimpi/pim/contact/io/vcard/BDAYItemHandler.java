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

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Class implementing a handler for the <tt>BDAY</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.1.5 BDAY Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class BDAYItemHandler
    implements ItemHandler {

  //instance attributes
  private DateFormat m_SimpleDateFormat;
  private DateFormat m_ExtendedDateFormat;

  public BDAYItemHandler() {
    m_SimpleDateFormat = new SimpleDateFormat("yyyyMMdd");
    m_ExtendedDateFormat = new SimpleDateFormat("yyyy-MM-dd");
  }//constructor

  public void handleItem(Contact ct, versitItem item) {
    String datestr = item.getDecodedValue();
    Date bday = null;
    //handle date only i.e. splitting off time
    datestr = (StringUtil.split(datestr, "T"))[0];
    try {
      if (datestr.indexOf("-") == -1) {
        //simple format
        bday = m_SimpleDateFormat.parse(datestr);
      } else {
        //extended format
        bday = m_ExtendedDateFormat.parse(datestr);
      }
    } catch (ParseException pex) {
      //handle
    }
    PersonalIdentity pid = ct.getPersonalIdentity();
    //ensure it exists
    if (pid == null) {
      pid = Pim.getContactModelFactory().createPersonalIdentity();
      ct.setPersonalIdentity(pid);
    }
    pid.setBirthDate(bday);
  }//handleItem

  public versitItem[] createItems(Contact ct) {
    PersonalIdentity pid = ct.getPersonalIdentity();
    if (pid == null) {
      return new versitItem[0];
    } else {
      versitItem[] items = new versitItem[1];
      Date bday = pid.getBirthDate();
      if (bday == null) {
        return new versitItem[0];
      }
      versitItem item = new versitItem(versitToken.BDAY);
      //using extended format v3
      item.setValue(m_ExtendedDateFormat.format(bday));
      items[0] = item;
      return items;
    }
  }//createItems

}//BDAYItemHandler
