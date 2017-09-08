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
import net.wimpi.pim.contact.io.ContactMarshaller;
import net.wimpi.pim.contact.model.Contact;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitToken;
import net.wimpi.pim.util.versitio.versitWriter;

import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * Class implementing a <tt>ContactMarshaller</tt>
 * for the vCard Mime Directory format
 * specification (see RFC 2426).
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class vCardMarshaller
    implements ContactMarshaller {

  private String m_Encoding = null;
  private versitItem m_ENDITEM =
      new versitItem(versitToken.END, versitToken.VCARD);
  private versitItem m_STARTITEM =
      new versitItem(versitToken.START, versitToken.VCARD);
  private versitItem m_V3ITEM =
      new versitItem(versitToken.VERSION, versitToken.VCARD_VERSION_3);
  private versitItem m_PRODIDITEM =
      new versitItem(versitToken.PRODID, Pim.PRODUCT_ID);

  private ItemHandlerManager m_ItemHandlerManager = ItemHandlerManager.getReference();

  public vCardMarshaller() {
  }//vCardUnmarshaller

  public vCardMarshaller(String enc) {
    m_Encoding = enc;
  }//constructor

  public String getEncoding() {
    return m_Encoding;
  }//getEncoding

  public void setEncoding(String encoding) {
    m_Encoding = encoding;
  }

  public void marshallContact(OutputStream out, Contact contact) {
    Contact[] contacts = new Contact[1];
    contacts[0] = contact;
    marshallContacts(out, contacts);
  }//marshallContact

  public void marshallContacts(OutputStream out, Contact[] contacts) {
    try {
      for (int i = 0; i < contacts.length; i++) {
        marshallCard(out, contacts[i]);
      }
    } catch (Exception ex) {
      ex.printStackTrace();
    }
  }//marshallContacts

  private void marshallCard(OutputStream out, Contact contact) {
    ArrayList list = new ArrayList(20);
    //add begin
    list.add(m_STARTITEM);
    //add version
    list.add(m_V3ITEM);
    //add all through
    for (int i = 0; i < versitToken.VCARD_ITEM_LIST.length; i++) {
      addItem(list, versitToken.VCARD_ITEM_LIST[i], contact);
    }
    //add extensions
    String[] xtnd = m_ItemHandlerManager.listExtensions();
    for (int i = 0; i < xtnd.length; i++) {
      addItem(list, xtnd[i], contact);
    }
    //add PRODID if announced
    if (Pim.isAnnounced()) {
      list.add(m_PRODIDITEM);
    }
    //add end
    list.add(m_ENDITEM);
    try {
      versitWriter writer = null;
      if (m_Encoding != null) {
        writer = new versitWriter(out, m_Encoding);
      } else {
        writer = new versitWriter(out);
      }
      writer.writeCard(list);

    } catch (Exception ex) {
      ex.printStackTrace();
    }
  }//processCard

  private void addItem(List list, String str, Contact ct) {
    //DEBUG:System.out.println("addItem():str="+str);
    ItemHandler handler = m_ItemHandlerManager.getItemHandler(str);
    if (handler != null) {
      versitItem[] items = handler.createItems(ct);
      for (int i = 0; i < items.length; i++) {
        //DEBUG:System.out.println("addItem():i="+i);
        list.add(items[i]);
      }
    }
  }//addItem

}//vCardMarshaller
