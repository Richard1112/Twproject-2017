/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.io.vcard;

import net.wimpi.pim.contact.basicimpl.ContactImpl;
import net.wimpi.pim.contact.io.ContactUnmarshaller;
import net.wimpi.pim.contact.model.Contact;
import net.wimpi.pim.util.versitio.versitException;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitParser;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.Iterator;
import java.util.List;

/**
 * Class implementing a <tt>ContactUnmarshaller</tt>
 * for the vCard Mime Directory format
 * specification (see RFC 2426).
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class vCardUnmarshaller
    implements ContactUnmarshaller {

  private String m_Encoding = null;
  private ItemHandlerManager m_ItemHandlerManager = ItemHandlerManager.getReference();
  private boolean m_Strict = true;

  public vCardUnmarshaller() {
  }//vCardUnmarshaller

  public vCardUnmarshaller(String enc) {
    m_Encoding = enc;
  }//vCardUnmarshaller

  public String getEncoding() {
    return m_Encoding;
  }//getEncoding

  public void setEncoding(String encoding) {
    m_Encoding = encoding;
  }//setEncoding

  public Contact unmarshallContact(InputStream in) {
    return unmarshallContacts(in)[0];
  }//unmarshallContact

  public Contact[] unmarshallContacts(InputStream in) {
    Contact[] contacts = null;
    try {
      List cards = parseStream(in);
      contacts = new Contact[cards.size()];
      int i = 0;
      for (Iterator iter = cards.iterator(); iter.hasNext(); i++) {
        List card = (List) iter.next();
        contacts[i] = processCard(card);
      }
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    return contacts;
  }//unmarshallContacts

  public void setStrict(boolean b) {
    m_Strict = b;
  }//setStrict

  public boolean isStrict() {
    return m_Strict;
  }//isStrict

  private List parseStream(InputStream in)
      throws versitException {
    versitParser parser = null;
    if (m_Encoding == null) {
      parser = new versitParser(in);
    } else {
      try {
        parser = new versitParser(in, m_Encoding);
        parser.setValidating(m_Strict);
      } catch (UnsupportedEncodingException ex) {
        throw new versitException("Encoding " + m_Encoding + "not supported.");
      }
    }
    parser.parse();
    //System.out.println("DEBUG:parS #"+parser.getCardObjects().size());
    return parser.getCardObjects();
  }//parseStream

  private Contact processCard(List items) {
    Contact ct = new ContactImpl();
    if (items != null) {
      for (Iterator iter = items.listIterator(); iter.hasNext();) {
        versitItem item = (versitItem) iter.next();
        processItem(ct, item);
      }
    }

    return ct;
  }//processCard

  private void processItem(Contact ct, versitItem item) {
    ItemHandler handler =
        m_ItemHandlerManager.getItemHandler(item.getIdentifier());
    if (handler != null) {
      handler.handleItem(ct, item);
    }
  }//processItem

  public static void main(String[] args) {
    try {
      File infile = new File(args[0]);
      FileInputStream fin = new FileInputStream(infile);
      vCardUnmarshaller unmarshall = new vCardUnmarshaller();
      Contact testct = unmarshall.unmarshallContact(fin);
      vCardMarshaller marshall = new vCardMarshaller();
      marshall.marshallContact(System.out, testct);
    } catch (Exception ex) {
      ex.printStackTrace();
    }
  }//main

}//vCardUnmarshaller
