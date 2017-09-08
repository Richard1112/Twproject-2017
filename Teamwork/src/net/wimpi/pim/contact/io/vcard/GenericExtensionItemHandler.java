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
import net.wimpi.pim.contact.basicimpl.GenericExtension;
import net.wimpi.pim.contact.model.Contact;
import net.wimpi.pim.contact.model.Extensions;
import net.wimpi.pim.util.versitio.versitItem;

import java.util.Iterator;

/**
 * Class implementing a handler for simple
 * extensions.
 * <p>
 * For reference see RFC 2426:<br>
 * 3.8 Extended Types<br>
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 *
 * @see net.wimpi.pim.contact.basicimpl.SimpleExtension
 */
public class GenericExtensionItemHandler
    implements ItemHandler {

  protected String m_Identifier;
  protected GenericExtension m_Type;

  public GenericExtensionItemHandler(GenericExtension ext) {
    m_Identifier = ext.getIdentifier();
    m_Type = ext;
  }//SimpleExtensionHandler

  public void handleItem(Contact ct, versitItem item) {
    Extensions extensions = ct.getExtensions();
    if (extensions == null) {
      extensions = Pim.getContactModelFactory().createExtensions();
      ct.setExtensions(extensions);
    }
    GenericExtension ext = m_Type.createExtension();

    ext.setValue(item.getDecodedValue());
    ext.setParameters(item.getParameters());
    extensions.add(ext);
  }//handleItem

  public versitItem[] createItems(Contact ct) {
    Extensions extensions = ct.getExtensions();
    if (extensions == null) {
      return new versitItem[0];
    }
    versitItem[] items = new versitItem[extensions.size(m_Identifier)];
    int n = 0;
    for (Iterator iter = extensions.iterator(m_Identifier); iter.hasNext(); n++) {
      GenericExtension ext = (GenericExtension) iter.next();
      versitItem item = new versitItem(m_Identifier);
      //value
      item.setValue(ext.getValue());
      //parameters
      String[] params = ext.listParameters();
      for (int i = 0; i < params.length; i++) {
        String[] paramvals = ext.getParameterValues(params[i]);
        for (int j = 0; j < paramvals.length; j++) {
          item.addParameter(params[i], paramvals[j]);
        }
      }
      items[n] = item;
    }
    return items;
  }//createItems

}//SimpleExtensionItemHandler
