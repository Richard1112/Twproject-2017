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
import net.wimpi.pim.contact.model.Key;
import net.wimpi.pim.util.EncodingUtility;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitToken;

/**
 * Class implementing a handler for the <tt>PHOTO</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.7.2 KEY Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class KEYItemHandler
    implements ItemHandler {

  public void handleItem(Contact ct, versitItem item) {
    Key key = Pim.getContactModelFactory().createKey();
    String[] paramvalues = null;
    paramvalues = item.getParameter(versitToken.VALUE);

    if (paramvalues != null &&
        (paramvalues[0].equalsIgnoreCase(versitToken.URL) ||
        paramvalues[0].equalsIgnoreCase(versitToken.URI))
    ) {
      //handle URI key
      key.setURI(item.getDecodedValue());
    } else {
      //handle embedded key

      //1. handle the data
      paramvalues = item.getParameter(versitToken.ENCODING);
      if (paramvalues != null &&
          (paramvalues[0].equalsIgnoreCase(versitToken.ENCODING_B) ||
          paramvalues[0].equalsIgnoreCase(versitToken.ENCODING_BASE64))
      ) {
        //set decoded data (BASE64 encoding)
        key.setData(
            EncodingUtility.decodeBase64(
                EncodingUtility.removeWhiteSpace(
                    item.getValue().getBytes()
                )
            )
        );
      } else {
        //exception
      }

      //2. handle the type if available
      paramvalues = item.getParameter(versitToken.TYPE);
      if (paramvalues == null) {
        paramvalues = item.getParameter(versitToken.DEFAULT);
      }
      if (paramvalues != null) {
        key.setContentType(paramvalues[0].toLowerCase());
      } else {
        //exception
      }
    }
    ct.setPublicKey(key);
    return;
  }//handleItem

  public versitItem[] createItems(Contact ct) {
    if (!ct.hasPublicKey()) {
      return new versitItem[0];
    }
    versitItem[] items = new versitItem[1];
    Key key = ct.getPublicKey();
    if (key == null) {
      return new versitItem[0];
    }
    versitItem item = new versitItem(versitToken.KEY);
    if (key.isURI()) {
      item.addParameter(versitToken.VALUE, versitToken.URI);
      item.setValue(key.getURI());
    } else {
      String ctype = key.getContentType();
      ctype = ctype.substring(ctype.indexOf("/") + 1, ctype.length()).toUpperCase();
      item.addParameter(versitToken.ENCODING, versitToken.ENCODING_B);
      item.addParameter(versitToken.TYPE, ctype);
      item.setValue(
          new String(
              EncodingUtility.removeWhiteSpace(
                  EncodingUtility.encodeBase64(
                      key.getData()
                  )
              )
          )
      );
    }
    items[0] = item;
    return items;
  }//createItem


}//KeyItemHandler
