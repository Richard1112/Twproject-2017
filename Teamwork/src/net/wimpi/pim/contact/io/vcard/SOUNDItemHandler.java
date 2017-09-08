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
import net.wimpi.pim.contact.model.Sound;
import net.wimpi.pim.util.EncodingUtility;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitToken;

/**
 * Class implementing a handler for the <tt>SOUND</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.6.6 SOUND Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class SOUNDItemHandler
    implements ItemHandler {

  public void handleItem(Contact ct, versitItem item) {
    Sound sound = Pim.getContactModelFactory().createSound();
    String[] paramvalues = null;
    paramvalues = item.getParameter(versitToken.VALUE);

    if (paramvalues != null &&
        (paramvalues[0].equalsIgnoreCase(versitToken.URL) ||
        paramvalues[0].equalsIgnoreCase(versitToken.URI))
    ) {
      //handle URI image
      sound.setURI(item.getDecodedValue());
    } else {
      //handle embedded image

      //1. handle the data
      paramvalues = item.getParameter(versitToken.ENCODING);
      if (paramvalues != null &&
          (paramvalues[0].equalsIgnoreCase(versitToken.ENCODING_B) ||
          paramvalues[0].equalsIgnoreCase(versitToken.ENCODING_BASE64))
      ) {
        //set decoded data (BASE64 encoding)
        sound.setData(
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
        sound.setContentType("audio/" + paramvalues[0].toLowerCase());
      } else {
        //exception
      }
    }
    ct.setSound(sound);
    return;
  }//handleItem

  public versitItem[] createItems(Contact ct) {
    if (!ct.hasSound()) {
      return new versitItem[0];
    }
    versitItem[] items = new versitItem[1];
    Sound sound = ct.getSound();
    versitItem item = new versitItem(versitToken.SOUND);
    if (sound.isURI()) {
      item.addParameter(versitToken.VALUE, versitToken.URI);
      item.setValue(sound.getURI());
    } else {
      String ctype = sound.getContentType();
      ctype = ctype.substring(ctype.indexOf("/") + 1, ctype.length()).toUpperCase();
      item.addParameter(versitToken.ENCODING, versitToken.ENCODING_B);
      item.addParameter(versitToken.TYPE, ctype);
      item.setValue(
          new String(
              EncodingUtility.removeWhiteSpace(
                  EncodingUtility.encodeBase64(
                      sound.getData()
                  )
              )
          )
      );
    }
    items[0] = item;
    return items;
  }//createItems

}//SOUNDItemHandler
