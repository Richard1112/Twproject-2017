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
import net.wimpi.pim.contact.model.Image;
import net.wimpi.pim.contact.model.PersonalIdentity;
import net.wimpi.pim.util.EncodingUtility;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitToken;

/**
 * Class implementing a handler for the <tt>PHOTO</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.1.4 PHOTO Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class PHOTOItemHandler
    implements ItemHandler {

  public void handleItem(Contact ct, versitItem item) {
    Image photo = Pim.getContactModelFactory().createImage();
    String[] paramvalues = null;
    paramvalues = item.getParameter(versitToken.VALUE);

    if (paramvalues != null &&
        (paramvalues[0].equalsIgnoreCase(versitToken.URL) ||
        paramvalues[0].equalsIgnoreCase(versitToken.URI))
    ) {
      //handle URI image
      photo.setURI(item.getDecodedValue());
    } else {
      //handle embedded image

      //1. handle the data
      paramvalues = item.getParameter(versitToken.ENCODING);
      if (paramvalues != null &&
          (paramvalues[0].equalsIgnoreCase(versitToken.ENCODING_B) ||
          paramvalues[0].equalsIgnoreCase(versitToken.ENCODING_BASE64))
      ) {
        //set decoded data (BASE64 encoding)
        photo.setData(
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
        photo.setContentType("image/" + paramvalues[0].toLowerCase());
      } else {
        //exception
      }
    }
    PersonalIdentity pid = ct.getPersonalIdentity();
    if (pid == null) {
      pid = Pim.getContactModelFactory().createPersonalIdentity();
      ct.setPersonalIdentity(pid);
    }
    pid.setPhoto(photo);
    return;
  }//handleItem

  public versitItem[] createItems(Contact ct) {
    if (!ct.hasPersonalIdentity()) {
      return new versitItem[0];
    } else if (!ct.getPersonalIdentity().hasPhoto()) {
      return new versitItem[0];
    }
    versitItem[] items = new versitItem[1];
    Image photo = ct.getPersonalIdentity().getPhoto();
    if (photo == null) {
      return new versitItem[0];
    }
    versitItem item = new versitItem(versitToken.PHOTO);
    if (photo.isURI()) {
      item.addParameter(versitToken.VALUE, versitToken.URI);
      item.setValue(photo.getURI());
    } else {
      String ctype = photo.getContentType();
      ctype = ctype.substring(ctype.indexOf("/") + 1, ctype.length()).toUpperCase();
      item.addParameter(versitToken.ENCODING, versitToken.ENCODING_B);
      item.addParameter(versitToken.TYPE, ctype);
      item.setValue(
          new String(
              EncodingUtility.removeWhiteSpace(
                  EncodingUtility.encodeBase64(
                      photo.getData()
                  )
              )
          )
      );
    }
    items[0] = item;
    return items;
  }//createItem


}//PHOTOItemHandler
