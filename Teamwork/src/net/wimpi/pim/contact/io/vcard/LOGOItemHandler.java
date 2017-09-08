/***
 * jpim Java PIM Library
 * Copyright (c) 2001 Dieter Wimberger
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.io.vcard;

import net.wimpi.pim.Pim;
import net.wimpi.pim.contact.model.Contact;
import net.wimpi.pim.contact.model.Image;
import net.wimpi.pim.contact.model.Organization;
import net.wimpi.pim.contact.model.OrganizationalIdentity;
import net.wimpi.pim.util.EncodingUtility;
import net.wimpi.pim.util.versitio.versitItem;
import net.wimpi.pim.util.versitio.versitToken;

/**
 * Class implementing a handler for the <tt>LOGO</tt>
 * item of the vCard Mime directory profile
 * standard specification.<p>
 *
 * For reference see RFC 2426:<br>
 * 3.5.3 LOGO Type Definition<br>
 * <p>
 * This implementation is able to read v2.1 and
 * to read/write v3.0.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
class LOGOItemHandler implements ItemHandler {

  public void handleItem(Contact ct, versitItem item) {
    Image logo = Pim.getContactModelFactory().createImage();
    String[] paramvalues = null;
    paramvalues = item.getParameter(versitToken.VALUE);

    if (paramvalues != null &&
        (paramvalues[0].equalsIgnoreCase(versitToken.URL) ||
        paramvalues[0].equalsIgnoreCase(versitToken.URI))
    ) {
      //handle URI image
      logo.setURI(item.getDecodedValue());
    } else {
      //handle embedded image

      //1. handle the data
      paramvalues = item.getParameter(versitToken.ENCODING);
      if (paramvalues != null &&
          (paramvalues[0].equalsIgnoreCase(versitToken.ENCODING_B) ||
          paramvalues[0].equalsIgnoreCase(versitToken.ENCODING_BASE64))
      ) {
        //set decoded data (BASE64 encoding)
        logo.setData(
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
        logo.setContentType("image/" + paramvalues[0].toLowerCase());
      } else {
        //exception
      }
    }
    OrganizationalIdentity orgid = ct.getOrganizationalIdentity();
    if (orgid == null) {
      orgid = Pim.getContactModelFactory().createOrganizationalIdentity();
      ct.setOrganizationalIdentity(orgid);
    }
    Organization org = orgid.getOrganization();
    if (org == null) {
      org = Pim.getContactModelFactory().createOrganization();
      orgid.setOrganization(org);
    }
    org.setLogo(logo);
    return;
  }//handleItem

  public versitItem[] createItems(Contact ct) {
    if (!ct.hasOrganizationalIdentity()) {
      return new versitItem[0];
    } else if (!ct.getOrganizationalIdentity().hasOrganization()) {
      return new versitItem[0];
    } else if (!ct.getOrganizationalIdentity().getOrganization().hasLogo()) {
      return new versitItem[0];
    }
    Image logo = ct.getOrganizationalIdentity().getOrganization().getLogo();
    versitItem[] items = new versitItem[1];
    versitItem item = new versitItem(versitToken.LOGO);
    if (logo.isURI()) {
      item.addParameter(versitToken.VALUE, versitToken.URI);
      item.setValue(logo.getURI());
    } else {
      String ctype = logo.getContentType();
      ctype = ctype.substring(ctype.indexOf("/") + 1, ctype.length()).toUpperCase();
      item.addParameter(versitToken.ENCODING, versitToken.ENCODING_B);
      item.addParameter(versitToken.TYPE, ctype);
      item.setValue(
          new String(
              EncodingUtility.removeWhiteSpace(
                  EncodingUtility.encodeBase64(
                      logo.getData()
                  )
              )
          )
      );
    }
    items[0] = item;
    return items;
  }//createItem

}//LOGOItemHandler
