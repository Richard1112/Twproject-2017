/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.util.versitio;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

public class versitParser {

  private versitInputStream m_Input;
  private List m_Items;
  private List m_CardObjects;
  private List m_CalObjects;
  private boolean m_Validating = true;

  public versitParser(InputStream in) {
    //wrap stream
    m_Input = new versitInputStream(in);
    //create items list
    m_Items =
        Collections.synchronizedList(new ArrayList(35));
  }//constructor

  public versitParser(InputStream in, String enc)
      throws UnsupportedEncodingException {
    //wrap stream
    m_Input = new versitInputStream(in, enc);
    //create items list
    m_Items =
        Collections.synchronizedList(new ArrayList(35));
  }//constructor

  public void parse()
      throws versitException {
    try {
      //itemize
      while (m_Input.hasLine()) {
        String line = m_Input.readLine();
        //skip empty lines
        if (line != null && line.length() > 0) {
          m_Items.add(versitItem.createItem(line));
        }
      }
      //bundle objects
      bundleObjects();
    } catch (IOException ex) {
      ex.printStackTrace();
    }
  }//parse

  public boolean isValidating() {
    return m_Validating;
  }//isValidating

  public void setValidating(boolean b) {
    m_Validating = true;
  }//setValidating

  private void bundleObjects()
      throws versitException {

    m_CardObjects = new ArrayList(3);
    m_CalObjects = new ArrayList(3);

    List itembundle = null;
    versitItem item = null;
    int pos = 0;

    for (Iterator iter = m_Items.listIterator(); iter.hasNext(); pos++) {
      item = (versitItem) iter.next();
      //check begin
      if (item.getIdentifier().equalsIgnoreCase(versitToken.START)) {
        if (item.getValue().equalsIgnoreCase(versitToken.VCARD)) {
          int endpos = findObjectEnd(versitToken.VCARD, iter, pos);
          itembundle = m_Items.subList(pos + 1, endpos);
          pos = endpos;
          // Validating if set
          if (isValidating()) {
            validateCard(itembundle);
          }
          //add bundle representing vcard object
          m_CardObjects.add(itembundle);
        } else if (item.getValue().equalsIgnoreCase(versitToken.VCALENDAR)) {
          int endpos = findObjectEnd(versitToken.VCALENDAR, iter, pos);
          itembundle = m_Items.subList(pos + 1, endpos);
          pos = endpos;

          //add bundle representing calendar object
          m_CalObjects.add(itembundle);
        }
      }
    }
  }//checkObjects

  public List getCardObjects() {
    return m_CardObjects;
  }//getCardObjects

  public List getCalendarObjects() {
    return m_CalObjects;
  }//getCalendarObjects

  private int findObjectEnd(String objecttype, Iterator iter, int pos)
      throws versitException {

    boolean done = false;
    versitItem item = null;

    while (iter.hasNext() || !done) {
      pos++;
      item = (versitItem) iter.next();
      if (item.getIdentifier().equalsIgnoreCase(versitToken.END)
          && item.getValue().equalsIgnoreCase(objecttype)) {
        return pos;
      } else {
        continue;
      }
    }
    if (!done) {
      throw new versitException("Object end token missing.");
    } else {
      return pos;
    }
  }//findObjectEnd

  private void validateCard(List items) throws versitException {
    String version = getVersion(items);
    if (version.equals(versitToken.VCARD_VERSION_2)) {
      String[] mandatoryTokens = {versitToken.N};
      if (!hasMandatoryTokens(items, mandatoryTokens)) {
        throw new versitException("Mandatory tokens missing.");
      }
    } else if (version.equals(versitToken.VCARD_VERSION_3)) {
      String[] mandatoryTokens = {versitToken.N, versitToken.FN};
      if (!hasMandatoryTokens(items, mandatoryTokens)) {
        throw new versitException("Mandatory tokens missing.");
      }
    } else {
      throw new versitException(
          "Factory cannot handle format version " + version + ".");
    }
  }//validateCard

  private String getVersion(List items)
      throws versitException {

    versitItem item = null;
    //int pos=1;
    for (Iterator iter = items.listIterator();
         iter.hasNext(); /*pos++*/) {

      item = (versitItem) iter.next();
      //System.out.println("Inspecting item "+pos+":"+item.getIdentifier());
      if (item.getIdentifier().equalsIgnoreCase(versitToken.VERSION)) {
        return item.getValue();
      }
    }
    //if not encountered, format invalid
    throw new versitException(
        "vCard invalid: the version property is mandatory.");
  }//getVersion

  private boolean hasMandatoryTokens(List items, String[] tokens) {
    versitItem item = null;
    int count = 0;

    for (Iterator iter = items.listIterator();
         iter.hasNext();) {

      item = (versitItem) iter.next();
      for (int i = 0; i < tokens.length; i++) {
        if (item.getIdentifier().equalsIgnoreCase(tokens[i])) {
          count++;
        }
      }
    }
    return (count == tokens.length);
  }//hasMandatoryTokens


  public static void main(String[] args) {
    try {
      File infile = new File(args[0]);
      FileInputStream fin = new FileInputStream(infile);
      versitParser parser = new versitParser(fin);
      parser.parse();
    } catch (Exception ex) {
      ex.printStackTrace();
    }
  }//main

}//versitParser
