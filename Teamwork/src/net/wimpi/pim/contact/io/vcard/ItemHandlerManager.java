/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.io.vcard;

import net.wimpi.pim.util.versitio.versitException;
import net.wimpi.pim.util.versitio.versitToken;

import java.util.HashMap;
import java.util.Set;
import java.util.TreeSet;

/**
 * Singleton for handling {@link ItemHandler} instances.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class ItemHandlerManager {

  private static ItemHandlerManager c_Self;
  private HashMap m_Handlers;
  private Set m_Extensions;


  private ItemHandlerManager() {
    c_Self = this;
    prepareHandlers();
    prepareExtensions();
  }//constructor

  /**
   * Returns the {@link ItemHandler} for the item with the
   * given identifier.
   *
   * @param identifier an identifier as <tt>String</tt>.
   * @return an <tt>ItemHandler</tt> instance.
   */
  public ItemHandler getItemHandler(String identifier) {
    return (ItemHandler) m_Handlers.get(identifier);
  }//getItemHandler

  private void addItemHandler(String identifier, ItemHandler handler) {
    m_Handlers.put(identifier, handler);
  }//addItemHandler

  /**
   * Tests if an {@link ItemHandler} for the item with the
   * given identifier exists.
   *
   * @param identifier an identifier as <tt>String</tt>.
   * @return true if it exists, false otherwise.
   */
  public boolean hasHandler(String identifier) {
    return m_Handlers.containsKey(identifier);
  }//hasHandler

  /**
   * Returns a list of all extension identifiers for
   * which an <tt>ItemHandler</tt> has been registered.
   *
   * @return the list of identifiers as <tt>String[]</tt>.
   */
  public String[] listExtensions() {
    String[] xtnd = new String[m_Extensions.size()];
    return (String[]) m_Extensions.toArray(xtnd);
  }//listExtensions

  /**
   * Registers an {@link ItemHandler} for the given
   * extension identifier.
   *
   * @param xidentifier an identifier as <tt>String</tt>.
   * @param handler the <tt>ItemHandler</tt> to be registered.
   *
   * @throws versitException if an <tt>ItemHandler</tt> has already been
   *         registered for the given extension identifier.
   */
  public void addExtensionHandler(String xidentifier, ItemHandler handler)
      throws versitException {
    if (!m_Extensions.contains(xidentifier)) {
      m_Extensions.add(xidentifier);
      addItemHandler(xidentifier, handler);
    } else {
      throw new versitException("Extension tokens have to be unique.");
    }
  }//addExtensionHandler

  private void prepareHandlers() {
    //prepare Hashtable
    m_Handlers = new HashMap(35);
    //put handlers
    m_Handlers.put(versitToken.N, new NItemHandler());
    m_Handlers.put(versitToken.FN, new FNItemHandler());
    m_Handlers.put(versitToken.NICKNAME, new NICKNAMEItemHandler());
    m_Handlers.put(versitToken.PHOTO, new PHOTOItemHandler());
    m_Handlers.put(versitToken.BDAY, new BDAYItemHandler());
    m_Handlers.put(versitToken.ADR, new ADRItemHandler());
    m_Handlers.put(versitToken.LABEL, new LABELItemHandler());
    m_Handlers.put(versitToken.TEL, new TELItemHandler());
    m_Handlers.put(versitToken.EMAIL, new EMAILItemHandler());
    m_Handlers.put(versitToken.MAILER, new MAILERItemHandler());
    m_Handlers.put(versitToken.TZ, new TZItemHandler());
    m_Handlers.put(versitToken.GEO, new GEOItemHandler());
    m_Handlers.put(versitToken.TITLE, new TITLEItemHandler());
    m_Handlers.put(versitToken.ROLE, new ROLEItemHandler());
    m_Handlers.put(versitToken.LOGO, new LOGOItemHandler());
    m_Handlers.put(versitToken.ORG, new ORGItemHandler());
    m_Handlers.put(versitToken.CATEGORIES, new CATEGORIESItemHandler());
    m_Handlers.put(versitToken.NOTE, new NOTEItemHandler());
    m_Handlers.put(versitToken.REV, new REVItemHandler());
    m_Handlers.put(versitToken.SOUND, new SOUNDItemHandler());
    m_Handlers.put(versitToken.SORTSTRING, new SORTSTRINGItemHandler());
    m_Handlers.put(versitToken.KEY, new KEYItemHandler());
    m_Handlers.put(versitToken.URL, new URLItemHandler());
    m_Handlers.put(versitToken.CLASS, new CLASSItemHandler());
  }//prepareHandlers

  private void prepareExtensions() {
    m_Extensions = new TreeSet();

    //put standard extensions
    try {
      addExtensionHandler(versitToken.XORGURL, new XORGURLItemHandler());
    } catch (Exception ex) {
      //should not happen
    }
  }//prepareExtensions

  /**
   * Returns the reference to this <tt>ItemHandlerManager</tt> singleton.
   *
   * @return the singleton reference.
   */
  public static ItemHandlerManager getReference() {
    if (c_Self != null) {
      return c_Self;
    } else {
      return new ItemHandlerManager();
    }
  }//getReference

}//class ItemHandlerManager
