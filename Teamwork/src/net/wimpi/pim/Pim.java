/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim;

import net.wimpi.pim.factory.ContactDBFactory;
import net.wimpi.pim.factory.ContactIOFactory;
import net.wimpi.pim.factory.ContactModelFactory;

/**
 * Utility class.
 *
 * Provides factories and package wide settings/defaults.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class Pim {

  private static ContactModelFactory c_CMFactory =
      new net.wimpi.pim.contact.basicimpl.BasicContactModelFactory();

  private static ContactIOFactory c_CIOFactory =
      new net.wimpi.pim.contact.io.vcard.vCardIOFactory();

  private static ContactDBFactory c_CDBFactory =
      new net.wimpi.pim.contact.db.serializable.SerializableContactDBFactory();

  private static boolean c_Announce = false;

  /**
   * Private to prevent construction.
   */
  private Pim() {

  }//constructor

  /**
   * Returns a reference to the <tt>ContactModelFactory</tt> for
   * applications.
   *
   * @return a <tt>ContactModelFactory</tt> instance.
   */
  public static ContactModelFactory getContactModelFactory() {
    return c_CMFactory;
  }//getContactModelFactory


  /**
   * Sets the <tt>ContactModelFactory</tt> instance to be
   * used by applications.
   *
   * @param factory a <tt>ContactModelFactory</tt> instance.
   */
  public static void setContactModelFactory(ContactModelFactory factory) {
    if (factory != null) {
      c_CMFactory = factory;
    }
  }//setContactModelFactory

  /**
   * Returns a reference to the <tt>ContactIOFactory</tt> for
   * applications.
   *
   * @return a <tt>ContactIOFactory</tt> instance.
   */
  public static ContactIOFactory getContactIOFactory() {
    return c_CIOFactory;
  }//getContactIOFactory

  /**
   * Sets the <tt>ContactIOFactory</tt> instance to be
   * used by applications.
   *
   * @param factory a <tt>ContactIOFactory</tt> instance.
   */
  public static void setContactIOFactory(ContactIOFactory factory) {
    if (factory != null) {
      c_CIOFactory = factory;
    }
  }//setContactIOFactory

  /**
   * Returns a reference to the <tt>ContactDBFactory</tt> for
   * applications.
   *
   * @return a <tt>ContactDBFactory</tt> instance.
   */
  public static ContactDBFactory getContactDBFactory() {
    return c_CDBFactory;
  }//getContactIOFactory

  /**
   * Sets the <tt>ContactDBFactory</tt> instance to be
   * used by applications.
   *
   * @param factory a <tt>ContactDBFactory</tt> instance.
   */
  public static void setContactIOFactory(ContactDBFactory factory) {
    if (factory != null) {
      c_CDBFactory = factory;
    }
  }//setContactDBFactory


  /**
   * Tests if the product should be announced
   * when marshalling.<br>
   * The <tt>vCardMarshaller</tt> will append the
   * <tt>PRODID</tt> field to the card for example.<br>
   * The default is <tt>false</tt>.
   *
   * @return true if announced, false otherwise.
   */
  public static boolean isAnnounced() {
    return c_Announce;
  }//isAnnounced

  /**
   * Sets if the product should be announced
   * when marshalling.<br>
   * The <tt>vCardMarshaller</tt> will append the
   * <tt>PRODID</tt> field to the card for example.<br>
   * The default is <tt>false</tt>.
   *
   * @param b true if announce, false otherwise.
   */
  public static void setAnnounced(boolean b) {
    c_Announce = b;
  }//setAnnounced

  /**
   * Defines the product identifier that announces jpim.
   */
  public static final String PRODUCT_ID = "jpim.sourceforge.net";

}//class Pim
