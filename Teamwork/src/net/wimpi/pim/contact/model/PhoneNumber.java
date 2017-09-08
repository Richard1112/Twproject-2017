/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.model;

import net.wimpi.pim.util.Identifiable;

import java.io.Serializable;

/**
 * An interface modeling a phone number based on the
 * types and information of the vCard Mime directory
 * profile standard specification.
 * <p>
 * For reference see RFC 2426:<br>
 * 3.3.1 TEL Type Definition<br>
 * <br>
 * The TEL type is based on the X.500 Telephone Number
 * attribute.
 * <br>
 * Note that for a standard conformant implementation
 * you have to observe that the default set flag
 * is <tt>voice</tt> only.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public interface PhoneNumber
    extends Identifiable, Serializable {

  /**
   * Returns the number of this <tt>PhoneNumber</tt>.
   *
   * @return the number as <tt>String</tt>.
   */
  public String getNumber();

  /**
   * Sets the number of this <tt>PhoneNumber</tt>.
   *
   * @param number the number as <tt>String</tt>.
   */
  public void setNumber(String number);

  /**
   * Tests if this <tt>PhoneNumber</tt> represents a
   * preferred number.
   *
   * @return true if representing a preferred number,
   *         false otherwise.
   */
  public boolean isPreferred();

  /**
   * Sets or resets the preferred flag of
   * this <tt>PhoneNumber</tt>.
   *
   * @param b true if representing a preferred number,
   *         false otherwise.
   */
  public void setPreferred(boolean b);

  /**
   * Tests if this <tt>PhoneNumber</tt> is
   * associated with a residence.
   *
   * @return true if associated with a residence,
   *         false otherwise.
   */
  public boolean isHome();

  /**
   * Sets or resets the home flag of
   * this <tt>PhoneNumber</tt>.
   *
   * @param b true if associated with a residence,
   *         false otherwise.
   */
  public void setHome(boolean b);

  /**
   * Tests if this <tt>PhoneNumber</tt> is
   * associated with a place of work.
   *
   * @return true if associated with a place of work,
   *         false otherwise.
   */
  public boolean isWork();

  /**
   * Sets or resets the work flag of
   * this <tt>PhoneNumber</tt>.
   *
   * @param b true if associated with a place of work,
   *         false otherwise.
   */
  public void setWork(boolean b);

  /**
   * Tests if this <tt>PhoneNumber</tt> is
   * a voice number.
   *
   * @return true if voice number, false otherwise.
   */
  public boolean isVoice();

  /**
   * Sets or resets the voice flag of
   * this <tt>PhoneNumber</tt>.
   *
   * @param b true if voice number, false otherwise.
   */
  public void setVoice(boolean b);

  /**
   * Tests if this <tt>PhoneNumber</tt> has
   * voice messaging support.
   *
   * @return true if has messaging, false otherwise.
   */
  public boolean isMessaging();

  /**
   * Sets or resets the messaging flag of
   * this <tt>PhoneNumber</tt>.
   *
   * @param b true if has messaging, false otherwise.
   */
  public void setMessaging(boolean b);

  /**
   * Tests if this <tt>PhoneNumber</tt> is a facsimile
   * number.
   *
   * @return true if facsimile, false otherwise.
   */
  public boolean isFax();

  /**
   * Sets or resets the fax flag of
   * this <tt>PhoneNumber</tt>.
   *
   * @param b true if facsimile, false otherwise.
   */
  public void setFax(boolean b);

  /**
   * Tests if this <tt>PhoneNumber</tt> is a
   * cellular phone number.
   *
   * @return true if cellular phone, false otherwise.
   */
  public boolean isCellular();

  /**
   * Sets or resets the cellular flag of
   * this <tt>PhoneNumber</tt>.
   *
   * @param b true if cellular phone, false otherwise.
   */
  public void setCellular(boolean b);

  /**
   * Tests if this <tt>PhoneNumber</tt> is a
   * video conferencing number.
   *
   * @return true if video conferencing,
   *         false otherwise.
   */
  public boolean isVideo();

  /**
   * Sets or resets the video conferencing flag of
   * this <tt>PhoneNumber</tt>.
   *
   * @param b true if video conferencing,
   *        false otherwise.
   */
  public void setVideo(boolean b);

  /**
   * Tests if this <tt>PhoneNumber</tt> is a
   * paging device number.
   *
   * @return true if paging device number,
   *         false otherwise.
   */
  public boolean isPager();

  /**
   * Sets or resets the paging device flag of this
   * <tt>PhoneNumber</tt>.
   *
   * @param b true if paging device number,
   *        false otherwise.
   */
  public void setPager(boolean b);

  /**
   * Tests if this <tt>PhoneNumber</tt> is
   * associated to a bulletin board system.
   *
   * @return true if associated with a bbs,
   *         false otherwise.
   */
  public boolean isBBS();

  /**
   * Sets or resets the bbs flag of
   * this <tt>PhoneNumber</tt>.
   *
   * @param b true if associated with a bbs,
   *        false otherwise.
   */
  public void setBBS(boolean b);

  /**
   * Tests if this <tt>PhoneNumber</tt> is
   * connected with a MODEM.
   *
   * @return true if modem connected,
   *         false otherwise.
   */
  public boolean isMODEM();

  /**
   * Sets or resets the modem flag of
   * this <tt>PhoneNumber</tt>.
   *
   * @param b true if modem connected, false otherwise.
   */
  public void setMODEM(boolean b);

  /**
   * Tests if this <tt>PhoneNumber</tt> is
   * associated with ISDN service.
   *
   * @return true if ISDN service, false otherwise.
   */
  public boolean isISDN();

  /**
   * Sets or resets the ISDN flag of
   * this <tt>PhoneNumber</tt>.
   *
   * @param b true if ISDN, false otherwise.
   */
  public void setISDN(boolean b);


  /**
   * Tests if this <tt>PhoneNumber</tt> is a
   * car phone number.
   *
   * @return true if car phone, false otherwise.
   */
  public boolean isCar();

  /**
   * Sets or resets the car flag of
   * this <tt>PhoneNumber</tt>.
   *
   * @param b true if car phone, false otherwise.
   */
  public void setCar(boolean b);

  /**
   * Tests if this <tt>PhoneNumber</tt> is a
   * personal communications services number.
   *
   * @return true if PCS number, false otherwise.
   */
  public boolean isPCS();

  /**
   * Sets or resets the PCS flag of
   * this <tt>PhoneNumber</tt>.
   *
   * @param b true if PCS number, false otherwise.
   */
  public void setPCS(boolean b);

  /**
   * Tests if this <tt>PhoneNumber</tt> is
   * of a specific type.
   * Types are defined as constants of this
   * interface.
   *
   * @param TYPE a type as <tt>int</tt>.
   */
  public boolean isType(int TYPE);

  /**
   * Type that indicates a phone number associated
   * with a work place.
   */
  public final static int TYPE_WORK = 100;

  /**
   * Type that indicates a phone number associated
   * with a residence.
   */
  public final static int TYPE_HOME = 101;

  /**
   * Type that indicates a facsimile phone number.
   */
  public final static int TYPE_FAX = 102;

  /**
   * Type that indicates a paging device phone number.
   */
  public final static int TYPE_PAGER = 103;

  /**
   * Type that indicates a voice phone number.
   */
  public final static int TYPE_VOICE = 104;

  /**
   * Type that indicates a bulletin board
   * phone number.
   */
  public final static int TYPE_BBS = 105;

  /**
   * Type that indicates a modem connected
   * phone number.
   */
  public final static int TYPE_MODEM = 106;

  /**
   * Type that indicates an ISDN service number.
   */
  public final static int TYPE_ISDN = 107;

  /**
   * Type that indicates a cellular phone number.
   */
  public final static int TYPE_CELLULAR = 108;

  /**
   * Type that indicates that the phone number
   * has messaging support.
   */
  public final static int TYPE_MESSAGING = 109;

  /**
   * Type that indicates a video conferencing
   * phone number.
   *
   */
  public final static int TYPE_VIDEO = 110;

  /**
   * Type that indicates a car phone number.
   */
  public final static int TYPE_CAR = 111;

  /**
   * Type that indicates a personal communication
   * services phone number.
   */
  public final static int TYPE_PCS = 112;

}//interface PhoneNumber
