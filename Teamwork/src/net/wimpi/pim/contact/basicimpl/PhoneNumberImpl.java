/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.basicimpl;

import net.wimpi.pim.contact.model.PhoneNumber;
import net.wimpi.pim.util.AbstractIdentifiable;

/**
 * A basic and simple implementation of a
 * {@link net.wimpi.pim.contact.model.PhoneNumber}.
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class PhoneNumberImpl
    extends AbstractIdentifiable
    implements PhoneNumber {

  static final long serialVersionUID = 4681085614118423521L;

  //Attributes
  protected String m_Number;
  protected boolean m_Home;
  protected boolean m_Work;
  protected boolean m_Voice;
  protected boolean m_Fax;
  protected boolean m_Messaging;
  protected boolean m_Cellular;
  protected boolean m_Pager;
  protected boolean m_BBS;
  protected boolean m_MODEM;
  protected boolean m_ISDN;
  protected boolean m_Video;
  protected boolean m_PCS;
  protected boolean m_Car;
  protected boolean m_Preferred;
  protected int m_Index = -1;

  public int getIndex() {
    return m_Index;
  }//getIndex

  public void setIndex(int index) {
    m_Index = index;
  }//setIndex

  public String getNumber() {
    return m_Number;
  }//getNumber

  public void setNumber(String number) {
    m_Number = number;
  }//setNumber;

  public boolean isWork() {
    return m_Work;
  }//isWork

  public void setWork(boolean b) {
    m_Work = b;
  }//setWork

  public boolean isHome() {
    return m_Home;
  }//isHome

  public void setHome(boolean b) {
    m_Home = b;
  }//setHome

  public void setVoice(boolean b) {
    m_Voice = true;
  }//setVoice

  public boolean isVoice() {
    return m_Voice;
  }//isVoice

  public boolean isVideo() {
    return m_Video;
  }//isVideo

  public void setVideo(boolean b) {
    m_Video = b;
  }//setVideo

  public boolean isFax() {
    return m_Fax;
  }//isFax

  public void setFax(boolean b) {
    m_Fax = b;
  }//setFax

  public boolean isMessaging() {
    return m_Messaging;
  }//isMessaging

  public void setMessaging(boolean b) {
    m_Messaging = b;
  }//setMessaging

  public boolean isCellular() {
    return m_Cellular;
  }//isCellular

  public void setCellular(boolean b) {
    m_Cellular = b;
  }//setCellular

  public boolean isPager() {
    return m_Pager;
  }//isPager

  public void setPager(boolean b) {
    m_Pager = b;
  }//setPager

  public boolean isBBS() {
    return m_BBS;
  }//isBBS

  public void setBBS(boolean b) {
    m_BBS = b;
  }//setBBS

  public boolean isMODEM() {
    return m_MODEM;
  }//isMODEM

  public void setMODEM(boolean b) {
    m_MODEM = b;
  }//setMODEM

  public boolean isISDN() {
    return m_ISDN;
  }//isISDN

  public void setISDN(boolean b) {
    m_ISDN = b;
  }//setISDN

  public boolean isPCS() {
    return m_PCS;
  }//isPCS

  public void setPCS(boolean PCS) {
    m_PCS = PCS;
  }//setPCS

  public boolean isCar() {
    return m_Car;
  }//isCar

  public void setCar(boolean car) {
    m_Car = car;
  }//setCar

  public boolean isType(int TYPE) {
    switch (TYPE) {
      case TYPE_HOME:
        return isHome();
      case TYPE_WORK:
        return isWork();
      case TYPE_BBS:
        return isBBS();
      case TYPE_CELLULAR:
        return isCellular();
      case TYPE_FAX:
        return isFax();
      case TYPE_ISDN:
        return isISDN();
      case TYPE_VOICE:
        return isVoice();
      case TYPE_MODEM:
        return isMODEM();
      case TYPE_PAGER:
        return isPager();
      case TYPE_VIDEO:
        return isVideo();
      case TYPE_CAR:
        return isCar();
      case TYPE_PCS:
        return isPCS();
      default:
        return false;
    }
  }//isType

  public void setPreferred(boolean b) {
    m_Preferred = b;
  }//setPreferred

  public boolean isPreferred() {
    return m_Preferred;
  }//isPreferred

}//class PhoneNumberImpl
