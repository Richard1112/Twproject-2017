/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.util.versitio;


public final class versitToken {

  public static final String VCARD = "VCARD";
  public static final String VCALENDAR = "VCALENDAR";
  public static final String START = "BEGIN";
  public static final String END = "END";
  public static final String VERSION = "VERSION";

  public static final String VCARD_VERSION_2 = "2.1";
  public static final String VCARD_VERSION_3 = "3.0";

  public static final String N = "N";
  public static final String FN = "FN";
  public static final String SORTSTRING = "SORT-STRING";
  public static final String NICKNAME = "NICKNAME";
  public static final String PHOTO = "PHOTO";
  public static final String BDAY = "BDAY";
  public static final String ADR = "ADR";
  public static final String LABEL = "LABEL";
  public static final String TEL = "TEL";
  public static final String EMAIL = "EMAIL";
  public static final String MAILER = "MAILER";
  public static final String TZ = "TZ";
  public static final String GEO = "GEO";
  public static final String TITLE = "TITLE";
  public static final String ROLE = "ROLE";
  public static final String LOGO = "LOGO";
  public static final String AGENT = "AGENT";
  public static final String ORG = "ORG";
  public static final String CATEGORIES = "CATEGORIES";
  public static final String NOTE = "NOTE";	//groupable
  public static final String REV = "REV";
  public static final String SOUND = "SOUND";
  public static final String URL = "URL";
  public static final String URI = "URI";
  public static final String CLASS = "CLASS";
  public static final String UID = "UID";
  public static final String KEY = "KEY";
  public static final String PRODID = "PRODID";

  //extensions
  public static final String EXTENSION_PREFIX = "X-";
  //jpim Extensions
  public static final String XORGURL = EXTENSION_PREFIX + "ORG-URL";

  public static final String DEFAULT = "DEFAULT";
  public static final String VALUE = "VALUE";
  public static final String ENCODING = "ENCODING";
  public static final String ENCODING_B = "B";
  public static final String ENCODING_BASE64 = "BASE64";
  public static final String ENCODING_QUOTEDPRINTABLE = "QUOTED-PRINTABLE";

  public static final String TYPE = "TYPE";
  public static final String TYPE_PREF = "PREF";	//adr,label,tel
  public static final String TYPE_DOM = "DOM";	//adr,label
  public static final String TYPE_INTL = "INTL"; 	//adr,label
  public static final String TYPE_POSTAL = "POSTAL";//adr,label
  public static final String TYPE_PARCEL = "PARCEL";//adr,label
  public static final String TYPE_HOME = "HOME";//adr,label,tel
  public static final String TYPE_WORK = "WORK";//adr,label,tel


  public static final String TYPE_VOICE = "VOICE";	//tel
  public static final String TYPE_FAX = "FAX";	//tel
  public static final String TYPE_MSG = "MSG";	//tel
  public static final String TYPE_CELL = "CELL";	//tel
  public static final String TYPE_PAGER = "PAGER";	//tel
  public static final String TYPE_BBS = "BBS";	//tel
  public static final String TYPE_MODEM = "MODEM";	//tel
  public static final String TYPE_CAR = "CAR";	//tel
  public static final String TYPE_ISDN = "ISDN"; //tel
  public static final String TYPE_VIDEO = "VIDEO";	//tel
  public static final String TYPE_PCS = "PCS"; //tel

  public static final String TYPE_INTERNET = "INTERNET";	//email
  public static final String TYPE_X400 = "X400";			//email
  public static final String TYPE_X509 = "X509";	//key
  public static final String TYPE_PGP = "PGP";		//key


  public final static String[] VCARD_ITEM_LIST = {
    N, FN, SORTSTRING, NICKNAME, PHOTO, BDAY, ADR, LABEL,
    TEL, EMAIL, MAILER, TZ, GEO, TITLE, ROLE,
    LOGO, AGENT, ORG, CATEGORIES, NOTE, REV, UID,
    SOUND, URL, CLASS, KEY
  };

}//class versitTokens
