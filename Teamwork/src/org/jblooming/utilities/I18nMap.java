package org.jblooming.utilities;

import org.jblooming.tracer.Tracer;

import java.util.Locale;


public class I18nMap extends HashTable {

  public static boolean alwaysInternazionalize = true;
  public static boolean useFallBackLocale = false;
  public static Locale fallBackLocale = Locale.ENGLISH;


  public synchronized Object put(Object key, Object value) {
    if (this.keySet().contains(key)) {
      //throw new PlatformRuntimeException("This map doesn't support duplicated put for key.\nKey : "+key + " Value: " + value);
      Tracer.i18nLogger.error("Duplicated put for Key : " + key + " Value: " + value);
    }
    return super.put(key, value);
  }

  public synchronized Object get(Object key) {
    String skey = (String) key;
    String message = (String) super.get(skey);
    if (message == null) {
      if (useFallBackLocale && fallBackLocale != null) {
        final int i = skey.lastIndexOf(".");
        if (i > 0)
          message = (String) super.get(skey.substring(0, i) + '.' + fallBackLocale.getLanguage());
      }
      if (message == null)
        message = (alwaysInternazionalize ? "? " : "") + skey;
      Tracer.i18nLogger.warn("Missing i18n entry for: " + skey);
    }
    return message;
  }


}
