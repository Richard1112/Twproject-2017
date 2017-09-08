package org.jblooming.utilities;

import org.jblooming.tracer.Tracer;


public class FinalKeyMap extends HashTable {

  public synchronized Object put(Object key, Object value) {
    if (this.keySet().contains(key)) {
      Tracer.platformLogger.warn("FinalKeyMap.put duplicated put for Key : \"" + key + "\" Value: \"" + value+"\"");
      //throw new PlatformRuntimeException("This map doesn't support duplicated put for key.\nKey : "+key + " Value: " + value);
      return super.put(key, value);
    }
    return super.put(key, value);
  }

}
