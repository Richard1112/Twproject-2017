package org.jblooming.ontology;

import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.settings.ApplicationState;

import java.io.Serializable;
import java.util.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class SerializedMap<K,V> extends HashTable<K,V> implements Serializable {

  public static final String META_SEPARATOR = "$~$";
  public static final String SEPARATOR = "~~";

  public static SerializedMap deserialize(String serObj) {

    SerializedMap sm = new SerializedMap();
    if (serObj != null) {
      List<String> paramPairs = StringUtilities.splitToList(serObj, META_SEPARATOR);
      for (String pair : paramPairs) {
        if (pair != null && pair.trim().length() > 0) {
          List<String> labelValue = StringUtilities.splitToList(pair, SEPARATOR);
          String label = labelValue.get(0);
          String value = labelValue.get(1);
          sm.put(label, value);
        }
      }
    }
    return sm;
  }


  public String serialize() {

    StringBuffer sb = new StringBuffer();
    boolean isFirst = true;
    for (Object k : keySet()) {
      if (!isFirst) {
        sb.append(META_SEPARATOR);
      } else
        isFirst = false;
      Object value = get(k);
      sb.append(k).append(SEPARATOR).append((value+"").replace('~','-'));
    }
    return sb.toString();
  }

  public V put(K key, V value) {
    if (value == null)
      return super.put(key, (V) "");
     else
      return super.put(key, value);
  }

  public String toString() {
    return serialize();
  }


}
