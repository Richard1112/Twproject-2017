package org.jblooming.ontology;

import org.jblooming.utilities.StringUtilities;

import java.io.Serializable;
import java.util.List;
import java.util.LinkedList;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class SerializedList<K> extends LinkedList<K> implements Serializable {

  public static final String META_SEPARATOR = "$~$";

  public static SerializedList deserialize(String serObj) {

    SerializedList sm = new SerializedList();
    if (serObj != null) {
      List<String> params = StringUtilities.splitToList(serObj, SerializedList.META_SEPARATOR);
      for (String key : params) {
        if (key != null && key.trim().length() > 0) {
          sm.add(key);
        }
      }
    }
    return sm;
  }


  public String serialize() {

    StringBuffer sb = new StringBuffer();
    boolean isFirst = true;
    for (Object k : this) {
      if (!isFirst) {
        sb.append(SerializedList.META_SEPARATOR);
      } else
        isFirst = false;
      sb.append((k+"").replace('~','-'));
    }
    return sb.toString();
  }

  public String toString() {
    return serialize();
  }  


}
