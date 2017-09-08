package org.jblooming.utilities;

import java.util.HashMap;
import java.util.Set;
import java.util.Map;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class HashTable<K,V> extends HashMap<K,V> {

  public V put(K key, V value) {
    if (key == null || value == null)
      throw new RuntimeException("Cannot put null key or value as in Hashtable");
    return super.put(key, value);
  }


}
