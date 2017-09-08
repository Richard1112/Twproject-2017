package org.jblooming.tracer;

import java.util.Map;
import java.util.IdentityHashMap;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Apr 2, 2007
 * Time: 10:14:10 AM
 */
public class MemorySizes {
  private final Map primitiveSizes = new IdentityHashMap() {
    {
      put(boolean.class, new Integer(1));
      put(byte.class, new Integer(1));
      put(char.class, new Integer(2));
      put(short.class, new Integer(2));
      put(int.class, new Integer(4));
      put(float.class, new Integer(4));
      put(double.class, new Integer(8));
      put(long.class, new Integer(8));
    }
  };
  public int getPrimitiveFieldSize(Class clazz) {
    return ((Integer) primitiveSizes.get(clazz)).intValue();
  }
  public int getPrimitiveArrayElementSize(Class clazz) {
    return getPrimitiveFieldSize(clazz);
  }
  public int getPointerSize() {
    return 4;
  }
  public int getClassSize() {
    return 8;
  }
}
