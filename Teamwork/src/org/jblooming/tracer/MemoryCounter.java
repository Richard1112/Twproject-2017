package org.jblooming.tracer;

import org.jblooming.ontology.Identifiable;
import org.jblooming.waf.SessionState;

import java.lang.reflect.Field;
import java.lang.reflect.Array;
import java.lang.reflect.Modifier;
import java.util.Map;
import java.util.Stack;
import java.util.IdentityHashMap;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Apr 2, 2007
 * Time: 10:24:08 AM
 */
/**
 * This class can estimate how much memory an Object uses.  It is
 * fairly accurate for JDK 1.4.2.  It is based on the newsletter #29.
 */
public class MemoryCounter {
  
  private static final MemorySizes sizes = new MemorySizes();
  private final Map visited = new IdentityHashMap();
  private final Stack stack = new Stack();

  public synchronized long estimate(Object obj, boolean includeStatic) {
    assert visited.isEmpty();
    assert stack.isEmpty();
    long result = _estimate(obj,includeStatic);
    while (!stack.isEmpty()) {
      Object localObj = stack.pop();
      long localEstimate = _estimate(localObj,includeStatic);
      result += localEstimate;

    }
    visited.clear();
    return result;
  }

  private boolean skipObject(Object obj) {
    if (obj instanceof String) {
      // this will not cause a memory leak since
      // unused interned Strings will be thrown away
      if (obj == ((String) obj).intern()) {
        return true;
      }
    }
    return (obj == null)
        || visited.containsKey(obj);
  }

  private long _estimate(Object obj, boolean includeStatic) {
    if (skipObject(obj)) return 0;
    visited.put(obj, null);
    long result = 0;
    Class clazz = obj.getClass();
    if (clazz.isArray()) {
      return _estimateArray(obj,includeStatic);
    }
    while (clazz != null) {
      Field[] fields = clazz.getDeclaredFields();
      for (int i = 0; i < fields.length; i++) {
        if (includeStatic || !Modifier.isStatic(fields[i].getModifiers())) {
          if (fields[i].getType().isPrimitive()) {
            result += sizes.getPrimitiveFieldSize(
                fields[i].getType());
          } else {
            result += sizes.getPointerSize();
            fields[i].setAccessible(true);
            try {
              //if (localEstimate > 1024)
              //  System.out.println(localObj.getClass().getSimpleName()+ " "+Tracer.objectSize(localEstimate));
              //System.out.println(fields[i].getName());
              Object toBeDone = fields[i].get(obj);
              if (toBeDone != null && !(toBeDone instanceof Identifiable)) {
                stack.add(toBeDone);
              }
            } catch (IllegalAccessException ex) { assert false; }
          }
        }
      }
      clazz = clazz.getSuperclass();
    }
    result += sizes.getClassSize();
    long l = roundUpToNearestEightBytes(result);
    return l;
  }

  private long roundUpToNearestEightBytes(long result) {
    if ((result % 8) != 0) {
      result += 8 - (result % 8);
    }
    return result;
  }

  protected long _estimateArray(Object obj, boolean includeStatic) {
    long result = 16;
    int length = Array.getLength(obj);
    if (length != 0) {
      Class arrayElementClazz = obj.getClass().getComponentType();
      if (arrayElementClazz.isPrimitive()) {
        result += length *
            sizes.getPrimitiveArrayElementSize(arrayElementClazz);
      } else {
        for (int i = 0; i < length; i++) {
          result += sizes.getPointerSize() +
              _estimate(Array.get(obj, i),includeStatic);
        }
      }
    }
    return result;
  }
}
