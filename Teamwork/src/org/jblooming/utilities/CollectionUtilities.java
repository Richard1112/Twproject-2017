package org.jblooming.utilities;

import java.util.*;

/**
 * @author Pietro Polsinelli : ppolsinelli@open-lab.com
 */
public class CollectionUtilities {
  public static void addIntValue(Map map, Object key, int value) {
    Object o = map.get(key);
    if (o == null)
      map.put(key, new Integer(value));
    else
      map.put(key, new Integer(((Integer) o).intValue() + value));
  }

  public static void addIntPair(Map map, Object key, int value1, int value2) {
    Object o = map.get(key);
    List pair;
    if (o == null) {
      pair = new ArrayList();
      pair.add(new Integer(value1));
      pair.add(new Integer(value2));
      map.put(key, pair);
    } else {
      pair = (List) o;
      List newPair = new ArrayList();
      int new1 = ((Integer) pair.get(0)).intValue() + value1;
      int new2 = ((Integer) pair.get(1)).intValue() + value2;
      newPair.add(new Integer(new1));
      newPair.add(new Integer(new2));
      map.put(key, newPair);
    }
  }

  public static List list(Iterator ite) {
    ArrayList ret = new ArrayList();
    while (ite.hasNext()) {
      Object o = ite.next();
      ret.add(o);
    }
    return ret;
  }

  public static String[] addElementToEndOfArray(String[] attributes, String element) {
    String[] attributesPlus = new String[attributes.length + 1];
    System.arraycopy(attributes, 0, attributesPlus, 0, attributes.length);
    attributesPlus[attributes.length] = element;
    attributes = attributesPlus;
    return attributes;
  }

  public static Object[] addElementToEndOfArray(Object[] attributes, Object element) {
    Object[] attributesPlus = new Object[attributes.length + 1];
    System.arraycopy(attributes, 0, attributesPlus, 0, attributes.length);
    attributesPlus[attributes.length] = element;
    attributes = attributesPlus;
    return attributes;
  }

  public static Object[] mergeTwoObjectArrays(Object[] pa, Object[] pb) {

    if (pa == null)
      return pb;

    if (pb == null)
      return pa;

    Object[] arr = new Object[pa.length + pb.length];
    for (int x = 0; x < pa.length; x++) {
      arr[x] = pa[x];
    }
    for (int x = 0; x < pb.length; x++) {
      arr[x + pa.length] = pb[x];
    }
    return arr;
  }

  public static String[] mergeTwoStringArrays(String[] pa, String[] pb) {

    if (pa == null)
      return pb;

    if (pb == null)
      return pa;

    String[] arr = new String[pa.length + pb.length];
    for (int x = 0; x < pa.length; x++) {
      arr[x] = pa[x];
    }
    for (int x = 0; x < pb.length; x++) {
      arr[x + pa.length] = pb[x];
    }
    return arr;
  }


  public static List resizableList(Object[] array) {
    List tmp = new ArrayList();
    for (int k = 0; k < array.length; k++)
      tmp.add(array[k]);
    return tmp;
  }

  public static  <T> List intersection(Collection <T>  a, Collection <T>  b) {
    List <T>intersection = new ArrayList<T>();
    if (a != null && b != null) {
      for (T  o : a) {
        if (b.contains(o))
          intersection.add(o);
      }
    }
    return intersection;
  }


   /**
   * Filters the src collection and puts the objects matching the
   * clazz into the dest collection.
   */
  public static <T> void filter(Class<T> clazz,
                                Collection<?> src,
                                Collection<T> dest) {
    for (Object o : src) {
      if (clazz.isInstance(o)) {
        dest.add(clazz.cast(o));
      }
    }
  }

  /**
   * Filters the src collection and puts all matching objects into
   * an ArrayList, which is then returned.
   */
  public static <T> Collection<T> filter(Class<T> clazz,Collection<?> src) {
    Collection<T> result = new ArrayList<T>();
    filter(clazz, src, result);
    return result;
  }

  public static <T> List<T> toList(T... arr) {
    List<T> list = new ArrayList<T>();
    for (T t : arr) list.add(t);
    return list;
  }

  public static <T> Set<T> toSet(T... arr) {
    Set<T> set = new HashSet<T>();
    for (T t : arr) set.add(t);
    return set;
  }

  public static <T> Set<T> toListSet(T... arr) {
    Set<T> set = new LinkedHashSet<T>();
    for (T t : arr) set.add(t);
    return set;
  }


}
