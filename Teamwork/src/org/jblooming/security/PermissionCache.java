package org.jblooming.security;

import org.jblooming.ontology.Identifiable;
import org.jblooming.operator.Operator;
import org.jblooming.operator.User;

import java.util.LinkedHashMap;

/**
 * Created by roberto bicchierai on 09/05/2017.
 */
public class PermissionCache {

  public static interface PermissionCacheEnabled extends Securable, Identifiable {
    boolean hasPermissionForUnCached(User user, Permission permission);
  }

  public static long MAX_AGE = 30000;
  public static boolean USECACHE = true;

  public static long hitCached=0;
  public static long hitUnCached=0;

  private static LinkedHashMap<String, HasPermissionResult> cache = new LinkedHashMap<>();  //key= operatorId-classname-objid-permission   value


  public static boolean hasPermissionFor(User user, PermissionCacheEnabled securableObject, Permission permission) {
    if (!USECACHE)
      return securableObject.hasPermissionForUnCached(user, permission);

    String key = user.getId() + "-" + securableObject.getClass().getName() + "-" + securableObject.getId() + "-" + permission.getName();
    boolean ret;
    HasPermissionResult hasPermissionResult = cache.remove(key);  //si rimuove sempre dalla lista
    if (hasPermissionResult != null && System.currentTimeMillis() - hasPermissionResult.computedOn < MAX_AGE) {
      hitCached++;
      ret = hasPermissionResult.result;
      hasPermissionResult.computedOn = System.currentTimeMillis(); // si rinfresca la data
    } else {
      hitUnCached++;
      ret = securableObject.hasPermissionForUnCached(user, permission);
      hasPermissionResult = new HasPermissionResult(ret);
    }
    cache.put(key, hasPermissionResult);
    return ret;
  }


  public static boolean hasPermissionFor(User user, Permission permission) {
    if (!USECACHE)
      return user.hasPermissionForUnCached(permission);

    String key = user.getId() + Operator.class.getName() + user.getId() + permission.getName();

    boolean ret;
    HasPermissionResult hasPermissionResult = cache.remove(key);
    if (hasPermissionResult != null && System.currentTimeMillis() - hasPermissionResult.computedOn < MAX_AGE) {
      hitCached++;
      ret = hasPermissionResult.result;
      hasPermissionResult.computedOn = System.currentTimeMillis(); // si rinfresca la data
    } else {
      hitUnCached++;
      ret = user.hasPermissionForUnCached(permission);
      hasPermissionResult = new HasPermissionResult(ret);
    }
    cache.put(key, hasPermissionResult);
    return ret;
  }


  public static class HasPermissionResult {
    public long computedOn;
    public boolean result;

    HasPermissionResult(boolean result) {
      this.computedOn = System.currentTimeMillis();
      this.result = result;
    }
  }


  /**
   * svuota tutta la cache di tutti
   */
  public static synchronized void emptyCache() {
    cache.clear();
    hitUnCached=0;
    hitCached=0;
  }


  /**
   * toglie dalla cache gli oggetti scaduti
   */
  public static synchronized void clearCache() {
    while (true) {
      if (cache.size() <= 0)
        break;

      String key = cache.keySet().iterator().next();

      HasPermissionResult hasPermissionResult = cache.get(key);
      if (hasPermissionResult.computedOn < System.currentTimeMillis() - MAX_AGE)
        cache.remove(key);
      else
        break;
    }
  }

  public static int getCacheSize() {
    return cache.size();
  }


  /*  ------------------- implementazion con hashTable  ---------------------  sembra meno performante
  private static Hashtable<String, HasPermissionResult> cache = new Hashtable<>();  //key= operatorId-classname-objid-permission   value


  public static boolean hasPermissionFor(User user, PermissionCacheEnabled securableObject, Permission permission) {
    if (!USECACHE)
      return securableObject.hasPermissionForUnCached(user, permission);

    String key = user.getId() +"-" +securableObject.getClass().getSimpleName() + "-" +securableObject.getId() + "-" +permission.getName();

    boolean ret;
    HasPermissionResult hasPermissionResult = cache.get(key);
    if (hasPermissionResult != null && System.currentTimeMillis() - hasPermissionResult.computedOn < MAX_AGE) {
      ret = hasPermissionResult.result;
      System.out.print("+");
    } else {
      ret = securableObject.hasPermissionForUnCached(user, permission);
      HasPermissionResult result = new HasPermissionResult(ret);
      cache.put(key, result);
      System.out.print("-");
    }
    return ret;
  }


  public static boolean hasPermissionFor(User user, Permission permission) {
    if (!USECACHE)
      return user.hasPermissionForUnCached(permission);

    String key = user.getId() + Operator.class.getSimpleName() + user.getId() + permission.getName();

    boolean ret;
    HasPermissionResult hasPermissionResult = cache.get(key);
    if (hasPermissionResult != null && System.currentTimeMillis() - hasPermissionResult.computedOn < MAX_AGE) {
      ret = hasPermissionResult.result;
    } else {
      ret = user.hasPermissionForUnCached(permission);
      HasPermissionResult result = new HasPermissionResult(ret);
      cache.put(key, result);
    }
    return ret;
  }


  public static class HasPermissionResult {
    public long computedOn;
    public boolean result;

    HasPermissionResult(boolean result) {
      this.computedOn = System.currentTimeMillis();
      this.result = result;
    }

  }

  public static synchronized void clearCache(){
    return;
  }

  public static void emptyCache(){
    cache.clear();
  }


  public static int getCacheSize(){
    return cache.size();
  }

*/


}