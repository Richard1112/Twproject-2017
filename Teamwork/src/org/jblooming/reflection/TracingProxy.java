package org.jblooming.reflection;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class TracingProxy implements InvocationHandler {


  public static Object createProxy(Object obj) {
    return Proxy.newProxyInstance(obj.getClass().getClassLoader(),
      obj.getClass().getInterfaces(),
      new TracingProxy(obj));
  }

  private Object target;

  private TracingProxy(Object obj) {
    target = obj;
  }

  public Object invoke(Object proxy, Method method, Object[] args)
    throws Throwable {
    Object result = null;
    try {
      System.out.println(method.getName() + "(...) called");
      result = method.invoke(target, args);
    } catch (InvocationTargetException e) {
      System.out.println(method.getName() + " throws " + e.getCause());
      throw e.getCause();

    }
    System.out.println(method.getName() + " returns");
    return result;
  }
}
