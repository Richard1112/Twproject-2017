package org.jblooming.ontology;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Oct 26, 2007
 * Time: 10:16:39 AM
 */
public class Pair<A, B> {

  public A first;
  public B second;

  public Pair() {
  }

  public Pair(A obj, B obj1) {
    first = obj;
    second = obj1;
  }

  public String toString() {
    return (new StringBuilder()).append("Pair[").append(first).append(",").append(second).append("]").toString();
  }

  private static boolean equals(Object obj, Object obj1) {
    return obj == null && obj1 == null || obj != null && obj.equals(obj1);
  }

  public boolean equals(Pair pair) {
    return (pair instanceof Pair) && equals(first, (pair).first) && equals(second, (pair).second);
  }

  public int hashCode() {
    if (first == null)
      return second != null ? second.hashCode() + 1 : 0;
    if (second == null)
      return first.hashCode() + 2;
    else
      return first.hashCode() * 17 + second.hashCode();
  }
}
