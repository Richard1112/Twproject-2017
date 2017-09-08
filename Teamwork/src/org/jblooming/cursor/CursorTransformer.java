package org.jblooming.cursor;

import org.jblooming.cursor.exceptions.CursorException;

import java.util.*;

/*
* @deprecated use org.jblooming.cursor.Page
*/

class CursorTransformer {

  protected static List toList(Cursor c) {
    return (List) buildCollection(c, new LinkedList());
  }

  protected static Set toSet(Cursor c) {
    return (Set) buildCollection(c, new TreeSet());
  }


  protected static Set toSet(Cursor c, Comparator comparator) {
    return (Set) buildCollection(c, new TreeSet(comparator));
  }

  private static Collection buildCollection(Cursor c, Collection coll) {
    try {
      int pageCount = c.getPageCount();

      for (int i = 0; i < pageCount; ++i) {
        Iterator e = c.getPage(i);
        while (e.hasNext())
          coll.add(e.next());
      }

      return coll;
    } catch (CursorException se) {
      throw new Error(se.getMessage());
    }
  }

}
