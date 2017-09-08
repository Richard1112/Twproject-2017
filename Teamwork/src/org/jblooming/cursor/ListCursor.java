package org.jblooming.cursor;

import org.jblooming.cursor.exceptions.CursorException;

import java.util.*;


/**
 * @deprecated use org.jblooming.cursor.Page
 * Page number starts at 0 ends at (getPageCount() - 1)
 */


/**
 * Author: Andrea Costantinis (costantinis@remsoftware.it)
 * Date: October 2003
 * Time: 12:00:00
 */

public class ListCursor extends AbstractCursor {

  protected List objects;

  public ListCursor() {
    super();
    objects = new LinkedList();
  }

  public ListCursor(Collection objs) {
    this(objs, 20);
  }

  public ListCursor(Collection objs, int pageSize) {
    super(pageSize);
    objects = new LinkedList();
    objects.addAll(objs);
  }

  /**
   * @throws java.lang.IndexOutOfBoundsException
   *          if the element number is greater than getPageCount().
   */
  public Iterator getPage(int i) throws CursorException {
    List chunk = getChunk(i);
    currentPage = i;
    currentElement = 0;
    return chunk.iterator();
  }

  /**
   * @throws java.lang.IndexOutOfBoundsException
   *          if the element number is greater than getPageCount().
   */
  public List getChunk(int i) throws CursorException {
    if (i >= getPageCount() || i < 0) {
      if (getElementCount() > 0)
        throw new IndexOutOfBoundsException();
      else
        return Collections.EMPTY_LIST;
    }

    int fromIndex = getPageElementCount() * currentPage;
    int toIndex = fromIndex + getPageElementCount();
    if (toIndex > (objects.size() - 1)) toIndex = objects.size();
    return objects.subList(fromIndex, toIndex);
  }

  public Object getElement(int i) throws CursorException {
    if (i >= getCurrentPageElementCount() || i < 0) throw new IndexOutOfBoundsException();
    currentElement = i;
    return objects.get(getCurrentPageNumber() * getPageElementCount() + i);
  }

  public int getElementCount() {
    if (objects == null)
      return 0;
    else
      return objects.size();
  }

  protected int seek(Object o) {
    ListIterator iterator = objects.listIterator();
    while (iterator.hasNext()) {
      int index = iterator.nextIndex();
      if (iterator.next().equals(o))
        return index;
    }
    throw new NoSuchElementException();
  }

  public Cursor union(Cursor c) throws CursorException {
    return listUnion(objects, c);
  }

  public Cursor intersection(Cursor c) throws CursorException {
    return listIntersection(objects, c);
  }

  public Cursor difference(Cursor c) throws CursorException {
    return listDifference(objects, c);
  }

  public boolean contains(Object o) {
    return objects.contains(o);
  }
}
