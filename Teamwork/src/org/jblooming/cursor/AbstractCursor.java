package org.jblooming.cursor;

import org.jblooming.cursor.exceptions.CursorException;
import org.jblooming.tracer.Tracer;

import java.util.*;

/**
 * Page number starts at 0 ends at (getPageCount() - 1)
 */

/**
 * Author: Andrea Costantinis (costantinis@remsoftware.it)
 * modified by: Pietro Polsinelli (dev@open-lab.com)
 * Date: October 2003
 * Date modified: March 2003
 */

public abstract class AbstractCursor implements Cursor, java.io.Serializable {

  protected int pageElementCount = 20;
  protected int currentPage;
  protected int currentElement = 0;

  public AbstractCursor() {
    currentPage = 0;
    currentElement = 0;
  }

  public AbstractCursor(int pageSize) {
    this();
    this.setPageElementCount(pageSize);
  }

  /**
   * @throws java.lang.IllegalArgumentException
   *          if the argument is greater than getElementCount().
   */
  public void setPageElementCount(int d) {
    int oldSize = pageElementCount;
    if (d < 1)
      pageElementCount = 1;
    else
      pageElementCount = d;

    currentPage = (oldSize * currentPage) / pageElementCount;

    currentElement = 0;
    invalidateBuffer();
  }

  public void invalidateBuffer() {
  }

  public int getPageElementCount() throws CursorException {
    return pageElementCount;
  }


  /**
   * Return current position
   */
  public int getCurrentPageNumber() throws CursorException {
    return currentPage;
  }


  /**
   * Set current position
   *
   * @throws java.lang.IndexOutOfBoundsException
   *          if the element number is greater than getPageCount().
   */
  public void setCurrentPageNumber(int i) throws CursorException {
    if (i >= getPageCount() || i < 0) throw new IndexOutOfBoundsException();
    if (currentPage != i)
      invalidateBuffer();
    currentPage = i;
  }

  /**
   * Return the actual number of pages
   */
  public int getPageCount() throws CursorException {
    if (getPageElementCount() == 0) return 0;
    if ((getElementCount() % getPageElementCount()) == 0)
      return (getElementCount() / getPageElementCount());
    else
      return ((getElementCount() / getPageElementCount()) + 1);
  }


  /**
   * Return the actual number of elements in the current page
   */
  public int getCurrentPageElementCount() throws CursorException {
    if (currentPage < getPageCount() - 1)
      return pageElementCount;
    else
      return (getElementCount() - 1) % pageElementCount + 1;
  }

  /**
   * Return current element position in the page
   */
  public int getCurrentElementNumber() throws CursorException {
    return currentElement;
  }


  public Iterator getPage() throws CursorException {
    return getPage(currentPage);
  }

  public Iterator firstPage() throws CursorException {
    if (getPageCount() >= 1)
      return getPage(0);
    else
      throw new NoSuchElementException();
  }

  public Iterator lastPage() throws CursorException {
    if (getPageCount() >= 1)
      return getPage(getPageCount() - 1);
    else
      throw new NoSuchElementException();
  }

  public Iterator nextPage() throws CursorException {
    if ((currentPage + 1) < getPageCount())
      return getPage(currentPage + 1);
    else
      throw new NoSuchElementException();
  }

  public boolean hasNextPage() throws CursorException {
    if ((currentPage + 1) < getPageCount())
      return true;
    else
      return false;
  }

  public Iterator previousPage() throws CursorException {
    if ((currentPage - 1) >= 0) {
      return getPage(currentPage - 1);
    } else {
      throw new NoSuchElementException();
    }
  }

  public boolean hasPreviousPage() {
    if ((currentPage - 1) >= 0)
      return true;
    else
      return false;
  }


  public Object getElement() throws CursorException {
    return getElement(currentElement);
  }

  public Object lastElement() throws CursorException {
    return getElement(getCurrentPageNumber() * getPageElementCount() + getCurrentPageElementCount() - 1);
  }

  public Object firstElement() throws CursorException {
    return getElement(getCurrentPageNumber() * getPageElementCount());
  }

  public Object nextElement() throws CursorException {
    if (currentElement >= getCurrentPageElementCount() - 1) {
      nextPage();
      return getElement();
    } else {
      currentElement++;
      return getElement();
    }
  }

  public Object prevElement() throws CursorException {
    if (currentElement <= 0) {
      previousPage();
      currentElement = getCurrentPageElementCount() - 1;
      return getElement();
    } else {
      currentElement--;
      return getElement();
    }
  }

  abstract protected int seek(Object o) ;

  public Object find(Object key)  throws CursorException{
    if (getElementCount() == 0)
      throw new NoSuchElementException();
    int offset = seek(key);
    getPage(offset / getPageElementCount());
    return getElement(offset % getPageElementCount());
  }

  protected Cursor listUnion(List l, Cursor c) {
    CursorIterator ci = new CursorIterator(c);
    List li = CursorTransformer.toList(this);
    while (ci.hasNext()) {
      li.add(ci.next());
    }
    return new ListCursor(li);
  }

  protected Cursor listIntersection(List l, Cursor c)  {
    Collection co = CursorTransformer.toList(c);
    co.retainAll(l);
    return new ListCursor(co);
  }

  protected Cursor listDifference(List l, Cursor c) {
    Collection co = CursorTransformer.toList(c);
    co.addAll(l); //this sequance is used couse we dont want o modify list l
    co.removeAll(l);
    return new ListCursor(co);
  }

  public List toList() {
    return CursorTransformer.toList(this);
  }

  public Set toSet() {
    return CursorTransformer.toSet(this);
  }

  public Set toSet(Comparator c) {
    return CursorTransformer.toSet(this, c);
  }

  public Iterator iterator() {

    Iterator i = Collections.EMPTY_LIST.iterator();

    try {
      this.setPageElementCount(this.getElementCount());
      i = getPage();
    } catch (CursorException e) {
      Tracer.platformLogger.error(e);
    }

    return i;
  }

}
