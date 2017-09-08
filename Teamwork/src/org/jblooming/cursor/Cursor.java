package org.jblooming.cursor;

import org.jblooming.cursor.exceptions.CursorException;

import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * @deprecated use org.jblooming.cursor.Page
 *             Page number starts at 0 ends at (getPageCount() - 1)
 */

public interface Cursor {
  /**
   * Set the number of objects per page
   *
   * @throws java.lang.IllegalArgumentException
   *          if the argument
   *          is greater than getElementCount().
   */
  public void setPageElementCount(int d) throws CursorException;

  /**
   * Get the number of objects per page
   */
  public int getPageElementCount() throws CursorException;

  /**
   * Return the actual number of elements in current page
   * (they can be less than getPageElementCount() on last page)
   */
  public int getCurrentPageElementCount() throws CursorException;


  /**
   * Return number of pages in function of getPageElementCount()
   */
  public int getPageCount() throws CursorException;

  /**
   * Return cursor size (i.e. contained objects count)
   */
  public int getElementCount() throws CursorException;

  /**
   * Return current page
   */
  public int getCurrentPageNumber() throws CursorException;

  /**
   * Set current page
   *
   * @throws java.lang.IndexOutOfBoundsException
   *          if
   *          the element number is greater than getPageCount().
   */
  public void setCurrentPageNumber(int i) throws CursorException;

  /**
   * @throws java.lang.IndexOutOfBoundsException
   *          if the
   *          element number is greater than getPageCount().
   */
  public Iterator getPage(int i) throws CursorException;

  public Iterator getPage() throws CursorException;

  public Iterator firstPage() throws CursorException;

  public Iterator lastPage() throws CursorException;

  public Iterator nextPage() throws CursorException;

  public boolean hasNextPage() throws CursorException;

  public Iterator previousPage() throws CursorException;

  public boolean hasPreviousPage();

  public Cursor union(Cursor c) throws CursorException;

  public Cursor intersection(Cursor c) throws CursorException;

  public Cursor difference(Cursor c) throws CursorException;

  public List toList();

  public Set toSet();

  public Set toSet(Comparator c);

  /**
   * @return
   */
  public Iterator iterator();

  public boolean contains(Object o);

  /**
   * find an object in current cursor, Returns null if not found.
   */
  public Object find(Object key) throws CursorException;


  /**
   * Bunch of (deprecated) method used to shift cursor
   * line by line among pages.
   */

  /**
   * Returns current element position in current page
   *
   * @deprecated
   */
  public int getCurrentElementNumber() throws CursorException;


  /**
   * return current element
   *
   * @deprecated
   */
  public Object getElement() throws CursorException;


  /**
   * Returns the element at index i within current page
   *
   * @deprecated
   */
  public Object getElement(int i) throws CursorException;


  /**
   * Returns last element of current page
   *
   * @deprecated
   */
  public Object lastElement() throws CursorException;


  /**
   * Returns first element of current page
   *
   * @deprecated
   */
  public Object firstElement() throws CursorException;


  /**
   * Returns next element of the cursor,
   * changing the current page if needed
   *
   * @deprecated
   */
  public Object nextElement() throws CursorException;


  /**
   * Returns previous element of the cursor,
   * changing the current page if needed
   *
   * @deprecated
   */
  public Object prevElement() throws CursorException;


}
