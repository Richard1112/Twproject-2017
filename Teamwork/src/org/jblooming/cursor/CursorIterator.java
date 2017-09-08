package org.jblooming.cursor;

import org.jblooming.cursor.exceptions.CursorException;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * @deprecated use org.jblooming.cursor.Page
 */
class CursorIterator implements Iterator {
  protected Cursor cursor;
  protected boolean started = false;

  protected CursorIterator(Cursor c) {
    setCursor(c);
  }

  public boolean hasNext() {
    try {
      if (cursor.getElementCount() <= 0)
        return false;
      int pos = getAbsolutePosition();
      int ref = (cursor.getElementCount() - (started ? 1 : 0));
      return pos < ref;
    } catch (CursorException e) {
      throw new Error(e.getMessage());
    }
  }

  public void remove() {
    throw new UnsupportedOperationException();
  }

  public Object next() {
    if (!hasNext())
      throw new NoSuchElementException();
    try {
      if (started) {
        return cursor.nextElement();
      } else {
        started = true;
        cursor.getPage(0);
        return cursor.firstElement();
      }
    } catch (CursorException e) {
      throw new Error(e.getMessage());
    }
  }

  protected void restartCursor() {
    started = false;
  }

  protected void setCursor(Cursor c) {
    setCursor(c, true);
  }

  protected void setCursor(Cursor c, boolean restart) {
    cursor = c;
    if (restart)
      restartCursor();
  }

  protected int getAbsolutePosition() throws CursorException {
    int pos = cursor.getPageElementCount() * cursor.getCurrentPageNumber() + cursor.getCurrentElementNumber();
    return pos;
  }

}

