package org.jblooming.cursor.exceptions;

import org.jblooming.persistence.exceptions.PersistenceException;


public class CursorException extends PersistenceException {
  public CursorException() {
    super();
  }

  public CursorException(String s) {
    super(s);
  }

  public CursorException(Throwable ex) {
    super(ex);
  }

  /*
  public CursorException( String msg, Throwable ex) {
    //super( msg , ex );
    this( errorString(msg,ex) );
  }

*/

}
