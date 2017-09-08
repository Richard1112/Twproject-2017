package org.jblooming.persistence.exceptions;


public class RemoveException extends PersistenceException {

  public RemoveException() {
    super();
  }

  public RemoveException(String s) {
    super(s);
  }

  public RemoveException(Exception e) {
    super(e);
  }

  public RemoveException(String s, Exception ex) {
    super(s, ex);
  }

  public RemoveException(Throwable ex) {
    super(ex);
  }
}
