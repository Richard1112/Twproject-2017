package org.jblooming.persistence.exceptions;

public class PersistenceException extends Exception {

  public PersistenceException() {
    super();
  }

  public PersistenceException(String s) {
    super(s);
  }

  public PersistenceException(Exception e) {
    super(e);
  }

  public PersistenceException(String s, Exception ex) {
    super(s, ex);
  }

  public PersistenceException(Throwable ex) {
    super(ex);
  }

}
