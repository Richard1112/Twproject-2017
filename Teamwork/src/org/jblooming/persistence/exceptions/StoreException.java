package org.jblooming.persistence.exceptions;


public class StoreException extends PersistenceException {

  public StoreException() {
    super();
  }

  public StoreException(String s) {
    super(s);
  }

  public StoreException(Exception e) {
    super(e);
  }

  public StoreException(Throwable e) {
    super(e);
  }

  public StoreException(String s, Exception ex) {
    super(s, ex);
  }
}
