package org.jblooming.persistence.exceptions;

public class FindByPrimaryKeyException extends PersistenceException {

  public FindByPrimaryKeyException() {
    super();
  }

  public FindByPrimaryKeyException(String s) {
    super(s);
  }

  public FindByPrimaryKeyException(Exception e) {
    super(e);
  }

  public FindByPrimaryKeyException(String s, Exception e) {
    super(s, e);
  }
}
