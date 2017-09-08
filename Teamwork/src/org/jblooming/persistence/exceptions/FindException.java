package org.jblooming.persistence.exceptions;

public class FindException extends PersistenceException {

  public FindException() {
    super();
  }

  public FindException(String s) {
    super(s);
  }

  public FindException(Exception e) {
    super(e);
  }

  public FindException(String s, Exception e) {
    super(s, e);
  }
}
