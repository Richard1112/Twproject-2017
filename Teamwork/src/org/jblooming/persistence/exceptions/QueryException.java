package org.jblooming.persistence.exceptions;


public class QueryException extends PersistenceException {

  public QueryException(Exception e) {
    super(e);
  }

  public QueryException() {
    super();
  }

  public QueryException(String s) {
    super(s);
  }

}
