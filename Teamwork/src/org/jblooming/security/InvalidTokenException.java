package org.jblooming.security;

public class InvalidTokenException extends SecurityException{


  public InvalidTokenException() {
    super();
  }

  public InvalidTokenException(String s) {
    super(s);
  }
  
}
