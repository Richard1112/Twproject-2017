package org.jblooming;

public class InitializationRuntimeException extends RuntimeException {

  String mess;
  public String recoveringErrorURL="";

  public InitializationRuntimeException(String message) {
    super(message);
    this.mess=message;
  }

  public InitializationRuntimeException(Throwable cause) {
    this(cause.getMessage());
  }

  public String getMessage(){
    return mess;
  }
}
