/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.ericdaugherty.mail.server.errors;

/**
 *
 * @author andreas
 */
public class VaultException extends Exception {

   /**
    * Creates a new instance of
    * <code>VaultException</code> without detail message.
    */
   public VaultException() {
   }

   /**
    * Constructs an instance of
    * <code>VaultException</code> with the specified detail message.
    *
    * @param msg the detail message.
    */
   public VaultException(String msg) {
      super(msg);
   }
}
