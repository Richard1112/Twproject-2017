/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.ericdaugherty.mail.server.info;

/**
 *
 * @author Andreas Kyrmegalos
 */
public interface NetAddress {
   
   Domain getDomain();
   
   byte[] getIP();
}
