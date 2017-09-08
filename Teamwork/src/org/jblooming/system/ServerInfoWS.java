package org.jblooming.system;

import org.jblooming.PlatformRuntimeException;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Dec 13, 2006
 * Time: 12:19:45 PM
 */
public class ServerInfoWS {

  public static String systemPropsWS() {
     try {
       return new ServerInfo().systemProps(true).toString();
     } catch (Exception e) {
       throw new PlatformRuntimeException();
     }
   }
  
}
