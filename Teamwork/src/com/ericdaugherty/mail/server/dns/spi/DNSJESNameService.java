/******************************************************************************
 * This program is a 100% Java Email Server.
 ******************************************************************************
 * Copyright (c) 2001-2013, Eric Daugherty (http://www.ericdaugherty.com)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the copyright holder nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 ******************************************************************************
 * For current versions and more information, please visit:
 * http://javaemailserver.sf.net/
 *
 * or contact the author at:
 * andreaskyrmegalos@hotmail.com
 *
 ******************************************************************************
 * This program is based on the CSRMail project written by Calvin Smith.
 * http://crsemail.sourceforge.net/
 ******************************************************************************
 *
 * $Rev: 322 $
 * $Date: 2013-08-28 18:36:20 +0200 (Wed, 28 Aug 2013) $
 *
 ******************************************************************************/

package com.ericdaugherty.mail.server.dns.spi;

//Java import
import java.lang.reflect.Method;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.Locale;

//Other import
import org.xbill.DNS.spi.DNSJavaNameService;

//Local import
import com.ericdaugherty.mail.server.configuration.ConfigurationManager;

/**
 *
 * @author Andreas Kyrmegalos
 */
public class DNSJESNameService extends DNSJavaNameService{
   
   private static ConfigurationManager cm;
   private static final Locale locale = Locale.ENGLISH;
   private static final String dnsSearch = System.getProperty("dns.search", "");
   private static final String hostname;
   static {
      
      String aHostname;
      try {
         aHostname = InetAddress.getLocalHost().getHostName();
      } catch (Exception e) {
         aHostname = System.getenv("COMPUTERNAME");
         if (aHostname == null)
            aHostname = System.getenv("HOSTNAME");
         if (aHostname == null)
            aHostname = "localhost";
      }
      hostname = aHostname;
   }
   
   @Override
   public InetAddress[] lookupAllHostAddr(String host) throws UnknownHostException {
      
      if (cm == null) {
         cm = ConfigurationManager.getInstance();
      }
      if (host == null) {
         return new InetAddress[]{InetAddress.getByName(null)};
      }
      String[] labels = host.split("\\.");
      if (labels.length == 1) {
         if (host.toLowerCase(locale).equals("localhost")) {
            return new InetAddress[]{InetAddress.getByName(null)};
         } else if (hostname.equalsIgnoreCase(host)) {
            if (hostname.equals("localhost")
                  || dnsSearch.isEmpty()
                  || dnsSearch.split("\\.").length < 2) { 
               return new InetAddress[]{InetAddress.getByName(null)};
            }
         }
      }
      return super.lookupAllHostAddr(host);
   }

   @Override
   public String getHostByAddr(byte [] addr) throws UnknownHostException {

      if (cm == null) {
         cm = ConfigurationManager.getInstance();
      }
      if (addr.length==0||Arrays.equals(addr, new byte[]{0,0,0,0})) {
         return cm.isIPv6Preferred()?"::":"0.0.0.0";
      }
      String host = super.getHostByAddr(addr);
      if (host.endsWith(".")) {
         host = host.substring(0, host.length() - 1);
      }
      return host;
   }
}
