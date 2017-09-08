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
 * $Rev$
 * $Date$
 *
 ******************************************************************************/

package com.ericdaugherty.mail.server.services.smtp.server.support;

//Java Imports
import java.net.InetAddress;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantReadWriteLock;

//Logging Imports
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

//dnsjava Imports
import org.xbill.DNS.*;

//Local Imports
import com.ericdaugherty.mail.server.configuration.ConfigurationManager;
import com.ericdaugherty.mail.server.configuration.VerifyIPConfigurator;
import com.ericdaugherty.mail.server.configuration.VerifyIPConfigurator.DnsSubList;

/**
 * The factory that supplies info concerning opening a connection or dropping one.
 *
 * @author Andreas Kyrmegalos
 */
public class VerifyIPFactory {


   /** Logger Category for this class. */
   //private static Log log = LogFactory.getLog( VerifyIPFactory.class );
  private static Log log = LogFactory.getLog("JESLogger");

   // There is a chance that the dns search suffix allows any lookup to resolve
   // to its IP. If that is the case then verify the result against that IP
   // as well.
   private static InetAddress alternateVerifyIP;

   private static final ReentrantReadWriteLock rrwlock = new ReentrantReadWriteLock();
   
   private static InetAddress getAlternateVerifyIP() {

      rrwlock.readLock().lock();
      try {
         return alternateVerifyIP;
      }
      finally {
         rrwlock.readLock().unlock();
      }
   }

   /** Called by the Mail.class watchdog on every 3rd cycle to update the alternate verify IP. */
   public static void updateAlternateVerifyIP() {

      String alternate = System.getProperty("dns.search");
      if (alternate==null) {
         if (log.isDebugEnabled()) {
            log.debug("alternativeVerifyIP will be ignored");
         }
         return;
      }
      try {
         Lookup lookup = new Lookup(alternate, Type.A);
         lookup.setCache(null);
         Record[] record = lookup.run();
         InetAddress temp = record.length!=0?((ARecord)record[0]).getAddress():null;
         
         try {
            int count = 0;
            while(!(rrwlock.writeLock().tryLock()||rrwlock.writeLock().tryLock(200, TimeUnit.MILLISECONDS))){count++;if (count==25)return;};
         }
         catch (InterruptedException ie) {
            return;
         }
         try {
            alternateVerifyIP = temp;
         }
         finally {
            rrwlock.writeLock().unlock();
         }
      }
      catch (TextParseException e) {
         if (log.isDebugEnabled()) {
            log.debug("alternativeVerifyIP will be ignored");
         }
      }
   }

   public static synchronized VerifyIP getNewVerifyIPInstance(boolean useDummy, boolean useAmavisSMTPDirectory) {

      updateAlternateVerifyIP();

      if (useDummy) {
         return new Dummy(useAmavisSMTPDirectory);
      }
      if (ConfigurationManager.getInstance().isAmavisSupportActive()) {
         return new WithAmavisdInstance();
      }
      else {
         return new NoAmavisdInstance();
      }
   }

   private static class Dummy implements VerifyIP {
      
      private boolean useAmavisSMTPDirectory;
      
      private Dummy(boolean useAmavisSMTPDirectory) {
         this.useAmavisSMTPDirectory = useAmavisSMTPDirectory;
      }
      
      public boolean blockIP(String clientIP) {
         return false;
      }
   
      public boolean getUseAmavisDirectory() {
         return useAmavisSMTPDirectory;
      }
   }

   private static class NoAmavisdInstance implements VerifyIP {

      protected String result;

      public boolean blockIP(String clientIP) {
         if (log.isDebugEnabled()) {
            log.debug("verifying IP "+clientIP);
         }
         VerifyIPConfigurator vIP = VerifyIPConfigurator.getInstance();

         String reversedIP = reverseIP(clientIP);

         //WhiteList Stage (mandatory)
         DnsSubList list = vIP.getWhiteList();
         String hostname = list.getHostName();
         Lookup lookup = null;
         try {
            if (log.isDebugEnabled()) {
               log.debug("looking Up "+(reversedIP + hostname));
            }
            lookup = new Lookup(reversedIP + hostname, Type.A);
         } catch (TextParseException ex) {
            log.error("dnsjava unable to parse ", ex);
            result = bypassAmavis;
            return false;
         }
         Record [] records = null;
         int lookUpCount = 0;
         do {
            records = lookup.run();
            if (records == null) {
               int lookupResult = lookup.getResult();
               //If the list lookup responds with a "TRY_AGAIN" status
               //repeat the process.
               if (lookupResult == Lookup.TRY_AGAIN) {
                  lookUpCount++;
                  if (lookUpCount==5) {
                     if (log.isDebugEnabled()) {
                        log.debug("tried too many times to look up "+clientIP);
                     }
                     result = bypassAmavis;
                     return false;
                  }
                  log.error("Trying again to lookup "+(reversedIP + hostname));
                  try {
                     Thread.sleep(500);
                  } catch (InterruptedException ex) {}
               }
               else {
                  if (lookupResult != Lookup.HOST_NOT_FOUND) {
                     log.error("dnsjava result: "+lookup.getErrorString());
                  }
                  break;
               }
            }
            else {
               //Since this a whiteList and the lookup concerned A type records
               //if the array is empty then treat the ip as not listed and
               //continue with the process.
               if (records.length==0) break;
               //The array is not empty, simply check if the first entry is a
               //loopback address
               ARecord record = (ARecord)records[0];
               InetAddress verify = record.getAddress();
               if (verify.isLoopbackAddress()||verify.equals(getAlternateVerifyIP())) {
                  
                  log.debug("whitelisted "+clientIP);
                  result = bypassAmavis;
                  return false;
               }
               //Not a loopback address (perhaps the list hostname is wrong)
               log.error("please check if "+hostname+" is a valid list hostname");
               break;
            }
         }while(true);
         
         //MixedList Stage (optional)
         list = vIP.getMixedList();
         if (list !=null) {
            hostname = list.getHostName();
            lookup = null;
            try {
               log.debug("looking Up "+(reversedIP + hostname));
               lookup = new Lookup(reversedIP + hostname, Type.A);
            } catch (TextParseException ex) {
               log.error("dnsjava unable to parse ", ex);
               result = bypassAmavis;
               return false;
            }
            records = null;
            lookUpCount = 0;
mixedList:  do {
               records = lookup.run();
               if (records == null) {
                  int lookupResult = lookup.getResult();
                  //If the list lookup responds with a "TRY_AGAIN" status
                  //repeat the process.
                  if (lookupResult == Lookup.TRY_AGAIN) {
                     lookUpCount++;
                     if (lookUpCount==5) {
                        log.debug("tried too many times to look up "+clientIP);
                        result = bypassAmavis;
                        return false;
                     }
                     log.error("Trying again to lookup "+(reversedIP + hostname));
                     try {
                        Thread.sleep(500);
                     } catch (InterruptedException ex) {}
                  }
                  else {
                     if (lookupResult != Lookup.HOST_NOT_FOUND) {
                        log.error("dnsjava result: "+lookup.getErrorString());
                     }
                     break;
                  }
               }
               else {
                  //Since this a mixedList and the lookup concerned A type records
                  //if the array is empty then treat the ip as not listed and
                  //continue with the process.
                  if (records.length==0) break;
                  
                  //The array is not empty, validate the answers.
                  //Note that since this is a mixedList a reply can be either black
                  //or white but all replies must be of the same "color".
                  String recordReply, recordSeverity, severity = null;
                  int recordsCount = records.length;
                  ARecord record;
                  for (int i=0;i<recordsCount;i++) {
                     record = (ARecord)records[i];
                     recordReply = record.getAddress().getHostAddress();
                     if (!recordReply.startsWith("127")||!record.getAddress().equals(getAlternateVerifyIP())) {
                        log.error("A lookup reply is not a loopback address, check that hostname "+hostname+" is correct");
                        break mixedList;
                     }
                     boolean isIPv4 = recordReply.indexOf('.')!=-1;
                     if (list.getGeneral().containsKey(recordReply) || isListed(isIPv4, list.getGeneral(), recordReply.split(isIPv4?"\\.":":"))) {
                        recordSeverity = (String)list.getGeneral().get(recordReply);
                        if (severity==null) severity = recordSeverity;
                        else {
                           if ((severity.startsWith("W") && recordSeverity.startsWith("B")) ||
                                 (severity.startsWith("B") && recordSeverity.startsWith("W"))) {
                              log.error("mixedList returned both black and white replies");
                              break mixedList;
                           }
                           else {
                              severity = recordSeverity;
                           }
                        }
                     }
                  }
                  //Nothing listed or blacklisted, continue to the next step
                  if (severity==null || severity.startsWith("B")) break;
                  //Whitelisted, don't block the clientIP address
                  log.debug("whitelisted "+clientIP);
                  result = bypassAmavis;
                  return false;
               }
            }while(true);

         }

         //BlackList Stage
         list = vIP.getBlackList();
         hostname = list.getHostName();
         boolean useStrict = vIP.isStrictUsed();
         boolean blockIP = vIP.isBlockingIP();
         boolean relayAmavis = vIP.isAmavisRelayed();
         lookup = null;
         try {
            if (log.isDebugEnabled()) {
               log.debug("looking Up "+(reversedIP + hostname));
            }
            lookup = new Lookup(reversedIP + hostname, Type.A);
         } catch (TextParseException ex) {
            log.error("dnsjava unable to parse ", ex);
            result = relayAmavis?sendToAmavis:bypassAmavis;
            return false;
         }
         records = null;
         lookUpCount = 0;
         do {
            records = lookup.run();
            if (records == null) {
               int lookupResult = lookup.getResult();
               //If the list lookup responds with a "TRY_AGAIN" status
               //repeat the process.
               if (lookupResult == Lookup.TRY_AGAIN) {
                  lookUpCount++;
                  if (lookUpCount==5) {
                     log.debug("tried too many times to look up "+clientIP);
                     result = relayAmavis?sendToAmavis:bypassAmavis;
                     return false;
                  }
                  log.error("Trying again to lookup "+(reversedIP + hostname));
                  try {
                     Thread.sleep(500);
                  } catch (InterruptedException ex) {}
               }
               else {
                  if (lookupResult != Lookup.HOST_NOT_FOUND) {
                     log.error("dnsjava result: "+lookup.getErrorString());
                  }
                  result = relayAmavis?sendToAmavis:bypassAmavis;
                  return false;
               }
            }
            else {
               //Since this a blackList and the lookup concerned A type records
               //if the array is empty then treat the ip as not listed and
               //conclude the process.
               if (records.length==0) {
                  log.debug("not blacklisted "+clientIP);
                  result = relayAmavis?sendToAmavis:bypassAmavis;
                  return false;
               }

               //The array is not empty, validate the answers
               String recordReply, recordSeverity, severity = null;
               int recordsCount = records.length;
               ARecord record;
               for (int i=0;i<recordsCount;i++) {
                  record = (ARecord)records[i];
                  recordReply = record.getAddress().getHostAddress();
                  if (!recordReply.startsWith("127")||!record.getAddress().equals(getAlternateVerifyIP())) {
                     log.error("A lookup reply is not a loopback address, check that hostname "+hostname+" is correct");
                     result = relayAmavis?sendToAmavis:bypassAmavis;
                     return false;
                  }
                  boolean isIPv4 = recordReply.indexOf('.')!=-1;
                  if (list.getGeneral().containsKey(recordReply) || isListed(isIPv4, list.getGeneral(), recordReply.split(isIPv4?"\\.":":"))) {
                     recordSeverity = (String)list.getGeneral().get(recordReply);
                     if (severity==null) severity = recordSeverity;
                     else if (recordSeverity.length()>severity.length()){
                        severity = recordSeverity;
                     }
                  }
                  if (useStrict) {
                     if (list.getStrict().containsKey(recordReply) || isListed(isIPv4, list.getStrict(), recordReply.split(isIPv4?"\\.":":"))) {
                        recordSeverity = (String)list.getGeneral().get(recordReply);
                        if (severity==null) severity = recordSeverity;
                        else if (recordSeverity.length()>severity.length()){
                           severity = recordSeverity;
                        }
                     }
                  }
               }
               if (severity!=null) {
                  //If highest severity and admin requests IPs to be blocked
                  //then return true
                  if (severity.equals("BBB") && blockIP) {
                     log.debug("clientIP's "+clientIP+" reply has a maximum severity and the admin has requested that the IP be blocked");
                     result = relayAmavis?sendToAmavis:bypassAmavis;
                     return true;
                  }
                  //Blacklisted
                  log.debug("blacklisted "+clientIP);
                  result = relayAmavis?sendToAmavis:bypassAmavis;
                  return false;
               }
               //Not listed
               log.debug("not listed "+clientIP+" and "+
                     (relayAmavis?"the admin has requested that all messages are filtered through amavis":"allowed to bypass amavisd-new"));
               result = relayAmavis?sendToAmavis:bypassAmavis;
               return false;
            }
         }while(true);
      }

      private boolean isListed(boolean isIPv4Record, Map list, String[] recordReplyParts) {

         Iterator iter = list.keySet().iterator();
         String reply;
         String[] replyParts;
         while (iter.hasNext()) {
            reply = (String)iter.next();
            if ((reply.indexOf('.')!=-1)!=isIPv4Record) continue;
            replyParts = reply.split(isIPv4Record?"\\.":":");
            if (isIPv4Record) {
               if (replyParts[1].equals("*") || replyParts[1].equals(recordReplyParts[1])) {
                  if (replyParts[2].equals("*") || replyParts[2].equals(recordReplyParts[2])) {
                     if (replyParts[3].equals(recordReplyParts[3])) return true;
                  }
               }
            }
            //TO-DO check IPv6 addresses
            else {
               continue;
            }
         }
         return false;
      }

      private String reverseIP(String clientIP) {
         boolean isIPv4 = clientIP.indexOf('.')!=-1;
         String[] parts = clientIP.split(isIPv4?"\\.":":");
         int partsCount = parts.length;
         StringBuilder sb = new StringBuilder(isIPv4?16:40);
         for (int i=partsCount-1;i>=0;i--) {
            sb.append(parts[i]+(isIPv4?".":":"));
         }
         return sb.toString();
      }

      //With no running instance of amavis and if the sender doesn't get blocked this can only be
      //saved in incoming.directory
      public boolean getUseAmavisDirectory() {
         return false;
      }

      protected final String sendToAmavis = "sendTo";
      protected final String bypassAmavis = "bypass";
   }

   private static class WithAmavisdInstance extends NoAmavisdInstance {

      //Having a running instance of amavis and had the sender not been blocked, then based on
      //the result of the client IP's check, either save the message in amavis.incoming.directory
      //(effectively bypassing amavis) or in incoming.directory to be picked up by amavis.
      public boolean getUseAmavisDirectory() {
         return !result.equals(sendToAmavis);
      }
   }

}
