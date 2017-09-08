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

package com.ericdaugherty.mail.server.auth;

//Java Imports
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import javax.security.sasl.*;

//Log Imports
import org.apache.commons.logging.LogFactory;
import org.apache.commons.logging.Log;

//Local Imports
import com.ericdaugherty.mail.server.errors.AuthenticationException;
import com.ericdaugherty.mail.server.info.EmailAddress;
import com.ericdaugherty.mail.server.services.general.DeliveryService;
import com.ericdaugherty.mail.server.services.pop3.Pop3Processor;

/**
 * Verify client authentication using SASL PLAIN.
 *
 * @author Andreas Kyrmegalos
 */
public class PlainServerMode extends AbstractSaslServerMode{

   /** Logger Category for this class. */
   //private static Log log = LogFactory.getLog( PlainServerMode.class );
   private static Log log = LogFactory.getLog("JESLogger");

   public PlainServerMode(boolean smtp) {
      if (smtp) {
         finalizeAuthentication = new PlainServerMode.FinalizeAuthenticationSMTP();
      }
      else {
         pop3 = true;
         finalizeAuthentication = new PlainServerMode.FinalizeAuthenticationPOP3();
      }
   }
   public byte[] evaluateResponse(byte[] responseBytes) throws SaslException{

      if (completed) throw new SaslException("Authentication already completed.");
      if (failed) throw new SaslException("Authentication already tried and failed.");
      
      try {
         
         ByteBuffer bb = ByteBuffer.wrap(responseBytes);
         CharBuffer cb = null;
         synchronized(charset) {
            cb = charset.decode(bb);
         }
         char[] response = new char[cb.remaining()];
         cb.get(response);
         
         int lastIndexOfNull = response.length;
         char NULL = '\u0000';
         for (;lastIndexOfNull-->0;) {
            if (response[lastIndexOfNull]==NULL) break;
         }
         char[] password = new char[response.length-lastIndexOfNull-1];
         System.arraycopy(response, lastIndexOfNull+1, password, 0, password.length);
         String[] authentications;
         authentications = new String(response, 0, lastIndexOfNull).split("\u0000");
         
         String authenticationID, authorizationID;
         if (authentications.length<1) {
            throw new SaslException("A username and/or password is required");
         }
         else if (authentications.length>2) {
            throw new SaslException("Illegal use of the UTF-8 NULL character");
         }
         if (authentications.length==1) {
            authorizationID = null;
            authenticationID = authentications[0];
         }
         else {
            authorizationID = authentications[0];
            if (authorizationID.isEmpty()) {
               authorizationID = null;
            }
            authenticationID = authentications[1];
         }
         
         try {
            authenticationID = getValidAuthenticationID(authenticationID);
         }
         catch (SaslException se) {
            if (pop3&&se.getCause().getMessage().equals(DOMAIN_REQUIRED)) {
               se.initCause(new AuthenticationException(Pop3Processor.MESSAGE_NEED_USER_DOMAIN));
            }
            throw se;
         }
         return finalizeAuthentication.finalize(authenticationID, authorizationID, password);
      }
      catch (SaslException se) {
         failed = true;
         throw se;
      }
   }
   
   protected class FinalizeAuthenticationSMTP extends AbstractSaslServerMode.FinalizeAuthentication {

      public byte[] finalize(String authenticationID, String authorizationID, char[] password) throws SaslException{

            user = configurationManager.getUser(EmailAddress.getEmailAddress(authenticationID));
            if (user!=null&&isPasswordValid(password)) {

               if (authorizationID == null) {
                  authorizationID = authenticationID;
               }
               else if (configurationManager.isSingleDomainMode() && authorizationID.indexOf('@') == -1) {
                  authorizationID = authorizationID + "@" + configurationManager.getSingleDomain().getDomainName();
               }
               PlainServerMode.this.authorizationID = authorizationID;
               log.info( "User " + authenticationID + " logged in successfully and authorized as "+authorizationID);
               completed = true;
               return null;
            }
            else {
               throw new SaslException("User "+authenticationID+" not authenticated",
                     new AuthenticationException());
            }
      }
   }
   protected class FinalizeAuthenticationPOP3 extends AbstractSaslServerMode.FinalizeAuthentication {

      public byte[] finalize(String authenticationID, String authorizationID, char[] password) throws SaslException{
         
         EmailAddress emailAddress = EmailAddress.getEmailAddress(authenticationID);
         
         user = configurationManager.getUser(emailAddress);
         if (user!=null&&isPasswordValid(password)) {

            DeliveryService deliveryService = DeliveryService.getDeliveryService();
            if( deliveryService.isMailboxLocked( emailAddress ) ) {
               user = null;
               throw new SaslException(authenticationID+" mailbox is locked",
                     new AuthenticationException(Pop3Processor.MESSAGE_USER_MAILBOX_LOCKED));
            }
            else {
               deliveryService.ipAuthenticated( clientIp );
               deliveryService.lockMailbox( emailAddress );
               log.info( "User: " + authenticationID + " logged in successfully and authorized as "+authorizationID);
               completed = true;
               return null;
            }
         }
         else {
            user = null;
            throw new SaslException("User "+authenticationID+" not authenticated",
                  new AuthenticationException(Pop3Processor.MESSAGE_INVALID_LOGIN));
         }
      }
   }
   
   protected boolean isPasswordValid(char[] password) {
      return user.isPasswordValid(password);
   }
   
   public String getMechanismName() {
      return "PLAIN";
   }

   public byte[] unwrap(byte[] incoming, int start, int len) throws SaslException {
      
      throw new IllegalStateException("Integrity and/or privacy quality of protection is not supported");
   }

   public byte[] wrap(byte[] outgoing, int start, int len) throws SaslException {
      
      throw new IllegalStateException("Integrity and/or privacy quality of protection is not supported");
   }

   public Object getNegotiatedProperty(String propName) {
      if(!completed) {
         throw new IllegalStateException("Authentication still in progress");
      }
      if(Sasl.QOP.equals(propName)) {
         return "auth";
      }
      return null;
   }
}
