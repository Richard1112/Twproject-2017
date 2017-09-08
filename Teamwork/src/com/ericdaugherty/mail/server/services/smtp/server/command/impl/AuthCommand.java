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

package com.ericdaugherty.mail.server.services.smtp.server.command.impl;

//Java imports
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import javax.security.sasl.SaslException;

//Logging imports
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

//Local imports
import com.ericdaugherty.mail.server.configuration.ConfigurationParameterConstants.CLEAR_TEXT;
import com.ericdaugherty.mail.server.errors.*;
import com.ericdaugherty.mail.server.services.smtp.server.*;
import com.ericdaugherty.mail.server.services.smtp.server.action.*;
import com.ericdaugherty.mail.server.services.smtp.server.command.AbstractParCommand;
import com.ericdaugherty.mail.server.services.smtp.server.parser.impl.AuthInterpreter;

//Other imports
import org.apache.commons.codec.binary.Base64;

/**
 *
 * @author Andreas Kyrmegalos
 */
public class AuthCommand extends AbstractParCommand {

   /**
    * Logger Category for this class.
    */
   //private static final Log log = LogFactory.getLog(AuthCommand.class);
  private static Log log = LogFactory.getLog("JESLogger");
   
   private int passAttempts = 0;
   
   private String identity;

   public AuthCommand() {
      super(COMMAND_VERB.AUTH, null, new AuthInterpreter());
      reset();
   }

   public void checkMailPrerequisites(SMTPServerSessionControl control) throws SMTPReplyException {
      control.incrementErrorCount();
      throw new SMTPReplyException(SMTPServerSessionControl.MESSAGE_COMMAND_ORDER_INVALID);
   }
   
   public void reset() {
      super.reset();
      passAttempts = 0;
      identity = null;
      setPreCommandAction(new AuthPreCommandAction());
      setCommandAction(new AuthCommandAction());
      setPostCommandAction(new AuthPostCommandAction());
   }

   class AuthPreCommandAction extends PreCommandAction {

      public void execute(SMTPServerSessionControl control) throws SMTPReplyException {
         
         if (control.isAuthenticated()) {
            control.incrementErrorCount();
            throw new SMTPReplyException(SMTPServerSessionControl.MESSAGE_ALREADY_AUTHENTICATED);
         }
      }
   }

   class AuthCommandAction extends CommandAction {

      public void execute(SMTPServerSessionControl control) throws SMTPReplyException, TooManyErrorsException, IOException {

         int maxPassAttempts = configurationManager.getMaxPassAttempts();

         if (maxPassAttempts != 0) {
            passAttempts++;
            log.info("Client "+control.getClientDomain()+" ["+control.getClientIP()+ "] making authentication attempt no. "+passAttempts);
            if (passAttempts > maxPassAttempts) {
               control.incrementErrorCount();
               throw new SMTPReplyException(SMTPServerSessionControl.MESSAGE_AUTH_FAILED);
            }
         }
         
         String mechanism = argument;
         
         log.info("Client "+control.getClientDomain()+" ["+control.getClientIP()+ "] requested the "+mechanism+" authentication mechanism.");

         String clientResponse = parameters==null?null:parameters[0];
         
         boolean contains = false;
         int mechCount = control.getAuthMechs().length - (control.getClearTextAllowed() == CLEAR_TEXT.ALWAYS ? 0 : (control.getClearTextAllowed() == CLEAR_TEXT.NEVER ? 2 : (control.isEncrypted() ? 0 : 2)));
         for (int i = 0; i < mechCount; i++) {
            if (control.getAuthMechs()[i].contains(mechanism)) {
               contains = true;
               break;
            }
         }
         if (contains) {
            if (mechanism.equals("PLAIN")) {
               boolean pipeline = true;
               control.createSASLServer(SASL_SERVER_MODE.PLAIN, null);
               if (clientResponse == null) {
                  pipeline = false;
                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_INTERMEDIATE);
                  clientResponse = control.getClientResponse();
               }
               try {

                  if (clientResponse.equals("*")) {
                     throw new AuthenticationException("Client cancelled authentication process");
                  }
                  if (!Base64.isArrayByteBase64(clientResponse.getBytes(US_ASCII))) {
                     throw new SaslException("Can not decode Base64 Content", new MalformedBase64ContentException());
                  }
                  control.evaluateSASLResponse(Base64.decodeBase64(clientResponse.getBytes(US_ASCII)));
                  
                  identity = control.getAuthorizationID();
                  control.setSuccessSASLNegotiation();
                  log.info("Client "+control.getClientDomain()+" ["+control.getClientIP()+ "] authenticated using "+mechanism+". Authorized as "+identity);

                  if (pipeline) {
                     control.setReplyAny(SMTPServerSessionControl.MESSAGE_AUTH_SUCCESS + " Authentication successful");
                  } else {
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_SUCCESS + " Authentication successful");
                  }
               } catch (AuthenticationException ae) {
                  log.error(ae.getLocalizedMessage());
                  control.incrementErrorCount();
                  if (pipeline) {
                     control.setReplyAny(SMTPServerSessionControl.MESSAGE_AUTH_CANCELLED + ae.getMessage());
                  } else {
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_CANCELLED + ae.getMessage());
                  }
               } catch (SaslException ex) {
                  log.error(ex.getMessage());
                  control.incrementErrorCount();
                  if (ex.getCause() != null && ex.getCause() instanceof MalformedBase64ContentException) {
                     if (pipeline) {
                        control.setReplyAny(SMTPServerSessionControl.MESSAGE_AUTH_FAILED_CUSTOM + ex.getMessage());
                     } else {
                        control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_FAILED_CUSTOM + ex.getMessage());
                     }
                  } else {
                     if (pipeline) {
                        control.setReplyAny(SMTPServerSessionControl.MESSAGE_AUTH_FAILED);
                     } else {
                        control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_FAILED);
                     }
                  }
               } catch (UnsupportedEncodingException ex) {
                  control.incrementErrorCount();
                  if (pipeline) {
                     control.setReplyAny(SMTPServerSessionControl.MESSAGE_FAILED_TRANSACTION);
                  } else {
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_FAILED_TRANSACTION);
                  }
               }
               
               
            } else if (mechanism.equals("LOGIN")) {
               
               control.createSASLServer(SASL_SERVER_MODE.LOGIN, null);
               try {

                  if (clientResponse != null) {
                     throw new SaslException("No initial clent response is specified by LOGIN");
                  }
                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_INTERMEDIATE + "VXNlcm5hbWU6");
                  clientResponse = control.getClientResponse();
                  //if (clientResponse.equals("*")) {
                  if (clientResponse==null || clientResponse.equals("*")) {
                     throw new AuthenticationException("Client cancelled authentication process");
                  }
                  if (!Base64.isArrayByteBase64(clientResponse.getBytes(US_ASCII))) {
                     throw new SaslException("Can not decode Base64 Content", new MalformedBase64ContentException());
                  }
                  
                  control.evaluateSASLResponse(Base64.decodeBase64(clientResponse.getBytes(US_ASCII)));

                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_INTERMEDIATE + "UGFzc3dvcmQ6");
                  clientResponse = control.getClientResponse();
                  if (clientResponse.equals("*")) {
                     throw new AuthenticationException("Client cancelled authentication process");
                  }
                  if (!Base64.isArrayByteBase64(clientResponse.getBytes(US_ASCII))) {
                     throw new SaslException("Can not decode Base64 Content", new MalformedBase64ContentException());
                  }
                  control.evaluateSASLResponse(Base64.decodeBase64(clientResponse.getBytes(US_ASCII)));
                  
                  identity = control.getAuthorizationID();
                  control.setSuccessSASLNegotiation();
                  log.info("Client "+control.getClientDomain()+" ["+control.getClientIP()+ "] authenticated using "+mechanism+". Authorized as "+identity);
                  
                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_SUCCESS + " Authentication successful");
                  
               } catch (AuthenticationException ae) {
                  log.error(ae.getLocalizedMessage());
                  control.incrementErrorCount();
                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_CANCELLED + ae.getMessage());
               } catch (SaslException ex) {
                  log.error(ex.getMessage());
                  control.incrementErrorCount();
                  if (ex.getCause() != null&&ex.getCause() instanceof MalformedBase64ContentException) {
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_FAILED_CUSTOM + ex.getMessage());
                  } else {
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_FAILED);
                  }
               }
               
            } else if (mechanism.startsWith("CRAM")) {
               
               control.createSASLServer(SASL_SERVER_MODE.CRAM, mechanism);
               try {

                  if (clientResponse != null) {
                     throw new SaslException("No initial clent response is specified by CRAM");
                  }
                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_INTERMEDIATE + new String(Base64.encodeBase64(control.evaluateSASLResponse(null)), US_ASCII));

                  clientResponse = control.getClientResponse();
                  if (clientResponse.equals("*")) {
                     throw new AuthenticationException("Client cancelled authentication process");
                  }
                  if (!Base64.isArrayByteBase64(clientResponse.getBytes(US_ASCII))) {
                     throw new SaslException("Can not decode Base64 Content", new MalformedBase64ContentException());
                  }
                  control.evaluateSASLResponse(Base64.decodeBase64(clientResponse.getBytes(US_ASCII)));
                  
                  identity = control.getAuthorizationID();
                  control.setSuccessSASLNegotiation();
                  log.info("Client "+control.getClientDomain()+" ["+control.getClientIP()+ "] authenticated using "+mechanism+". Authorized as "+identity);

                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_SUCCESS + "CRAM Authentication Successful");
                  
               } catch (AuthenticationException ae) {
                  log.error(ae.getLocalizedMessage());
                  control.incrementErrorCount();
                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_CANCELLED + ae.getMessage());
               } catch (SaslException ex) {
                  log.error(ex.getMessage());
                  control.incrementErrorCount();
                  if (ex.getCause() != null&&ex.getCause() instanceof MalformedBase64ContentException) {
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_FAILED_CUSTOM + ex.getMessage());
                  } else {
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_FAILED);
                  }
               }

            } else if (mechanism.equals("DIGEST-MD5")) {
               
               control.createSASLServer(SASL_SERVER_MODE.MD5_DIGEST, null);
               try {
                  if (clientResponse != null) {
                     throw new SaslException("No initial client response is specified by DIGEST-MD5");
                  }
                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_INTERMEDIATE + new String(control.evaluateSASLResponse(null), US_ASCII));
                  clientResponse = control.getClientResponse();
                  if (clientResponse.equals("*")) {
                     throw new AuthenticationException("Client cancelled authentication process");
                  }
                  byte[] rspauth = control.evaluateSASLResponse(clientResponse.getBytes(US_ASCII));
                  if (rspauth == null) {
                     control.incrementErrorCount();
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_FAILED);
                  } else {
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_INTERMEDIATE + new String(rspauth, US_ASCII));
                     control.getClientResponse();
                  
                     identity = control.getAuthorizationID();
                     control.setSuccessSASLNegotiation();
                     log.info("Client "+control.getClientDomain()+" ["+control.getClientIP()+ "] authenticated using "+mechanism+". Authorized as "+identity);
                  
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_SUCCESS + "Authentication successful");
                     
                  }
               } catch (AuthenticationException ae) {
                  log.error(ae.getMessage());
                  control.incrementErrorCount();
                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_CANCELLED + "Authentication cancelled");
               } catch (SaslException ex) {
                  log.error(ex.getMessage());
                  control.incrementErrorCount();
                  if (ex.getCause() != null) {
                     if (ex.getCause() instanceof AuthenticationException) {
                        control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_FAILED_CUSTOM + ex.getMessage());
                     } else if (ex.getCause() instanceof MalformedBase64ContentException) {
                        control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_CANCELLED + ex.getMessage());
                     } else {
                        control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_FAILED);
                     }
                  } else {
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_FAILED);
                  }
               }
            } else if (mechanism.startsWith("SCRAM")) {
               
               control.createSASLServer(SASL_SERVER_MODE.SCRAM, mechanism);
               if (clientResponse == null) {
                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_INTERMEDIATE);
                  clientResponse = control.getClientResponse();
               }
               try {

                  if (clientResponse.equals("*")) {
                     throw new AuthenticationException("Client cancelled authentication process");
                  }
                  byte[] challenge = control.evaluateSASLResponse(clientResponse.getBytes(UTF_8));
                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_INTERMEDIATE + new String(challenge, UTF_8));

                  clientResponse = control.getClientResponse();
                  if (clientResponse.equals("*")) {
                     throw new AuthenticationException("Client cancelled authentication process");
                  }
                  challenge = control.evaluateSASLResponse(clientResponse.getBytes(UTF_8));
                  
                  identity = control.getAuthorizationID();
                  control.setSuccessSASLNegotiation();
                  log.info("Client "+control.getClientDomain()+" ["+control.getClientIP()+ "] authenticated using "+mechanism+". Authorized as "+identity);
                  
                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_SUCCESS + new String(challenge, UTF_8));
                  
               } catch (AuthenticationException ae) {
                  log.error(ae.getLocalizedMessage());
                  control.incrementErrorCount();
                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_CANCELLED + ae.getMessage());
               } catch (SaslException ex) {
                  log.error(ex.getMessage());
                  control.incrementErrorCount();
                  if (ex.getCause() != null&&ex.getCause() instanceof MalformedBase64ContentException) {
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_FAILED_CUSTOM + ex.getMessage());
                  } else {
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_FAILED);
                  }
               } catch (UnsupportedEncodingException ex) {
                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_FAILED_TRANSACTION);
               }
            } else if (mechanism.equals("GSSAPI")) {
               
               try {
                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_INTERMEDIATE);
                  control.createSASLServer(SASL_SERVER_MODE.GSSAPI, null);
                  int tokenLength = 12289;
                  byte[] token = new byte[tokenLength], temp;
                  while (true) {
                     tokenLength = control.getGSSResponse(token, 0, tokenLength);
                     if (tokenLength==12289) {
                        throw new SMTPReplyException(SMTPServerSessionControl.MESSAGE_AUTH_TOO_LONG_COMMAND);
                     }
                     temp = new byte[tokenLength];
                     System.arraycopy(token, 0, temp, 0, tokenLength);
                     if (temp.length == 1 && temp[0] == 0x2a) {
                        throw new AuthenticationException("Client cancelled authentication process");
                     }
                     token = control.evaluateSASLResponse(temp);
                     if (control.isSASLComplete()) {
                  
                        identity = control.getAuthorizationID();
                        control.setSuccessSASLNegotiation();
                        log.info("Client "+control.getClientDomain()+" ["+control.getClientIP()+ "] authenticated using "+mechanism+". Authorized as "+identity);
                  
                        control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_SUCCESS + "Authentication successful");
                        break;
                     }
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_INTERMEDIATE + new String(token, US_ASCII));
                     tokenLength = 12289;
                     token = new byte[tokenLength];
                  }

               } catch (AuthenticationException ae) {
                  log.error(ae.getMessage());
                  control.incrementErrorCount();
                  control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_CANCELLED + "Authentication cancelled");
               } catch (SaslException ex) {
                  log.error(ex.getMessage());
                  control.incrementErrorCount();
                  if (ex.getCause() != null&&ex.getCause() instanceof MalformedBase64ContentException) {
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_CANCELLED + ex.getMessage());
                  } else {
                     control.setReplyLast(SMTPServerSessionControl.MESSAGE_AUTH_FAILED);
                  }
               }
            }
                  
         } else {
            control.setReplyLast(SMTPServerSessionControl.MESSAGE_UNRECOGNIZED_AUTH_MECH);
         }
      }
   }
   class AuthPostCommandAction extends PostCommandAction {

      public void execute(SMTPServerSessionControl control) {
         control.setInitState(true);
      }
   }
   private static final String US_ASCII = "US-ASCII";
   private static final String UTF_8 = "UTF-8";
}
