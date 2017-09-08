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

package com.ericdaugherty.mail.server.services.pop3;

//Java imports

import java.net.*;
import java.io.*;
import java.util.*;
import javax.net.ssl.SSLPeerUnverifiedException;
import javax.net.ssl.SSLSocket;
import javax.security.sasl.SaslException;

//Log imports
import org.apache.commons.logging.LogFactory;
import org.apache.commons.logging.Log;

//Local imports
import com.ericdaugherty.mail.server.Mail;
import com.ericdaugherty.mail.server.auth.*;
import com.ericdaugherty.mail.server.configuration.ConfigurationManager;
import com.ericdaugherty.mail.server.configuration.ConfigurationParameterConstants.CLEAR_TEXT;
import com.ericdaugherty.mail.server.errors.*;
import com.ericdaugherty.mail.server.info.*;
import com.ericdaugherty.mail.server.security.transport.TransportLayer;
import com.ericdaugherty.mail.server.services.general.*;

//Other imports
import org.apache.commons.codec.binary.Base64;

/**
 * Handles an incoming Pop3 connection.  See RFC 1939, 1957, 2449 for details.
 *
 * @author Eric Daugherty
 * @author Andreas Kyrmegalos (2.x branch)
 */
public class Pop3Processor extends Thread implements ConnectionProcessor {

  //***************************************************************
  // Variables
  //***************************************************************

  /**
   * Logger Category for this class.
   */
  //private static final Log log = LogFactory.getLog( Pop3Processor.class );
  private static Log log = LogFactory.getLog("JESLogger");

  /**
   * The ConfigurationManager
   */
  private final ConfigurationManager configurationManager = ConfigurationManager.getInstance();

  /**
   * Indicates if this thread should continue to run or shut down
   */
  private boolean running = true;

  /**
   * The server socket used to listen for incoming connections
   */
  private ServerSocket serverSocket;

  /**
   * Socket connection to the client
   */
  private Socket socket;

  /**
   * The IP address of the client
   */
  private String clientIp;

  /**
   * The user currently logged in
   */
  private User user;

  /**
   * The class responsible for handling a user's messages.
   * Used only during the transaction phase.
   */
  private Pop3MessageHandler pop3mh;

  /**
   * This setting defines the security state of the session.
   * Please note the following:
   * A connection over a standard port with the setting standardsecure=true
   * starts as non-secure.
   * A connection over a standard port with the setting standardsecure=false
   * is considered secure for the duration of the session.
   * A connection over a secure port
   * is considered secure for the duration of the session.
   */
  private boolean isSecure;

  /**
   * This setting is used to certify the encryption state of the connection
   */
  private boolean isEncrypted;

  private boolean updateState;

  /**
   * The number of errors during a given session
   */
  private int errorcount;

  /**
   * The maximum number of allowed errors
   */
  private int maxerrorcount = configurationManager.getMaxErrorCount();

  private CLEAR_TEXT allowClearText = configurationManager.allowClearTextPOP3();

  private volatile boolean updatingServerSocket;

  private String[] instanceAuthMech;

  private DigestMd5ServerMode digestMd5ServerMode;
  private GSSServerMode gssServerMode;

  private ProcessorStreamHandler pop3SH = new ProcessorStreamHandler();

  //***************************************************************
  // Public Interface
  //***************************************************************

  /**
   * Sets the socket used to communicate with the client.
   */
  public void setSocket(ServerSocket serverSocket) {

    this.serverSocket = serverSocket;
    if (log.isDebugEnabled()) {
      log.debug("serverSocketUpdated");
    }
  }

  private boolean isDelayedStart() {
    return updatingServerSocket;
  }

  public boolean isUpdatingServerSocket() {
    return updatingServerSocket;
  }

  public void setDelayedStart(boolean delayed) {
    setUpdatingServerSocket(delayed);
  }

  public void setUpdatingServerSocket(boolean updating) {
    synchronized (this) {
      updatingServerSocket = updating;
      if (!updating) {
        notify();
      }
    }
  }

  /**
   * Entrypoint for the Thread, this method handles the interaction with
   * the client socket.
   */
  public void run() {

    if (isDelayedStart()) {
      synchronized (this) {
        while (isDelayedStart() && running) {
          try {
            wait(500);
          } catch (InterruptedException ie) {
            if (!running) {
              break;
            }
          }
        }
      }
    }

    String[] failedMessages;
    while (running) {

      failedMessages = null;
      errorcount = 0;
      StringBuilder sb = new StringBuilder(30);
      if (configurationManager.isGSSEnabled()) {
        sb.append(AUTH_MECH[0]);
        sb.append(",");
      }
      if (configurationManager.getSCRAMMembers() != null) {
        for (int i = 1; i < 5; i++) {
          if (configurationManager.getSCRAMMembers().contains(AUTH_MECH[i])) {
            sb.append(AUTH_MECH[i]);
            sb.append(",");
          }
        }
      }
      if (configurationManager.isDigestMD5Enabled()) {
        sb.append(AUTH_MECH[5]);
        sb.append(",");
      }
      if (configurationManager.getCRAMMembers() != null) {
        for (int i = 6; i < 11; i++) {
          if (configurationManager.getCRAMMembers().contains(AUTH_MECH[i])) {
            sb.append(AUTH_MECH[i]);
            sb.append(",");
          }
        }
      }
      sb.append(AUTH_MECH[11]);
      sb.append(",");
      sb.append(AUTH_MECH[12]);
      instanceAuthMech = sb.toString().split(",");
      try {
        socket = serverSocket.accept();
        updateState = false;
        isSecure = serverSocket.getLocalPort() == configurationManager.getSecurePOP3Port() ?
                true : configurationManager.isStandardPOP3Secure() ? false : true;
        isEncrypted = serverSocket.getLocalPort() == configurationManager.getSecurePOP3Port() ?
                true : false;

        //Prepare the input and output streams.
        InetAddress remoteAddress = null;
        pop3SH.setStreams(socket);

        remoteAddress = socket.getInetAddress();

        clientIp = remoteAddress.getHostAddress();

        if (log.isInfoEnabled()) {
          log.info(remoteAddress.getHostName() + "(" + clientIp + ") socket connected via POP3.");
        }

        //Output the welcome message.
        write(WELCOME_MESSAGE, 0);

        //Enter AUTHORIZATION state
        user = authorization();

        //Enter TRANSACTION state
        if (user != null) {
          pop3mh = new Pop3MessageHandler(user);
          failedMessages = transaction();
        }
      } catch (TooManyErrorsException e) {
        log.error("The session generated too many errors");
      } catch (java.net.SocketTimeoutException e) {
        //log.debug( "Socket timed out " );
      } catch (IOException e) {
        if (running) {
          log.error(" There was a error with the connection: " + e);
          //There is a chance that an instance of this class has been instantiated but
          //its reference in ServiceListener points to null. Check the Mail instance for
          //the case of a server shutdown
          if (Mail.getInstance().isShuttingDown()) {
            shutdown();
          }
        }
      }
      //If any exception gets to here uncaught, it means we should just disconnect.
      catch (Throwable e) {
        log.error("Unknown Exception:", e);
      } finally {
        if (digestMd5ServerMode != null) {
          try {
            digestMd5ServerMode.dispose();
          } catch (SaslException ex) {
          }
        }
        if (socket != null && socket.isConnected()) {
          try {
            if (failedMessages == null) {
              write(MESSAGE_DISCONNECT, 0);
            } else if (updateState) {
              int failedMessageCount = failedMessages.length;
              StringBuilder failedMessageList = new StringBuilder(50).append(" [");
              failedMessageCount--;
              for (int i = 0; i < failedMessageCount; i++) {
                failedMessageList.append(failedMessages[i]).append(" ");
              }
              failedMessageList.append(failedMessages[failedMessageCount]).append("]");
              write(MESSAGE_ERROR + failedMessageList.toString(), 0);
            } else {
              write("-ERR Unknown Error", 0);
            }
          } catch (TooManyErrorsException ex) {
            //Safe to ignore
          } catch (IOException ioe) {
            if (log.isDebugEnabled()) {
              log.debug(ioe.getLocalizedMessage());
            }
          }
        }
        //log.info( "Disconnecting" );
        if (user != null) {
          //Unlock the user's mailbox
          EmailAddress userAddress = EmailAddress.getEmailAddress(user.getUsername(), user.getDomain());
          DeliveryService.getDeliveryService().unlockMailbox(userAddress);
          pop3mh.dispose();
          user = null;
        }
        if (socket != null) {
          try {
            socket.close();
          } catch (IOException e) {
            log.error("Error disconnecting.", e);
            //Nothing to do.
          } finally {
            socket = null;
          }
        }
        if (gssServerMode != null) {
          try {
            gssServerMode.dispose();
          } catch (SaslException ex) {
          }
        }
        pop3SH = new ProcessorStreamHandler();
      }
      if (isUpdatingServerSocket()) {
        synchronized (this) {
          while (isUpdatingServerSocket()) {
            try {
              wait(500);
            } catch (InterruptedException ie) {
              if (!running) {
                break;
              }
            }
          }
        }
      }
    }
    log.warn("Pop3Processor shut down gracefully");
  }

  /**
   * Notifies this thread to stop processing and exit.
   */
  public void shutdown() {
    log.warn("Shutting down Pop3Processor.");
    running = false;
    if (socket != null) {
      if (socket.isConnected()) {
        Timer timer = new Timer(true);
        timer.schedule(new TimerTask() {

          public void run() {
            try {
              socket.close();
            } catch (IOException ex) {
              //Just ignore
            }
          }
        }, 60 * 1000L);
      } else {
        try {
          socket.close();
        } catch (IOException ex) {
        }
      }
    }
  }

  //***************************************************************
  // Private Interface
  //***************************************************************

  /**
   * Checks to verify that the command is not a quit command.  If it is,
   * the current state is finalized (all messaged marked as deleted are
   * actually deleted) and closes the connection.
   */
  private boolean checkQuit(String command) {

    if (command.equals(COMMAND_QUIT)) {
      if (log.isDebugEnabled()) {
        log.debug("User has QUIT the session.");
      }
      return true;
    }
    return false;
  }

  /**
   * A class containing user data for use during the authorization state of a session
   */
  private class SessionUser {

    private char[] password = {};
    private EmailAddress address;

    public char[] getPassword() {
      return password;
    }

    public void setPassword(char[] password) {
      this.password = password;
    }

    public EmailAddress getAddress() {
      return address;
    }

    public void setAddress(EmailAddress address) {
      this.address = address;
    }

  }

  /**
   * The user must authenticate before moving on to enter
   * more commands.  This method will listen to incoming
   * commands until the user either successfully authenticates
   * or quits.
   */
  private User authorization() throws TooManyErrorsException,
          SocketTimeoutException, SocketException, IOException {

    //Reusable Variables.
    String inputString;
    String command;
    String argument;

    User user = null;
    int attempts = 0;

    Pop3Processor.SessionUser su = new Pop3Processor.SessionUser();
    DeliveryService deliveryService = DeliveryService.getDeliveryService();

    do {
      inputString = read();

      command = parseCommand(inputString);
      argument = parseArgument(inputString);
      if (command == null) {
        write(MESSAGE_UNRECOGNIZED_COMMAND, 1);
      } else if (checkQuit(command)) {
        return null;
      } else if (command.equals(COMMAND_STLS)) {
        if (isEncrypted) {
          log.info("Client attempted to re-authenticate.");
          write(MESSAGE_ALREADY_AUTHENTICATED, 1);
          continue;
        }
        boolean acceptable = false;
        TransportLayer transportLayer = new TransportLayer();
        try {
          transportLayer.init(socket, true, true, false);
          write(MESSAGE_START_TLS, 0);
          transportLayer.verifyPeer(true, false);
          String cipher = ((SSLSocket) transportLayer.getSocket()).getSession().getCipherSuite();
          log.info("Negotiated Cipher: " + cipher);
          String[] ec = configurationManager.getEnabledCiphers();
          for (int i = 0; i < ec.length; i++) {
            if (cipher.equals(ec[i])) {
              acceptable = true;
              break;
            }
          }
          if (!acceptable) {
            log.info("Negotiated Cipher Suite not acceptable!");
            continue;
          }
          socket = transportLayer.getSocket();
        } catch (SSLPeerUnverifiedException sslpue) {
          log.error(sslpue.getMessage());
          write("-ERR Peer Verification Failed", 1);
          continue;
        } catch (IOException ioe) {
          log.error(ioe.getMessage());
          write("-ERR TLS temporarily unavailable", 1);
          continue;
        } finally {
          transportLayer.conclude();
        }
        isSecure = true;
        isEncrypted = true;
        try {
          pop3SH.setSecureStreams(socket);
        } catch (Exception ex) {
          log.error(ex.getMessage());
          return null;
        }
        continue;
      }
      //Check to see if the user command was send
      else if (command.equals(COMMAND_USER)) {
        if (allowClearText == CLEAR_TEXT.NEVER || (allowClearText != CLEAR_TEXT.ALWAYS && !isEncrypted)) {
          write(MESSAGE_AUTH_FAILED + "Security policy rejects clear text passwords", 1);
        } else {
          user = processUsername(argument, su, deliveryService);
        }
      }
      //Check to see if the AUTH command was transmitted
      else if (command.equals(COMMAND_AUTH)) {
        if (argument.length() == 0) {
          //The NTLM authentication scheme is not supported and most likely will never be
          write(MESSAGE_ERROR, 1);
        } else {
          String[] authBreakDown = argument.split(" ");
          String mechanism = authBreakDown[0].toUpperCase(Locale.ENGLISH);
          String clientResponse = null;
          if (authBreakDown.length > 1) {
            clientResponse = authBreakDown[1];
          }
          boolean contains = false;
          int mechCount = instanceAuthMech.length - (allowClearText == CLEAR_TEXT.ALWAYS ? 0 : (allowClearText == CLEAR_TEXT.NEVER ? 2 : (isEncrypted ? 0 : 2)));
          for (int i = 0; i < mechCount; i++) {
            if (instanceAuthMech[i].contains(mechanism)) {
              contains = true;
              break;
            }
          }
          if (contains) {
            if (mechanism.equals("PLAIN")) {
              PlainServerMode plainServerMode = new PlainServerMode(false);
              plainServerMode.setClientIp(clientIp);
              if (clientResponse == null) {
                write(MESSAGE_INTERMEDIATE, 0);
                clientResponse = read();
              }
              try {

                if (clientResponse.equals("*")) throw new SaslException("Client cancelled authentication process");
                if (!Base64.isArrayByteBase64(clientResponse.getBytes(US_ASCII))) {
                  throw new SaslException("Can not decode Base64 Content", new MalformedBase64ContentException());
                }
                plainServerMode.evaluateResponse(Base64.decodeBase64(clientResponse.getBytes(US_ASCII)));

                user = plainServerMode.getUser();
                write(MESSAGE_AUTH_SUCCESS, 0);
              } catch (SaslException ex) {
                log.error(ex.getMessage());
                if (ex.getCause() != null) {
                  write(ex.getCause().getMessage(), 1);
                } else {
                  String message = "";
                  if (ex.getCause() != null) {
                    message = ex.getMessage();
                  }
                  write(MESSAGE_AUTH_FAILED + message, 1);
                }
              }
              plainServerMode.dispose();
            } else if (mechanism.equals("LOGIN")) {
              LoginServerMode loginServerMode = new LoginServerMode(false);
              loginServerMode.setClientIp(clientIp);
              try {

                write(MESSAGE_INTERMEDIATE + "VXNlcm5hbWU6", 0);
                clientResponse = read();
                if (clientResponse.equals("*")) throw new SaslException("Client cancelled authentication process");
                if (!Base64.isArrayByteBase64(clientResponse.getBytes(US_ASCII))) {
                  throw new SaslException("Can not decode Base64 Content", new MalformedBase64ContentException());
                }
                loginServerMode.evaluateResponse(Base64.decodeBase64(clientResponse.getBytes(US_ASCII)));

                write(MESSAGE_INTERMEDIATE + "UGFzc3dvcmQ6", 0);
                clientResponse = read();
                if (clientResponse.equals("*")) throw new SaslException("Client cancelled authentication process");
                if (!Base64.isArrayByteBase64(clientResponse.getBytes(US_ASCII))) {
                  throw new SaslException("Can not decode Base64 Content", new MalformedBase64ContentException());
                }
                loginServerMode.evaluateResponse(Base64.decodeBase64(clientResponse.getBytes(US_ASCII)));

                user = loginServerMode.getUser();
                write(MESSAGE_AUTH_SUCCESS, 0);
              } catch (SaslException ex) {
                log.error(ex.getMessage());
                if (ex.getCause() != null) {
                  write(ex.getCause().getMessage(), 1);
                } else {
                  String message = "";
                  if (ex.getCause() != null) {
                    message = ex.getMessage();
                  }
                  write(MESSAGE_AUTH_FAILED + message, 1);
                }
              }
              loginServerMode.dispose();
            } else if (mechanism.startsWith("CRAM")) {

              CRAMServerMode cramServerMode = new CRAMServerMode(false,
                      configurationManager.getBackEnd().getDefaultDomain().getDomainName(),
                      mechanism);
              cramServerMode.setClientIp(clientIp);
              try {

                if (clientResponse != null) {
                  throw new SaslException("No initial clent response is specified by CRMA-MD5");
                }
                write(MESSAGE_INTERMEDIATE + new String(Base64.encodeBase64(cramServerMode.evaluateResponse(null)), US_ASCII), 0);

                clientResponse = read();
                if (clientResponse.equals("*")) throw new SaslException("Client cancelled authentication process");
                if (!Base64.isArrayByteBase64(clientResponse.getBytes(US_ASCII))) {
                  throw new SaslException("Can not decode Base64 Content", new MalformedBase64ContentException());
                }
                cramServerMode.evaluateResponse(Base64.decodeBase64(clientResponse.getBytes(US_ASCII)));

                user = cramServerMode.getUser();
                write(MESSAGE_AUTH_SUCCESS, 0);
              } catch (SaslException ex) {
                log.error(ex.getMessage());
                if (ex.getCause() != null) {
                  write(ex.getCause().getMessage(), 1);
                } else {
                  String message = "";
                  if (ex.getCause() != null) {
                    message = ex.getMessage();
                  }
                  write(MESSAGE_AUTH_FAILED + message, 1);
                }
              }
              cramServerMode.dispose();
            } else if (mechanism.equals("DIGEST-MD5")) {
              digestMd5ServerMode = new DigestMd5ServerMode(false);
              digestMd5ServerMode.setClientIp(clientIp);
              try {
                write(MESSAGE_INTERMEDIATE + new String(digestMd5ServerMode.evaluateResponse(null), "US-ASCII"), 0);
                clientResponse = read();
                if (clientResponse.equals("*")) throw new SaslException("Client cancelled authentication process");
                byte[] rspauth = digestMd5ServerMode.evaluateResponse(clientResponse.getBytes("US-ASCII"));
                write(MESSAGE_INTERMEDIATE + new String(rspauth, "US-ASCII"), 0);

                read();
                write(MESSAGE_AUTH_SUCCESS, 0);
                if (digestMd5ServerMode.isProtected()) pop3SH.setSaslServer(digestMd5ServerMode);
                user = digestMd5ServerMode.getUser();
              } catch (SaslException ex) {
                log.error(ex.getMessage());
                if (ex.getCause() != null) {
                  write(ex.getCause().getMessage(), 1);
                } else {
                  String message = "";
                  if (ex.getCause() != null) {
                    message = ex.getMessage();
                  }
                  write(MESSAGE_AUTH_FAILED + message, 1);
                }
              }
            } else if (mechanism.startsWith("SCRAM")) {
              SCRAMServerMode scramServerMode = new SCRAMServerMode(true, mechanism);
              scramServerMode.setClientIp(clientIp);
              if (clientResponse == null) {
                write(MESSAGE_INTERMEDIATE, 0);
                clientResponse = read();
              }
              try {

                if (clientResponse.equals("*")) throw new SaslException("Client cancelled authentication process");
                byte[] challenge = scramServerMode.evaluateResponse(clientResponse.getBytes(UTF_8));
                write(MESSAGE_INTERMEDIATE + new String(challenge, UTF_8), 0);

                clientResponse = read();
                if (clientResponse.equals("*")) throw new SaslException("Client cancelled authentication process");
                challenge = scramServerMode.evaluateResponse(clientResponse.getBytes(UTF_8));

                user = scramServerMode.getUser();
                write(MESSAGE_AUTH_SUCCESS + new String(challenge, UTF_8), 0);
              } catch (SaslException ex) {
                log.error(ex.getMessage());
                if (ex.getCause() != null) {
                  write(ex.getCause().getMessage(), 1);
                } else {
                  String message = "";
                  if (ex.getCause() != null) {
                    message = ex.getMessage();
                  }
                  write(MESSAGE_AUTH_FAILED + message, 1);
                }
              }
              scramServerMode.dispose();
            } else if (mechanism.equals("GSSAPI")) {
              try {
                write(MESSAGE_INTERMEDIATE, 0);
                gssServerMode = AuthContext.getInstance().getGSSServerMode(false, clientIp);
                gssServerMode.setClientIp(clientIp);
                int tokenLength = 12288;
                byte[] token = new byte[tokenLength], temp;
                while (true) {
                  tokenLength = pop3SH.read(token, 0, tokenLength);
                  temp = new byte[tokenLength];
                  System.arraycopy(token, 0, temp, 0, tokenLength);
                  if (temp.length == 1 && temp[0] == 0x2a)
                    throw new SaslException("Client cancelled authentication process");
                  token = gssServerMode.evaluateResponse(temp);
                  if (gssServerMode.isComplete()) {
                    write(MESSAGE_AUTH_SUCCESS, 0);
                    if (gssServerMode.isProtected()) pop3SH.setSaslServer(gssServerMode);
                    user = gssServerMode.getUser();
                  }
                  write(MESSAGE_INTERMEDIATE + new String(token, "US-ASCII"), 0);
                  tokenLength = 12288;
                  token = new byte[tokenLength];
                }

              } catch (SaslException ex) {
                log.error(ex.getMessage(), ex);
                if (ex.getCause() != null) {
                  write(ex.getCause().getMessage(), 1);
                } else {
                  String message = "";
                  if (ex.getCause() != null) {
                    message = ex.getMessage();
                  }
                  write(MESSAGE_AUTH_FAILED + message, 1);
                }
              }
            }
          } else {
            write(MESSAGE_ERROR, 1);
          }
        }
      }
      //Check to see if the CAPA command was sent
      else if (command.equals(COMMAND_CAPA)) {
        write(MESSAGE_CAPA, 0);
        if (!isSecure) write("STLS", 0);
        if (allowClearText == CLEAR_TEXT.ALWAYS || (allowClearText != CLEAR_TEXT.NEVER && isEncrypted)) {
          write("USER", 0);
        }
        StringBuilder auth_mech = new StringBuilder();
        int mechCount = instanceAuthMech.length - (allowClearText == CLEAR_TEXT.ALWAYS ? 0 : (allowClearText == CLEAR_TEXT.NEVER ? 2 : (isEncrypted ? 0 : 2)));
        if (mechCount > 0) {
          for (int i = 0; i < mechCount; i++) {
            auth_mech.append(" ").append(instanceAuthMech[i]);
          }
          write("SASL" + auth_mech.toString(), 0);
        }
        write("TOP", 0);
        write("UIDL", 0);
        write("RESP-CODES", 0);
        write(".", 0);
        continue;
      } else {
        write(MESSAGE_INVALID_COMMAND + command, 1);
      }
      if (user == null) attempts++;
      else return user;
      if (configurationManager.getMaxPassAttempts() != 0 && attempts > configurationManager.getMaxPassAttempts())
        return null;
    } while (true);
  }

  /**
   * Processes input to derive username
   *
   * @param argument        the argument part of the input
   * @param su              a session user class
   * @param deliveryService used to check the state of the user's mailbox
   * @return a valid user otherwise null
   */
  private User processUsername(String argument, Pop3Processor.SessionUser su, DeliveryService deliveryService)
          throws TooManyErrorsException, SocketException, IOException {

    //Make sure a username is sent
    if (argument.equals("")) {
      write(MESSAGE_TOO_FEW_ARGUMENTS, 1);
    } else {
      int atIndex = argument.indexOf("@");

      //Verify that the username contains the domain.
      if (atIndex == -1) {
        write(MESSAGE_NEED_USER_DOMAIN, 1);
      } else {
        //Accept the user, and proceed to get the password.
        try {
          su.setAddress(new EmailAddress(argument));
        } catch (InvalidAddressException iae) {
          write(MESSAGE_AUTH_FAILED + iae.getMessage(), 1);
          return null;
        }

        //Check to see if the user's mailbox is locked
        if (deliveryService.isMailboxLocked(su.getAddress())) {
          write(MESSAGE_USER_MAILBOX_LOCKED, 1);
        } else {
          write(MESSAGE_USER_ACCEPTED + argument, 0);
          return processPassword(su, deliveryService);
        }
      }
    }
    return null;
  }

  /**
   * Processes input to derive password
   *
   * @param su              a session user class
   * @param deliveryService used to check the state of the user's mailbox
   * @return a valid user otherwise null
   */
  private User processPassword(Pop3Processor.SessionUser su, DeliveryService deliveryService)
          throws TooManyErrorsException, SocketException, IOException {
    String inputString = read();

    String command = parseCommand(inputString);
    String argument = parseArgument(inputString);
    if (command == null) {
      write(MESSAGE_UNRECOGNIZED_COMMAND, 0);
    }
    //Check to see if the user command was dispatched.
    else if (command.equals(COMMAND_PASS)) {

      //Make sure they sent a password
      if (argument.equals("")) {
        write(MESSAGE_TOO_FEW_ARGUMENTS, 1);
      } else {
        su.setPassword(argument.toCharArray());
        User user = configurationManager.getUser(su.getAddress());
        if (user != null && user.isPasswordValid(su.getPassword())) {
          deliveryService.ipAuthenticated(clientIp);
          deliveryService.lockMailbox(su.getAddress());
          write(MESSAGE_LOGIN_SUCCESSFUL, 0);
          if (log.isInfoEnabled())
            log.info("User: " + su.getAddress() + " logged in successfully.");
          return user;
        } else {
          //The login failed, display a message to the user and disconnect.
          write(MESSAGE_INVALID_LOGIN + su.getAddress().getUsername(), 1);
          log.info("Login failed for user: " + su.getAddress());
        }
      }
    } else {
      write(MESSAGE_INVALID_COMMAND + command, 1);
    }
    return null;
  }

  /**
   * Handles all the commands related to the retrieval of mail.
   *
   * @return true if connection is to be terminated
   */
  private String[] transaction() throws TooManyErrorsException,
          SocketTimeoutException, SocketException, IOException {

    //Reusable Variables.
    String inputString;
    String command;
    String argument;

    //This just runs until a SystemException is thrown, which
    //signals us to disconnect.
    while (true) {

      inputString = read();

      if (errorcount >= maxerrorcount) {
        return new String[]{};
      }

      command = parseCommand(inputString);
      argument = parseArgument(inputString);
      if (command == null) {
        write(MESSAGE_UNRECOGNIZED_COMMAND, 1);
      } else if (checkQuit(command)) {
        //Enter UPDATE state
        updateState = true;
        //Delete the messages marked as deleted from persistence unit
        if (user != null) {
          return pop3mh.deleteMessages();
        }
        updateState = false;
        return new String[]{};

      }
      //Identify the command and call the appropriate helper method.
      else if (command.equals(COMMAND_STAT)) {
        handleStat();
      } else if (command.equals(COMMAND_LIST)) {
        handleList(argument);
      } else if (command.equals(COMMAND_RETR)) {
        handleRetr(argument);
      } else if (command.equals(COMMAND_DELE)) {
        handleDele(argument);
      } else if (command.equals(COMMAND_NOOP)) {
        write("+OK", 0);
      } else if (command.equals(COMMAND_RSET)) {
        handleRset();
      } else if (command.equals(COMMAND_TOP)) {
        handleTop(argument);
      } else if (command.equals(COMMAND_UIDL)) {
        handleUidl(argument);
      } else {
        write(MESSAGE_INVALID_COMMAND + command, 1);
      }
    }
  }

  /**
   * Handles the 'stat' command, which returns the total number of message
   * and the total size of those message.
   */
  private void handleStat() throws TooManyErrorsException, IOException {

    write("+OK " + pop3mh.getNumberOfNonDeletedMessages() + " " + pop3mh.getSizeOfAllNonDeletedMessages(), 0);
  }

  /**
   * Handles the 'list' command, which returns the total number of messages and
   * size along with a list of the individual message sizes.
   */
  private void handleList(String argument) throws TooManyErrorsException, IOException {

    if (argument.equals("")) {
      long numMessages = pop3mh.getNumberOfNonDeletedMessages();
      if (numMessages == 0L) {
        write("+OK ", 0);
      } else {
        long sizeMessage = pop3mh.getSizeOfAllNonDeletedMessages();

        write("+OK " + numMessages + " messages (" + sizeMessage + " octets)", 0);

        for (int index = 0; index < numMessages; index++) {
          write((index + 1) + " " + pop3mh.getMessage(index + 1).getMessageSize(user), 0);
        }
      }
      write(".", -1);
    } else {
      int messageNumber = 0;

      try {
        messageNumber = Integer.parseInt(argument);
      } catch (NumberFormatException nfe) {
        write(MESSAGE_NOT_A_NUMBER, 1);
        return;
      }

      long numMessages = pop3mh.getNumberOfMessage();

      if (messageNumber > numMessages || pop3mh.getMessage(messageNumber).isDeleted()) {
        write(MESSAGE_NO_SUCH_MESSAGE, 1);
        return;
      }

      write("+OK " + messageNumber + " " + pop3mh.getMessage(messageNumber).getMessageSize(user), 0);
    }
  }

  /**
   * Sends the specified email message to the client.
   */
  private void handleRetr(String argument) throws TooManyErrorsException, IOException {

    int messageNumber = 0;

    try {
      messageNumber = Integer.parseInt(argument);
    } catch (NumberFormatException nfe) {
      write(MESSAGE_NOT_A_NUMBER, 1);
      return;
    }

    long numMessages = pop3mh.getNumberOfMessage();

    if (log.isDebugEnabled()) {
      log.debug("Is Msg Deleted: " + pop3mh.getMessage(messageNumber).isDeleted());
      log.debug("Message: " + messageNumber + " of " + numMessages);
    }
    if (messageNumber > numMessages || pop3mh.getMessage(messageNumber).isDeleted()) {
      write(MESSAGE_NO_SUCH_MESSAGE, 1);
      return;
    }

    write(MESSAGE_OK, 0);
    try {
      pop3mh.retreiveMessage(pop3SH, messageNumber);
    } catch (FileNotFoundException fnfe) {
      log.error("Requested message for user " + user.getUserAdress() + " could not be found on disk.", fnfe);
      write("-ERR Error retrieving message", 1);
    } catch (Exception e) {
      log.error("Error retrieving message.", e);
      write("-ERR Error retrieving message", 1);
    }
  }

  /**
   * Marks the specified message for deletion.  The message will only
   * be deleted if the user later enters the QUIT command, as per the
   * spec.
   */
  private void handleDele(String argument) throws TooManyErrorsException, IOException {

    int messageNumber = 0;

    try {
      messageNumber = Integer.parseInt(argument);
    } catch (NumberFormatException nfe) {
      write(MESSAGE_NOT_A_NUMBER, 1);
      return;
    }

    long numMessages = pop3mh.getNumberOfMessage();

    if (messageNumber > numMessages) {
      write(MESSAGE_NO_SUCH_MESSAGE, 1);
    } else if (pop3mh.getMessage(messageNumber).isDeleted()) {
      write(MESSAGE_ALREADY_DELETED, 1);
    } else {
      pop3mh.getMessage(messageNumber).setDeleted(true);
      write(MESSAGE_OK, 0);
    }
  }

  /**
   * Unmarks all deleted messages.
   */
  private void handleRset() throws TooManyErrorsException, IOException {

    Pop3Message[] messages = pop3mh.getMessages();
    int numMessage = messages.length;

    for (int index = 0; index < numMessage; index++) {
      messages[index].setDeleted(false);
    }
    updateState = false;
    write(MESSAGE_OK, 0);
  }

  /**
   * Returns the header and first x lines for the
   * specified message.
   */
  private void handleTop(String argument) throws TooManyErrorsException, IOException {

    if (log.isDebugEnabled()) {
      log.debug("In Top");
    }

    int messageNumber = 0;
    int numLines = 0;

    int spaceIndex = argument.indexOf(" ");
    if (spaceIndex == -1) {
      write(MESSAGE_TOO_FEW_ARGUMENTS, 1);
      return;
    }
    String arg1 = argument.substring(0, spaceIndex).trim();
    String arg2 = argument.substring(spaceIndex + 1).trim();

    try {
      messageNumber = Integer.parseInt(arg1);
      numLines = Integer.parseInt(arg2);
    } catch (NumberFormatException nfe) {
      write(MESSAGE_NOT_A_NUMBER, 1);
      return;
    }

    long numMessages = pop3mh.getNumberOfMessage();

    if (log.isDebugEnabled()) {
      log.debug("Is Msg Deleted: " + pop3mh.getMessage(messageNumber).isDeleted());
      log.debug("Message: " + messageNumber + " of " + numMessages);
    }
    if (messageNumber > numMessages || pop3mh.getMessage(messageNumber).isDeleted()) {
      write(MESSAGE_NO_SUCH_MESSAGE, 1);
      return;
    }

    write(MESSAGE_OK, 0);

    try {
      pop3mh.retreiveMessageTop(pop3SH, messageNumber, numLines);
    } catch (FileNotFoundException fnfe) {
      log.error("Requested message for user " + user.getUserAdress() + " could not be found on disk.", fnfe);
      write("-ERR Error retrieving message", 1);
    } catch (IOException ioe) {
      log.error("Error retrieving message.", ioe);
      write("-ERR Error retrieving message", 1);
    }

  }

  /**
   * Returns the unique id of the specified message, or all the unique
   * ids of the non-deleted messages.
   */
  private void handleUidl(String argument) throws TooManyErrorsException, IOException {

    //Return all messages unique ids
    if (argument == null || argument.length() == 0) {

      long numMessages = pop3mh.getNumberOfMessage();
      Pop3Message message;

      write(MESSAGE_OK, 0);

      //Write out each non-deleted message id.
      for (int index = 0; index < numMessages; index++) {
        message = pop3mh.getMessage(index + 1);
        if (!message.isDeleted()) {
          write((index + 1) + " " + message.getUniqueId(), 0);
        }
      }

      write(".", 0);
    }
    //Ouput a single messages unique id.
    else {

      int messageNumber = 0;

      try {
        messageNumber = Integer.parseInt(argument);
      } catch (NumberFormatException nfe) {
        write(MESSAGE_NOT_A_NUMBER, 1);
        return;
      }

      long numMessages = pop3mh.getNumberOfMessage();

      if (messageNumber > numMessages || pop3mh.getMessage(messageNumber).isDeleted()) {
        write(MESSAGE_NO_SUCH_MESSAGE, 1);
        return;
      }

      write(MESSAGE_OK + " " + messageNumber + " " + pop3mh.getMessage(messageNumber).getUniqueId(), 0);
    }
  }

  /**
   * Reads a line from the input stream and returns it.
   */
  public String read() throws SocketException, SocketTimeoutException, IOException {
    try {
      socket.setSoTimeout(10 * 60 * 1000);
      String inputLine = pop3SH.readLine();
      //Log the input, unless it is a password.
      if (log.isDebugEnabled() && !inputLine.startsWith("PASS")) {
        log.debug("Read Input: " + inputLine);
      }
      return inputLine;
    } catch (NullPointerException npe) {
      return null;
    }
  }

  /**
   * Writes the specified output message to the client.
   */
  public void write(String message, int errorIncrement) throws TooManyErrorsException, IOException {
    errorcount += errorIncrement;
    if (errorcount >= maxerrorcount) throw new TooManyErrorsException();
    if (message != null) {
      if (log.isDebugEnabled()) {
        log.debug("Writing Output: " + message);
      }
      pop3SH.print(message);
    }
  }

  /**
   * Parses the input stream for the command.  The command is the
   * begining of the input stream to the first space.  If there is
   * space found, the entire input string is returned.
   * <p/>
   * This method converts the returned command to uppercase to allow
   * for easier comparison.
   * <p/>
   * Additinally, this method checks to verify that the quit command
   * was not issued.  If it was, a null String is returned to terminate
   * the connection.
   */
  private String parseCommand(String inputString) {

    if (inputString == null) return null;
    int index = inputString.indexOf(" ");
    String command = index == -1 ?
            inputString.toUpperCase() : inputString.substring(0, index).toUpperCase();
    return command;
  }

  /**
   * Parses the input stream for the argument.  The argument is the
   * text starting afer the first space until the end of the inputstring.
   * If there is no space found, an empty string is returned.
   * <p/>
   * This method does not convert the case of the argument.
   */
  private String parseArgument(String inputString) {

    if (inputString == null) return null;
    int index = inputString.indexOf(" ");
    return index == -1 ? "" : inputString.substring(index + 1).trim();
  }

  //***************************************************************
  // Constants
  //***************************************************************

  //Message Constants
  //General Pop3Message
  public static final String WELCOME_MESSAGE = "+OK Pop3 Server Ready";
  public static final String MESSAGE_DISCONNECT = "+OK Pop server signing off";
  public static final String MESSAGE_OK = "+OK";
  public static final String MESSAGE_ERROR = "-ERR";
  public static final String MESSAGE_INVALID_COMMAND = "-ERR Unknown command: ";
  public static final String MESSAGE_UNRECOGNIZED_COMMAND = "-ERR Unrecognized command: ";
  public static final String MESSAGE_TOO_FEW_ARGUMENTS = "-ERR Too few arguments for this command";

  //Authorization Messages
  public static final String MESSAGE_NEED_USER_DOMAIN = "-ERR User names must contain the username and domain.  ex: \"root@mydomain.com\"";
  public static final String MESSAGE_USER_ACCEPTED = "+OK Password required for ";
  public static final String MESSAGE_LOGIN_SUCCESSFUL = "+OK Login successful";
  public static final String MESSAGE_CAPA = "+OK Capability list follows";
  public static final String MESSAGE_USER_MAILBOX_LOCKED = "-ERR [IN-USE] User's Mailbox is locked";
  public static final String MESSAGE_INVALID_LOGIN = "-ERR Authentication credentials are invalid ";
  public static final String MESSAGE_START_TLS = "+OK begin TLS negotiation";
  public static final String MESSAGE_AUTH_FAILED = "-ERR ";
  public static final String MESSAGE_AUTH_FAILED_UNKNOWN = "-ERR Unknown authentication error";
  public static final String MESSAGE_ALREADY_AUTHENTICATED = "-ERR Already authenticated";
  public static final String MESSAGE_AUTH_SUCCESS = "+OK Maildrop locked and ready";
  public static final String MESSAGE_INTERMEDIATE = "+ ";

  //Other Messages
  public static final String MESSAGE_NOT_A_NUMBER = "-ERR Command requires a valid number as an argument";
  public static final String MESSAGE_NO_SUCH_MESSAGE = "-ERR No such message";
  public static final String MESSAGE_ALREADY_DELETED = "-ERR Message already deleted";

  //Command Constants
  private static final String COMMAND_QUIT = "QUIT";
  private static final String COMMAND_USER = "USER";
  private static final String COMMAND_PASS = "PASS";
  private static final String COMMAND_STAT = "STAT";
  private static final String COMMAND_LIST = "LIST";
  private static final String COMMAND_RETR = "RETR";
  private static final String COMMAND_DELE = "DELE";
  private static final String COMMAND_NOOP = "NOOP";
  private static final String COMMAND_RSET = "REST";
  private static final String COMMAND_TOP = "TOP";
  private static final String COMMAND_UIDL = "UIDL";
  private static final String COMMAND_CAPA = "CAPA";
  private static final String COMMAND_AUTH = "AUTH";
  private static final String COMMAND_STLS = "STLS";

  private static final String[] AUTH_MECH = new String[]{"GSSAPI", "SCRAM-SHA-512", "SCRAM-SHA-384", "SCRAM-SHA-256", "SCRAM-SHA-1", "DIGEST-MD5", "CRAM-SHA-512", "CRAM-SHA-384", "CRAM-SHA-256", "CRAM-SHA-1", "CRAM-MD5", "LOGIN", "PLAIN"};

  protected static final String US_ASCII = "US-ASCII";
  protected static final String UTF_8 = "UTF-8";
}

