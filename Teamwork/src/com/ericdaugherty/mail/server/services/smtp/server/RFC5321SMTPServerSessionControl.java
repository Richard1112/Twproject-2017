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

package com.ericdaugherty.mail.server.services.smtp.server;

//Java imports
import java.io.IOException;
import java.net.*;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.net.ssl.SSLSocket;
import javax.security.sasl.*;

//Log imports
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

//Local imports
import com.ericdaugherty.mail.server.Mail;
import com.ericdaugherty.mail.server.auth.*;
import com.ericdaugherty.mail.server.configuration.*;
import com.ericdaugherty.mail.server.configuration.ConfigurationParameterConstants.CLEAR_TEXT;
import com.ericdaugherty.mail.server.errors.*;
import com.ericdaugherty.mail.server.security.transport.TransportLayer;
import com.ericdaugherty.mail.server.services.general.*;
import com.ericdaugherty.mail.server.services.smtp.server.command.Command;
import com.ericdaugherty.mail.server.services.smtp.server.command.impl.*;
import com.ericdaugherty.mail.server.services.smtp.server.support.*;
import com.ericdaugherty.mail.server.services.smtp.server.transaction.TransactionControl;

/**
 *
 * @author Andreas Kyrmegalos
 */
public class RFC5321SMTPServerSessionControl implements ConnectionProcessor, SMTPServerSessionControl {

   /**
    * Logger Category for this class.
    */
   //private static final Log log = LogFactory.getLog(RFC5321SMTPServerSessionControl.class);
  private static Log log = LogFactory.getLog("JESLogger");
   /**
    * The ConfigurationManager
    */
   protected final ConfigurationManager configurationManager = ConfigurationManager.getInstance();
   
   /**
    * A list of commands allowed when the SMPT extensions are not allowed.
    */
   private static final Set<COMMAND_VERB> baseCommands = new LinkedHashSet<COMMAND_VERB>();
   static {
      baseCommands.add(COMMAND_VERB.HELO);
      baseCommands.add(COMMAND_VERB.MAIL);
      baseCommands.add(COMMAND_VERB.RCPT);
      baseCommands.add(COMMAND_VERB.DATA);
      baseCommands.add(COMMAND_VERB.QUIT);
      baseCommands.add(COMMAND_VERB.NOOP);
      baseCommands.add(COMMAND_VERB.RSET);
      baseCommands.add(COMMAND_VERB.VRFY);
   }
   
   private Command lastCommand;
   private long anyCommand = 0;
   private FlowControl flowControl;
   private SESSION_STATE sessionState;
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
   private String clientIP;
   /**
    * The host name of the client
    */
   private String clientDomain;
   /**
    * The client's host as declared by the EHLO/HELO command.
    */
   private String declaredClientHost;
   /**
    * This setting defines the security state of the session. Please note the following: A
    * connection over a standard port with the setting standardsecure=true starts as non-secure. A
    * connection over a standard port with the setting standardsecure=false is considered secure for
    * the duration of the session. A connection over a secure port is considered secure for the
    * duration of the session.
    */
   private boolean secured;
   /**
    * This setting is used to certify the encryption state of the connection
    */
   private boolean encrypted;
   /**
    * This setting is used to track whether ESMTP was used during the last session
    */
   private boolean eSMTP;
   private boolean rejected;
   private boolean mime8bitSupported;
   private boolean pipeliningSupported;
   private boolean heloEnabled;
   /**
    * The number of errors during a given session
    */
   private int errorCount;
   /**
    * The maximum number of allowed errors
    */
   private int maxErrorCount = configurationManager.getMaxErrorCount();
   private CLEAR_TEXT clearTextAllowed = configurationManager.allowClearTextSMTP();
   private volatile boolean updatingServerSocket;
   protected boolean useAmavisSMTPDirectory;
   private boolean authenticated;
   private boolean authenticating;
   protected VerifyIP verifyIP;
   private ProcessorStreamHandler smtpPSH = new ProcessorStreamHandler();
   private StandardReplyWriter replyWriter;
   private String[] instanceAuthMech;
   private SaslServer saslServer;
   private TransactionControl transactionControl;
   private TransportLayer transportLayer;
   private final FlowControl initFlowControl = new InitFlowControl();
   private final FlowControl mailFlowControl = new MailFlowControl();
   
   private final Map<String, Command> commandMap;

   {
      Map<String, Command> tempMap = new LinkedHashMap<String, Command>();
      
      tempMap.put(COMMAND_VERB.EHLO.getLiteral(), new EhloCommand());
      tempMap.put(COMMAND_VERB.STLS.getLiteral(), new StlsCommand());
      tempMap.put(COMMAND_VERB.AUTH.getLiteral(), new AuthCommand());
      tempMap.put(COMMAND_VERB.MAIL.getLiteral(), new MailCommand());
      tempMap.put(COMMAND_VERB.RCPT.getLiteral(), new RcptCommand());
      tempMap.put(COMMAND_VERB.DATA.getLiteral(), new DataCommand());
      tempMap.put(COMMAND_VERB.QUIT.getLiteral(), new QuitCommand());
      tempMap.put(COMMAND_VERB.RSET.getLiteral(), new RsetCommand());
      tempMap.put(COMMAND_VERB.NOOP.getLiteral(), new NoopCommand());
      tempMap.put(COMMAND_VERB.VRFY.getLiteral(), new VrfyCommand());
      tempMap.put(COMMAND_VERB.EXPN.getLiteral(), new ExpnCommand());
      tempMap.put(COMMAND_VERB.HELO.getLiteral(), new HeloCommand());

      commandMap = Collections.unmodifiableMap(tempMap);
   }

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

   private boolean isUpdatingServerSocket() {
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

   public boolean getUseAmavisSMTPDirectory() {
      return useAmavisSMTPDirectory;
   }

   public void setUseAmavisSMTPDirectory(boolean useAmavisSMTPDirectory) {
      this.useAmavisSMTPDirectory = useAmavisSMTPDirectory;
   }

   public void setupVerifyIP() {
      verifyIP = VerifyIPFactory.getNewVerifyIPInstance(useAmavisSMTPDirectory || !configurationManager.isVerifyIP(), useAmavisSMTPDirectory);
   }

   /**
    * Entrypoint for the Thread, this method handles the interaction with the client socket.
    */
   public void run() {
      
      if (isDelayedStart()) {
         synchronized(this) {
            while(isDelayedStart()&&running) {
               try {
                  wait(500);
               }
               catch(InterruptedException ie) {
                  if (!running) {
                     break;
                  }
               }
            }
         }
      }

      if (running) {
         transactionControl = new TransactionControl(this, configurationManager.isAmavisSupportActive() && useAmavisSMTPDirectory);
      }

      boolean connected;
      boolean forcedExit;
      while (running) {

         connected = false;
         forcedExit = false;
         authenticated = false;
         authenticating = false;
         eSMTP = true;
         mime8bitSupported = configurationManager.is8bitMIME();
         pipeliningSupported = configurationManager.isPipelining();
         replyWriter = pipeliningSupported ? new PipelinedReplyWriter() : new StandardReplyWriter();
         transactionControl.setProcessorStreamHandler(smtpPSH);
         transactionControl.setReplyWriter(replyWriter);
         heloEnabled = configurationManager.isHELOEnabled();
         rejected = false;
         errorCount = 0;
         StringBuilder sb = new StringBuilder(30);
         AUTH_MECH[] am = AUTH_MECH.values();
         if (configurationManager.isGSSEnabled()) {
            sb.append(AUTH_MECH.GSSAPI.getName());
            sb.append(",");
         }
         if (configurationManager.getSCRAMMembers() != null) {
            for (int i = 1; i < 5; i++) {
               if (configurationManager.getSCRAMMembers().contains(am[i].getName())) {
                  sb.append(am[i].getName());
                  sb.append(",");
               }
            }
         }
         if (configurationManager.isDigestMD5Enabled()) {
            sb.append(AUTH_MECH.DIGEST_MD5.getName());
            sb.append(",");
         }
         if (configurationManager.getCRAMMembers() != null) {
            for (int i = 6; i < 11; i++) {
               if (configurationManager.getCRAMMembers().contains(am[i].getName())) {
                  sb.append(am[i].getName());
                  sb.append(",");
               }
            }
         }
         sb.append(AUTH_MECH.PLAIN.getName());
         sb.append(",");
         sb.append(AUTH_MECH.LOGIN.getName());
         instanceAuthMech = sb.toString().split(",");
         try {
            socket = serverSocket.accept();

            secured = serverSocket.getLocalPort() == configurationManager.getSecureSMTPPort()
                  ? true : configurationManager.isStandardSMTPSecure() ? false : true;
            encrypted = serverSocket.getLocalPort() == configurationManager.getSecureSMTPPort()
                  ? true : false;
            if (secured&&encrypted) {
               TransportLayer transportLayer = new TransportLayer((SSLSocket) socket);
               try {
                  transportLayer.verifyPeer(true, true);
               } catch (IOException ioe) {
                  log.error(ioe.getMessage());
                  encrypted = false;
                  //replace all the commands with ones that give a 554 reply
                  setFailedTLSHandshake();
               } finally {
                  transportLayer.conclude();
               }
            }
            connected = true;

            //Prepare the input and output streams.
            InetAddress remoteAddress = null;
            smtpPSH.setStreams(socket);

            remoteAddress = socket.getInetAddress();

            clientIP = remoteAddress.getHostAddress();
            clientDomain = remoteAddress.getHostName();

            if (log.isInfoEnabled()) {
               log.info(clientDomain + "(" + clientIP + ") socket connected via SMTP.");
            }

            //Output the welcome/reject message.
            if (!(remoteAddress.isLoopbackAddress() || remoteAddress.isSiteLocalAddress()) && verifyIP.blockIP(clientIP)) {
               replyWriter.writeLast(REJECT_MESSAGE);
               rejected = true;
            } else {
               replyWriter.writeLast(WELCOME_MESSAGE);
            }


            //Parses the input for commands and delegates to the appropriate methods.
            forcedExit = sessionFlowControl();

         } catch (TooManyErrorsException e) {
            log.error("The session generated too many errors");
         } catch (SocketTimeoutException e) {
            if (connected) {
               log.error(" Timedout waiting for client command.");
               forcedExit = true;
            }
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
            if (socket != null) {
               if (!socket.isClosed()) {
                  forcedExit = true;
               } else {
                  connected = false;
               }
            }
         } //If any exception gets to here uncaught, it means we should just disconnect.
         catch (Throwable e) {
            log.error("Unknown Exception:", e);
            connected = false;
         } finally {
            if (socket != null && connected) {
               try {
                  if (!forcedExit) {
                     replyWriter.writeLast((errorCount >= maxErrorCount) ? FORCED_EXIT_MESSAGE : MESSAGE_DISCONNECT);
                  } else {
                     if (transactionControl.isForceExitRCPT()) {
                        replyWriter.writeLast(MESSAGE_EXCESS_FAIL_RCPT_DISCONNECT);
                     } else {
                        replyWriter.writeLast(FORCED_EXIT_MESSAGE);
                     }
                  }
               } catch (TooManyErrorsException ex) {
               } catch (IOException ioe) {
               }
            }
            try {
               if (socket != null) {
                  socket.close();
               }
            } catch (IOException e) {
               log.error("Error disconnecting.", e);
               //Nothing to do.
            } finally {
               socket = null;
            }
            if (saslServer != null) {
               try {
                  saslServer.dispose();
               } catch (SaslException ex) {
               }
               saslServer = null;
            }
            //reset the initial state
            smtpPSH = new ProcessorStreamHandler();
            sessionState = SESSION_STATE.INIT;
            transactionControl.resetMessage();
            anyCommand = 0;
            Iterator<Command> iter = commandMap.values().iterator();
            while(iter.hasNext()) {
               iter.next().reset();
            }
         }
         if (isUpdatingServerSocket()) {
            synchronized(this) {
               while(isUpdatingServerSocket()) {
                  try {
                     wait(500);
                  }
                  catch(InterruptedException ie) {
                     if (!running) {
                        break;
                     }
                  }
               }
            }
         }
      }
      log.warn("SMTPProcessor shut down gracefully");
   }

   /**
    * Notifies this thread to stop processing and exit.
    */
   public void shutdown() {
      log.warn("Shutting down SMTPProcessor.");
      running = false;
      if (transactionControl==null||!transactionControl.isFinishedData()) {
         if (socket != null) {
            try {
               socket.close();
            } catch (IOException ex) {
            }
         }
      }
   }

   private boolean sessionFlowControl() throws TooManyErrorsException,
         SocketTimeoutException, SocketException, IOException {

      String line;
      Command command;
      flowControl = initFlowControl;
      sessionState = SESSION_STATE.INIT;
      lastCommand = commandMap.get(COMMAND_VERB.NOOP.getLiteral());

      do {

         line = read();

         try {

            if (line == null) {
               incrementErrorCount();
               throw new SMTPReplyException(MESSAGE_INVALID_COMMAND);
            }

            command = flowControl.getCommand(sessionState, line);

            if (command.isQuit()) {
               quitSession();
               return false;
            } else if (rejected) {
               incrementErrorCount();
               throw new SMTPReplyException(MESSAGE_INVALID_COMMAND);
            }

            if (transactionControl.isForceExitRCPT()) {
               //reply will be sent by the calling code
               return true;
            }

            if (!sessionState.isStateAllowedCommand(command.getCommandVerb())) {
               incrementErrorCount();
               throw new SMTPReplyException(MESSAGE_COMMAND_ORDER_INVALID);
            }
            else if(!eSMTP&&!baseCommands.contains(command.getCommandVerb())) {
               incrementErrorCount();
               throw new SMTPReplyException(MESSAGE_COMMAND_ORDER_INVALID);
            }

            flowControl.checkPrerequisites(command);

            command.parseInput(line);
            command.resetParser();
            command.executeActions(this);

            registerCommand(command.getCommandVerb());
            lastCommand = command;
         } catch (SMTPFatalReplyException e) {
            log.debug(e);
            return true;
         } catch (UnrecognizedCommandException e) {
            if (log.isDebugEnabled()) {
               log.debug("An unrecognized command in \""+line+"\" was received");
            }
            replyWriter.writeAny(e.getMessage());
         } catch (SMTPReplyException e) {
            if (log.isDebugEnabled()) {
               log.debug(e);
            }
            replyWriter.writeAny(e.getMessage());
         }
      } while (true);
   }

   private void registerCommand(COMMAND_VERB commandVerb) {
      List<COMMAND_VERB> commands = Arrays.asList(COMMAND_VERB.values());
      anyCommand |= 1 << commands.indexOf(commandVerb);
   }

   abstract class FlowControl {

      private final COMMAND_VERB[] commands = COMMAND_VERB.values();
      private int index;
      private Pattern commandPattern;
      
      public FlowControl(int index) {
         this.index = 0;
         this.commandPattern = Pattern.compile("(?i)" + commands[index].getLiteral() + "(?-i)");
      }

      public abstract void checkPrerequisites(Command command) throws SMTPReplyException;

      public Command getCommand(SESSION_STATE sessionState, String line) throws SMTPReplyException, UnrecognizedCommandException {

         int initialIndex = index;
         Matcher matcher = commandPattern.matcher(line);
         do {
            if (matcher.find() && matcher.start() == 0) {

               return commandMap.get(commands[index].getLiteral());
            }
            index++;
            if (index == initialIndex) {
               incrementErrorCount();
               throw new UnrecognizedCommandException(SMTPServerSessionControl.MESSAGE_INVALID_COMMAND + " " + line);
            }
            if (index == commands.length) {
               index = 0;
            }
            commandPattern = Pattern.compile("(?i)" + commands[index].getLiteral() + "(?-i)");
            matcher = commandPattern.matcher(line);
         } while (true);
      }
      
      public void setCommandIndex(int index) {
         this.index = index;
         commandPattern = Pattern.compile("(?i)" + commands[this.index].getLiteral() + "(?-i)");
      }
   }

   private class InitFlowControl extends FlowControl {

      public InitFlowControl() {
         super(0);
      }
      
      protected InitFlowControl(int index) {
         super(index);
      }

      public void checkPrerequisites(Command command) throws SMTPReplyException {
         command.checkInitPrerequisites(RFC5321SMTPServerSessionControl.this);
      }
   }

   private class MailFlowControl extends InitFlowControl {

      public MailFlowControl() {
         super(4);
      }

      public void checkPrerequisites(Command command) throws SMTPReplyException {
         super.checkPrerequisites(command);
         command.checkMailPrerequisites(RFC5321SMTPServerSessionControl.this);
      }
   }

   public boolean handleMailFrom(String address, String[] parameters) throws TooManyErrorsException, IOException {
      return transactionControl.handleMailFrom(address, parameters);
   }

   public void handleRcptTo(String address, String[] parameters) throws TooManyErrorsException, IOException {
      transactionControl.handleRcptTo(address, parameters);
   }

   public void handleData() throws TooManyErrorsException, SocketTimeoutException,
         SocketException, IOException {
      transactionControl.handleData();
   }

   public void startTLSHandshake() throws SMTPReplyException {

      transportLayer = new TransportLayer();

      try {
         transportLayer.init(socket, true, true, true);
      } catch (IOException ioe) {
         log.error(ioe.getMessage());
         transportLayer = null;
         throw new SMTPReplyException(MESSAGE_TLS_NOT_AVAILABLE);
      }
   }

   public void concludeTLSHandshake() throws SMTPFatalReplyException {

      boolean acceptable = false;
      try {
         transportLayer.verifyPeer(true, true);
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
            setFailedTLSHandshake();
            return;
         }

         socket = transportLayer.getSocket();
         try {
            setSuccessTLSHandshake();
         } catch (IOException ioe) {
            log.error(ioe.getLocalizedMessage());
            throw new SMTPFatalReplyException();
         }
      } catch (IOException ioe) {
         log.error(ioe.getMessage());
         setFailedTLSHandshake();
      } finally {
         transportLayer.conclude();
         transportLayer = null;
      }
   }

   private void setSuccessTLSHandshake() throws IOException {
      secured = true;
      encrypted = true;
      smtpPSH.setSecureStreams(socket);
   }

   private void setFailedTLSHandshake() {
      Iterator<Command> iter = commandMap.values().iterator();
      while (iter.hasNext()) {
         iter.next().setNoEncryptedCommandActions();
      }
      secured = false;
   }

   public void incrementErrorCount() {
      errorCount++;
   }

   public void setDeclaredClientHost(String declaredClientHost) {
      if (this.declaredClientHost != null) {
         return;
      }
      this.declaredClientHost = declaredClientHost;
   }

   public String getDeclaredClientHost() {
      return declaredClientHost;
   }

   public String getClientIP() {
      return clientIP;
   }

   public String getClientDomain() {
      return clientDomain;
   }

   public boolean isCommandReceived(COMMAND_VERB commandVerb) {
      List<COMMAND_VERB> commands = Arrays.asList(COMMAND_VERB.values());
      int pos = 1 << commands.indexOf(commandVerb);
      return ((pos & anyCommand) == pos);
   }

   public boolean isLastCommand(COMMAND_VERB commandVERB) {
      return commandVERB == lastCommand.getCommandVerb();
   }

   public boolean isSecured() {
      return secured;
   }

   public boolean isEncrypted() {
      return encrypted;
   }

   public boolean isAuthenticated() {
      return authenticated;
   }

   public boolean isMime8bitSupported() {
      return mime8bitSupported;
   }

   public boolean isPipeliningSupported() {
      return pipeliningSupported;
   }

   public boolean isHeloEnabled() {
      return heloEnabled;
   }

   public void setESMTP(boolean eSMTP) {
      this.eSMTP = eSMTP;
      pipeliningSupported = false;
      mime8bitSupported = false;
      replyWriter = new StandardReplyWriter();
      transactionControl.setReplyWriter(replyWriter);
   }

   public boolean isESMTP() {
      return eSMTP;
   }

   public boolean isTooManyRCPT() {
      return transactionControl.isTooManyRCPT();
   }

   public boolean isExcessRCPT() {
      return transactionControl.isExcessRCPT();
   }

   public boolean isSingleRCPT() {
      return transactionControl.isSingleRCPT();
   }

   public void setSingleRCPT(boolean singleRCPT) {
      transactionControl.setSingleRCPT(singleRCPT);
   }

   public boolean isRCPTListEmpty() {
      return transactionControl.isRCPTListEmpty();
   }

   public CLEAR_TEXT getClearTextAllowed() {
      return clearTextAllowed;
   }

   public String[] getAuthMechs() {
      return instanceAuthMech.clone();
   }

   public String getSSLHeaderField() {

      if (secured
            && ((configurationManager.isSecureActive() && serverSocket.getLocalPort() == configurationManager.getSecureSMTPPort())
            || (configurationManager.isStandardSMTPSecure() && serverSocket.getLocalPort() == configurationManager.getSMTPPort()))) {
         StringBuilder sb = new StringBuilder("        (using ");
         sb.append(((SSLSocket) socket).getSession().getProtocol()).append(" protocol with ");
         sb.append(((SSLSocket) socket).getSession().getCipherSuite()).append(" ciphersuite.)");
         return sb.toString();
      }
      return null;
   }

   public void setReplyAny(String reply) throws TooManyErrorsException, IOException {
      replyWriter.writeAny(reply);
   }

   public void setReplyLast(String reply) throws TooManyErrorsException, IOException {
      replyWriter.writeLast(reply);
   }

   public void setMultiReplyLast(List<String> reply) throws TooManyErrorsException, IOException {
      for (int i = 0; i < reply.size(); i++) {
         replyWriter.writeLast(reply.get(i));
      }
   }

   public void createSASLServer(SASL_SERVER_MODE saslServerMode, String mechanism) throws SaslException {
      
      log.debug("Creating new SMTP server mode SASL server: "+saslServerMode);
      switch (saslServerMode) {
         case PLAIN:
            saslServer = new PlainServerMode(true);
            break;
         case SCRAM:
            saslServer = new SCRAMServerMode(true, mechanism);
            break;
         case LOGIN:
            saslServer = new LoginServerMode(true);
            break;
         case CRAM:
            saslServer = new CRAMServerMode(true,
                  configurationManager.getBackEnd().getDefaultDomain().getDomainName(),
                  mechanism);
            break;
         case GSSAPI:
            saslServer = AuthContext.getInstance().getGSSServerMode(false, null);
            break;
         case MD5_DIGEST:
            saslServer = new DigestMd5ServerMode(true);
            break;
         default:
            throw new AssertionError();
      }
      authenticating = true;
   }

   public String getClientResponse() throws SocketException, SocketTimeoutException, IOException{
      if (authenticating) {
         return read();
      }
      throw new IllegalStateException("Currently not authenticating.");
   }

   public int getGSSResponse(byte[] token, int startIndex, int tokenLength) throws IOException {
      if (saslServer.getMechanismName().equals("GSSAPI") && !saslServer.isComplete()) {
         return smtpPSH.read(token, 0, tokenLength);
      }
      return -1;
   }

   public byte[] evaluateSASLResponse(byte[] response) throws SaslException {
      return saslServer.evaluateResponse(response);
   }

   public boolean isSASLComplete() {
      return saslServer.isComplete();
   }

   public void setSuccessSASLNegotiation() throws IOException {

      String qop = (String) saslServer.getNegotiatedProperty(Sasl.QOP);
      if (qop.equals("auth-int") || qop.equals("auth-conf")) {
         smtpPSH.setSaslServer(saslServer);
      }
      authenticating = false;
      authenticated = true;
   }

   public String getAuthorizationID() {
      if (saslServer != null && saslServer.isComplete()) {
         return saslServer.getAuthorizationID();
      }
      return null;
   }

   public SESSION_STATE getSessionState() {
      return sessionState;
   }

   public void setInitState(boolean reset) {
      if (this.sessionState == SESSION_STATE.INIT) {
         if (reset) {
            //According to RFC4954:
            //"When a security layer takes effect, the SMTP protocol is reset to the
            //initial state (the state in SMTP after a server issues a 220 service
            //ready greeting)."
            //That definition includes knowledge of previously issued commands. Un-
            //fortunately, JavaMail ignores this statement and during a session
            //proceeds to issue a MAIL command after a successful AUTH command. For
            //the shake of interoperability, knowledge of previous commands is retained.
            
            //anyCommand = 0;
         }
         flowControl.setCommandIndex(reset?0:1);
         lastCommand = commandMap.get(COMMAND_VERB.NOOP.getLiteral());
      } else {
         if (reset) {
            throw new IllegalArgumentException("Reseting is not allowed while a mail transaction is"
                  + " in progress.");
         }
         flowControl = initFlowControl;
         flowControl.setCommandIndex(3);

         //Need to unregister MAIL, RCPT, DATA
         List<COMMAND_VERB> commands = Arrays.asList(COMMAND_VERB.values());
         long mask = 0;
         mask |= 1 << commands.indexOf(COMMAND_VERB.MAIL);
         mask |= 1 << commands.indexOf(COMMAND_VERB.RCPT);
         mask |= 1 << commands.indexOf(COMMAND_VERB.DATA);


         mask = ~mask;
         anyCommand &= mask;
      }
      this.sessionState = SESSION_STATE.INIT;
      transactionControl.resetMessage();
   }

   public void setMailState() {
      flowControl = mailFlowControl;
      flowControl.setCommandIndex(4);
      this.sessionState = SESSION_STATE.MAIL;
   }

   public void quitSession() {
      if (log.isDebugEnabled()) {
         log.debug("Client " + (declaredClientHost==null?"":declaredClientHost) + " with actual address " + socket.getRemoteSocketAddress() + " has QUIT the session.");
      }
   }

   /**
    * Reads a line from the input stream and returns it.
    */
   public String read() throws SocketException, SocketTimeoutException, IOException {

      try {
         socket.setSoTimeout(5 * 60 * 1000);
         String inputLine = smtpPSH.readLine();
         //Log the input, unless it is a password.
         if (log.isDebugEnabled() && !inputLine.toUpperCase().startsWith("PASS")) {
            log.debug("Read Input: " + inputLine);
         }
         return inputLine;
      } catch (NullPointerException npe) {
         return null;
      }
   }

   public class StandardReplyWriter implements ReplyWriter {

      /**
       * Writes the specified output message to the client.
       */
      public void writeAny(String reply) throws TooManyErrorsException, IOException {
         writeLast(reply);
      }

      /**
       * Writes the specified output message to the client.
       */
      public void writeLast(String reply) throws TooManyErrorsException, IOException {
         if (errorCount >= maxErrorCount) {
            throw new TooManyErrorsException();
         }
         if (reply != null) {
            if (log.isDebugEnabled()) {
               log.debug("Writing Output: " + reply);
            }
            smtpPSH.print(reply);
         }
      }
   }

   private final class PipelinedReplyWriter extends StandardReplyWriter {

      private static final String CRLF = "\r\n";
      private int pipelineSize = 0;
      private boolean tooManyErrors = false;
      List<String> pipeline = new ArrayList<String>(10);
      private int supportCount = 0;
      private boolean supported = false;

      /**
       * Writes the specified output message to the client.
       */
      public void writeAny(String reply) throws TooManyErrorsException, IOException {
         if (errorCount >= maxErrorCount) {
            tooManyErrors = true;
            throw new TooManyErrorsException();
         }
         if (reply != null) {
            if (!supported) {
               //Handle the SASL PLAIN case. The AUTH is always the last command in a queue except
               //for PLAIN (and EXTERNAL) that may not be. Check for incoming data. The support
               //count is not incremented in this case, but if there is available data, then there
               //is no reason not to accept that the client does indeed support PIPELINING.
               if (authenticating&&saslServer.getMechanismName().equals("PLAIN")) {
                  verifyPipelining(reply, 1, 500);
                  if (!supported) {
                     return;
                  }
               }
               //We need somehow to deal with the fact that the client may not support pipelining.
               else {
                  if (supportCount<3) {
                     supportCount = verifyPipelining(reply, supportCount, 333);
                     if (!supported) {
                        return;
                     }
                  }
                  //If two PIPELINING verifications fail, switch to StandardReplyWriter
                  else {
                     if (log.isDebugEnabled()) {
                        log.debug("Client does not support PIPELINING. Switching to StandardReplyWriter.");
                     }
                     writeLast(reply);
                     replyWriter = new StandardReplyWriter();
                     transactionControl.setReplyWriter(replyWriter);
                     return;
                  }
               }
            }
            if (log.isDebugEnabled()) {
               log.debug("Queuing Output: " + reply);
            }
            pipelineSize += reply.length();
            pipeline.add(reply);
         }
      }
      
      private int verifyPipelining(String reply, int supportCount, int time) throws IOException, TooManyErrorsException{
         int available = smtpPSH.available();
         //Need to account for network latency
         if (available==0) {
            int count = 1;
            do {
               try {
                  Thread.sleep(1L*supportCount*count*count*time);
                  available = smtpPSH.available();
                  //If it is not the last command, there should be input queuing up.
                  if (available!=0) {
                     break;
                  }
               }
               catch (InterruptedException ie) {
                  if (!running) {
                     writeLast(reply);
                     //TODO In the future come up with something more elegant
                     throw new TooManyErrorsException("Shutting down.");
                  }
               }
               count++;
            }while(count<4);
         }
         //Treat it as if it were last
         if (available==0) {
            supportCount++;
            writeLast(reply);
            return supportCount;
         }
         //If even once there is data available, it is undeniable evidence that PIPELINING
         //is indeed supported
         else {
            supported = true;
            return 0;
         }
      }

      /**
       * Writes the specified output message to the client.
       */
      public void writeLast(String reply) throws TooManyErrorsException, IOException {
         if (errorCount >= maxErrorCount) {
            if (!tooManyErrors) {
               tooManyErrors = true;
               throw new TooManyErrorsException();
            } else {
               StringBuilder sb = new StringBuilder(pipelineSize);
               pipelineSize = 0;
               for (String line : pipeline) {
                  sb.append(line);
                  sb.append(CRLF);
               }
               sb.append(SMTPServerSessionControl.FORCED_EXIT_MESSAGE);
               pipeline.clear();
               pipeline = null;
               if (log.isDebugEnabled()) {
                  log.debug("Writing Output: " + reply);
               }
               smtpPSH.print(sb.toString());
               return;
            }
         }
         if (reply != null) {
            if (log.isDebugEnabled()) {
               log.debug("Writing Output: " + reply);
            }
            pipelineSize += reply.length();
            StringBuilder sb = new StringBuilder(pipelineSize);
            pipelineSize = 0;
            for (String line : pipeline) {
               sb.append(line);
               sb.append(CRLF);
            }
            sb.append(reply);
            pipeline.clear();
            pipeline = new ArrayList(10);
            smtpPSH.print(sb.toString());
         }

      }
   }

   /**
    * Writes the specified output message to the client.
    */
   public void write(String reply, int errorIncrement) throws TooManyErrorsException, IOException {
      throw new RuntimeException("Replies are exclusively handled by instances of the ResponseHandler class.");
   }
}