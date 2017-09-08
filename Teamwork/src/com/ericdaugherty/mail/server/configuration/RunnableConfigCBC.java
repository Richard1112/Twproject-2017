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

package com.ericdaugherty.mail.server.configuration;

//Java imports
import java.io.*;
import java.net.Socket;
import java.util.*;
import java.util.concurrent.Callable;

//Log imports
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

//Local imports
import com.ericdaugherty.mail.server.configuration.backEnd.PersistException;
import com.ericdaugherty.mail.server.configuration.backEnd.PersistExecutor;
import com.ericdaugherty.mail.server.configuration.cbc.*;

//Local imports
/**
 *
 * @author Andreas Kyrmegalos
 */
public class RunnableConfigCBC<V> implements Callable<V> {

   /** Logger Category for this class. */
  // private static final Log log = LogFactory.getLog(ConnectionBasedConfigurator.class);
  private static Log log = LogFactory.getLog("JESLogger");

   private final ConfigurationManager cm = ConfigurationManager.getInstance();
   /** Socket connection to the client */
   private Socket socket;

   public RunnableConfigCBC(Socket socket) {
      this.socket = socket;
   }

   public V call(){

      BufferedReader reader = null;
      List<String> lines = null;
      try {

         reader = new BufferedReader(new InputStreamReader(socket.getInputStream(), "UTF-8"));
         lines = readInput(reader);
         if (lines != null) {
            processLines(lines);
         }
      } catch (IOException ioe) {
         log.error(ioe);
      } catch (CBCResponseException ioe) {
         log.error(ioe);
      }
      finally {
         if (lines!=null) {
            lines.clear();
         }
         if (null!=reader) {
            try {
               reader.close();
            } catch (IOException ex) {}
            reader = null;
         }
         if (null!=socket) {
            
            try {
               socket.close();
            } catch (IOException ex) {
               //Just ignore
            }
            cm.removeCBCSocket(socket);
            socket = null;
         }
         return null;
      }
   }

   private void processLines(List<String> lines) throws CBCResponseException, IOException {

      String line;
      ListIterator<String> iter = lines.listIterator();
      BackEndTypeEnum backEndType = cm.getBackEndType();
      
      CBCExecutor cbcExecutor = null;
      OutputStream os = null;
      try {
         os = socket.getOutputStream();
         os.flush();
         while (iter.hasNext()) {
            line = iter.next();
            log.debug(line);
            if (backEndType == BackEndTypeEnum.FILE) {

               cbcExecutor = new AddUsersPLL1(iter);
               cbcExecutor.processLines();
               cm.updateThroughConnection(cbcExecutor);
               
               writeNormalOutput(os, SUCCESS);
               
            } else if (backEndType == BackEndTypeEnum.RDBM) {
               if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_APPLY_CONFIG)) {

                  ApplyConfiguration ac = new ApplyConfiguration(iter);
                  byte[] response = ac.processLines();
                  
                  writeNormalOutput(os, response);
                     
               } else if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_RETRIEVE_CONFIG)) {

                  CBCResponseExecutor cbcResponseExecutor = new RetrieveConfiguration(iter);
                  byte[] response = cbcResponseExecutor.processLines();
                  
                  writeNormalOutput(os, response);
                     
               } else if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_RETRIEVE_DB_PASSWORD)) {

                  CBCResponseExecutor cbcResponseExecutor = new RetrieveDbPasswordPLL1(iter);
                  byte[] response = cbcResponseExecutor.processLines();

                  writeNormalOutput(os, response);
                     
               } else {
                  
                  if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_INSERT_DOMAIN)) {

                     cbcExecutor = new InsertDomainPLL1(iter);
                     cbcExecutor.processLines();
                  } else if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_DELETE_DOMAIN)) {

                     cbcExecutor = new DeleteDomainPLL1(iter);
                     cbcExecutor.processLines();
                  } else if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_SET_DEFAULT_DOMAIN)) {

                     cbcExecutor = new SetDefaultDomainPLL1(iter);
                     cbcExecutor.processLines();
                  } else if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_INSERT_USER)) {

                     cbcExecutor = new InsertUserPLL1(iter);
                     cbcExecutor.processLines();
                  } else if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_DELETE_USER)) {

                     cbcExecutor = new DeleteUserPLL1(iter);
                     cbcExecutor.processLines();
                  } else if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_SET_USER_PASSWORD)) {

                     cbcExecutor = new SetUserPasswordPLL1(iter);
                     cbcExecutor.processLines();
                  } else if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_ADD_FORWARD_ADDRESS)) {

                     cbcExecutor = new AddForwardAddressPLL1(iter);
                     cbcExecutor.processLines();
                  } else if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_REMOVE_FORWARD_ADDRESS)) {

                     cbcExecutor = new RemoveForwardAddressPLL1(iter);
                     cbcExecutor.processLines();
                  } else if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_SET_DEFAULT_MAILBOX)) {

                     cbcExecutor = new SetDefaultMailboxPLL1(iter);
                     cbcExecutor.processLines();
                  } else if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_INSERT_REALM)) {

                     cbcExecutor = new InsertRealmPLL1(iter);
                     cbcExecutor.processLines();
                  } else if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_DELETE_REALM)) {

                     cbcExecutor = new RemoveRealmPLL1(iter);
                     cbcExecutor.processLines();
                  } else if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_ADD_USER_TO_REALM)) {

                     cbcExecutor = new AddUserToRealmPLL1(iter);
                     cbcExecutor.processLines();
                  } else if (line.startsWith(ConnectionBasedConfiguratorConstants.COMMAND_REMOVE_USER_FROM_REALM)) {

                     cbcExecutor = new RemoveUserFromRealmPLL1(iter);
                     cbcExecutor.processLines();
                  }
                  else {
                     throw new CBCResponseException("Invalid command specified.");
                  }
                  
                  cm.updateThroughConnection(cbcExecutor);
                  writeNormalOutput(os, SUCCESS);
               }
            }//Reject anything else
            else {
               throw new CBCResponseException("LDAP not implemented, or invalid command or other exception");
            }
         }
      }
      catch (Exception e) {
         e.printStackTrace();
         if (e instanceof IOException) throw (IOException)e;
         else if (e instanceof RuntimeException) throw (RuntimeException)e;
         else if (e instanceof CBCResponseException || e instanceof PersistException) {
            os.write(FAILURE);
            os.write(e.getLocalizedMessage().getBytes("UTF-8"));
            os.write(NORMAL_END);
            os.flush();
            throw new CBCResponseException(e.getLocalizedMessage());
         }
         else {
            log.error(e.getClass()+" "+e.getLocalizedMessage());
         }
      }
      log.info("Configuration exchange concluded.");
   }
   
   private void writeNormalOutput(OutputStream os, byte[] output) throws IOException {
      
      os.write(output);
      os.write(NORMAL_END);
      os.flush();
   }

   private List<String> readInput(BufferedReader reader) throws IOException {
      
      String line;
      List<String> inputLines = new ArrayList<String>();
      int lineCount = 0;
      for (;;) {
         line = reader.readLine();
         if (line==null) {
            throw new IOException("Client has closed the connection.");
         }
         line = line.trim();
         if (line.equals(".")) {
            break;
         }
         if (line.toLowerCase().indexOf("password") == -1) {
            log.info(line);
         }
         inputLines.add(line);
         lineCount++;
         if (lineCount == 100) {
            break;
         }
      }
      return inputLines;
   }

   public final class AddUsersPLL1 extends CBCExecutor {

      private final List<NewUser> newUsers = new ArrayList<NewUser>(10);

      public AddUsersPLL1(ListIterator<String> iter) {
         super(iter);
      }

      public void processLines() {
         String line;
         for (; iter.hasNext();) {
            line = iter.next();
            if (line.toLowerCase().startsWith(COMMAND_ADD_USER)) {
               NewUser newUser = new NewUser();
               for (; iter.hasNext();) {
                  line = iter.next();
                  if (line.toLowerCase().startsWith(USERNAME)) {
                     newUser.username = line.substring(USERNAME.length()).trim();
                  } else if (line.toLowerCase().startsWith(PASSWORD)) {
                     newUser.password = line.substring(PASSWORD.length()).trim().toCharArray();
                  } else if (line.toLowerCase().startsWith(REALM)) {
                     newUser.realm = line.substring(REALM.length()).trim();
                  } else {
                     break;
                  }
               }
               if (newUser.username != null && newUser.password != null) {
                  newUsers.add(newUser);
               }
            } //Reject anything else
            else {
               //TODO should we return anything???
            }
         }
      }

      public List<NewUser> getNewUsers() {
         return newUsers;
      }

      public void execute(PersistExecutor pe) throws PersistException {
         pe.insertUser(newUsers);
      }
   }
   
   private static final byte[] SUCCESS = "success".getBytes();
   private static final byte[] FAILURE = "error:".getBytes();
   private static final byte[] NORMAL_END = new byte[]{0x0d, 0x0a, 0x2e, 0x0d, 0x0a};
}
