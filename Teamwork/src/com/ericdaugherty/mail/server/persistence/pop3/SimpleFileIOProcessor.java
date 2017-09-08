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

package com.ericdaugherty.mail.server.persistence.pop3;

//Java Imports
import java.io.*;
import java.util.ArrayList;
import java.util.List;

//Local Imports
import com.ericdaugherty.mail.server.errors.TooManyErrorsException;
import com.ericdaugherty.mail.server.info.*;
import com.ericdaugherty.mail.server.persistence.LocalDeliveryFactory;
import com.ericdaugherty.mail.server.persistence.POP3MessagePersistenceProccessor;
import com.ericdaugherty.mail.server.services.general.ProcessorStreamHandler;
import com.ericdaugherty.mail.server.services.pop3.Pop3Message;
import com.ericdaugherty.mail.server.utils.DelimitedInputStream;

/**
 * A file system based POP3 persistence engine.
 *
 * @author Andreas Kyrmegalos
 */
public class SimpleFileIOProcessor implements POP3MessagePersistenceProccessor {

   private static final String US_ASCII = "US-ASCII";
   
   private User user;
   private String userRepository;

   public SimpleFileIOProcessor() {}

   public void setUser(User user) {
      this.user = user;
      userRepository = LocalDeliveryFactory.getInstance().getLocalDeliveryProccessor().getUserRepository(user);
   }

   private String getUserRepository() {
      return userRepository;
   }

   public String[] populatePOP3MessageList() {

      final File directory = new File(getUserRepository());

      String[] messageNames = directory.list(new FilenameFilter() {

         public boolean accept(File directoryFile, String file) {
            if (directoryFile.equals(directory)) {
               if (file.toLowerCase().endsWith(".loc")) {
                  return true;
               }
            }
            return false;
         }
      });

      return messageNames;
   }

   public String[] deleteMessages(Pop3Message[] messages) {
      int numMessage = messages.length;
     if (numMessage == 0) {
         return null;
      }
      File userDirectory = new File(getUserRepository());
      int attempts;
      String[] failedMessages = new String[numMessage];
      int failedMessageCount = 0;
      File aMessageFile;
      for (Pop3Message currentMessage : messages) {
         attempts = 0;
         if (currentMessage.isDeleted()) {
            aMessageFile = new File(userDirectory, currentMessage.getMessageLocation());
            do {
               if (!aMessageFile.delete()&&aMessageFile.exists()) {
                  if (attempts == 5) {
                     failedMessages[failedMessageCount++] = currentMessage.getUniqueId();
                  }
                  try {
                     Thread.sleep(attempts * 5000);
                  } catch (InterruptedException ex) {}
               }
            }while (attempts++ < 5);
         }
      }
      if (failedMessageCount == 0) {
         return null;
      }
      String[] returnFailedMessages = new String[failedMessageCount];
      System.arraycopy(failedMessages, 0, returnFailedMessages, 0, failedMessageCount);
      return returnFailedMessages;
   }

   public void retreiveMessage(ProcessorStreamHandler pop3CH, String messageLocation) throws TooManyErrorsException,
         FileNotFoundException, IOException {

      DelimitedInputStream reader = null;
      List<byte[]> dataLines = new ArrayList<byte[]>(250);
      try {
         //Open an reader to read the file.
         reader = new DelimitedInputStream(new FileInputStream(new File(getUserRepository(), messageLocation)));

         //messageLocation is used as a placeholder for the SMTP uid
         messageLocation = messageLocation.substring(messageLocation.lastIndexOf(File.separator) + 1, messageLocation.lastIndexOf('.'));
         boolean foundRPLCRCPT = false, foundRPLCID = false;

         String singleLine;
         //Write the file to the client.
         byte[] currentLine = reader.readLine();
         while (currentLine != null) {
            dataLines.add(currentLine);
            singleLine = new String(currentLine, US_ASCII);
            if (singleLine.indexOf("<REPLACE-RCPT>") != -1) {
               dataLines.set(dataLines.size() - 1, ("        for <" + user.getUserAdress() + ">" + singleLine.substring(singleLine.indexOf(';'))).getBytes(US_ASCII));
               foundRPLCRCPT = true;
            } else if (singleLine.indexOf("<REPLACE-ID>") != -1) {
               dataLines.set(dataLines.size() - 1, (singleLine.substring(0, singleLine.indexOf('<')) + messageLocation
                     + (singleLine.charAt(singleLine.length() - 1) == ';' ? ";" : "")).getBytes(US_ASCII));
               foundRPLCID = true;
            }
            currentLine = reader.readLine();
            if (currentLine.length == 0 || (foundRPLCRCPT && foundRPLCID)) {
               break;
            }
         }
         while (currentLine != null) {
            dataLines.add(currentLine);
            currentLine = reader.readLine();
            if (dataLines.size() == 250) {
               for (byte[] readLine : dataLines) {
                  pop3CH.write(readLine);
               }
               dataLines.clear();
            }
         }
         int lineCount = dataLines.size();
         if (lineCount > 0) {
            for (byte[] readLine : dataLines) {
               pop3CH.write(readLine);
            }
            dataLines.clear();
         }
         //Send the command end data transmission.
         pop3CH.write(new byte[]{0x2e});
      } finally {
         //Make sure the input stream gets closed.
         if (reader != null) {
            try {
               reader.close();
            } catch (IOException ioe) {
            }
            reader = null;
         }
         dataLines.clear();
         dataLines = null;
      }
   }

   public void retreiveMessageTop(ProcessorStreamHandler pop3CH, String messageLocation, long numLines) throws TooManyErrorsException,
         FileNotFoundException, IOException {

      DelimitedInputStream reader = null;
      try {
         //Open an reader to read the file.
         reader = new DelimitedInputStream(new FileInputStream(new File(getUserRepository(), messageLocation)));

         //Write the Pop3Message Header.
         byte[] currentLine = reader.readLine();
         while (currentLine != null && currentLine.length != 0) {

            pop3CH.write(currentLine);
            currentLine = reader.readLine();
         }

         //Write an empty line to seperate header from body.
         pop3CH.write(currentLine);
         currentLine = reader.readLine();

         //Write the requested number of lines from the body of the
         //message, or until the entire message has been written.
         int index = 0;
         while (index < numLines && currentLine != null) {
            pop3CH.write(currentLine);
            currentLine = reader.readLine();
            index++;
         }
         //Send the command end data transmission.
         pop3CH.write(new byte[]{0x2e});
      } finally {
         //Make sure the input stream gets closed.
         if (reader != null) {
            try {
               reader.close();
            } catch (IOException ioe) {
               //Nothing to do...
            }
         }
      }

   }
}
