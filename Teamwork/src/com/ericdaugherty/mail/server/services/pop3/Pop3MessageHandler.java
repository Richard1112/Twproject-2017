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
import java.io.FileNotFoundException;
import java.io.IOException;

//Local imports
import com.ericdaugherty.mail.server.errors.TooManyErrorsException;
import com.ericdaugherty.mail.server.info.User;
import com.ericdaugherty.mail.server.persistence.POP3MessagePersistenceFactory;
import com.ericdaugherty.mail.server.persistence.POP3MessagePersistenceProccessor;
import com.ericdaugherty.mail.server.services.general.ProcessorStreamHandler;

/**
 *
 * @author Andreas Kyrmegalos
 */
public class Pop3MessageHandler {
   
   private User user;
   
   private Pop3Message[] messages = null;
   
   private POP3MessagePersistenceProccessor pOP3MessagePersistenceProccessor;
   
   public Pop3MessageHandler(User user) {
      this.user = user;

      if (!(user.getEmailAddress().isNULL() || user.getEmailAddress().isMailerDaemon())) {
         pOP3MessagePersistenceProccessor = POP3MessagePersistenceFactory.getInstance().getPOP3PersistenceProccessor();
      } else {
         pOP3MessagePersistenceProccessor = POP3MessagePersistenceFactory.getInstance().getNullPeristenceProccessor();
      }
      pOP3MessagePersistenceProccessor.setUser(user);
   }

   /**
    * Returns an array of Pop3Message objects that represents all messages
    * stored for this user.
    */
   public Pop3Message[] getMessages() {

      if (messages == null) {

         String[] fileNames = pOP3MessagePersistenceProccessor.populatePOP3MessageList();

         int numMessage = fileNames.length;

         messages = new Pop3Message[numMessage];
         Pop3Message currentMessage;

         for (int index = 0; index < numMessage; index++) {
            currentMessage = new Pop3Message();
            currentMessage.setMessageLocation(fileNames[index]);
            messages[index] = currentMessage;
         }
      }
      return messages;
   }
   
   public String[] deleteMessages() {
      return pOP3MessagePersistenceProccessor.deleteMessages(getMessages());
   }

   public void retreiveMessage(ProcessorStreamHandler pop3CH, int messageNumber) throws TooManyErrorsException,
         FileNotFoundException, IOException {
      pOP3MessagePersistenceProccessor.
            retreiveMessage(pop3CH, getMessage(messageNumber).getMessageLocation());
   }
   
   public void retreiveMessageTop(ProcessorStreamHandler pop3CH, int messageNumber, long numLines)throws TooManyErrorsException,
         FileNotFoundException, IOException {
      pOP3MessagePersistenceProccessor.
            retreiveMessageTop(pop3CH, getMessage(messageNumber).getMessageLocation(), numLines);
   }

   /**
    * Returns an array of Pop3Message objects that represents all messaged
    * stored for this user not marked for deletion.
    */
   public Pop3Message[] getNonDeletedMessages() {
      Pop3Message[] allMessages = getMessages();
      int allCount = allMessages.length;
      int nonDeletedCount = 0;
      for (int i = 0; i < allCount; i++) {
         if (!allMessages[i].isDeleted()) {
            nonDeletedCount++;
         }
      }
      Pop3Message[] nonDeletedMessages = new Pop3Message[nonDeletedCount];
      nonDeletedCount = 0;
      for (int i = 0; i < allCount; i++) {
         if (!allMessages[i].isDeleted()) {
            nonDeletedMessages[nonDeletedCount++] = allMessages[i];
         }
      }
      return nonDeletedMessages;
   }

   /**
    * Gets the specified message.  Pop3Message numbers are 1 based.  
    * This method counts on the calling method to verify that the
    * messageNumber actually exists.
    */
   public Pop3Message getMessage(int messageNumber) {

      return getMessages()[messageNumber - 1];
   }

   /**
    * Gets the total number of messages currently stored for this user.
    */
   public long getNumberOfMessage() {

      return getMessages().length;
   }

   /**
    * Gets the total number of non deleted messages currently stored for this user.
    */
   public long getNumberOfNonDeletedMessages() {

      return getNonDeletedMessages().length;
   }

   /**
    * Gets the total size of the non deleted messages currently stored for this user.
    */
   public long getSizeOfAllNonDeletedMessages() {

      Pop3Message[] message = getNonDeletedMessages();

      long totalSize = 0;

      for (int index = 0; index < message.length; index++) {
         totalSize += message[index].getMessageSize(user);
      }

      return totalSize;
   }

   /**
    * This method removes any cached message information this user may have stored.
    */
   public void dispose() {

      user = null;
      messages = null;
      pOP3MessagePersistenceProccessor = null;
   }
   
}
