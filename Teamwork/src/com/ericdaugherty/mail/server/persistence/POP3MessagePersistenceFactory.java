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

package com.ericdaugherty.mail.server.persistence;

//Java imports
import java.io.FileNotFoundException;
import java.io.IOException;

//Local Imports
import com.ericdaugherty.mail.server.errors.TooManyErrorsException;
import com.ericdaugherty.mail.server.info.User;
import com.ericdaugherty.mail.server.persistence.pop3.SimpleFileIOProcessor;
import com.ericdaugherty.mail.server.services.general.ProcessorStreamHandler;
import com.ericdaugherty.mail.server.services.pop3.Pop3Message;

/**
 * A factory class to generate classes that handle POP3 messages using the file system
 * as a back-end.
 *
 * @author Andreas Kyrmegalos
 */
public class POP3MessagePersistenceFactory {

   private static POP3MessagePersistenceFactory instance = null;

   private POP3MessagePersistenceFactory() {
   }

   public static POP3MessagePersistenceFactory getInstance() {
      if (instance == null) {
         instance = new POP3MessagePersistenceFactory();
      }
      return instance;
   }

   public POP3MessagePersistenceProccessor getPOP3PersistenceProccessor() {
      return new SimpleFileIOProcessor();
   }

   public POP3MessagePersistenceProccessor getNullPeristenceProccessor() {
      return new POP3MessagePersistenceFactory.NullMessagePersistenceProccessor();
   }

   public final class NullMessagePersistenceProccessor implements POP3MessagePersistenceProccessor{

      public void setUser(User user) {

      }

      public String[] populatePOP3MessageList() {

         return new String[]{};
      }

      public String[] deleteMessages(Pop3Message[] messages) {

         return new String[]{};
      }

      public void retreiveMessage(ProcessorStreamHandler pop3CH, String messageLocation) throws TooManyErrorsException,
              FileNotFoundException, IOException {

      }
      public void retreiveMessageTop(ProcessorStreamHandler pop3CH, String messageLocation, long numLines) throws TooManyErrorsException,
              FileNotFoundException, IOException {
         
      }
   }
}
