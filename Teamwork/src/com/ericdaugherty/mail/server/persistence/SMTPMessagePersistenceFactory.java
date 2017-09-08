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

//Java Imports
import java.io.File;
import java.io.FileFilter;
import java.util.Locale;
import java.util.concurrent.ConcurrentHashMap;

//Logging Improts
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

//Local Imports
import com.ericdaugherty.mail.server.persistence.smtp.IncrementalFileIOProccessor;

/**
 * A factory class to generate classes that persist SMTP messages.
 *
 * @author Andreas Kyrmegalos
 */
public final class SMTPMessagePersistenceFactory {

   /** Logger */
   //private static final Log log = LogFactory.getLog( SMTPMessagePersistenceFactory.class );
  private static Log log = LogFactory.getLog("JESLogger");

   private static SMTPMessagePersistenceFactory instance;

   private static volatile boolean running;

   private final SMTPPersistenceEngine smtpPersistenceEngine = SMTPPersistenceEngine.FILEIO;

   public static void shutdown() {
      log.warn("SMTPMessagePersistenceFactory going offline");
      running = false;
      instance = null;
   }

   private SMTPMessagePersistenceFactory() {}
   
   public static void instantiate() {
      if (!running) {
         instance = new SMTPMessagePersistenceFactory();
         running = true;
      }
   }

   public static SMTPMessagePersistenceFactory getInstance() {
      return instance;
   }

   public SMTPPersistenceEngine getSMTPPersistenceEngine() {
      return smtpPersistenceEngine;
   }

   public SMTPMessagePersistenceProccessor getSMTPPersistenceProccessor() {
      return new IncrementalFileIOProccessor();
   }

   public SMTPMessageLister getSMTPMessageLister(String smtpRepository) {
      return new SMTPFileMessageLister(smtpRepository);
   }

   public abstract class SMTPMessageLister {

      public String smtpRepository;

      private SMTPMessageLister(String smtpRepository) {
         this.smtpRepository = smtpRepository;
      }

      public abstract void populateSMTPMessageList(final ConcurrentHashMap<String,Boolean> queueditems);
   }

   public final class SMTPFileMessageLister extends SMTPMessageLister{

      private File smtpDirectory;

      private FileFilter fileFilter = new FileFilter(){

         public boolean accept(File file) {
            if (file.isDirectory()) return false;
            if (!file.getName().toLowerCase(Locale.ENGLISH).endsWith(".ser")) return false;
            return true;
         }
      };

      private SMTPFileMessageLister(String smtpRepository) {
         super(smtpRepository);
         smtpDirectory = new File(smtpRepository);
      }

      public void populateSMTPMessageList(final ConcurrentHashMap<String,Boolean> queuedItems) {

         if (!running) {
            return;
         }
         else if (!smtpDirectory.exists()) {
            if (log.isDebugEnabled()) {
               log.debug("SMTP DIRECTORY DOES NOT EXIST!");
            }
            return;
         }
         File[] files = smtpDirectory.listFiles(fileFilter);
         int numFiles = files.length;

         for( int index = 0; index < numFiles; index++ ) {
            queuedItems.putIfAbsent(files[index].getPath(), false);
         }
      }
   }

   public enum SMTPPersistenceEngine {
      FILEIO
   }
}
