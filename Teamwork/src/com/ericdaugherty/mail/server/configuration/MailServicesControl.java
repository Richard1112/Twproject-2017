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
import java.lang.reflect.Method;

//Logging imports
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

//Local imports
import com.ericdaugherty.mail.server.Mail;
import com.ericdaugherty.mail.server.persistence.SMTPMessagePersistenceFactory;
import com.ericdaugherty.mail.server.services.general.ServiceListener;
import com.ericdaugherty.mail.server.services.pop3.Pop3Processor;
import com.ericdaugherty.mail.server.services.smtp.server.RFC5321SMTPServerSessionControl;
import com.ericdaugherty.mail.server.services.smtp.client.*;


/**
 * All ServiceListener and Sender instances and the associated Threads are
 * administered via this class.
 *
 * @author Andreas Kyrmegalos
 */
public class MailServicesControl {

  /** Logger for this class. **/
  //private static Log log = LogFactory.getLog( MailServicesControl.class );
  private static Log log = LogFactory.getLog("JESLogger");

   private static MailServicesControl instance;
   
   private final ConfigurationManager configurationManager = ConfigurationManager.getInstance();

   public boolean testing;
   
   private enum Status {
      INITIALIZED, STARTED, SHUTTING_DOWN, FAILED; 
   }
   
   private volatile Status status;

   /** The threadGroup that all listener/sender threads are attached to. **/
   private final ThreadGroup threadGroup = new ThreadGroup("JESThreadGroup");
    
   /** The various e-mail ServiceListeners **/
   private ServiceListener popListener;
   private ServiceListener smtpListener;
   private ServiceListener amavisSmtpListener;
   private ServiceListener securepopListener;
   private ServiceListener securesmtpListener;
    
   /** The SMTP senders */
   private SMTPSender smtpSender, amavisSmtpSender;
    
   /** The SMTP sender threads */
   private Thread smtpSenderThread, amavisSmtpSenderThread;
   
   public static void instantiate(boolean testing) {
      if (instance==null) {
         //Never called from more than one Threads. No worries.
         instance = new MailServicesControl(testing);
         instance.init();
      }
   }
   
   public static MailServicesControl getInstance() {
      return instance;
   }

   private MailServicesControl(boolean testing) {
      
      this.testing = testing;
      Class serviceListener = ServiceListener.class;
      try {
         Method method = serviceListener.getMethod("setTotalSL", int.class);
         Integer totalSL = Integer.valueOf(1+(configurationManager.getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3?1:0)+
               (configurationManager.isAmavisSupportActive()||testing?1:0)+
               (configurationManager.isSecureActive()?1+((configurationManager.getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3?1:0)):0));
         if (log.isDebugEnabled()) {
            log.debug("Total number of service listeners to be instantiated "+totalSL);
         }
         method.invoke(null, totalSL);
      }
      catch (Exception e) {
         log.error(e.getMessage());
      }

      //Start the threads.
      int port;
      int executeThreads = configurationManager.getExecuteThreadCount();

      //Start the Pop3 Thread.
      if (configurationManager.getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3) {
         port = configurationManager.getPOP3Port();
         if( log.isDebugEnabled() ) log.debug( "Starting POP3 Service on port: " + port );
         popListener = new ServiceListener( port, Pop3Processor.class, executeThreads, false , true);
      }
      else {
         popListener = null;
      }

      //Start the SMTP Threads.
      port = configurationManager.getSMTPPort();
      if( log.isDebugEnabled() ) log.debug( "Starting SMTP Service on port: " + port );
      smtpListener = new ServiceListener( port, RFC5321SMTPServerSessionControl.class, false, executeThreads, false, true );

      if (configurationManager.isAmavisSupportActive()||testing) {
         port = !testing?configurationManager.getAmavisFilteredSMTPPort():(configurationManager.getSMTPPort()+1);
         if( log.isDebugEnabled() ) log.debug( "Starting Transmiting MTA's SMTP Service on port: " + port );
         amavisSmtpListener = new ServiceListener( port, RFC5321SMTPServerSessionControl.class, true, executeThreads, false, true );

      }
      else {
         amavisSmtpListener = null;
      }

      if (configurationManager.isSecureActive()) {

         int secureexecuteThreads = configurationManager.getSecureExecuteThreadCount();
         //Start secure Pop3 threads.
         if (configurationManager.getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3) {
            port = configurationManager.getSecurePOP3Port();
            if( log.isDebugEnabled() ) log.debug( "Starting secure POP3 Service on port: " + port );
            securepopListener = new ServiceListener( port, Pop3Processor.class, secureexecuteThreads, true, true );
         }
         else {
            securepopListener = null;
         }

         //Start secure SMTP Threads.
         port = configurationManager.getSecureSMTPPort();
         if( log.isDebugEnabled() ) log.debug( "Starting secure SMTP Service on port: " + port );
         securesmtpListener = new ServiceListener( port, RFC5321SMTPServerSessionControl.class, secureexecuteThreads, true, true );
      }
      else {
         securepopListener = null;
         securesmtpListener = null;
      }

      //Start the SMTPSender thread (This thread actually delivers the mail received
      //by the SMTP threads).
      if (configurationManager.isAmavisSupportActive()) {
         smtpSenderThread = new Thread( threadGroup, smtpSender = new SMTPSenderAmavis(), "SMTPSender" );
         amavisSmtpSenderThread = new Thread( threadGroup,
               amavisSmtpSender = new SMTPSenderStandard(false), "SMTPSender2" );
      }
      else if (testing) {
         smtpSenderThread = new Thread( threadGroup, smtpSender = new SMTPSenderStandard(true), "SMTPSenderTest" );
         amavisSmtpSenderThread = new Thread( Thread.currentThread().getThreadGroup(),
               amavisSmtpSender = new SMTPSenderStandard(false), "SMTPSender" );
      }
      else {
         smtpSenderThread = new Thread( threadGroup, smtpSender = new SMTPSenderStandard(false), "SMTPSender" );
         amavisSmtpSender = null;
         amavisSmtpSenderThread = null;
      }
      status = Status.INITIALIZED;
   }
   
   private void init() {
      
      if (status!=Status.INITIALIZED) {
         throw new IllegalStateException("The service is not yet initialized");
      }
      if (popListener!=null) {
         if (log.isDebugEnabled()) log.debug("Starting POP3 thread.");
         new Thread( threadGroup,popListener, "POP3" ).start();
      }
      if (smtpListener!=null) {
         if (log.isDebugEnabled()) log.debug("Starting SMTP thread.");
         new Thread( threadGroup,smtpListener, "SMTP" ).start();
      }
      if (amavisSmtpListener!=null) {
         String threadName = !testing?"Amavis SMTP":"Testing SMTP";
         if (log.isDebugEnabled()) log.debug("Starting "+threadName+" thread.");
         new Thread( threadGroup,amavisSmtpListener, threadName ).start();
      }
      if (securepopListener!=null) {
         if (log.isDebugEnabled()) log.debug("Starting secure POP3 thread.");
         new Thread( threadGroup,securepopListener, "secure POP3" ).start();
      }
      if (securesmtpListener!=null) {
         if (log.isDebugEnabled()) log.debug("Starting secure POP3 thread.");
         new Thread( threadGroup,securesmtpListener, "secure SMTP" ).start();
      }
      if (smtpSenderThread!=null) {
         if (log.isDebugEnabled()) log.debug("Starting SMTPSender thread.");
         smtpSenderThread.start();
      }
      if (amavisSmtpSenderThread!=null) {
         if (log.isDebugEnabled()) log.debug("Starting POP3Sender thread.");
         amavisSmtpSenderThread.start();
      }
   }
   
   public boolean isListenerServiceInitVerified() {
      
      if (status!=Status.INITIALIZED) {
         throw new IllegalStateException("The service is not yet initialized");
      }
      
      Class serviceListener = ServiceListener.class;
      try {
         if (!ServiceListener.isSLsloadingComplete()) {
            if (log.isDebugEnabled()) {
               log.debug("Entering lock state");
            }
            Method method = serviceListener.getMethod("getLock");
            Object lock = method.invoke(null);
            synchronized(lock) {
               do {
                  try {
                     lock.wait(10*1000L);
                     if (log.isDebugEnabled()) {
                        log.debug("All ServiceListener instances completed initialization");
                     }
                  } catch (InterruptedException ex) {
                     log.debug(ex.getLocalizedMessage());
                  }
               }while(!ServiceListener.isSLsloadingComplete()&&(Mail.getInstance()!=null&&!Mail.getInstance().isShuttingDown()));
            }
         }
         else {
            if (log.isDebugEnabled()) {
               log.debug("All ServiceListener instances completed initialization");
            }
         }
      }
      catch (Exception e) {
         log.error(e.getMessage());
      }
      
      boolean failedListenerInitialization = false;
      //Verify that all listerer services have initialized successfully
      if (configurationManager.getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3&&!popListener.isInitialized()) {
         failedListenerInitialization = true;
      }
      if(!smtpListener.isInitialized()) {
         failedListenerInitialization = true;
      }
      if (configurationManager.isSecureActive()) {
         if (configurationManager.getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3&&!securepopListener.isInitialized()) {
            failedListenerInitialization = true;
         }
         if(!securesmtpListener.isInitialized()) {
            failedListenerInitialization = true;
         }
      }
      if (configurationManager.isAmavisSupportActive()||testing) {
         if (!amavisSmtpListener.isInitialized()) {
            failedListenerInitialization = true;
         }
      }
      if (failedListenerInitialization) {
         status = Status.FAILED;
      }
      else {
         status = Status.STARTED;
      }
      return !failedListenerInitialization;
   }
   
   public void releaseListeners() {
      
      if (status!=Status.STARTED) {
         return;
      }

      if (log.isDebugEnabled()) {
         log.debug("Bringing Receiver(s) out of stand by.");
      }
      
      if (configurationManager.getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3) {
         popListener.start();
      }
      smtpListener.start();
      if (configurationManager.isSecureActive()) {
         if (configurationManager.getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3&&!securepopListener.isInitialized()) {
            securepopListener.start();
         }
         securesmtpListener.start();
      }
      if (configurationManager.isAmavisSupportActive()||testing) {
         amavisSmtpListener.start();
      }

      if (log.isDebugEnabled()) {
         log.debug("Bringing SMTPSender(s) out of stand by.");
      }
      synchronized(smtpSender) {
         smtpSender.notify();
      }
      if (configurationManager.isAmavisSupportActive()||testing) {
         synchronized(amavisSmtpSender) {
            amavisSmtpSender.notify();
         }
      }
   }
   
   public void shutdown() {
      
      if (status==Status.SHUTTING_DOWN) {
         return;
      }
      status = Status.SHUTTING_DOWN;
      
      if (ConfigurationManager.getInstance().getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3) {
         if (popListener!=null) popListener.notifyShutdown();
      }
      if (smtpListener!=null) smtpListener.notifyShutdown();
      if (ConfigurationManager.getInstance().isAmavisSupportActive()||testing) {
         if (amavisSmtpListener!=null) amavisSmtpListener.notifyShutdown();
      }
      
      if (ConfigurationManager.getInstance().isSecureActive()) {
         if (ConfigurationManager.getInstance().getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3) {
            if (securepopListener!=null) securepopListener.notifyShutdown();
         }
         if (securesmtpListener!=null) securesmtpListener.notifyShutdown();
      }
      if (smtpSender!=null) smtpSender.notifyShutdown();
      if (ConfigurationManager.getInstance().isAmavisSupportActive()||testing) {
         if (amavisSmtpSender!=null) amavisSmtpSender.notifyShutdown();
      }

      if (popListener!=null) popListener.initiateShutdown();
      if (smtpListener!=null) smtpListener.initiateShutdown();
      if (ConfigurationManager.getInstance().isAmavisSupportActive()||testing) {
         if (amavisSmtpListener!=null) amavisSmtpListener.initiateShutdown();
      }
      if (ConfigurationManager.getInstance().isSecureActive()) {
         if (ConfigurationManager.getInstance().getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3) {
            if (securepopListener!=null) securepopListener.initiateShutdown();
         }
         if (securesmtpListener!=null) securesmtpListener.initiateShutdown();
      }
      ServiceListener.shutdown();

      try {
         if (smtpSenderThread!=null) {
            //Perhaps this class' thread is locked in a wait state. Joining straight away will only block the flow
            synchronized(smtpSender) {
               smtpSender.notify();
            }
            smtpSenderThread.join();
         }
      }
      catch (InterruptedException ie) {}
      if (ConfigurationManager.getInstance().isAmavisSupportActive()||testing) {
         try {
            if (amavisSmtpSenderThread!=null) {
               synchronized(amavisSmtpSender) {
                  amavisSmtpSender.notify();
               }
               amavisSmtpSenderThread.join();
            }
         }
         catch (InterruptedException ie) {}
      }
      SMTPMessagePersistenceFactory.shutdown();
      instance = null;
   }

    public void notifyChange() {
       
      if (status!=Status.STARTED) {
         return;
      }
       if (ConfigurationManager.getInstance().getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3) {
          if (configurationManager.getPOP3Port()!=popListener.getPort()) {
             new Thread(popListener.updateServerSocket(configurationManager.getPOP3Port())).start();
          }
       }
       if (configurationManager.getSMTPPort()!=smtpListener.getPort()) {
          new Thread(smtpListener.updateServerSocket(configurationManager.getSMTPPort())).start();
       }
       if (ConfigurationManager.getInstance().isSecureActive()) {
          if (ConfigurationManager.getInstance().getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3) {
             if (configurationManager.getSecurePOP3Port()!=securepopListener.getPort()) {
                new Thread(securepopListener.updateServerSocket(configurationManager.getSecurePOP3Port())).start();
             }
          }
          if (configurationManager.getSecureSMTPPort()!=securesmtpListener.getPort()) {
             new Thread(securesmtpListener.updateServerSocket(configurationManager.getSecureSMTPPort())).start();
          }
       }
    }
    
   public void updateServiceListeners() {
       
      if (status!=Status.STARTED) {
         return;
      }
      new Thread(popListener.updateServerSocket(-1)).start();
      new Thread(smtpListener.updateServerSocket(-1)).start();
      if (ConfigurationManager.getInstance().isSecureActive()) {
         new Thread(securepopListener.updateServerSocket(-1)).start();
         new Thread(securesmtpListener.updateServerSocket(-1)).start();
      }
   }
   
   public void logStatus() {
      
      if (status!=Status.STARTED) {
         log.warn("A request for a status report, without the services being started, is a request ignored...");
         return;
      }
      int maxthreadcount = 5+
            configurationManager.getExecuteThreadCount()*(1+(configurationManager.getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3?1:0))+
            (configurationManager.getConfigurationAddress()!=null?1:0)+
            (configurationManager.isSecureActive()?configurationManager.getSecureExecuteThreadCount()*(1+(configurationManager.getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3?1:0)):0) +
            ((configurationManager.isAmavisSupportActive()||testing)?configurationManager.getExecuteThreadCount()+5:0)+
            (configurationManager.isConfigurationEnabled()?1:0);
      int maxstandardsmtp,maxstandardpop3,maxsecuresmtp,maxsecurepop3,maxamavissmtp;
      int curstandardsmtp,curstandardpop3,cursecuresmtp,cursecurepop3,curamavissmtp,curdeliversmtp, curamavisdeliversmtp;
      maxstandardpop3 = maxstandardsmtp = configurationManager.getExecuteThreadCount();
      maxsecurepop3 = maxsecuresmtp = configurationManager.isSecureActive()? configurationManager.getSecureExecuteThreadCount():0;
      maxamavissmtp = (configurationManager.isAmavisSupportActive()||testing)?configurationManager.getExecuteThreadCount():0;
      
      curamavissmtp = curstandardsmtp = curstandardpop3 = cursecuresmtp =
            cursecurepop3 = curdeliversmtp = curamavisdeliversmtp = 0;
      Thread[] threadlist = new Thread[maxthreadcount];
      int threadcount = threadGroup.enumerate(threadlist);
      if (threadcount>0) {
         StringBuilder jesstatus = new StringBuilder(" Active Modules: ");
         for (int i=0;i<threadcount;i++) {
            if (threadlist[i].getName().equals("SMTPSender")) {
               jesstatus.append("SMTPSender ");
            }
            else if (threadlist[i].getName().startsWith("DelS:")) {
               curdeliversmtp++;
            }
            else if (threadlist[i].getName().startsWith("DelA:")) {
               curamavisdeliversmtp++;
            }
            else if (threadlist[i].getName().startsWith("SMTP:")) {
               curstandardsmtp++;
            }
            else if (threadlist[i].getName().startsWith("POP3:")) {
               curstandardpop3++;
            }
            else if (threadlist[i].getName().startsWith("Amavis")||
                  threadlist[i].getName().startsWith("Testing")) {
               curamavissmtp++;
            }
            else {
               if (threadlist[i].getName().startsWith("secure S")) {
                  cursecuresmtp++;
               }
               else if (threadlist[i].getName().startsWith("secure P")) {
                  cursecurepop3++;
               }
            }
         }
         jesstatus.append("Deliver Standard:"+curdeliversmtp+"/4 ");
         if (configurationManager.isAmavisSupportActive()) jesstatus.append("Deliver Amavis:"+curamavisdeliversmtp+"/4 ");
         jesstatus.append("SMTP:"+curstandardsmtp+"/"+maxstandardsmtp+" ");
         if (configurationManager.isAmavisSupportActive()) jesstatus.append("SMTP Amavis:"+curamavissmtp+"/"+maxamavissmtp+" ");
         else if (testing) jesstatus.append("SMTP Testing:"+curamavissmtp+"/"+maxamavissmtp+" ");
         if (configurationManager.getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3) {
            jesstatus.append("POP3: "+curstandardpop3+"/"+maxstandardpop3+" ");
         }
         if (configurationManager.isSecureActive()) {
            jesstatus.append("secure SMTP: "+cursecuresmtp+"/"+maxsecuresmtp+" ");
            if (configurationManager.getRetrievalMode()==ConfigurationManager.RetrievalMode.POP3) {
               jesstatus.append("secure POP3: "+cursecurepop3+"/"+maxsecurepop3+" ");
            }
         }
         log.info( jesstatus );
         jesstatus = null;
      }
      else {
         log.info(" No active threads");
      }
   }
}
