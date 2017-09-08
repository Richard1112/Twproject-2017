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

package com.ericdaugherty.mail.server.services.smtp.client;

//Java imports
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

//Log imports
import org.apache.commons.logging.LogFactory;
import org.apache.commons.logging.Log;

//Local imports
import com.ericdaugherty.mail.server.Mail;
import com.ericdaugherty.mail.server.configuration.ConfigurationManager;
import com.ericdaugherty.mail.server.persistence.SMTPMessagePersistenceFactory;
import com.ericdaugherty.mail.server.persistence.SMTPMessagePersistenceFactory.SMTPMessageLister;
import com.ericdaugherty.mail.server.services.smtp.SMTPMessage;
import com.ericdaugherty.mail.server.services.smtp.SMTPMessageImpl;

/**
 * This class is tasked to collect unsent messages and deliver them
 * to the proper local mailbox or relay them to a remote SMTP server.
 * Only a single instance of this class is meant to be active during the
 * lifetime of the application.
 *
 * @author Eric Daugherty
 * @author Andreas Kyrmegalos (2.x branch)
 */
public abstract class SMTPSender implements Runnable {

    /** Logger */
    //protected static final Log log = LogFactory.getLog( SMTPSender.class );
    protected static Log log = LogFactory.getLog("JESLogger");

    /** The ConfigurationManager */
    protected final ConfigurationManager configurationManager = ConfigurationManager.getInstance();

    //***************************************************************
    // Variables
    //***************************************************************

    private volatile boolean running = true;
    
    private final ConcurrentHashMap<String,Boolean> queuedItems = new ConcurrentHashMap<String,Boolean>(25,0.75f,5);
    
    private SenderPool senderPool;

    private String name;

    private String smtpRepository;

    protected final boolean useAmavisSMTPDirectory = configurationManager.isAmavisSupportActive();

    //***************************************************************
    // Package private Constructor
    //***************************************************************
    SMTPSender(String name, String smtpRepository) {
      this.name = name;
      this.smtpRepository = smtpRepository;
    }

    //***************************************************************
    // Public Interface
    //***************************************************************

    public abstract Deliver getNewDeliverInstance();

    public void run() {

       //It is possible that a shutdown command is issued before the thread is activated
       if (Mail.getInstance().isShuttingDown()) {
          return;
       }
       if (!Mail.isStarted()) {
          synchronized(this) {
             do {
                try {
                   if (log.isDebugEnabled()) {
                      log.debug("SMTPSender standing by.");
                   }
                   this.wait();
                } catch (InterruptedException ex) {
                   if (!running||Mail.getInstance().isShuttingDown()) return;
                }
             }
             while (!Mail.isStarted());

             if (log.isDebugEnabled()) {
                log.debug("SMTPSender resuming operation.");
             }
          }
       }

        SMTPMessagePersistenceFactory.instantiate();
        SMTPMessageLister smtpML = SMTPMessagePersistenceFactory.getInstance().getSMTPMessageLister(smtpRepository);
        
        senderPool = new SenderPool(4);
        senderPool.init(name);

        Deliver deliverInstance;

        while( running ) {

           try {
                 
              smtpML.populateSMTPMessageList(queuedItems);

              Iterator<Map.Entry<String,Boolean>> iter = queuedItems.entrySet().iterator();
              Map.Entry<String,Boolean> tempEntry;
              while(iter.hasNext()) {
                 tempEntry = iter.next();
                 if (!tempEntry.getValue()) {
                    tempEntry.setValue(true);
                    try {
                       deliverInstance = getNewDeliverInstance();
                       deliverInstance.initialize((String)tempEntry.getKey());
                       //Since the number of pooled messages can be a multiple
                       //of the senderPool's queue capacity, checking whether
                       //the runnable is added to the senderPool's queue prevents
                       //further attempts to add to a full queue. The iteration
                       //is interrupted if the queue is full and a grace period of
                       //30 seconds is enforced if there are more than half the
                       //queue's capacity items still waiting to be executed.
                       //Although the prudent thing to do is check if the runnable
                       //was added to the queue, it is not neccessery since such a
                       //failure simply means that the runnable linked to a specific
                       //message is discarded and the corresponding message will be
                       //picked up by the next iteration over the smtp message pool.
                       if (!senderPool.getQueue().offer(deliverInstance)) break;
                    }
                    catch(Exception e) {
                       if (SMTPMessagePersistenceFactory.getInstance().getSMTPPersistenceEngine()==
                             SMTPMessagePersistenceFactory.SMTPPersistenceEngine.FILEIO) {
                          if (!new java.io.File((String)tempEntry.getKey()).exists()) {
                             log.info("Message "+tempEntry.getKey()+" has been deleted. Safe to continue.");
                          }
                          else {
                             log.error(e.getMessage(), e);
                          }
                       }
                       else {
                          log.error(e.getMessage(), e);
                       }
                       iter.remove();
                    }
                    finally {
                       deliverInstance = null;
                    }
                 }
              }

              //Rest the specified sleep time.  If it is greater than 10 seconds
              //Wake up every 10 seconds to check to see if the thread is shutting
              //down.
              long sleepTime = configurationManager.getDeliveryIntervalSeconds() * 1000;
              if( sleepTime < 10000 ) {
                 Thread.sleep( sleepTime );
              }
              else {
                 long totalSleepTime = sleepTime;
                 while( totalSleepTime > 0 && running ) {
                    if( totalSleepTime > 10000 ) {
                       totalSleepTime -= 10000;
                       Thread.sleep( 10000);
                    }
                    else {
                       Thread.sleep( totalSleepTime );
                       totalSleepTime = 0;
                    }
                 }
              }

              //Provision for resource exhaustion
              if (senderPool.getSenderPoolQueuedItemCount()>100) {
                 Thread.sleep(30 * 1000);
              }
           }
           catch( InterruptedException ie ) {}
           catch( Throwable throwable ) {
              log.error( "An error occurred while querying the pool for new messages. " + throwable.getMessage(), throwable );
           }
       }
       log.warn( "SMTPSender shut down gracefully.");
    }

    public void notifyShutdown() {
        log.warn( "Attempting to shut down SMTPSender." );
        if (senderPool!=null) {
           senderPool.active = false;
        }
        running = false;
    }

    //***************************************************************
    // Private Interface
    //***************************************************************


    /**
     * This class (or rather concrete implementors of this class) performs
     * the actual delivery/relaying. It acts upon the assumption that all the
     * addresses were validated beforehand. Further, no delivery rules are applied.
     */
    public abstract class Deliver implements Runnable {
       
       String messagePersistanceName;
       SMTPMessage message;
       
       public Deliver() {}

       public void initialize(String messagePersistanceName) throws Exception{
           this.messagePersistanceName = messagePersistanceName;
           //Just load the JES headers. Loading of the message body is deffered
           //until the last possible moment
           message = new SMTPMessageImpl(messagePersistanceName, true);

       }

       public abstract void run();
       
       void updatequeueitem() {
          alterqueue(false);
       }
       
       void deletequeueitem() {
          alterqueue(true);
       }
       
       private void alterqueue(boolean delete) {

          Iterator<Map.Entry<String,Boolean>> iter = queuedItems.entrySet().iterator();
          Map.Entry<String,Boolean> tempEntry;
          while (iter.hasNext()) {
             tempEntry = iter.next();
             if (tempEntry.getKey().equals(messagePersistanceName)) {
                if (delete) {
                   iter.remove();
                }
                else {
                   tempEntry.setValue(false);
                }
                break;
             }
          }
       }
    }
}
//EOF