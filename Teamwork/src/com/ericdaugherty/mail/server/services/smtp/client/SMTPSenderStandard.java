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

//Java Imports
import java.io.*;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;

//Local Imports
import com.ericdaugherty.mail.server.Mail;
import com.ericdaugherty.mail.server.configuration.ConfigurationManager;
import com.ericdaugherty.mail.server.configuration.ModuleControl;
import com.ericdaugherty.mail.server.errors.*;
import com.ericdaugherty.mail.server.info.Domain;
import com.ericdaugherty.mail.server.info.EmailAddress;
import com.ericdaugherty.mail.server.info.User;
import com.ericdaugherty.mail.server.persistence.LocalDeliveryFactory;
import com.ericdaugherty.mail.server.services.smtp.SMTPMessage;
import com.ericdaugherty.mail.server.services.smtp.SMTPMessageImpl;
import com.ericdaugherty.mail.server.services.smtp.client.support.FailedAddressItem;
import com.ericdaugherty.mail.server.utils.ByteUtils;

/**
 * A smtp sender meant to distribute incoming mail to its rightful owners.
 *
 * @author Andreas Kyrmegalos
 */
public class SMTPSenderStandard extends SMTPSender{

   private boolean testing;
   
   private final DeliverFactory deliverFactory;

   private class DeliverFactory {

      protected SMTPSenderStandard smtpSenderStandard;

      public DeliverFactory(SMTPSenderStandard smtpSenderStandard) {
         this.smtpSenderStandard = smtpSenderStandard;
      }

      public Deliver getDeliverInstance() {
         return smtpSenderStandard.new StandardDeliver();
      }

   }

   private class TestingDeliverFactory extends DeliverFactory {

      public TestingDeliverFactory(SMTPSenderStandard smtpSenderStandard) {
         super(smtpSenderStandard);
      }

      @Override
      public Deliver getDeliverInstance() {
         return smtpSenderStandard.new TestingDeliver();
      }
   }

   public SMTPSenderStandard(boolean testing) {
      super("DelS:", ConfigurationManager.getInstance().isAmavisSupportActive()||(Mail.isTesting()&&!testing)?
            ConfigurationManager.getInstance().getAmavisSMTPDirectory():
            ConfigurationManager.getInstance().getSMTPDirectory());
      this.testing = testing;
      this.deliverFactory = !ConfigurationManager.getInstance().isLocalTestingMode()?new DeliverFactory(this):new TestingDeliverFactory(this);
   }
   
   public final Deliver getNewDeliverInstance() {
      return deliverFactory.getDeliverInstance();
   }

   public class StandardDeliver extends SMTPSender.Deliver {

       private List<FailedAddressItem> failedAddresses;

       public void run() {

           failedAddresses = new ArrayList<FailedAddressItem>();
           List<EmailAddress> toAddresses = message.getToAddresses();
           EmailAddress address = null;

           // If the next scheduled delivery attempt is still in the future, skip.
           if( message.getScheduledDelivery().getTime() > System.currentTimeMillis() ) {
               updatequeueitem();
               if( log.isTraceEnabled() ) {
                  log.trace( "Skipping delivery of message " + message.getSMTPUID() +
                        " because the scheduled delivery time is still in the future: " + message.getScheduledDelivery() );
               }
               return;
           }

           Map<Domain,List<EmailAddress>> perDomainAddresses = new HashMap<Domain,List<EmailAddress>>();
           Iterator<EmailAddress> iter = toAddresses.iterator();

           while(iter.hasNext()) {
              address = iter.next();
              if (!perDomainAddresses.containsKey(address.getDomain())) {
                 perDomainAddresses.put(address.getDomain(), new ArrayList<EmailAddress>());
              }
              if (!perDomainAddresses.get(address.getDomain()).contains(address)) {
                 perDomainAddresses.get(address.getDomain()).add(address);
              }

           }

           Iterator<Entry<Domain,List<EmailAddress>>> iter2 = perDomainAddresses.entrySet().iterator();
           Iterator<EmailAddress> iter3;
           Iterator<FailedAddressItem> iter4;
           Entry<Domain, List<EmailAddress>> entry;
           EmailAddress[] deliveryAddresses;
           User user;
           while(iter2.hasNext()) {
              entry = iter2.next();
              if (configurationManager.isLocalDomain(entry.getKey().getDomainName())&&(testing?!entry.getKey().getUniqueName().equals("example.com"):true)) {
                 iter3 = entry.getValue().iterator();
                 while(iter3.hasNext()) {
                    address = iter3.next();
                    try {
                       user = configurationManager.getUser(address);
                       //Local user doesn't exist and server policy possibly allows delivery to a default mailbox
                       if (user==null) {
                           deliverLocalMessage ( address, message );
                       }
                       else {
                           deliveryAddresses = user.getForwardAddresses();
                           //No forward addresses, just perform local delivery
                           if (deliveryAddresses.length==0) {
                                 deliverLocalMessage ( address, message );
                           }
                           else {
                              int originalPosition = getOriginalRecipientPosition(address, deliveryAddresses);

                              //Forward addresses include the original recipient
                              if (originalPosition!=-1) {
                                 //The only forward address is the original recipient, perform local delivery
                                 if (deliveryAddresses.length==1) {
                                    deliverLocalMessage ( address, message );
                                 }
                                 //The original recipient is included
                                 else {
                                    //First create the forwarded message
                                    createForwardedMessage( address, deliveryAddresses, message );

                                    //Now perform the local delivery
                                    deliverLocalMessage ( address, message );
                                 }
                              }
                              //Original recipient is not included
                              else {
                                    //Simply create the forwarded message
                                    createForwardedMessage( address, deliveryAddresses, message );
                              }
                           }
                       }
                       if( log.isInfoEnabled() ) {
                          log.info( "Delivery complete for message " + message.getSMTPUID() + " to: " + address );
                       }
                    }
                    catch (NotFoundException e) {
                       log.warn( "Delivery attempted to unknown user: " + address );
                       //The addressee does not exist.  Notify the sender of the error.
                       if (message.getFromAddress().isNULL()) {
                          log.warn ( "Return path is NULL, cannot bounce." );
                       }
                       else if (message.getFromAddress().isMailerDaemon()) {
                          log.warn ( "Return mailbox is Mailer-Daemon, message will not be bounced." );
                       }
                       else {
                          if (failedAddresses.isEmpty()) {
                              failedAddresses.add( new FailedAddressItem( address, "550 "+e.getMessage() ) );
                          }
                          bounceMessage( BOUNCE_FAILED, failedAddresses, message, address.getDomain());
                          failedAddresses.clear();
                       }
                    }
                    catch (Throwable throwable) {
                       log.error( "Delivery failed for message from: " + message.getFromAddress() + " to: " + address + " - " + throwable, null );
                       failedAddresses.add( new FailedAddressItem( address, throwable.getMessage() ) );
                    }
                 }
              }
              else {
                 try {
                    deliverRemoteMessage( entry.getKey(), entry.getValue(), message );
                    List<FailedAddressItem> failed550RCPT = new ArrayList<FailedAddressItem>();
                    iter4 = failedAddresses.iterator();
                    FailedAddressItem fai;
                    while (iter4.hasNext()) {
                       fai = iter4.next();
                       if (fai.getMessage().startsWith("5")) {
                          failed550RCPT.add(fai);
                          iter4.remove();
                       }
                    }
                    if (failed550RCPT.size()>0) {
                       bounceMessage( BOUNCE_FAILED, failed550RCPT, message, entry.getKey() );
                    }
                    failed550RCPT.clear();fai = null;
                 }
                 catch( PermanentNegativeException pne) {
                    log.warn( "Delivery to " + entry.getKey() + " failed.");
                    //There was a permanent error. Notify the mail sender.
                    if (message.getFromAddress().isNULL()) {
                       log.warn ( "Return path is NULL, cannot bounce." );
                    }
                    else if (message.getFromAddress().isMailerDaemon()) {
                       log.warn ( "Return mailbox is Mailer-Daemon, message will not be bounced." );
                    }
                    else {
                       bounceMessage( BOUNCE_FAILED, failedAddresses, message, entry.getKey() );
                    }
                    failedAddresses.clear();
                 }
                 //TransientNegativeExceptions get caught here
                 catch( Throwable throwable ) {
                    log.error( "Delivery failed for message from: " + message.getFromAddress() +
                           " to domain: " + entry.getKey() + " - " + throwable, null );
                    failedAddresses.add( new FailedAddressItem( address, throwable.getMessage() ) );
                 }
              }

           }

           // If all addresses were successful, remove the message from the spool
           if( failedAddresses.isEmpty() ) {
               // Log an error if the delete fails.  This will cause the message to get
               // delivered again, but it is too late to roll back the delivery.
               if( !message.getSMTPPersistenceProccessor().deleteMessage() ) {
                   log.error( "Error removing SMTP message after delivery!  This message may be redelivered. " + message.getSMTPUID() );
               }
               deletequeueitem();
           }
           // Update the message with any changes.
           else {
               Vector<EmailAddress> failedToAddresses = new Vector<EmailAddress>(failedAddresses.size());
               iter4 = failedAddresses.iterator();
               while (iter4.hasNext()) {
                  failedToAddresses.add( iter4.next().getAddress() );
               }

               message.setToAddresses( failedToAddresses );
               int deliveryAttempts = message.getDeliveryAttempts();

               // If the message is a bounced email, just give up and move it to the failed directory.
               if(message.getFromAddress().isMailerDaemon()) {
                   try {
                       log.info( "Delivery of message from MAILER_DAEMON failed, moving to failed folder." );
                       message.getSMTPPersistenceProccessor().moveToFailedFolder();
                   }
                   catch (IOException e) {
                       log.error( "Unable to move failed message to 'failed' folder." );
                   }
                   deletequeueitem();
               }
               // If we have not passed the maximum delivery count, calculate the
               // next delivery time and save the message.
               else if(  deliveryAttempts < configurationManager.getDeliveryAttemptThreshold() ) {
                   message.setDeliveryAttempts( deliveryAttempts + 1 );

                   // Reschedule later, 1 min, 2 min, 4 min, 8 min, ... 2^n
                   // Cap delivery interval at 2^10 minutes. (about 17 hours)
                   if( deliveryAttempts > 10 ) {
                       deliveryAttempts = 10;
                   }
                   long offset = (long)Math.pow( 2, deliveryAttempts);
                   Date schedTime = new Date(System.currentTimeMillis() + offset*60*1000);
                   message.setScheduledDelivery( schedTime );

                   try {
                       message.getSMTPPersistenceProccessor().save(useAmavisSMTPDirectory);
                   }
                   catch( IOException ioe ) {
                       log.error( "Error updating spooled message for next delivery.  Message may be re-delivered.", ioe );
                   }
                   updatequeueitem();
                   if (deliveryAttempts==4) {
                       try {
                           bounceMessage(BOUNCE_DELAYED, failedAddresses, message,
                                 configurationManager.isLocalDomain(message.getFromAddress().getDomain().getDomainName())?message.getFromAddress().getDomain():failedToAddresses.get(0).getDomain());
                       }
                       catch(Exception e) {
                           log.error( "Problem bouncing message. " + message.getSMTPUID() );
                       }
                   }
               }
               // All delivery attempts failed, bounce message.
               else {
                   // Send a bounce message for all failed addresses.
                   try {
                       bounceMessage(BOUNCE_FAILED, failedAddresses, message,
                              configurationManager.isLocalDomain(message.getFromAddress().getDomain().getDomainName())?message.getFromAddress().getDomain():failedToAddresses.get(0).getDomain());
                   }
                   catch(Exception e) {
                       log.error( "Problem bouncing message. " + message.getSMTPUID() );
                   }
                   deletequeueitem();

                   // Remove the original message.
                   if( !message.getSMTPPersistenceProccessor().deleteMessage() ) {
                       log.error( "Error removed SMTP message after bounce! This message may be re-bounced. " + message.getSMTPUID() );
                   }
               }
           }
       }

       private int getOriginalRecipientPosition(EmailAddress originalRecipient, EmailAddress[] deliveryAddresses) {
          for (int i=0;i<deliveryAddresses.length;i++) {
             if (originalRecipient.equals(deliveryAddresses[i])) return i;
          }
          return -1;
       }

       private void createForwardedMessage(EmailAddress address, EmailAddress[] deliveryAddresses, SMTPMessage message) throws IOException{

          String US_ASCII = "US-ASCII";
          String boundary = createBoundary();
          Locale englishLocale = Locale.ENGLISH;
          List<byte[]> dataLines = null;
          List<byte[]> forwardDataLines = new ArrayList<byte[]>();
          try {
             //First get the subject
             dataLines = message.getSMTPPersistenceProccessor().loadIncrementally(8);
             Iterator<byte[]> iter = dataLines.iterator();
             String subject = "";
             while(iter.hasNext()) {
                subject = new String(iter.next(), US_ASCII);
                if (subject.trim().toLowerCase(englishLocale).startsWith("subject:")) {
                   subject = subject.substring(subject.indexOf(':')+1);
                   break;
                }
             }
             if (subject.length()==0) {
                subject = "[ FW: Message forwarded from "+address.getUsername()+" ]";
             }
             else {
                subject = "[ FW: "+subject+" ]";
             }

             SMTPMessage forwardedMessage = new SMTPMessageImpl();

             forwardedMessage.setFromAddress( address );
             for( int index = 0; index < deliveryAddresses.length; index++ ) {
                if (deliveryAddresses[index].equals(address)) continue;
                forwardedMessage.addToAddress( deliveryAddresses[index] );
             }
             forwardedMessage.getSMTPPersistenceProccessor().saveBegin(configurationManager.isAmavisSupportActive());

             forwardDataLines.add( string2Bytes("From: <" + address + ">") );
             forwardDataLines.add( string2Bytes("To: undisclosed recipients;")  );
             forwardDataLines.add( string2Bytes("Subject: "+subject) );
             forwardDataLines.add( string2Bytes("Date: " + new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z", Locale.ENGLISH).format(forwardedMessage.getTimeReceived())) );
             forwardDataLines.add( string2Bytes("MIME-Version: 1.0") );
             forwardDataLines.add( string2Bytes("Content-Type: multipart/mixed;") );
             forwardDataLines.add( string2Bytes(" boundary=\""+boundary+"\"") );
             forwardDataLines.add( string2Bytes("") );
             forwardDataLines.add( string2Bytes("--"+boundary) );
             forwardDataLines.add( string2Bytes("Content-Type: text/plain; charset=ISO-8859-1") );
             forwardDataLines.add( string2Bytes("Content-Transfer-Encoding: 7bit") );
             forwardDataLines.add( string2Bytes("") );
             forwardDataLines.add( string2Bytes("") );
             forwardDataLines.add( string2Bytes("--"+boundary) );
             forwardDataLines.add( string2Bytes("Content-Type: message/rfc822;") );
             forwardDataLines.add( string2Bytes(" name=\"FW: "+subject.substring(6, subject.lastIndexOf(' '))+".eml\"") );
             forwardDataLines.add( string2Bytes("Content-Transfer-Encoding: 7bit") );
             forwardDataLines.add( string2Bytes("") );

             forwardedMessage.getSMTPPersistenceProccessor().saveIncrement(forwardDataLines, true, false);
             forwardDataLines.clear();

             int count = 8;
              while (dataLines.size()>0) {

                forwardedMessage.getSMTPPersistenceProccessor().saveIncrement(dataLines, false, true);
                count+=250;
                dataLines.clear();
                dataLines = message.getSMTPPersistenceProccessor().loadIncrementally(count);
             }

             forwardDataLines.add( string2Bytes("--"+boundary+"--") );

             forwardedMessage.getSMTPPersistenceProccessor().saveIncrement(forwardDataLines, false, true);
             forwardDataLines.clear();

             if (!forwardedMessage.getSMTPPersistenceProccessor().saveFinish()) {
                throw new IOException("Forwarded message not saved, aborting.");
             }

          }
          finally {
             if (dataLines!=null) {
                dataLines.clear();
             }
             if (forwardDataLines!=null) {
                forwardDataLines.clear();
             }
          }

       }

       /**
        * This method takes a local SMTPMessage and attempts to deliver it.
        */
       protected void deliverLocalMessage( EmailAddress address, SMTPMessage message )
              throws NotFoundException, IOException {

           if( log.isDebugEnabled() ) {
             log.debug( "Delivering Message to local user: " + address );
           }

           User user = null;
           //Load the user.  If the user doesn't exist, a not found exception will
           //be thrown and the deliver() message will deal with the notification.
           user = configurationManager.getUser( address );
           if( user == null ) {
              if (log.isDebugEnabled()) {
                  log.debug( "User not found, checking for default delivery options" );
              }
               //Check to see if a default delivery mailbox exists, and if so, deliver it.
               //Otherwise, just throw the NotFoundException to bounce the email.
               EmailAddress defaultAddress = configurationManager.getDefaultMailbox(address.getDomain().getDomainName());
               if (!defaultAddress.isNULL()) {
                  //If this throws a NotFoundException, go ahead and let it bounce.
                  user = configurationManager.getUser( defaultAddress );
                  if( user == null ) throw new NotFoundException( "User does not exist and no default delivery options found." );
                  if( log.isDebugEnabled() ) {
                    log.info( "Delivering message addressed to: " + address + " to default user: " + defaultAddress );
                  }
               }
               else {
                   
                  throw new NotFoundException( "User does not exist and no default delivery options found." );
               }
           }

           Object persistedID = LocalDeliveryFactory.getInstance().getLocalDeliveryProccessor().persistLocalMessage(user, message, address);

           ModuleControl.getPassReceivedLocalMessage().passMessage(persistedID);
       }

       /**
        * Handles delivery of messages to addresses not handled by this server.
        */
       private void deliverRemoteMessage( Domain domain, List<EmailAddress> addresses, SMTPMessage message )
             throws TransientNegativeException, PermanentNegativeException{

          if( log.isDebugEnabled() ) {
            log.debug( "Delivering Message to remote " + domain );
          }

          //Delegate this request to the SMTPRemoteSender class.
          SMTPRemoteSender smtpRemoteSender = new SMTPRemoteSender();
          try {
             smtpRemoteSender.sendMessage( domain, addresses, message );
             if( log.isInfoEnabled() ) {
                log.info( "Delivery complete for message " + message.getSMTPUID() + " to recipient(s) at " + domain );
             }
          }
          finally {
             failedAddresses.addAll(smtpRemoteSender.getFailedAddresses());
             if( log.isDebugEnabled()&&failedAddresses.size()>0 ) {
                Iterator<FailedAddressItem> iter = failedAddresses.iterator();
                FailedAddressItem fai;
                while(iter.hasNext()) {
                   fai = iter.next();
                   log.debug("Recipient "+(fai.getAddress()).getUsername()+" "+(fai.getMessage().startsWith("5")?" was rejected.":"is delayed."));
                }

             }
             smtpRemoteSender.cleanUp();
          }
       }

       private static final String characterPool = "0123456789";

       private String createBoundary() {

          Random random = new Random();
          StringBuilder boundary = new StringBuilder(24);
          for (int i=0;i<24;i++) {
             boundary.append(characterPool.charAt(random.nextInt(10)));
          }
          return boundary.toString();
       }

       private String getDeliveryAttemptsTotalDuration() {

          int timeInMinutes = 0;
          for (int i=0;i<configurationManager.getDeliveryAttemptThreshold();i++) {
             timeInMinutes+=Math.pow(2, configurationManager.getDeliveryAttemptThreshold());
          }
          int days=0, hours=0, minutes=0;
          if (timeInMinutes>=1440) {
             days = timeInMinutes/1440;
             timeInMinutes -= days*1440;
          }
          if (timeInMinutes>=60) {
             hours = timeInMinutes/60;
             timeInMinutes -= hours*60;
          }
          minutes = timeInMinutes;

          String message="";
          if (days!=0) {
             if (hours!=0 || minutes!=0) {
                message = ""+days+" days, ";
             }
             else {
                message = ""+days+" days.";
             }
          }
          if (hours!=0) {
             if (minutes!=0) {
                message += ""+hours+" hours, ";
             }
             else {
                message += ""+hours+" hours.";
             }
          }
          if (minutes!=0) {
             message += ""+minutes+" minutes.";
          }

          return message;
       }

       private byte[] string2Bytes( String line ) {
          try {
             return line.getBytes("US-ASCII");
          }
          catch (UnsupportedEncodingException uee) {
             return line.getBytes();
          }
       }

      private String generateMUID() {

         byte[] salt = new byte[8];

         SecureRandom sr;
         try {
            sr = SecureRandom.getInstance("SHA1PRNG");
            sr.setSeed(System.currentTimeMillis());
            sr.nextBytes(salt);
         } catch (NoSuchAlgorithmException ex) {}

         return new String(ByteUtils.toHex(salt));
      }

       protected void bounceMessage( int bounceType, List<FailedAddressItem> failedAddresses, SMTPMessage message, Domain msgIdDomain ) {

           if( log.isInfoEnabled() ) {
             log.info( "Bouncing Messsage from " + message.getFromAddress() +
                 (failedAddresses.isEmpty()?" to mailbox at domain " + message.getToAddresses().get(0).getDomain():
                 (" to at least " + ((EmailAddress)((FailedAddressItem) failedAddresses.get(0)).getAddress()))));
           }

           SMTPMessage bounceMessage = new SMTPMessageImpl();
           String boundary = createBoundary();

           List<FailedAddressItem> remaining = new ArrayList<FailedAddressItem>(failedAddresses);
           Iterator<FailedAddressItem> iter;

           //Set the from address as mailserver@ the first (default) local domain.
           EmailAddress fromAddress = EmailAddress.getEmailAddress( "MAILER_DAEMON", message.getFromAddress().getDomain() );

           bounceMessage.setFromAddress( fromAddress );
           bounceMessage.addToAddress( message.getFromAddress() );
           bounceMessage.addDataLine( string2Bytes("From: Mail Delivery Subsystem <MAILER-DAEMON@" + message.getFromAddress().getDomain().getDomainName() + ">") );
           bounceMessage.addDataLine( string2Bytes("Message-ID: <"+generateMUID()+'@'+msgIdDomain.getDomainName()+">") );
           bounceMessage.addDataLine( string2Bytes("Auto-Submitted: auto-replied") );
           bounceMessage.addDataLine( string2Bytes("To: " + message.getFromAddress().getAddress()) );
           bounceMessage.addDataLine( string2Bytes("Subject: Message Delivery "+(bounceType==BOUNCE_FAILED?"Error.":"Delayed.")) );
           bounceMessage.addDataLine( string2Bytes("Date: " + new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z", Locale.ENGLISH).format(new Date())) );
           bounceMessage.addDataLine( string2Bytes("MIME-Version: 1.0") );
           bounceMessage.addDataLine( string2Bytes("Content-Type: multipart/report; report-type=delivery-status;") );
           bounceMessage.addDataLine( string2Bytes(" boundary=\""+boundary+"\"") );
           bounceMessage.addDataLine( string2Bytes("") );
           bounceMessage.addDataLine( string2Bytes("--"+boundary) );
           bounceMessage.addDataLine( string2Bytes("") );
           if (bounceType==BOUNCE_FAILED) {
              bounceMessage.addDataLine( string2Bytes("Error delivering message to: " + ((EmailAddress)failedAddresses.get(0).getAddress()).getAddress()) );
              iter = remaining.iterator();
              if (iter.hasNext()) iter.next();
              while(iter.hasNext()) {
                 bounceMessage.addDataLine( string2Bytes("                             " + ((EmailAddress)iter.next().getAddress()).getAddress()));
              }
              bounceMessage.addDataLine( string2Bytes("This message will not be delivered.") );
           }
           else {
              bounceMessage.addDataLine( string2Bytes("Message has not been delivered to the intended recipient: " + ((EmailAddress)failedAddresses.get(0).getAddress()).getAddress()) );
              iter = remaining.iterator();
              if (iter.hasNext()) iter.next();
              while(iter.hasNext()) {
                 bounceMessage.addDataLine( string2Bytes("                                                          " + ((EmailAddress)iter.next().getAddress()).getAddress()));
              }
              bounceMessage.addDataLine( string2Bytes("Attempts to be delivered will continue for up to "+getDeliveryAttemptsTotalDuration()));
           }
           bounceMessage.addDataLine( string2Bytes("------------------") );
           bounceMessage.addDataLine( string2Bytes("") );
           bounceMessage.addDataLine( string2Bytes("--"+boundary) );
           bounceMessage.addDataLine( string2Bytes("content-type: message/delivery-status") );
           bounceMessage.addDataLine( string2Bytes("") );
           bounceMessage.addDataLine( string2Bytes("Reporting-MTA: dns; "+message.getFromAddress().getDomain()) );
           bounceMessage.addDataLine( string2Bytes("") );
           iter = remaining.iterator();
           FailedAddressItem item;
           while(iter.hasNext()) {

              item = iter.next();
              bounceMessage.addDataLine( string2Bytes("Final-Recipient: rfc822; "+((EmailAddress)item.getAddress()).getAddress()) );
              bounceMessage.addDataLine( string2Bytes("Action: "+(bounceType==BOUNCE_FAILED?"failed":"delayed")) );
              bounceMessage.addDataLine( string2Bytes("Status: "+item.getMessage().charAt(0)+".0.0") );
              bounceMessage.addDataLine( string2Bytes("Diagnostic-Code: "+item.getMessage()) );
              bounceMessage.addDataLine( string2Bytes("") );
           }
           if (bounceType==BOUNCE_FAILED) {
              bounceMessage.addDataLine( string2Bytes("--"+boundary) );
              bounceMessage.addDataLine( string2Bytes("content-type: message/rfc822") );
              bounceMessage.addDataLine( string2Bytes("") );

              List<byte[]> dataLines;
              try {
                 dataLines = message.getSMTPPersistenceProccessor().loadIncrementally(8);
                 int numLines = Math.min(dataLines.size(), 30);

                 for( int index = 0; index <  numLines; index++ ) {
                    if (dataLines.get( index ).length==0) break;
                    bounceMessage.addDataLine( dataLines.get( index ) );
                 }
                 bounceMessage.addDataLine( string2Bytes("") );
              } catch (IOException ex) {
                 log.error(ex);
              }

           }
           bounceMessage.addDataLine( string2Bytes("--"+boundary+"--") );

           //Save this message so it will be delivered.
           try {
               bounceMessage.getSMTPPersistenceProccessor().saveBegin(useAmavisSMTPDirectory);
               bounceMessage.getSMTPPersistenceProccessor().saveIncrement(bounceMessage.getDataLines(), true, false);
               bounceMessage.getSMTPPersistenceProccessor().saveFinish();
           }
           catch (IOException ioe) {
               log.error( "Error storing outgoing 'bounce' email message");
               throw new RuntimeException(ioe);
           }
       }
   }

   public class TestingDeliver extends StandardDeliver {

      @Override
       public void run() {

           List<EmailAddress> toAddresses = message.getToAddresses();
           EmailAddress address = null;

           // If the next scheduled delivery attempt is still in the future, skip.
           if( message.getScheduledDelivery().getTime() > System.currentTimeMillis() ) {
               updatequeueitem();
               if( log.isTraceEnabled() ) {
                  log.trace( "Skipping delivery of message " + message.getSMTPUID() +
                        " because the scheduled delivery time is still in the future: " + message.getScheduledDelivery() );
               }
               return;
           }

           Map<Domain,List<EmailAddress>> perDomainAddresses = new HashMap<Domain,List<EmailAddress>>();
           Iterator<EmailAddress> iter = toAddresses.iterator();

           while(iter.hasNext()) {
              address = iter.next();
              if (!perDomainAddresses.containsKey(address.getDomain())) {
                 perDomainAddresses.put(address.getDomain(), new ArrayList<EmailAddress>());
              }
              if (!perDomainAddresses.get(address.getDomain()).contains(address)) {
                 perDomainAddresses.get(address.getDomain()).add(address);
              }

           }

           Iterator<Entry<Domain,List<EmailAddress>>> iter2 = perDomainAddresses.entrySet().iterator();
           Iterator<EmailAddress> iter3;
           Entry<Domain,List<EmailAddress>> entry;
           while(iter2.hasNext()) {
              entry = iter2.next();
              iter3 = entry.getValue().iterator();
              while(iter3.hasNext()) {
                 address = iter3.next();
                 try {
                    deliverLocalMessage ( address, message );
                    if( log.isInfoEnabled() ) {
                       log.info( "Delivery complete for message " + message.getSMTPUID() + " to: " + address );
                    }
                 }
                 catch (NotFoundException e) {
                    log.info( "Delivery attempted to unknown user: " + address );
                    //The addressee does not exist.  Notify the sender of the error.
                 }
                 catch (Throwable throwable) {
                    log.error( "Delivery failed for message from: " + message.getFromAddress() + " to: " + address + " - " + throwable, null );
                 }
              }

           }

           // Log an error if the delete fails.  This will cause the message to get
           // delivered again, but it is too late to roll back the delivery.
           if( !message.getSMTPPersistenceProccessor().deleteMessage() ) {
              log.error( "Error removed SMTP message after delivery!  This message may be redelivered. " + message.getSMTPUID() );
           }
           deletequeueitem();
       }

       @Override
       protected void deliverLocalMessage( EmailAddress address, SMTPMessage message )
              throws NotFoundException, IOException {

           LocalDeliveryFactory.getInstance().getLocalDeliveryProccessor().persistLocalMessage(null, message, address);
       }
   }

   static final int BOUNCE_FAILED=1;
   static final int BOUNCE_DELAYED=2;

}
