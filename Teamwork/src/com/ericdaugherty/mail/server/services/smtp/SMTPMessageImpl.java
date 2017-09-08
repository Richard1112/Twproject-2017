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

package com.ericdaugherty.mail.server.services.smtp;

//Java imports
import java.io.*;
import java.util.*;

//Local imports
import com.ericdaugherty.mail.server.info.EmailAddress;
import com.ericdaugherty.mail.server.persistence.SMTPMessagePersistenceFactory;
import com.ericdaugherty.mail.server.persistence.SMTPMessagePersistenceProccessor;

/**
 * A class representing an instance of a JES SMTP Message.
 *
 * @author Andreas Kyrmegalos
 */
public class SMTPMessageImpl implements SMTPMessage {

    //***************************************************************
    // Variables
    //***************************************************************

    private Date timeReceived;
    private Date scheduledDelivery;
    private int deliveryAttempts;
    private EmailAddress fromAddress;
    private boolean mime8bit;
    private List<EmailAddress> toAddresses = new ArrayList<EmailAddress>();
    private String smtpUID;
    private List<byte[]> dataLines = new ArrayList<byte[]>();
    private long size = 0;
    private SMTPMessagePersistenceProccessor sMTPPersistenceProccessor;

    //***************************************************************
    // Constructor
    //***************************************************************

    /**
     * Instantiates a new message with the current time.
     */
    public SMTPMessageImpl() {
       Date now = new Date();
       timeReceived = now;
       scheduledDelivery = now;
       deliveryAttempts = 0;
       sMTPPersistenceProccessor = SMTPMessagePersistenceFactory.getInstance().getSMTPPersistenceProccessor();
       sMTPPersistenceProccessor.setMessage(this);
    }

    /**
     * Instantiates a message using a stored file.
     */
    public SMTPMessageImpl(String messagePersistanceName, boolean headersOnly) throws IOException{
       sMTPPersistenceProccessor = SMTPMessagePersistenceFactory.getInstance().getSMTPPersistenceProccessor();
       sMTPPersistenceProccessor.setMessage(this);
       sMTPPersistenceProccessor.initializeMessage(messagePersistanceName, headersOnly);
    }

    //***************************************************************
    // Public Interface
    //***************************************************************

    @Override
    public int hashCode() {
       return getSMTPUID().hashCode();
    }

    @Override
    public boolean equals(Object object) {
       if (!(object instanceof SMTPMessage)) {
         return false;
      }
      SMTPMessage that = (SMTPMessage) object;
      if (!this.getSMTPUID().equals(that.getSMTPUID())) return false;
      return true;
    }

    public Date getTimeReceived() {
        return timeReceived;
    }

    public void setTimeReceived(Date timeReceived) {
        this.timeReceived = timeReceived;
    }

    public Date getScheduledDelivery() {
        return scheduledDelivery;
    }

    public void setScheduledDelivery(Date scheduledDelivery) {
        this.scheduledDelivery = scheduledDelivery;
    }

    public int getDeliveryAttempts() {
        return deliveryAttempts;
    }

    public void setDeliveryAttempts(int deliveryAttempts) {
        this.deliveryAttempts = deliveryAttempts;
    }

    public EmailAddress getFromAddress(){ return fromAddress; }

    public void setFromAddress(EmailAddress fromAddress){ this.fromAddress = fromAddress; }

    public List<EmailAddress> getToAddresses() { return toAddresses; }

    public void setToAddresses( List<EmailAddress> toAddresses ) { this.toAddresses = toAddresses; }

    public void addToAddress( EmailAddress toAddress ) { if (!toAddresses.contains(toAddress)) toAddresses.add( toAddress ); }

    public boolean is8bitMIME() {
       return mime8bit;
    }

    public void set8bitMIME(boolean mime8bit) {
       this.mime8bit = mime8bit;
    }

    public List<byte[]> getDataLines() { return dataLines; }

    public void addDataLine( byte[] line ) {
       sMTPPersistenceProccessor.addDataLine(line);
    }
    
    public final void setSMTPUID(String smtpUID) {
       this.smtpUID = smtpUID;
    }

    public void incrementSize(long increment) {
       size += increment;
    }

    public long getSize() {
       if (size==0) {
          size = sMTPPersistenceProccessor.getSize();
       }
       return size;
    }
    
    public String getSMTPUID() {
       return smtpUID;
    }

   /**
    * @return the SMTPMessagePersistenceProccessor
    */
   public SMTPMessagePersistenceProccessor getSMTPPersistenceProccessor() {
      return sMTPPersistenceProccessor;
   }
}