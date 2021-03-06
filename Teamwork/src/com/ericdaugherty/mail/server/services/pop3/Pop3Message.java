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

//Logging Imports

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

//Local Imports
import com.ericdaugherty.mail.server.info.User;
import com.ericdaugherty.mail.server.persistence.LocalDeliveryFactory;

/**
 * Simple bean class that represents a POP3 Pop3Message used in
 * POP3 Service.
 *
 * @author Eric Daugherty
 */
public class Pop3Message {

  /**
   * Logger Category for this class.
   */
  //private static Log log = LogFactory.getLog( Pop3Message.class );
  private static Log log = LogFactory.getLog("JESLogger");

  private String messageLocation;
  private boolean deleted = false;

  public String getMessageLocation() {
    return messageLocation;
  }

  public void setMessageLocation(String messageLocation) {
    this.messageLocation = messageLocation;
  }

  public long getMessageSize(User user) {
    return LocalDeliveryFactory.getInstance().getLocalDeliveryProccessor().getMessagePersistedSize(user, messageLocation);
  }

  public boolean isDeleted() {
    return deleted;
  }

  public void setDeleted(boolean deleted) {
    this.deleted = deleted;
    if (log.isDebugEnabled()) {
      log.debug("Setting is deleted to: " + deleted);
    }
  }

  public String getUniqueId() {
    return messageLocation.substring(0, messageLocation.lastIndexOf(".loc"));
  }

  @Override
  public int hashCode() {
    return getUniqueId().hashCode();
  }

  @Override
  public boolean equals(Object object) {
    if (!(object instanceof Pop3Message)) {
      return false;
    }
    Pop3Message that = (Pop3Message) object;
    if (!this.getUniqueId().equals(that.getUniqueId())) return false;
    return true;
  }
}