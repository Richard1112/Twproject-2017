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
 * $Rev: 321 $
 * $Date: 2013-08-27 22:43:27 +0200 (Tue, 27 Aug 2013) $
 *
 ******************************************************************************/

package com.ericdaugherty.mail.server.configuration;

//Java imports
import java.io.IOException;
import org.w3c.dom.Element;

//Local imports
import com.ericdaugherty.mail.server.configuration.cbc.CBCExecutor;
import com.ericdaugherty.mail.server.info.*;
import java.util.Set;


/**
 *
 * @author Andreas Kyrmegalos
 */
public final class ConfigurationManagerBackendLDAP implements ConfigurationManagerBackEnd{

   public void init(Element element) {}
   public void shutdown() {}
   /* Not implemented in a LDAP scope */
   public void restore(String backupDirectory) throws IOException {}
   /* Not implemented in a LDAP scope */
   public void doBackup(String backupDirectory) throws IOException {}
   /* Not implemented in a LDAP scope */
   public void doWeeklyBackup(String backupDirectory) throws IOException {}

   public void persistUsersAndRealms() {}
   public void updateThroughConnection(CBCExecutor cbcExecutor) {}
   public boolean persistUserUpdate() {return false;}
   public char[] getRealmPassword(Realm realmName, EmailAddress username) {return null;}
   public void loadUsersAndRealms() {}
   public void updateUsersAndRealmPasswords() {}
   public Set<Domain> getDomains(){return null;}
   public Set<Realm> getRealms() {return null;}
   public void updateDomains(String domains, String defaultMailboxes) {}
   public boolean isLocalDomain(String domain) {return false;}
   public boolean isSingleDomainMode() {return false;}
   public Domain getSingleDomain() {return null;}
   public Domain getDefaultDomain() {return null;}
   public void updateDefaultDomain() {}
   public EmailAddress getDefaultMailbox(String domain) {return null;}
   public User getUser(EmailAddress address) {return null;}
   public Realm getRealm(String realmName) {return null;}

   public boolean isUserARealmMember(Realm realm, String username_lower_case) {return false;}
}
