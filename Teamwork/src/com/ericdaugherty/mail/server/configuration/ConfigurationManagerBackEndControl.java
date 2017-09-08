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
import java.security.AccessController;
import java.util.*;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

//Log imports
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

//Local imports
import com.ericdaugherty.mail.server.Mail;
import com.ericdaugherty.mail.server.configuration.backEnd.PersistException;
import com.ericdaugherty.mail.server.configuration.cbc.CBCExecutor;
import com.ericdaugherty.mail.server.info.*;

/**
 *
 * @author Andreas Kyrmegalos
 */
final class ConfigurationManagerBackendControl implements ConfigurationParameterConstants {

   /** Logger */
   private static Log log = LogFactory.getLog("JESLogger");
   //private static Log log = LogFactory.getLog(ConfigurationManager.class);
   private final ConfigurationManager cm = ConfigurationManager.getInstance();
   
   void shutdown() {
      
      if (backEnd != null) {
         backEnd.shutdown();
         backEnd = null;
      }
   }
   
   Map<String,String> getConfiguration() {
      
      Map<String,String> configuration = new HashMap<String,String>();
      
      configuration.put("selectedBackEndType", backEndType.toString());
      configuration.put("backendSecure", Boolean.valueOf(backendSecure)+RESTART);
      configuration.put("backendMinimum", backendMinimum+RESTART);
      configuration.put("backendMinimumMin", "15");
      configuration.put("backendMinimumMax", "50");
      configuration.put("backendMaximum", backendMaximum+RESTART);
      configuration.put("backendMaximumMin", "100");
      configuration.put("backendMaximumMax", "1000");
      configuration.put("allowRemoteRestart", Boolean.toString(cm.isAllowRemoteRestart()));

      return configuration;
   }
   
   private JESVaultControl jesVaultControl;
   private char[] guiDbPassword;
   private ConfigurationManagerBackEnd backEnd;
   private BackEndTypeEnum backEndType;
   private boolean backendSecure;
   private int backendMinimum;
   private int backendMaximum;

   char[] getGUIDbPassword() {

      AccessController.checkPermission(new PropertyPermission("jes.guiDBPassword", "read"));
      return guiDbPassword;
   }

   public ConfigurationManagerBackEnd getBackEnd() {
      return backEnd;
   }

   /**
    * The type of backend used to store domains, users, realms
    *
    * @return backEndType BackEndTypeEnum
    */
   public BackEndTypeEnum getBackEndType() {
      return backEndType;
   }

   public boolean isBackendSecure() {
      return backendSecure;
   }

   /**
    * Checks the local domains to see if the specified parameter matches.
    *
    * @param domain a domain to check.
    * @return true if and only if it matches exactly an existing domain.
    */
   public boolean isLocalDomain(String domain) {
      return backEnd.isLocalDomain(domain);
   }
   
   public boolean isSingleDomainMode() {
      return backEnd.isSingleDomainMode();
   }
   
   public Domain getSingleDomain() {
      return backEnd.getSingleDomain();
   }

   public EmailAddress getDefaultMailbox(String domain) {
      if (domain == null) {
         return null;
      }
      return backEnd.getDefaultMailbox(domain);
   }

   public void updateDefaultDomain() {
      backEnd.updateDefaultDomain();
   }

   /**
    * Returns the specified user, or null if the user
    * does not exist.
    *
    * @param address the user's full email address.
    * @return null if the user does not exist.
    */
   public User getUser(EmailAddress address) {
      User user = backEnd.getUser(address);
      if (log.isInfoEnabled() && user == null) {
         log.info("Tried to load non-existent user: " + address);
      }

      return user;
   }

   /**
    * Returns the specified realm, or null if the realm
    * does not exist.
    *
    * @param realmName the realm's full name.
    * @return null if the realm does not exist.
    */
   public Realm getRealm(String realmName) {
      Realm realm = backEnd.getRealm(realmName);
      if (realm == null) {
         log.info("Tried to load non-existent realm: " + realmName);
      }
      return realm;
   }

   public Set<? extends Realm> getRealms() {
      return backEnd.getRealms();
   }

   public char[] getRealmPassword(Realm realm, EmailAddress emailAddress) {
      return backEnd.getRealmPassword(realm, emailAddress);
   }

   public void updateThroughConnection(CBCExecutor cbcExecutor) throws PersistException{
      backEnd.updateThroughConnection(cbcExecutor);
   }

   void backendConfiguration(Element element) {

      Element backendElement = null;
      if (!cm.isFixed()) {

         NodeList backendNodeList = element.getElementsByTagName("File");
         if (backendNodeList == null || backendNodeList.getLength() == 0) {
            backendNodeList = element.getElementsByTagName("Db");
         }
         if (backendNodeList == null || backendNodeList.getLength() == 0) {
            backendNodeList = element.getElementsByTagName("LDAP");
         }
         backendElement = (Element) backendNodeList.item(0);
         backEndType = BackEndTypeEnum.getBackEndTypeEnum(backendElement.getNodeName().toUpperCase(cm.englishLocale));
         
         backendSecure = Boolean.parseBoolean(element.getAttribute("secure"));

         if (!backendSecure&&
               Boolean.parseBoolean(((Element) element.getOwnerDocument().getElementsByTagName(ConfigurationParameterConstants.CBC).item(0)).getAttribute("enable"))&&
               Boolean.parseBoolean(((Element) element.getOwnerDocument().getElementsByTagName(ConfigurationParameterConstants.CBC).item(0)).getAttribute("secure"))) {
            cm.registerConfigDeviations("backendSecure", "true");
            backendSecure = true;
         }
         backendMinimum = Integer.valueOf(element.getAttribute("minimum"));
         backendMaximum = Integer.valueOf(element.getAttribute("maximum"));

         if (!Mail.getInstance().isRestarting()) {
            try {
               JESVaultControl.initialize();
               jesVaultControl = JESVaultControl.getInstance();
               jesVaultControl.loadPasswords();
            }
            catch (java.security.GeneralSecurityException e) {
               log.error("Could not initialize the Vault's Cipher mechanism.", e);
               throw new RuntimeException("Could not initialize the Vault's Cipher mechanism.");
            }
         }
         else {
            jesVaultControl = JESVaultControl.getInstance();
         }

         switch (backEndType) {
            case FILE: {
               backEnd = new ConfigurationManagerBackEndFile(cm);
            }
            break;
            
            case LDAP: {
               throw new RuntimeException("The LDAP backend has not yet been implemented.");
            }
            default: {
               throw new AssertionError();
            }
         }
      }
      
      switch(backEndType) {
         case FILE: {
         
            if (backendElement == null) {

               backendElement = (Element)element.getElementsByTagName("File").item(0);
            }
         };break;
         case RDBM: {
         
            if (backendElement == null) {

               backendElement = (Element)element.getElementsByTagName("Db").item(0);
            }
         }break;
         case LDAP: {
            throw new RuntimeException("The LDAP backend has not yet been implemented.");
         }
         default: {
            throw new AssertionError();
         }
      }
      backEnd.init(backendElement);
      
      if (!Mail.getInstance().isRestarting()) {
         PasswordFactory.instantiate(backEndType);
      }
   }
}
