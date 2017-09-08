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
 * $Rev: 292 $
 * $Date: 2013-03-01 05:55:36 +0100 (Fr, 01 Mrz 2013) $
 *
 ******************************************************************************/

package com.ericdaugherty.mail.server.configuration;

/**
 *
 * @author Andreas Kyrmegalos
 */
public interface ConnectionBasedConfiguratorConstants {

   //File backend only
   public static final String COMMAND_ADD_USER = "add user";
   
   //Db backend only
   public static final String COMMAND_INSERT_DOMAIN = "insertDomain";
   public static final String COMMAND_DELETE_DOMAIN = "deleteDomain";
   public static final String COMMAND_SET_DEFAULT_DOMAIN = "setDefaultDomain";
   public static final String COMMAND_INSERT_USER = "insertUser";
   public static final String COMMAND_DELETE_USER = "deleteUser";
   public static final String COMMAND_SET_USER_PASSWORD = "setUserPassword";
   public static final String COMMAND_ADD_FORWARD_ADDRESS = "addForwardAddress";
   public static final String COMMAND_REMOVE_FORWARD_ADDRESS = "removeForwardAddress";
   public static final String COMMAND_SET_DEFAULT_MAILBOX = "setDefaultMailBox";
   public static final String COMMAND_INSERT_REALM = "insertRealm";
   public static final String COMMAND_DELETE_REALM = "deleteRealm";
   public static final String COMMAND_ADD_USER_TO_REALM = "addUserToRealm";
   public static final String COMMAND_REMOVE_USER_FROM_REALM = "removeUserFromRealm";
   public static final String COMMAND_RETRIEVE_DB_PASSWORD = "retrieveDbPassword";
   public static final String COMMAND_RETRIEVE_CONFIG = "retrieveConfig";
   public static final String COMMAND_APPLY_CONFIG = "applyConfig";

   public static final String DOMAIN = "domain:";
   public static final String DOMAIN_ID = "domainId:";
   public static final String USER_ID = "userId:";
   public static final String USERNAME = "username:";
   public static final String PASSWORD = "password:";
   public static final String REALM = "realm:";
   public static final String FORWARD_ADDRESS = "forwardAddress:";
   public static final String FORWARD_ADDRESS_ID = "forwardAddressId:";
   public static final String REALM_ID = "realmId:";
   public static final String DB_PASSWORD = "DbPassword:";
   public static final String CONFIG_GENERAL = "ConfigGeneral";
   public static final String CONFIG_BACKEND = "ConfigBackend";
   public static final String CONFIG_MAIL = "ConfigMail";
   public static final String CONFIG_DIR = "ConfigDirectories";
   public static final String CONFIG_AMAVIS = "ConfigAmavis-dnew";
   public static final String CONFIG_OTHER = "ConfigOther";
}
