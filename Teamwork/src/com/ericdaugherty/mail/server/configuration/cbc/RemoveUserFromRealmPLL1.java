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

package com.ericdaugherty.mail.server.configuration.cbc;

//Java imports
import java.util.*;

//Local imports
import com.ericdaugherty.mail.server.configuration.backEnd.PersistException;
import com.ericdaugherty.mail.server.configuration.backEnd.PersistExecutor;

/**
 * A single user to add to a number of realms
 * 
 * @author Andreas Kyrmegalos
 */
public final class RemoveUserFromRealmPLL1 extends CBCExecutor {

   private final List<NewUser> users = new ArrayList<NewUser>();

   public RemoveUserFromRealmPLL1(ListIterator<String> iter) {
      super(iter);
   }

   public void processLines() {
      String line;
      String[] entries;
      NewUser newUser;
      for (; iter.hasNext();) {
         line = iter.next();
         if (line.startsWith(USER_ID)) {
            line = line.substring(USER_ID.length()).trim();
            entries = line.split(",");
            for (String entry : entries) {
               newUser = new NewUser();
               newUser.userId = Integer.valueOf(entry);
               users.add(newUser);
            }
         } else if (line.startsWith(REALM_ID)) {
            line = line.substring(REALM_ID.length()).trim();
            entries = line.split(",");
            String[] subEntries;
            for (int i = 0; i < entries.length; i++) {
               subEntries = entries[i].split(":");
               newUser = users.get(i);
               newUser.realmIds = new ArrayList<Integer>();
               for (String entry1 : subEntries) {
                  newUser.realmIds.add(Integer.valueOf(entry1));
               }
            }
         }
      }
   }

   public void execute(PersistExecutor pe) throws PersistException {

      pe.removeUserFromRealm(users);
   }
}
