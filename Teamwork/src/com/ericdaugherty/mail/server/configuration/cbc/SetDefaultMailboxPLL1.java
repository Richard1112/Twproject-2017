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
 * A single mailBox to be set as the default for a given domain
 * 
 * @author Andreas Kyrmegalos
 */
public final class SetDefaultMailboxPLL1 extends CBCExecutor {

   private int domainId;
   private int userId;

   public SetDefaultMailboxPLL1(ListIterator<String> iter) {
      super(iter);
   }

   public void processLines() {
      String line;
      for (; iter.hasNext();) {
         line = iter.next();
         if (line.startsWith(DOMAIN_ID)) {
            line = line.substring(DOMAIN_ID.length()).trim();
            domainId = Integer.valueOf(line).intValue();
         } else if (line.startsWith(USER_ID)) {
            line = line.substring(USER_ID.length()).trim();
            userId = Integer.valueOf(line).intValue();
         }
      }
   }

   public void execute(PersistExecutor pe) throws PersistException {

      pe.setDefaultMailBox(domainId, userId);
   }
}
