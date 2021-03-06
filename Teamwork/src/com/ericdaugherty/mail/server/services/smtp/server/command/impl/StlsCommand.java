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

package com.ericdaugherty.mail.server.services.smtp.server.command.impl;

//Local imports
import java.io.IOException;

//Logging imports
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

//Local imports
import com.ericdaugherty.mail.server.errors.*;
import com.ericdaugherty.mail.server.services.smtp.server.COMMAND_VERB;
import com.ericdaugherty.mail.server.services.smtp.server.SMTPServerSessionControl;
import com.ericdaugherty.mail.server.services.smtp.server.action.*;
import com.ericdaugherty.mail.server.services.smtp.server.command.AbstractCommand;
import com.ericdaugherty.mail.server.services.smtp.server.parser.impl.StlsInterpreter;

/**
 *
 * @author Andreas Kyrmegalos
 */
public class StlsCommand extends AbstractCommand {

   /**
    * Logger Category for this class.
    */
  // private static final Log log = LogFactory.getLog(StlsCommand.class);
  private static Log log = LogFactory.getLog("JESLogger");
   
   public StlsCommand() {
      super(COMMAND_VERB.STLS, null, new StlsInterpreter());
      reset();
   }
   
   public void reset() {
      super.reset();
      setPreCommandAction(new StlsPreCommandAction());
      setCommandAction(new StlsCommandAction());
      setPostCommandAction(new StlsPostCommandAction());
   }
   
   class StlsPreCommandAction extends PreCommandAction {
      
      public void execute(SMTPServerSessionControl control) throws SMTPReplyException {
         
         log.info("Client "+control.getClientDomain()+"["+control.getClientIP()+"] requested a security layer.");
         control.startTLSHandshake();
      }
   }

   class StlsCommandAction extends CommandAction {

      public void execute(SMTPServerSessionControl control) throws SMTPReplyException, TooManyErrorsException, IOException {
         
         control.setReplyLast("220 Ready to start TLS");
      }
   }

   class StlsPostCommandAction extends PostCommandAction {
      
      public void execute(SMTPServerSessionControl control) throws SMTPReplyException {
         
         control.concludeTLSHandshake();
      }
   }
}