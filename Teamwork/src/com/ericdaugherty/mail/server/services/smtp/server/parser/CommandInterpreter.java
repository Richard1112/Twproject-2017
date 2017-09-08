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

package com.ericdaugherty.mail.server.services.smtp.server.parser;

//Local imports
import com.ericdaugherty.mail.server.errors.SMTPReplyException;
import com.ericdaugherty.mail.server.services.smtp.server.COMMAND_VERB;

/**
 * This class will drive the action forward
 *
 * @author Andreas Kyrmegalos
 */
public interface CommandInterpreter {

   public void validateCommand(String literal, String line) throws SMTPReplyException;

   public String handleArgument() throws SMTPReplyException;

   public String[] handleParameters() throws SMTPReplyException;

   public void reset();
   
   public static final String POSTMASTER = "postmaster";
   public static final String WSP = "(\\ |\\t)";
   public static final String ctext = "[A-Za-z0-9!\"#$%&'*+,\\-./:;<=>?@\\[\\]\\\\^_`{|}~]";
   public static final String atext = "[A-Za-z0-9!#$%&'*+\\-/=?\\\\^_`{|}~]";
   public static final String VCHAR = "[ A-Za-z0-9!\"#$%&'()*+,\\-./:;<=>?@\\[\\\\]\\\\^_`{|}~]";
   public static final String letDigRegex = "[A-Za-z0-9]";
   public static final String ldhStrRegex = "[A-Za-z0-9\\-]*" + letDigRegex;
   public static final String subDomainRegex = letDigRegex + "(" + ldhStrRegex + ")?";
   public static final String domainRegex = subDomainRegex + "(\\." + subDomainRegex + ")*";
   public static final String atDomainRegex = "@" + domainRegex;
   public static final String adlRegex = atDomainRegex + "(," + atDomainRegex + ")*";
   //TODO the literals
   public static final String addressLiteralRegex = "\\[.+\\]";
   public static final String atomRegex = atext + "+";
   public static final String dotStringRegex = atomRegex + "(\\." + atomRegex + ")*";
   public static final String qtextSMTPRegex = "[ A-Za-z0-9!#$%&'()*+,\\-./:;<=>?@\\[\\]\\\\^_`{|}~]";
   public static final String quotedPairSMTPRegex = "\\\\[ A-Za-z0-9!\"#$%&'()*+,\\-./:;<=>?@\\[\\\\\\]\\\\^_`{|}~]";
   public static final String qcontentSMTPRegex = "(" + quotedPairSMTPRegex + "|" + qtextSMTPRegex + ")";
   public static final String quotedStringRegex = "\"" + qcontentSMTPRegex + "*\"";
   public static final String localPartRegex = "(" + dotStringRegex + "|" + quotedStringRegex + ")";
   public static final String mailboxRegex = localPartRegex + "@" + "(" + domainRegex + "|" + addressLiteralRegex + ")";
   public static final String pathRegex = "<(" + adlRegex + ":)?" + mailboxRegex + ">";
   public static final String forwardPathRegex = "(<(?i)" + POSTMASTER + "(?-i)@" + domainRegex + ">|<(?i)" + POSTMASTER + "(?-i)>|" + pathRegex + ")";
   public static final String reversePathRegex = "(" + pathRegex + "|(<>))";
   public static final String esmtpvalueRegex = "[A-Za-z0-9!\"#$%&'()*+,\\-./:;<>?@\\[\\\\\\]\\\\^_`{|}~]+";
   public static final String esmtpKeywordRegex = "[A-Za-z0-9][A-Za-z0-9\\-]*";
   public static final String esmtpParamRegex = esmtpKeywordRegex + "(=" + esmtpvalueRegex + ")?";
   public static final String mailParametersRegex = esmtpParamRegex;
   public static final String rcptParametersRegex = mailParametersRegex;
   public static final String stringRegex = "(" + atomRegex + "|" + quotedStringRegex + ")";
   public static final String saslMechRegex = "[A-Z0-9\\-_]{1,20}";
   public static final String base64CharRegex = "[A-Za-z0-9+/]";
   public static final String base64TerminalRegex = "(" + base64CharRegex + "{2}==|" + base64CharRegex + "{3}=)";
   public static final String base64Regex = "(" + base64TerminalRegex + "|" + base64CharRegex + "{4,}(==|=)?)";
   
   public static final String EHLORegex = "(?i)" + COMMAND_VERB.EHLO.getLiteral() + " (?-i)(" + domainRegex + "|" + addressLiteralRegex + "(" + ctext + "|\\s)?" + ctext + "*)";
   public static final String HELORegex = "(?i)" + COMMAND_VERB.HELO.getLiteral() + " (?-i)" + domainRegex;
   public static final String MAILRegex = "(?i)" + COMMAND_VERB.MAIL.getLiteral() + "(?-i)" + reversePathRegex + "( " + mailParametersRegex + ")*";
   public static final String RCPTRegex = "(?i)" + COMMAND_VERB.RCPT.getLiteral() + "(?-i)" + forwardPathRegex + "( " + rcptParametersRegex + ")*";
   public static final String DATARegex = "(?i)" + COMMAND_VERB.DATA.getLiteral() + "(?-i)";
   public static final String RSETRegex = "(?i)" + COMMAND_VERB.RSET.getLiteral() + "(?-i)";
   public static final String VRFYRegex = "(?i)" + COMMAND_VERB.VRFY.getLiteral() + " (?-i)" + stringRegex;
   public static final String EXPNRegex = "(?i)" + COMMAND_VERB.EXPN.getLiteral() + " (?-i)" + stringRegex;
   public static final String NOOPRegex = "(?i)" + COMMAND_VERB.NOOP.getLiteral() + "(?-i)( " + stringRegex + ")*";
   public static final String QUITRegex = "(?i)" + COMMAND_VERB.QUIT.getLiteral() + "(?-i)";
   public static final String STLSRegex = "(?i)" + COMMAND_VERB.STLS.getLiteral() + "(?-i)";
   public static final String AUTHRegex = "(?i)" + COMMAND_VERB.AUTH.getLiteral() + " (?-i)" + saslMechRegex + "( " + base64Regex + "| =)?";
}
