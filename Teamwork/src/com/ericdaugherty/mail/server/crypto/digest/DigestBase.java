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

package com.ericdaugherty.mail.server.crypto.digest;

//Java imports
import java.nio.ByteBuffer;
import java.security.DigestException;
import java.security.MessageDigestSpi;

/**
 *
 * @author Andreas
 */
abstract class DigestBase extends MessageDigestSpi implements Digest{

   protected final int DIGEST_LENGTH;
   protected final int TRUNCATION_LENGTH;

   public DigestBase(int digestLength, int truncationLength) {
      DIGEST_LENGTH = digestLength;
      TRUNCATION_LENGTH = truncationLength;
   }
   
   abstract void secondaryInit(byte[] buffer);
   
   abstract byte[] getBuffer();

   abstract public void update(byte in);

   abstract public void update(byte[] in, int inOff, int len);
   
   abstract public void reset();
   
   public int getTruncationLength() {
      return TRUNCATION_LENGTH;
   }

   //Methods from MessageDigestSpi
   
   protected int engineGetDigestLength() {
      return DIGEST_LENGTH;
   }

   protected void engineUpdate(byte input) {
      update(input);
   }

   protected void engineUpdate(byte[] input, int offset, int len) {
      update(input, offset, len);
   }

   protected void engineUpdate(ByteBuffer input) {
      throw new java.lang.UnsupportedOperationException("The current JES Digest "
            + "implementation does not support hashing contents in a ByteBuffer");
   }

   protected byte[] engineDigest() {

      byte[] b = new byte[DIGEST_LENGTH];
      doFinal(b, 0);
      return b;
   }

   protected int engineDigest(byte[] buf, int offset, int len)
         throws DigestException {
      
      if (DIGEST_LENGTH > len || DIGEST_LENGTH > buf.length - offset) {
         throw new DigestException("The digest length exceeds the allotted buffer space");
      }
      System.arraycopy(engineDigest(), 0, buf, offset, DIGEST_LENGTH);
      return DIGEST_LENGTH;

   }

   protected void engineReset() {
      reset();
   }
}
