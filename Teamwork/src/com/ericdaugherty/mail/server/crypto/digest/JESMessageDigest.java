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
import java.security.*;

//Local inmports
import com.ericdaugherty.mail.server.crypto.JESProvider;

/**
 *
 * @author Andreas
 */
public abstract class JESMessageDigest extends MessageDigest {

   private JESMessageDigest(String algorithm) {
      super(algorithm);
   }

   public static MessageDigest getInstance(String algorithm)
         throws NoSuchAlgorithmException {

      JESMessageDigest digest = new DefaultDigest((DigestBase) JESProvider.getInstance().
            getService("MessageDigest",algorithm).newInstance(null));
      return digest;
   }

   public static MessageDigest getInstance(String algorithm, String provider)
         throws NoSuchAlgorithmException, NoSuchProviderException {
      throw new java.security.ProviderException("No runtime provider accepted.");

   }

   public static MessageDigest getInstance(String algorithm, Provider provider)
         throws NoSuchAlgorithmException {
      throw new java.security.ProviderException("No runtime provider accepted.");

   }
   
   abstract void secondaryInit(byte[] buffer);
   
   abstract byte[] getBuffer();
   
   public abstract int getByteLength();
   
   public abstract int getTruncationLength();

   static final class DefaultDigest extends JESMessageDigest {

      DigestBase digestBase;

      public DefaultDigest(DigestBase digestBase) {
         super(digestBase.getAlgorithmName());
         this.digestBase = digestBase;
      }

      protected int engineGetDigestLength() {
         return digestBase.engineGetDigestLength();
      }

      protected void engineUpdate(byte input) {
         digestBase.engineUpdate(input);
      }

      protected void engineUpdate(byte[] input, int offset, int len) {
         digestBase.engineUpdate(input, offset, len);
      }

      protected void engineUpdate(ByteBuffer input) {
         digestBase.engineUpdate(input);
      }

      protected byte[] engineDigest() {
         return digestBase.engineDigest();
      }

      protected int engineDigest(byte[] buf, int offset, int len)
            throws DigestException {
         return digestBase.engineDigest(buf, offset, len);
      }

      protected void engineReset() {
         digestBase.engineReset();
      }
      
      final void secondaryInit(byte[] buffer) {
         digestBase.secondaryInit(buffer);
      }
      
      final byte[] getBuffer() {
         return digestBase.getBuffer();
      }
      
      public int getByteLength() {
         return ((ExtendedDigest)digestBase).getByteLength();
      }
      
      public int getTruncationLength() {
         return digestBase.getTruncationLength();
      }
   }
}
