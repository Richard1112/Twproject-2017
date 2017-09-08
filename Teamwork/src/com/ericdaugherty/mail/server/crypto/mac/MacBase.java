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

package com.ericdaugherty.mail.server.crypto.mac;

//Java imports
import java.nio.ByteBuffer;
import java.security.*;
import java.security.spec.AlgorithmParameterSpec;
import java.util.Arrays;
import javax.crypto.MacSpi;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

//Local imports
import com.ericdaugherty.mail.server.crypto.digest.JESMessageDigest;
import com.ericdaugherty.mail.server.crypto.digest.MacSupport;

/**
 *
 * @author Andreas Kyrmegalos
 */
abstract class MacBase extends MacSpi {

   private final JESMessageDigest messageDigest;
   private final String algorithm;
   private final MacSupport macSupport;
   private byte[] inner;
   private byte[] outer;
   private byte[] initOnlyBuffer;
   private boolean initOnly = false;
   private boolean preInit = false;
   private boolean firstPass = true;
   private boolean truncated = false;
   private int truncatedLength;

   public MacBase(String algorithm) throws NoSuchAlgorithmException {

      if (algorithm.contains("SHA") && algorithm.indexOf('-') == -1) {
         StringBuilder sb = new StringBuilder(algorithm.length() + 1);
         sb.append(algorithm.substring(0, algorithm.indexOf("SHA") + 3));
         sb.append('-');
         sb.append(algorithm.substring(algorithm.indexOf("SHA") + 3));
         algorithm = sb.toString();
      }
      this.algorithm = algorithm;
      this.messageDigest = (JESMessageDigest) JESMessageDigest.getInstance(algorithm.substring(algorithm.indexOf("Hmac") + 4));
      this.macSupport = new MacSupport(messageDigest);
   }
   
   protected int engineGetTruncationLength() {
      return truncatedLength;
   }

   protected int engineGetMacLength() {
      return messageDigest.getDigestLength();
   }
   
   protected void engineInit(AlgorithmParameterSpec aps) throws InvalidKeyException, InvalidAlgorithmParameterException {
      engineInit(new SecretKeySpec(new byte[1],algorithm), aps);
   }

   protected void engineInit(Key key, AlgorithmParameterSpec aps) throws InvalidKeyException, InvalidAlgorithmParameterException {

      initOnly = false;
      preInit = false;
      truncated = false;
      truncatedLength = 0;
      
      if (aps != null && !aps.getClass().equals(HMACParameterSpec.class)) {
         throw new InvalidAlgorithmParameterException("AlgorithmParameterSpec must be an instance of HMACParameterSpec");
      }

      HMACParameterSpec spec = (HMACParameterSpec) aps;
      if (spec != null && spec.getInitialBuffer() != null) {
         preInit = true;
      }

      //This is actually a partially computed HMAC
      //The key is not required since it has already
      //been hashed
      if (preInit) {

         byte[] stored = spec.getInitialBuffer();
         int length = stored.length / 2;
         inner = new byte[length];
         outer = new byte[length];
         System.arraycopy(stored, 0, inner, 0, length);
         System.arraycopy(stored, length, outer, 0, length);

         macSupport.secondaryInit(inner);
         firstPass = false;
      } else {

         if (key == null && !preInit) {
            throw new InvalidKeyException("Secret key required");
         }
         if (!(key instanceof SecretKey)) {
            throw new InvalidKeyException("Secret key expected");
         }

         byte[] secret = key.getEncoded();

         if (secret == null || secret.length == 0) {
            throw new InvalidKeyException("No secret key found");
         }

         int byteLength = messageDigest.getByteLength();
         // if key is longer than the block length, reset it using
         // the message digest object.
         if (secret.length > byteLength) {
            byte[] temp = messageDigest.digest(secret);
            // now erase the secret
            Arrays.fill(secret, (byte) 0);
            secret = temp;
         }

         inner = new byte[byteLength];
         outer = new byte[byteLength];
         
         // XOR k with ipad and opad, respectively
         for (int i = 0; i < secret.length; i++) {
            inner[i] = (byte) (secret[i] ^ 0x36);
            outer[i] = (byte) (secret[i] ^ 0x5c);
         }
         for (int i = secret.length; i < byteLength; i++) {
            inner[i] = (byte) (0 ^ 0x36);
            outer[i] = (byte) (0 ^ 0x5c);
         }

         // now erase the secret
         Arrays.fill(secret, (byte) 0);
         secret = null;
         
         if (spec.isInitOnly()) {
            initOnly = true;
            int digestLength = messageDigest.getDigestLength();
            initOnlyBuffer = new byte[digestLength*2];
            messageDigest.update(inner);
            System.arraycopy(macSupport.getBuffer(),0,initOnlyBuffer,0,digestLength);
            messageDigest.reset();
            messageDigest.update(outer);
            System.arraycopy(macSupport.getBuffer(),0,initOnlyBuffer,digestLength,digestLength);
            messageDigest.reset();
         }
      }
      truncated = spec.isTruncated();
      if (truncated) {
         truncatedLength = messageDigest.getTruncationLength();
      }
   }

   protected void engineUpdate(byte input) {

      if (initOnly) {
         throw new java.lang.IllegalStateException("No HMAC operations permitted"
               + " when the engine has been started with the initOnly argument.");
      }
      if (firstPass) {
         messageDigest.update(inner);
         firstPass = false;
      }
      messageDigest.update(input);
   }

   protected void engineUpdate(byte[] input, int offset, int len) {

      if (initOnly) {
         throw new java.lang.IllegalStateException("No HMAC operations permitted"
               + " when the engine has been started with the initOnly argument.");
      }
      if (firstPass) {
         messageDigest.update(inner);
         firstPass = false;
      }
      messageDigest.update(input, offset, len);
   }

   protected void engineUpdate(ByteBuffer input) {
      throw new java.lang.UnsupportedOperationException("The current JES MAC "
            + "implementation does not support use of a ByteBuffer");
   }

   protected byte[] engineDoFinal() {

      if (initOnly) {
         throw new java.lang.IllegalStateException("No HMAC operations permitted"
               + " when the engine has been started with the initOnly argument.");
      }
      if (firstPass) {
         messageDigest.update(inner);
      }
      firstPass = !firstPass;

      byte[] digest = messageDigest.digest();
      if (preInit) {

         preInit = false;
         macSupport.secondaryInit(outer);
      } else {

         messageDigest.update(outer);
      }
      messageDigest.update(digest);
      try {
         messageDigest.digest(digest, 0, digest.length);
      } catch (DigestException ex) {
      }
      return !truncated?digest:getTruncatedDigest(digest);
   }
   
   private byte[] getTruncatedDigest(byte[] digest) {
      
      byte[] truncatedDigest = new byte[truncatedLength];
      System.arraycopy(digest, 0, truncatedDigest, 0, truncatedLength);
      return truncatedDigest;
   }

   protected void engineReset() {
      if (!preInit && !firstPass) {
         firstPass = true;
         messageDigest.reset();
      }
   }
   
   protected byte[] engineGetInitOnlyBuffer() {
      
      return initOnlyBuffer;
   }

   public static final class HmacMD5 extends MacBase {

      public HmacMD5(String algorithm) throws NoSuchAlgorithmException {
         super(algorithm);
      }
   }

   public static final class HmacSHA1 extends MacBase {

      public HmacSHA1(String algorithm) throws NoSuchAlgorithmException {
         super(algorithm);
      }
   }

   public static final class HmacSHA256 extends MacBase {

      public HmacSHA256(String algorithm) throws NoSuchAlgorithmException {
         super(algorithm);
      }
   }

   public static final class HmacSHA384 extends MacBase {

      public HmacSHA384(String algorithm) throws NoSuchAlgorithmException {
         super(algorithm);
      }
   }

   public static final class HmacSHA512 extends MacBase {

      public HmacSHA512(String algorithm) throws NoSuchAlgorithmException {
         super(algorithm);
      }
   }
}
