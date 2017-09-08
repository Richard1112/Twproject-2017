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

package com.ericdaugherty.mail.server.crypto;

//Java imports
import java.security.GeneralSecurityException;

//Local imports
import com.ericdaugherty.mail.server.crypto.mac.JESMac;

/**
 *
 * @author Andreas Kyrmegalos
 */
public class PBKDF2 {

   public void deriveKey(JESMac jesMac, byte[] salt, int c, byte[] dk) throws GeneralSecurityException {

      int dkLen = dk.length;
      if (dkLen == 0) {
         return;
      }
      int hLen = jesMac.getMacLength();

      int l = (int) Math.ceil(dkLen / (double) hLen);
      int r = dkLen - (l - 1) * hLen;


      byte[] U;
      byte[] T = l == 1 ? dk : new byte[l * hLen];

      byte[] text = new byte[salt.length + 4];
      System.arraycopy(salt, 0, text, 0, salt.length);

      int i, j;
      
      int tLen = (l==1&&dkLen<hLen)?dkLen:hLen;

      for (i = 0; i < l; i++) {

         be32enc(text, salt.length, i + 1);
         jesMac.update(text, 0, text.length);
         U = jesMac.doFinal();
         System.arraycopy(U, 0, T, i * hLen, tLen);

         for (j = 1; j < c; j++) {

            jesMac.update(U, 0, hLen);
            U = jesMac.doFinal();

            for (int k = 0; k < tLen; k++) {
               T[i * hLen + k] ^= U[k];
            }
         }
         if (T==dk) continue;
         System.arraycopy(T, i * hLen, dk, i * hLen, i==l-1?r:hLen);
      }
   }

   private void be32enc(byte[] B, int bOffset, int l) {

      B[bOffset + 0] = (byte) (l >> 24 & 0xff);
      B[bOffset + 1] = (byte) (l >> 16 & 0xff);
      B[bOffset + 2] = (byte) (l >> 8 & 0xff);
      B[bOffset + 3] = (byte) (l & 0xff);
   }
}
