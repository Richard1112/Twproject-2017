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

/******************************************************************************
 * This codepiece used under the provision of the "Legion of the Bouncy Castle"
 * license terms.
 * http://www.bouncycastle.org/
 * 
 ******************************************************************************/
package com.ericdaugherty.mail.server.crypto.digest;
/**
 * FIPS 180-2 implementation of SHA-512.
 *
 * <pre>
 *         block  word  digest
 * SHA-1   512    32    160
 * SHA-256 512    32    256
 * SHA-384 1024   64    384
 * SHA-512 1024   64    512
 * </pre>
 */
public final class SHA512Digest
    extends LongDigest
{
   
    /**
     * Standard constructor
     */
    public SHA512Digest()
    {
       super(64,32);
    }
    
    void secondaryInit(byte[] sha512Buffer)
    {
        super.reset(BYTE_LENGTH);
        
        H1 = ByteArrayToLong(sha512Buffer,0);
        H2 = ByteArrayToLong(sha512Buffer,8);
        H3 = ByteArrayToLong(sha512Buffer,16);
        H4 = ByteArrayToLong(sha512Buffer,24);
        H5 = ByteArrayToLong(sha512Buffer,32);
        H6 = ByteArrayToLong(sha512Buffer,40);
        H7 = ByteArrayToLong(sha512Buffer,48);
        H8 = ByteArrayToLong(sha512Buffer,56);
    }
    
    final protected long ByteArrayToLong(byte[] array, int offset) {
       
       long value = 0;
         for (int i = 0; i < 8; i++)
         {
            value= (value << 8) + (array[offset+i] & 0xff);
         }
         return value;
    }

    /**
     * Copy constructor.  This will copy the state of the provided
     * message digest.
     */
    public SHA512Digest(SHA512Digest t)
    {
        super(t);
    }

    public String getAlgorithmName()
    {
        return "SHA-512";
    }

    public int getDigestSize()
    {
        return engineGetDigestLength();
    }

    public int doFinal(
        byte[]  out,
        int     outOff)
    {
        finish();

        Pack.longToBigEndian(H1, out, outOff);
        Pack.longToBigEndian(H2, out, outOff + 8);
        Pack.longToBigEndian(H3, out, outOff + 16);
        Pack.longToBigEndian(H4, out, outOff + 24);
        Pack.longToBigEndian(H5, out, outOff + 32);
        Pack.longToBigEndian(H6, out, outOff + 40);
        Pack.longToBigEndian(H7, out, outOff + 48);
        Pack.longToBigEndian(H8, out, outOff + 56);

        reset();

        return engineGetDigestLength();
    }

    /**
     * reset the chaining variables
     */
    public void reset()
    {
        super.reset();

        /* SHA-512 initial hash value
         * The first 64 bits of the fractional parts of the square roots
         * of the first eight prime numbers
         */
        H1 = 0x6a09e667f3bcc908L;
        H2 = 0xbb67ae8584caa73bL;
        H3 = 0x3c6ef372fe94f82bL;
        H4 = 0xa54ff53a5f1d36f1L;
        H5 = 0x510e527fade682d1L;
        H6 = 0x9b05688c2b3e6c1fL;
        H7 = 0x1f83d9abfb41bd6bL;
        H8 = 0x5be0cd19137e2179L;
    }
    
    byte[] getBuffer() {

       byte[] out = new byte[64];
       Pack.longToBigEndian(H1, out, 0);
       Pack.longToBigEndian(H2, out, 8);
       Pack.longToBigEndian(H3, out, 16);
       Pack.longToBigEndian(H4, out, 24);
       Pack.longToBigEndian(H5, out, 32);
       Pack.longToBigEndian(H6, out, 40);
       Pack.longToBigEndian(H7, out, 48);
       Pack.longToBigEndian(H8, out, 56);
       return out;
    }
}

