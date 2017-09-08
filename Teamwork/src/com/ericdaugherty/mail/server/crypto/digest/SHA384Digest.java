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
 * FIPS 180-2 implementation of SHA-384.
 *
 * <pre>
 *         block  word  digest
 * SHA-1   512    32    160
 * SHA-256 512    32    256
 * SHA-384 1024   64    384
 * SHA-512 1024   64    512
 * </pre>
 */
public final class SHA384Digest
    extends LongDigest
{

    /**
     * Standard constructor
     */
    public SHA384Digest()
    {
       super(48,24);
    }
    
    void secondaryInit(byte[] sha384Buffer)
    {
        super.reset(BYTE_LENGTH);
        
        H1 = ByteArrayToLong(sha384Buffer,0);
        H2 = ByteArrayToLong(sha384Buffer,8);
        H3 = ByteArrayToLong(sha384Buffer,16);
        H4 = ByteArrayToLong(sha384Buffer,24);
        H5 = ByteArrayToLong(sha384Buffer,32);
        H6 = ByteArrayToLong(sha384Buffer,40);
        H7 = ByteArrayToLong(sha384Buffer,48);
        H8 = ByteArrayToLong(sha384Buffer,56);
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
    public SHA384Digest(SHA384Digest t)
    {
        super(t);
    }

    public String getAlgorithmName()
    {
        return "SHA-384";
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

        reset();

        return engineGetDigestLength();
    }

    /**
     * reset the chaining variables
     */
    public void reset()
    {
        super.reset();

        /* SHA-384 initial hash value
         * The first 64 bits of the fractional parts of the square roots
         * of the 9th through 16th prime numbers
         */
        H1 = 0xcbbb9d5dc1059ed8l;
        H2 = 0x629a292a367cd507l;
        H3 = 0x9159015a3070dd17l;
        H4 = 0x152fecd8f70e5939l;
        H5 = 0x67332667ffc00b31l;
        H6 = 0x8eb44a8768581511l;
        H7 = 0xdb0c2e0d64f98fa7l;
        H8 = 0x47b5481dbefa4fa4l;
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
