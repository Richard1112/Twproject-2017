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
 * implementation of MD5 as outlined in "Handbook of Applied Cryptography", pages 346 - 347.
 */
public final class MD5Digest
    extends GeneralDigest
{
    
    private int     H1, H2, H3, H4;         // IV's

    private int[]   X = new int[16];
    private int     xOff;

    /**
     * Standard constructor
     */
    public MD5Digest()
    {
       super(16, 12);
        reset();
    }
    
    void secondaryInit (byte[] md5Buffer)
    {
        super.reset(BYTE_LENGTH);
        
        H1 = ByteArrayToInteger(md5Buffer,0);
        H2 = ByteArrayToInteger(md5Buffer,4);
        H3 = ByteArrayToInteger(md5Buffer,8);
        H4 = ByteArrayToInteger(md5Buffer,12);

    }
    
    final protected int ByteArrayToInteger(byte[] array, int offset) {
       
       int value = 0;
         for (int i = 0; i < 4; i++)
         {
            value += (array[offset+i] & 0xff) << (8 * i);
         }
         return value;
    }

    /**
     * Copy constructor.  This will copy the state of the provided
     * message digest.
     */
    public MD5Digest(MD5Digest t)
    {
        super(t);

        H1 = t.H1;
        H2 = t.H2;
        H3 = t.H3;
        H4 = t.H4;

        System.arraycopy(t.X, 0, X, 0, t.X.length);
        xOff = t.xOff;
    }

    public String getAlgorithmName()
    {
        return "MD5";
    }

    public int getDigestSize()
    {
        return engineGetDigestLength();
    }

    protected void processWord(
        byte[]  in,
        int     inOff)
    {
        X[xOff++] = (in[inOff] & 0xff) | ((in[inOff + 1] & 0xff) << 8)
            | ((in[inOff + 2] & 0xff) << 16) | ((in[inOff + 3] & 0xff) << 24); 

        if (xOff == 16)
        {
            processBlock();
        }
    }

    protected void processLength(
        long    bitLength)
    {
        if (xOff > 14)
        {
            processBlock();
        }

        X[14] = (int)(bitLength & 0xffffffff);
        X[15] = (int)(bitLength >>> 32);
    }

    private void unpackWord(
        int     word,
        byte[]  out,
        int     outOff)
    {
        out[outOff]     = (byte)word;
        out[outOff + 1] = (byte)(word >>> 8);
        out[outOff + 2] = (byte)(word >>> 16);
        out[outOff + 3] = (byte)(word >>> 24);
    }

    public int doFinal(
        byte[]  out,
        int     outOff)
    {
        finish();

        unpackWord(H1, out, outOff);
        unpackWord(H2, out, outOff + 4);
        unpackWord(H3, out, outOff + 8);
        unpackWord(H4, out, outOff + 12);

        reset();

        return engineGetDigestLength();
    }

    /**
     * reset the chaining variables to the IV values.
     */
    public void reset()
    {
        super.reset();

        H1 = 0x67452301;
        H2 = 0xefcdab89;
        H3 = 0x98badcfe;
        H4 = 0x10325476;

        xOff = 0;

        for (int i = 0; i != X.length; i++)
        {
            X[i] = 0;
        }
    }
    
    byte[] getBuffer() {

       byte[] out = new byte[16];
       unpackWord(H1, out, 0);
       unpackWord(H2, out, 4);
       unpackWord(H3, out, 8);
       unpackWord(H4, out, 12);
       return out;
    }

    //
    // round 1 left rotates
    //
    private static final int S11 = 7;
    private static final int S12 = 12;
    private static final int S13 = 17;
    private static final int S14 = 22;

    //
    // round 2 left rotates
    //
    private static final int S21 = 5;
    private static final int S22 = 9;
    private static final int S23 = 14;
    private static final int S24 = 20;

    //
    // round 3 left rotates
    //
    private static final int S31 = 4;
    private static final int S32 = 11;
    private static final int S33 = 16;
    private static final int S34 = 23;

    //
    // round 4 left rotates
    //
    private static final int S41 = 6;
    private static final int S42 = 10;
    private static final int S43 = 15;
    private static final int S44 = 21;

    /*
     * rotate int x left n bits.
     */
    private int rotateLeft(
        int x,
        int n)
    {
        return (x << n) | (x >>> (32 - n));
    }

    /*
     * F, G, H and I are the basic MD5 functions.
     */
    private int F(
        int u,
        int v,
        int w)
    {
        return w ^ (u & (v ^ w));//Colin Plumb's public domain optimized code
    }

    private int G(
        int u,
        int v,
        int w)
    {
        return (u & w) | (v & ~w);
    }

    private int H(
        int u,
        int v,
        int w)
    {
        return u ^ v ^ w;
    }

    private int K(
        int u,
        int v,
        int w)
    {
        return v ^ (u | ~w);
    }

    protected void processBlock()
    {
        int a = H1;
        int b = H2;
        int c = H3;
        int d = H4;

        //
        // Round 1 - F cycle, 16 times.
        //
        a = rotateLeft(a + F(b, c, d) + X[ 0] + 0xd76aa478, S11) + b;
        d = rotateLeft(d + F(a, b, c) + X[ 1] + 0xe8c7b756, S12) + a;
        c = rotateLeft(c + F(d, a, b) + X[ 2] + 0x242070db, S13) + d;
        b = rotateLeft(b + F(c, d, a) + X[ 3] + 0xc1bdceee, S14) + c;
        a = rotateLeft(a + F(b, c, d) + X[ 4] + 0xf57c0faf, S11) + b;
        d = rotateLeft(d + F(a, b, c) + X[ 5] + 0x4787c62a, S12) + a;
        c = rotateLeft(c + F(d, a, b) + X[ 6] + 0xa8304613, S13) + d;
        b = rotateLeft(b + F(c, d, a) + X[ 7] + 0xfd469501, S14) + c;
        a = rotateLeft(a + F(b, c, d) + X[ 8] + 0x698098d8, S11) + b;
        d = rotateLeft(d + F(a, b, c) + X[ 9] + 0x8b44f7af, S12) + a;
        c = rotateLeft(c + F(d, a, b) + X[10] + 0xffff5bb1, S13) + d;
        b = rotateLeft(b + F(c, d, a) + X[11] + 0x895cd7be, S14) + c;
        a = rotateLeft(a + F(b, c, d) + X[12] + 0x6b901122, S11) + b;
        d = rotateLeft(d + F(a, b, c) + X[13] + 0xfd987193, S12) + a;
        c = rotateLeft(c + F(d, a, b) + X[14] + 0xa679438e, S13) + d;
        b = rotateLeft(b + F(c, d, a) + X[15] + 0x49b40821, S14) + c;

        //
        // Round 2 - G cycle, 16 times.
        //
        a = rotateLeft(a + G(b, c, d) + X[ 1] + 0xf61e2562, S21) + b;
        d = rotateLeft(d + G(a, b, c) + X[ 6] + 0xc040b340, S22) + a;
        c = rotateLeft(c + G(d, a, b) + X[11] + 0x265e5a51, S23) + d;
        b = rotateLeft(b + G(c, d, a) + X[ 0] + 0xe9b6c7aa, S24) + c;
        a = rotateLeft(a + G(b, c, d) + X[ 5] + 0xd62f105d, S21) + b;
        d = rotateLeft(d + G(a, b, c) + X[10] + 0x02441453, S22) + a;
        c = rotateLeft(c + G(d, a, b) + X[15] + 0xd8a1e681, S23) + d;
        b = rotateLeft(b + G(c, d, a) + X[ 4] + 0xe7d3fbc8, S24) + c;
        a = rotateLeft(a + G(b, c, d) + X[ 9] + 0x21e1cde6, S21) + b;
        d = rotateLeft(d + G(a, b, c) + X[14] + 0xc33707d6, S22) + a;
        c = rotateLeft(c + G(d, a, b) + X[ 3] + 0xf4d50d87, S23) + d;
        b = rotateLeft(b + G(c, d, a) + X[ 8] + 0x455a14ed, S24) + c;
        a = rotateLeft(a + G(b, c, d) + X[13] + 0xa9e3e905, S21) + b;
        d = rotateLeft(d + G(a, b, c) + X[ 2] + 0xfcefa3f8, S22) + a;
        c = rotateLeft(c + G(d, a, b) + X[ 7] + 0x676f02d9, S23) + d;
        b = rotateLeft(b + G(c, d, a) + X[12] + 0x8d2a4c8a, S24) + c;

        //
        // Round 3 - H cycle, 16 times.
        //
        a = rotateLeft(a + H(b, c, d) + X[ 5] + 0xfffa3942, S31) + b;
        d = rotateLeft(d + H(a, b, c) + X[ 8] + 0x8771f681, S32) + a;
        c = rotateLeft(c + H(d, a, b) + X[11] + 0x6d9d6122, S33) + d;
        b = rotateLeft(b + H(c, d, a) + X[14] + 0xfde5380c, S34) + c;
        a = rotateLeft(a + H(b, c, d) + X[ 1] + 0xa4beea44, S31) + b;
        d = rotateLeft(d + H(a, b, c) + X[ 4] + 0x4bdecfa9, S32) + a;
        c = rotateLeft(c + H(d, a, b) + X[ 7] + 0xf6bb4b60, S33) + d;
        b = rotateLeft(b + H(c, d, a) + X[10] + 0xbebfbc70, S34) + c;
        a = rotateLeft(a + H(b, c, d) + X[13] + 0x289b7ec6, S31) + b;
        d = rotateLeft(d + H(a, b, c) + X[ 0] + 0xeaa127fa, S32) + a;
        c = rotateLeft(c + H(d, a, b) + X[ 3] + 0xd4ef3085, S33) + d;
        b = rotateLeft(b + H(c, d, a) + X[ 6] + 0x04881d05, S34) + c;
        a = rotateLeft(a + H(b, c, d) + X[ 9] + 0xd9d4d039, S31) + b;
        d = rotateLeft(d + H(a, b, c) + X[12] + 0xe6db99e5, S32) + a;
        c = rotateLeft(c + H(d, a, b) + X[15] + 0x1fa27cf8, S33) + d;
        b = rotateLeft(b + H(c, d, a) + X[ 2] + 0xc4ac5665, S34) + c;

        //
        // Round 4 - K cycle, 16 times.
        //
        a = rotateLeft(a + K(b, c, d) + X[ 0] + 0xf4292244, S41) + b;
        d = rotateLeft(d + K(a, b, c) + X[ 7] + 0x432aff97, S42) + a;
        c = rotateLeft(c + K(d, a, b) + X[14] + 0xab9423a7, S43) + d;
        b = rotateLeft(b + K(c, d, a) + X[ 5] + 0xfc93a039, S44) + c;
        a = rotateLeft(a + K(b, c, d) + X[12] + 0x655b59c3, S41) + b;
        d = rotateLeft(d + K(a, b, c) + X[ 3] + 0x8f0ccc92, S42) + a;
        c = rotateLeft(c + K(d, a, b) + X[10] + 0xffeff47d, S43) + d;
        b = rotateLeft(b + K(c, d, a) + X[ 1] + 0x85845dd1, S44) + c;
        a = rotateLeft(a + K(b, c, d) + X[ 8] + 0x6fa87e4f, S41) + b;
        d = rotateLeft(d + K(a, b, c) + X[15] + 0xfe2ce6e0, S42) + a;
        c = rotateLeft(c + K(d, a, b) + X[ 6] + 0xa3014314, S43) + d;
        b = rotateLeft(b + K(c, d, a) + X[13] + 0x4e0811a1, S44) + c;
        a = rotateLeft(a + K(b, c, d) + X[ 4] + 0xf7537e82, S41) + b;
        d = rotateLeft(d + K(a, b, c) + X[11] + 0xbd3af235, S42) + a;
        c = rotateLeft(c + K(d, a, b) + X[ 2] + 0x2ad7d2bb, S43) + d;
        b = rotateLeft(b + K(c, d, a) + X[ 9] + 0xeb86d391, S44) + c;

        H1 += a;
        H2 += b;
        H3 += c;
        H4 += d;

        //
        // reset the offset and clean out the word buffer.
        //
        xOff = 0;
        for (int i = 0; i != X.length; i++)
        {
            X[i] = 0;
        }
    }
}
