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
import java.security.spec.AlgorithmParameterSpec;

/**
 *
 * @author Andreas Kyrmegalos
 */
public final class HMACParameterSpec implements AlgorithmParameterSpec {

    private final boolean initOnly;
    private final byte[] initialBuffer;
    private final boolean truncated;

    public HMACParameterSpec() {
        this(false, null, false);
    }

    public HMACParameterSpec(boolean initOnly, byte[] initialBuffer, boolean truncated) {
        this.initOnly = initOnly;
        if (initialBuffer!=null) {
           this.initialBuffer = initialBuffer.clone();
        }
        else {
           this.initialBuffer = null;
        }
        this.truncated = truncated;
        if (initOnly && (initialBuffer != null || truncated)) {
            throw new java.security.InvalidParameterException("If the HMAC algorith"
                    + " is only meant to be initialized the initial buffer has to be "
                    + "null and no output is to be generated.");
        }
    }

    public HMACParameterSpec(byte[] initialBuffer, boolean truncated) {
        this(false, initialBuffer, truncated);
    }

    public HMACParameterSpec(boolean truncated) {
        this(false, null, truncated);
    }

    public HMACParameterSpec(byte[] initialBuffer) {
        this(false, initialBuffer, false);
    }

    public byte[] getInitialBuffer() {
        if (initialBuffer==null) return null;
        byte[] copy = new byte[initialBuffer.length];
        System.arraycopy(initialBuffer, 0, copy, 0, initialBuffer.length);
        return copy;
    }

    public boolean isInitOnly() {
        return initOnly;
    }

    public boolean isTruncated() {
        return truncated;
    }
}
