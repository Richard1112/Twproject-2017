/**
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 *
 * This source has been modified and redistributed under the terms
 * of the following license:
 * �
 * Cryptix General License
 * Copyright � 1995-2001 The Cryptix Foundation Limited. All rights reserved.
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * Redistributions of source code must retain the copyright notice,
 * this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials,
 * provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE CRYPTIX FOUNDATION LIMITED AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CRYPTIX FOUNDATION LIMITED OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package net.wimpi.pim.util;


/**
 * This class provides methods for encoding and decoding data in
 * MIME quoted-printable format, as described in RFC 2045 (Section 6.7).
 * <p>
 * QuotedPrintable is a Content-Transfer-Encoding. That is, the input to
 * encoding is a sequence of bytes, and the output is a sequence of
 * characters (similar to base64). It is orthogonal to the character
 * encoding.
 * <p>
 * For example, suppose a MIME message consists of Unicode text with a
 * Content-Type of "text/plain; charset=UTF8", and a
 * Content-Transfer-Encoding of "quoted-printable" (this would be unusual,
 * but valid). A MIME message should be viewed as a sequence of abstract
 * characters, which in this case could be decoded as bytes according
 * to the "quoted-printable" transfer encoding, and then back to a
 * (different, in general) sequence of characters according to the "UTF8"
 * character encoding.
 * <p>
 * The input to the <code>encode</code> methods is always a byte array.
 * Strictly speaking the output represents a sequence of characters, but
 * since these characters are from a subset of both the Unicode and ASCII
 * character repertoires, it is possible to express the output either as
 * a String or as a byte array.
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> RFC 2045, <cite>Multipurpose Internet Mail Extensions (MIME) Part One,
 *        Format of Internet Message Bodies</cite>,
 *        "Section 6.7 Quoted-Printable Content-Transfer-Encoding,"
 *        <a href="http://www.imc.org/rfc2045">http://www.imc.org/rfc2045</a>
 *        <p>
 *   <li> Dan Connolly, draft-ietf-html-charset-harmful-00.txt,
 *        <cite>"Character Set" Considered Harmful</cite>,
 *        <a href="http://www.w3.org/pub/WWW/MarkUp/html-spec/charset-harmful.html">
 *        http://www.w3.org/pub/WWW/MarkUp/html-spec/charset-harmful.html</a>
 * </ol>
 * <p>
 *
 * @author  Jill Baker
 * @version Distributed with 0.1 (22/07/2003)
 */
public class QuotedPrintable {

  private QuotedPrintable() {
  }//constructor

  /**
   * Encodes a byte array using quoted-printable encoding, and returns
   * the result as a String.
   * <p>
   * Line breaks in the input MUST be represented as "\r\n" - any other
   * combination is treated as binary data. Line breaks in the output
   * will always be represented as "\r\n".
   *
   * @param  b    canonical data to be encoded
   * @return output in quoted-printable form.
   */
  public static final String encode(String data) {

    byte[] b = null;
    try {
      b = data.getBytes("ISOLatin1");
    } catch (Exception ex) {
      ex.printStackTrace();
      b = data.getBytes();
    }

    int len = b.length;

    StringBuffer buffer = new StringBuffer(2 * len);
    StringBuffer line = new StringBuffer(240);
    StringBuffer white = new StringBuffer(80);

    for (int i = 0; i < len; i++) {
      char c = (char) (b[i]);

      if (c == '\r' && i != len - 1 && b[i + 1] == '\n') {
        encodeEndOfLine(buffer, line, white);
        i++;
        continue;
      } else if (c == ' ' || c == '\t') {
        white.append(c);
      } else {
        if (white.length() != 0) {
          line.append(white);
          white.setLength(0);
        }

        if (c >= '!' && c <= '~' && c != '=') {
          line.append(c);
        } else {
          appendHex(line, c);
        }
      }
    }
    if (line.length() != 0 || white.length() != 0) {
      encodeEndOfLine(buffer, line, white);
    }
    return buffer.toString();
  }//encode

  private static final void encodeEndOfLine(StringBuffer buffer, StringBuffer line,
                                            StringBuffer white) {
    int i,j;

    for (i = 0; i < white.length(); i++) {
      appendHex(line, white.charAt(i));
    }
    String s = line.toString();
    int len = s.length();
    boolean split = false;
    for (i = 0; i < len; i = j) {
      j = i + 75;
      if ((j == len - 1) || (j > len)) j = len;
      if (j > 2) {
        if (s.charAt(j - 1) == '=') j -= 1;
        if (s.charAt(j - 2) == '=') j -= 2;
      }
      buffer.append(s.substring(i, j));
      if (j != len) buffer.append('=');
      buffer.append("\r\n");
    }
    line.setLength(0);
    white.setLength(0);
  }//encodeEndOfLine

  private static final void appendHex(StringBuffer line, int c) {
    c &= 0xFF;
    line.append('=');
    line.append("0123456789ABCDEF".charAt(c >> 4));
    line.append("0123456789ABCDEF".charAt(c & 0x0F));
  }//appendHex

  /**
   * Takes a quoted-printable encoding and decodes it to find the corresponding
   * sequence of bytes.
   * <p>
   * Unrecognised sequences starting with '=' are passed through unmodified,
   * as are characters that do not correspond to bytes that could have been
   * output by a correct quoted-printable encoder. Note that this method will
   * strip trailing whitespace from each line.
   *
   * @param  s    input string in quoted-printable form
   * @return the decoded data.
   */
  public static final String decode(String s) {
    StringBuffer buffer = new StringBuffer(s.length());
    StringBuffer white = new StringBuffer(80);
    int len = s.length();

    for (int i = 0; i < len; i++) {
      char c = s.charAt(i);

      if ((c == '=') && (i + 2 < len)) {
        char a = s.charAt(++i);
        char b = s.charAt(++i);
        if ((a == '\r') && (b == '\n')) continue;

        int ah = "0123456789ABCDEF".indexOf(a);
        int bh = "0123456789ABCDEF".indexOf(b);
        if ((ah == -1) || (bh == -1)) {
          buffer.append('=');
          buffer.append(a);
          buffer.append(b);
        } else {
          buffer.append((char) ((ah << 4) | bh));
        }
      } else if ((c == ' ') || (c == '\t')) {
        white.append(c);
      } else {
        if (white.length() != 0) buffer.append(white);
        buffer.append(c);
        white.setLength(0);
      }
    }
    return buffer.toString();
  }//decode

}//class QuotedPrintable
