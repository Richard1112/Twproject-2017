/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * Utility class providing encoding and decoding
 * methods.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 *
 * @see Base64
 * @see QuotedPrintable
 */
public class EncodingUtility {

  /**
   * Returns the decoded data.
   *
   * @param data the Base64 encoded data.
   * @return the decoded data.
   */
  public static byte[] decodeBase64(byte[] data) {
    return Base64.decode(data);
  }//decodeBase64

  /**
   * Returns the decoded <tt>String</tt>.
   *
   * @param data a Base64 encoded <tt>String</tt>.
   * @return the decoded data.
   */
  public static byte[] decodeBase64(String data) {
    return Base64.decode(data.getBytes());
  }//decodeBase64

  /**
   * Returns the Base64 encoded data.
   *
   * @param data the data to be encoded.
   * @return the Base64 encoded data.
   */
  public static byte[] encodeBase64(byte[] data) {
    return Base64.encode(data);
  }//encodeBase64

  /**
   * Returns the decoded <tt>String</tt>.
   *
   * @param data a Quoted-Printable encoded <tt>String</tt>.
   * @return the decoded <tt>String</tt>.
   */
  public static String decodeQP(String data) {
    return QuotedPrintable.decode(data);
  }//decodeQP

  /**
   * Returns the encoded <tt>String</tt>.
   *
   * @param data a <tt>String</tt> to be Quoted-Printable encoded.
   * @return the encoded <tt>String</tt>.
   */
  public static String encodeQP(String data) {
    return QuotedPrintable.encode(data);
  }//encodeQP

  /**
   * Removes whitespace from the given data.
   *
   * @param data the data as <tt>byte[]</tt>.
   * @return the data without whitespaces.
   */
  public static byte[] removeWhiteSpace(byte[] data) {
    ByteArrayOutputStream bout = new ByteArrayOutputStream(data.length);
    for (int i = 0; i < data.length; i++) {
      if (data[i] == 10 || data[i] == 13 || data[i] == 9 || data[i] == 32) {
        //ignore whitespace
        continue;
      } else {
        bout.write(data[i]);
      }
    }
    return bout.toByteArray();
  }//removeWhiteSpace

  /**
   * Returns an <tt>InputStream</tt> which will not include any whitespace from
   * the passed in <tt>InputStream</tt>.
   *
   * @param data the data as <tt>InputStream</tt>.
   * @return an <tt>InputStream</tt>.
   *
   * @throws IOException if an I/O error occurs.
   */
  public static InputStream removeWhiteSpace(InputStream data)
      throws IOException {
    ByteArrayOutputStream bout = new ByteArrayOutputStream(data.available());
    int in = 0;
    while (in != -1) {
      in = data.read();
      if (in == -1) {
        continue;
      }
      if (in == 10 || in == 13 || in == 9 || in == 32) {
        //ignore whitespace
        continue;
      } else {
        bout.write(in);
      }
    }
    return new ByteArrayInputStream(bout.toByteArray());
  }//removeWhiteSpace

  /**
   * Encodes line breaks into an escape sequence.
   *
   * @param data the data with raw line breaks.
   * @return the data with encoded line breaks.
   */
  public static byte[] encodeLineBreaks(byte[] data) {
    try {
      ByteArrayOutputStream bout = new ByteArrayOutputStream(data.length);
      for (int i = 0; i < data.length; i++) {
        if (data[i] == 13 && data[i + 1] == 10) {
          bout.write(NEWLINE_ENC);
          i++;
        } else if (data[i] == 13) {
          bout.write(NEWLINE_ENC);
        } else if (data[i] == 10) {
          bout.write(NEWLINE_ENC);
        } else {
          bout.write(data[i]);
        }
      }
      return bout.toByteArray();
    } catch (Exception ex) {
      return data;
    }
  }//encodeLineBreaks

  /**
   * Convenience method that encodes line breaks
   * into an escape sequence.
   *
   * @param str the data with raw line breaks as <tt>String</tt>.
   * @return the data with encoded line breaks as <tt>String</tt>.
   */
  public static String encodeLineBreaks(String str) {
    return new String(encodeLineBreaks(str.getBytes()));
  }//encodeLineBreaks

  /**
   * Returns an <tt>InputStream</tt> filtering raw line breaks
   * into escape sequences.
   *
   * @param data an <tt>InputStream</tt>.
   * @return an <tt>InputStream</tt>.
   *
   * @throws IOException if an I/O error occurs.
   */
  public static InputStream encodeLineBreaks(InputStream data)
      throws IOException {
    ByteArrayOutputStream bout = new ByteArrayOutputStream(data.available());
    int in = 0;
    while (in != -1) {
      in = data.read();
      if (in == -1) {
        continue;
      }
      if (in == 13) {
        in = data.read();
        if (in == 10) {
          bout.write(NEWLINE_ENC);
          continue;
        } else {
          bout.write(NEWLINE_ENC);
          bout.write(in);
          continue;
        }
      } else if (in == 10) {
        bout.write(NEWLINE_ENC);
      } else {
        bout.write(in);
      }
    }
    return new ByteArrayInputStream(bout.toByteArray());
  }//encodeLineBreaks

  /**
   * Defines the encoded newline (escape sequence [BACKSLASH][n]).
   */
  private static final byte[] NEWLINE_ENC = {92, 110};


}//EncodingUtility
