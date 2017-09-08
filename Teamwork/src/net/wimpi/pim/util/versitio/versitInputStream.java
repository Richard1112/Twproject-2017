/***
 * jpim Java PIM Library
 * Copyright (c) 2001 Dieter Wimberger
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.util.versitio;

import java.io.*;

public class versitInputStream {

  private BufferedReader m_Input;
  private boolean m_hasLine = true;
  private boolean m_Unread = false;
  private int m_Char;

  public versitInputStream(InputStream in) {
    m_Input = new BufferedReader(new InputStreamReader(in));
  }//constructor

  public versitInputStream(InputStream in, String enc)
      throws UnsupportedEncodingException {
    m_Input = new BufferedReader(new InputStreamReader(in, enc));
  }//constructor

  public String readLine()
      throws IOException {

    StringBuffer buf = new StringBuffer(100);
    boolean done = false;
    char b = 0;
    boolean trail = false;

    try {
      while (!done) {
        b = readCharacter();
        if (b == EQUAL) {
          //lookahead character
          b = readCharacter();
          if (b == CR || b == LF) {
            trail = true;
          } else {
            trail = false;
            //add EQUAL to buffer
            buf.append(EQUAL);
          }
          //unread lookahead character
          unreadCharacter(b);
          continue;
        }
        if (b == CR || b == LF) {
          b = readCharacter();
          if (b == SPACE || b == HTAB) {
            //non-standard "line folder" just swallow it
            continue;
          } else if (b == CR || b == LF) {
            //non-standard linebreak if CR
            //standard one if LF
            b = readCharacter();
            if (b == SPACE || b == HTAB) {
              //standard "line folder" swallow it
              continue;
            } else if (b == CR || b == LF) {
              throw new IOException("Linebreaks mangled.");
            } else if (trail) {
              //broken "line folder", swallow it
              trail = false;
              buf.append(b);
              continue;
            } else {
              unreadCharacter(b);
              done = true;
              continue;
            }
          } else {
            //non-standard linebreak
            unreadCharacter(b);
            done = true;
            continue;
          }
        } else {
          //store in buffer
          buf.append(b);
        }
      }
    } catch (EOFException ex) {
      //fixme: check if crlf ended the stream?
    } finally {
      return buf.toString();
    }
  }//readLine

  private char readCharacter()
      throws EOFException, IOException {
    int c = 0;
    if (m_Unread) {
      c = m_Char;
      m_Unread = false;
    } else {
      c = m_Input.read();
    }
    //System.out.println("c="+(int)c);
    if (c == -1) {
      m_hasLine = false;
      throw new EOFException();
    } else {
      return (char) c;
    }
  }//readCharacter

  private void unreadCharacter(char b) {
    m_Unread = true;
    m_Char = b;
  }//unreadCharacter

  public boolean hasLine() {
    return m_hasLine;
  }//hasLine

  public static final char CR = 13;
  public static final char LF = 10;
  public static final char SPACE = 32;
  public static final char HTAB = 9;
  public static final char EQUAL = 61;

}//class versitInputStream
