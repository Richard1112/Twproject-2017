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
import java.util.Iterator;
import java.util.List;

public class versitWriter {

  private BufferedWriter m_Out;

  public versitWriter(OutputStream out) {
    m_Out = new BufferedWriter(new OutputStreamWriter(out));
  }//versitWriter

  public versitWriter(OutputStream out, String enc)
      throws UnsupportedEncodingException {
    m_Out = new BufferedWriter(new OutputStreamWriter(out, enc));
  }//versitWriter

  public void writeCard(List items) throws IOException {
    StringBuffer sbuf = new StringBuffer();

    for (Iterator iter = items.iterator(); iter.hasNext();) {
      versitItem item = (versitItem) iter.next();
      sbuf.append(item.toString())
          .append("\r\n");
      //DEBUG:System.out.println("writeCard():writing=" + item.getIdentifier());
    }
    m_Out.write(sbuf.toString());
    m_Out.flush();
  }//writeCard


}//class versitWriter
