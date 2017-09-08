package org.jblooming.utilities.file.fileStorage;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

/**
 * Wrapperclass to wrap an OutputStream around a Writer
 *
 * @author Pietro Polsinelli ppolsinelli@twproject.com
 */
public class Writer2Stream extends OutputStream {
  Writer out;

  public Writer2Stream(Writer w) {
    super();
    out = w;
  }

  public void write(int i) throws IOException {
    out.write(i);
  }

  public void write(byte[] b) throws IOException {
    for (int i = 0; i < b.length; i++) {
      int n = b[i];
      //Convert byte to ubyte
      n = ((n >>> 4) & 0xF) * 16 + (n & 0xF);
      out.write(n);
    }
  }

  public void write(byte[] b, int off, int len) throws IOException {
    for (int i = off; i < off + len; i++) {
      int n = b[i];
      n = ((n >>> 4) & 0xF) * 16 + (n & 0xF);
      out.write(n);
    }
  }
}