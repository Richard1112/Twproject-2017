package org.jblooming.http;

import org.jblooming.utilities.HttpUtilities;
import org.jblooming.tracer.Tracer;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;
import java.io.*;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

public class HttpServletResponseCacher extends HttpServletResponseWrapper {

  private String root;
  private HttpServletRequest httpServletRequest;
  private MyServletOutputStream sout;
  private PrintWriter wout;
  protected File file;

  public HttpServletResponseCacher(HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse, String version) {
    super(httpServletResponse);
    this.root = version;
    this.httpServletRequest = httpServletRequest;
  }


  public synchronized PrintWriter getWriter() throws IOException {
    if (sout != null)
      throw new IllegalStateException("getOtputStream was called yet");
    if (wout == null) {
      prepareLocalFile();
      wout = new PrintWriter(new FileWriter(file));
    }
    return wout;
  }

  public synchronized ServletOutputStream getOutputStream() throws IOException {
    if (wout != null)
      throw new IllegalStateException("getWriter was called yet");
    if (sout == null) {
      prepareLocalFile();
      sout = new MyServletOutputStream();
    }
    return sout;

  }

  private void prepareLocalFile() {
    getLocalFile();
    file.getParentFile().mkdirs();
  }

  public File getLocalFile() {
    if (file == null) {
      String s = getLocalFileName();
      file = new File(s);
    }
    return file;
  }

  private String getLocalFileName() {
    String fileNameFromUri = HttpUtilities.getFileNameFromUri(httpServletRequest);
    TreeMap tm = new TreeMap(httpServletRequest.getParameterMap());
    StringBuffer sb = new StringBuffer(fileNameFromUri);
    for (Iterator iterator = tm.entrySet().iterator(); iterator.hasNext();) {
      Map.Entry entry = (Map.Entry) iterator.next();
      sb.append('_');
      sb.append(entry.getKey());
      String[] values = (String[]) entry.getValue();
      if (values != null) {
        for (int i = 0; i < values.length; i++) {
          sb.append('=');
          String value = values[i];
          if (i > 0)
            sb.append('@');
          sb.append(value);
        }
      }
    }
    String s = root + sb.toString();
    return s;
  }

  public void flush() {
    try {
      if (wout != null) {
        wout.flush();
        wout.close();
      }
      if (sout != null) {
        sout.flush();
        sout.close();
      }
    } catch (IOException e) {
      Tracer.platformLogger.error(e);
    }
  }

  private class MyServletOutputStream extends ServletOutputStream {
    FileOutputStream out;

    public MyServletOutputStream() throws FileNotFoundException {
      out = new FileOutputStream(file);
    }

    public void write(int b) throws IOException {
      if (out != null)
        out.write(b);
    }

    public void flush() throws IOException {
      if (out != null)
        out.flush();
    }

    public void close() throws IOException {
      if (out != null)
        out.close();
      out = null;
    }
  }
}
