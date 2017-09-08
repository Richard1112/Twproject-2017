package org.jblooming.utilities.file;

import org.jblooming.tracer.Tracer;

import java.io.*;

/**
 * Date: 4-mar-2003
 * Time: 13.17.48
 *
 * @author Pietro Polsinelli dev@open-lab.com
 */
public class FileCopy implements Visitor {
  // copying files from here.
  private File sourceDir;
  // copying files to here.
  private File destDir;

  public FileCopy(File sourceDir, File destDir) {
    this.sourceDir = sourceDir;
    this.destDir = destDir;
  }

  public void visit(Object arg) {
    File sourceFile = (File) arg;
    String relativeSourceFile = sourceFile.toString().substring(sourceDir.toString().length() + 1);

    try {
      copyFile(new File(sourceDir, relativeSourceFile), new File(destDir, relativeSourceFile));
    } catch (IOException e) {
      Tracer.platformLogger.error(e);
    }
  }

  public void delete(Object arg) {
    File sourceFile = (File) arg;
    sourceFile.delete();
  }

  public static void copyFile(File src, File dest) throws IOException {
    FileInputStream in = null;
    FileOutputStream out = null;
    try {
      if (dest.isDirectory()) {
        dest.mkdirs();
      } else {
        new File(dest.getParent()).mkdirs();
      }
      in = new FileInputStream(src);
      out = new FileOutputStream(dest);

      ByteArrayOutputStream outBuf = new ByteArrayOutputStream((int) src.length());

      byte[] buffer = new byte[4096];
      int count = in.read(buffer);
      while (count > 0) {
        outBuf.write(buffer, 0, count);
        count = in.read(buffer);
      }
      out.write(outBuf.toByteArray());
    } finally {
      try {
        if (in != null) in.close();
      } catch (IOException e) {
        Tracer.platformLogger.error(e);
      }
      try {
        if (out != null) out.close();
      } catch (IOException e) {
        Tracer.platformLogger.error(e);
      }
    }
  }

}

