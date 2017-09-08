package org.jblooming.http;

import org.jblooming.utilities.TimeConstants;
import org.jblooming.utilities.file.FileUtilities;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * @author Pietro Polsinelli
 */
public class ZipServe {

  public static File getTempFolder(HttpServletRequest request) throws IOException {
    String tempFolderName = request.getSession().getServletContext().getRealPath("/temp");
    File tempDir = new File(tempFolderName);
    if ((!tempDir.exists() && !tempDir.mkdir()) || !tempDir.isDirectory())
      throw new IOException("Can't getPageState/access temporary folder " + tempFolderName);
    return tempDir;
  }

  public static void serve(File file, HttpServletRequest request, HttpServletResponse response, boolean allowCache) throws IOException {
    serve(file, request, response, allowCache, false);
  }

  public static void serve(File file, HttpServletRequest request, HttpServletResponse response, boolean allowCache, boolean useWriter) throws IOException {
    String mime = request.getSession().getServletContext().getMimeType(file.getName());
    response.setContentType(mime);
    response.setContentLength((int) file.length());
    if (allowCache) {
      response.setDateHeader("Last-Modified", file.lastModified());
      response.setHeader("Cache-Control", "must-revalidate");  // HTTP/1.1
      response.setDateHeader("Expires", file.lastModified() + TimeConstants.DAY);    // Limit one day
    } else {
      response.setDateHeader("Expires", 0L);    // Data passata
      response.setDateHeader("Last-Modified", System.currentTimeMillis() + TimeConstants.HOUR);  // sempre modificato
      response.setHeader("Cache-Control", ": no-store, no-cache, must-revalidate");  // HTTP/1.1
      response.setHeader("Cache-Control", "post-check=0, pre-check=0");
      response.setHeader("Pragma", "no-cache");  // HTTP/1.0
    }
    if (useWriter) {
      FileReader in = new FileReader(file);
      try {
        Writer out = response.getWriter();
        FileUtilities.copy(in, out, false);
      } finally {
        in.close();
      }
    } else {
      FileInputStream in = new FileInputStream(file);
      try {
        OutputStream out = response.getOutputStream();
        FileUtilities.copy(in, out, false);
      } finally {
        in.close();
      }
    }
  }

  public static void serve(ZipFile archive, ZipEntry file, HttpServletRequest request, HttpServletResponse response, boolean allowCache, boolean useWriter) throws IOException {
    String mime = request.getSession().getServletContext().getMimeType(file.getName());
    response.setContentType(mime);
    response.setContentLength((int) file.getSize());
    if (allowCache) {
      response.setDateHeader("Last-Modified", file.getTime());
      response.setHeader("Cache-Control", "must-revalidate");  // HTTP/1.1
      response.setDateHeader("Expires", file.getTime() + TimeConstants.DAY);    // Limit one day
    } else {
      response.setDateHeader("Expires", 0L);    // Data passata
      response.setDateHeader("Last-Modified", System.currentTimeMillis() + TimeConstants.HOUR);  // sempre modificato
      response.setHeader("Cache-Control", ": no-store, no-cache, must-revalidate");  // HTTP/1.1
      response.setHeader("Cache-Control", "post-check=0, pre-check=0");
      response.setHeader("Pragma", "no-cache");  // HTTP/1.0
    }
    if (useWriter) {
      InputStreamReader in = new InputStreamReader(archive.getInputStream(file));
      try {
        Writer out = response.getWriter();
        FileUtilities.copy(in, out, false);
      } finally {
        in.close();
      }
    } else {
      InputStream in = archive.getInputStream(file);
      try {
        OutputStream out = response.getOutputStream();
        FileUtilities.copy(in, out, false);
      } finally {
        in.close();
      }
    }
  }

  public static void serve(ZipFile archive, String entrypath, HttpServletRequest request, HttpServletResponse response, boolean allowCache, boolean useWriter) throws IOException {
    ZipEntry entry = archive.getEntry(entrypath);
    if (entry == null) {
      entrypath = entrypath.replace('/', '\\');
      String[] attempts = new String[4];
      if (entrypath.startsWith("\\")) {
        attempts[0] = entrypath;
        attempts[1] = entrypath.substring(1);
      } else {
        attempts[0] = '\\' + entrypath;
        attempts[1] = entrypath;
      }
      attempts[2] = attempts[0].replace('\\', '/');
      attempts[3] = attempts[1].replace('\\', '/');
      for (int i = 0; i < attempts.length; i++) {
        String attempt = attempts[i];
        entry = archive.getEntry(entrypath);
        if (entry != null) {
          serve(archive, entry, request, response, allowCache, useWriter);
          return;
        }
      }
      throw new IllegalArgumentException(entrypath + " can't be served becouse does not exist into file " + archive.getName());
    } else {
      serve(archive, entry, request, response, allowCache, useWriter);
    }
  }


}
