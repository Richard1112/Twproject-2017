package org.jblooming.http.multipartfilter;

import com.oreilly.servlet.multipart.FilePart;
import com.oreilly.servlet.multipart.MultipartParser;
import com.oreilly.servlet.multipart.ParamPart;
import org.jblooming.http.ZipServe;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.tracer.Tracer;
import org.jblooming.waf.html.input.Uploader;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.system.SystemConstants;
import org.jblooming.PlatformRuntimeException;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import java.io.*;


public class MultipartFormRequestEncodingFilter implements Filter {

  public static final String TEMPORARY_FILENAME = ".tmpfilename";
  public static final String CONTENT_TYPE = ".content-type";
  public static final String multipartMimeType = "multipart/form-data";

  public void destroy() {
  }

  public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {

    File file = null;
    HttpServletRequest httpRequest = (HttpServletRequest) request;
    String contentType = httpRequest.getContentType();
    if (contentType != null && contentType.startsWith("multipart/form-data")) {
      try {

        String charset = "utf-8";

        int maxSize = Uploader.getMaxUploadSizeInByte();
        MultipartParser mp = new MultipartParser(httpRequest, maxSize);
        mp.setEncoding(charset);
        com.oreilly.servlet.multipart.Part part;

        MultiPartFormServletRequest servletRequest = new MultiPartFormServletRequest(httpRequest);

        while ((part = mp.readNextPart()) != null) {
          String name = part.getName();
          if (part.isParam()) {
            // it's a parameter part
            ParamPart paramPart = (ParamPart) part;
            String value = paramPart.getStringValue();

            servletRequest.updateMap(name, value, null);
          } else if (part.isFile()) {
            // it's a file part
            FilePart filePart = (FilePart) part;
            String fileName = filePart.getFileName();
            if (fileName != null) {
              // the part actually contained a file
              file = File.createTempFile("formTW_TEMP_FILE", ".tmp", ZipServe.getTempFolder(httpRequest));
              long size = filePart.writeTo(file);
              String mimeType = filePart.getContentType();
              if (mimeType == null)
                mimeType = "";
              // remove the original path in order to fix difference between explorer and mozilla: mozilla cut automatically the path, explorer no
              fileName = FileUtilities.getFileNameWithExtension(fileName);

              servletRequest.updateMap(name, fileName, null);

              servletRequest.updateMap(name + TEMPORARY_FILENAME, file.getAbsolutePath(), null);
              servletRequest.updateMap(name + CONTENT_TYPE, mimeType, null);

            } else {
              //this is NECESSARY as it means that the file should be reset
              servletRequest.updateMap(name, "", null);
            }
          }
        }
        chain.doFilter(servletRequest, response);

      } catch (IOException lEx) {
        Tracer.platformLogger.error("MultipartFormRequestEncodingFilter: error reading or saving file: " + lEx.getMessage());
        throw new PlatformRuntimeException("MultipartFormRequestEncodingFilter: error reading or saving file: " + lEx.getMessage(),lEx);

      } finally {
        if (file != null && file.exists() && !FileUtilities.tryHardToDeleteFile(file))
          file.deleteOnExit();
      }
    } else {
      chain.doFilter(request, response);
    }
  }


  public void init(FilterConfig p0) throws ServletException {
  }


}

