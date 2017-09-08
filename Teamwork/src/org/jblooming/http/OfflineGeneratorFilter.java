package org.jblooming.http;

import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.file.FileUtilities;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

public class OfflineGeneratorFilter implements Filter {

  public static boolean active = true;
  public static String BASE = "";
  public static String OFFLINE_VERSION = "";

  public void init(FilterConfig filterConfig) throws ServletException {

    String value = filterConfig.getInitParameter("BASE");
    if (value != null && value.trim().length() > 0)
      BASE = value;
  }

  public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws ServletException, IOException {

    if (active) {
      HttpServletRequest request = (HttpServletRequest) servletRequest;
      HttpServletResponse response = (HttpServletResponse) servletResponse;
      String rootPath = HttpUtilities.getFileSystemRootPathForRequest(request);
      String key = HttpUtilities.getCanonicalFileSystemPathOfPartFromURI(request);

      if (key.startsWith(rootPath + BASE)) {
        HttpServletResponseCacher hsrc = new HttpServletResponseCacher(request, response, BASE + '/' + OFFLINE_VERSION);
        File cache = hsrc.getLocalFile();
        if (!cache.exists()) {
          filterChain.doFilter(servletRequest, hsrc);
          hsrc.flush();
        }
        FileInputStream source = new FileInputStream(cache);
        try {
          FileUtilities.copy(source, response.getOutputStream(), false);
        } finally {
          source.close();
        }
        return;
      }
    }
    filterChain.doFilter(servletRequest, servletResponse);
  }

  public void destroy() {

  }
}
