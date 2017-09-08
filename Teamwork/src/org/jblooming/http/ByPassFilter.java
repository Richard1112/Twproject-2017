package org.jblooming.http;

import org.jblooming.utilities.HttpUtilities;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.File;
import java.util.HashSet;
import java.util.Set;

/**
 *
 */
public class ByPassFilter implements Filter {

  public static Set<String> freeFolders = new HashSet<String>();

  public void init(FilterConfig filterConfig) throws ServletException {
  }

  public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws ServletException, IOException {
    HttpServletResponse response = (HttpServletResponse) servletResponse;
    HttpServletRequest request = (HttpServletRequest) servletRequest;
    
    String rootPath = HttpUtilities.getFileSystemRootPathForRequest(request);
    String key = HttpUtilities.getCanonicalFileSystemPathOfPartFromURI(request);
    String admin = rootPath + "admin.jsp";
    String admin2 = rootPath +"commons"+File.separator+"administration"+File.separator+"admin.jsp";

    boolean forward = key.equalsIgnoreCase(admin) || key.equalsIgnoreCase(admin2);
    if (!forward) {
      if (freeFolders != null && freeFolders.size() > 0) {
        for (String s : freeFolders) {
          if (key.toLowerCase().startsWith(rootPath.toLowerCase() + s.toLowerCase())) {
            forward = true;
            break;
          }
        }
      }
    }
    if (forward)
      request.getRequestDispatcher(request.getRequestURI().substring(request.getContextPath().length())).forward(request, response);
    else
      filterChain.doFilter(request, response);
  }

  public void destroy() {
  }
}
