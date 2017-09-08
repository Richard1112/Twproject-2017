package org.jblooming.http;

import org.jblooming.utilities.HttpUtilities;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.tracer.Tracer;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.FrontControllerFilter;

import javax.servlet.*;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletRequest;
import java.util.Set;
import java.util.HashSet;
import java.io.IOException;
import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

/**
 *
 * If using Caucho Resin app server, in order to make this work you must add to resin.conf:
 *
 * <server>
 *   <case-insensitive>false</case-insensitive>
 * ...
 *
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class CaseSensitivityFilter implements Filter {

  public void init(FilterConfig filterConfig) throws ServletException {
  }

  public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws ServletException, IOException {

    // Tremendous Hack for RestfulApiFilter!
    String apiUrl = "/API";
    // cast to HttpServletRequest
    HttpServletRequest hRequest = (HttpServletRequest) servletRequest;
    HttpServletResponse hResponse = (HttpServletResponse) servletResponse;

    // Get Uri of request
    String uri = getPathWithoutContext(hRequest);


    if (ApplicationState.platformConfiguration!=null &&
        ApplicationState.platformConfiguration.development &&
        !uri.toUpperCase().startsWith(apiUrl) &&
        !uri.equalsIgnoreCase("/jcaptcha")) {

      HttpServletRequest request = (HttpServletRequest) servletRequest;
      String realPath = request.getSession(true).getServletContext().getRealPath(request.getServletPath());
      File file = new File(realPath);
      String canonicalPath = file.getCanonicalPath();
      if (!realPath.startsWith(canonicalPath)) {
        Tracer.platformLogger.error(realPath + " is different from " + canonicalPath);
        throw new RuntimeException(realPath + " is different from " + canonicalPath);

      } else if (!canonicalPath.endsWith(FrontControllerFilter.page)&&!canonicalPath.endsWith("command.jsp")&&!file.exists()){
        Tracer.platformLogger.error("File: "+canonicalPath + " doesn't exist ");
        throw new RuntimeException("File: "+canonicalPath + " doesn't exist ");

      }
    }

    filterChain.doFilter(servletRequest, servletResponse);
  }


  /**
   *  Obtain decoded Uri without contextPath
   *
   * @param request HttpServletRequest
   * @return decoded Uri without context Path
   */
  private String getPathWithoutContext(HttpServletRequest request) {
    String requestUri = request.getRequestURI();

    if (requestUri == null) {
      requestUri = "";
    }

    return decodeRequestString(request, requestUri);
  }


  /**
   * Decode the string with a URLDecoder. The encoding will be taken
   * from the request, falling back to the default for your platform ("ISO-8859-1" on windows).
   *
   * @param request HttpServletRequest
   * @param source String that contain URL to decode
   * @return String with decoded url
   */
  public String decodeRequestString(HttpServletRequest request, String source) {
    String enc = request.getCharacterEncoding();
    if (enc != null) {
      try {
        return URLDecoder.decode(source, enc);
      } catch (UnsupportedEncodingException ex) {
        Tracer.platformLogger.error("Could not decode: " + source + " (header encoding: '" + enc + "'); exception: " + ex.getMessage());
      }
    }

    return source;
  }


  public void destroy() {
  }
}

