package org.jblooming.http;

import org.jblooming.utilities.HttpUtilities;
import org.jblooming.waf.settings.ApplicationState;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.File;
import java.util.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

/**
 *
 */
public class CachingFilter implements Filter {


  public void init(FilterConfig filterConfig) throws ServletException {
  }

  public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws ServletException, IOException {
    HttpServletResponse response = (HttpServletResponse) servletResponse;
    HttpServletRequest request = (HttpServletRequest) servletRequest;

    String requr = request.getRequestURI().toUpperCase();
    if (requr.endsWith(".JS") ||
            requr.endsWith(".JS.JSP") ||
            requr.endsWith("CSS.JSP") ||
            requr.endsWith(".JPG") ||
            requr.endsWith(".PNG") ||
            requr.endsWith(".GIF") ||
            requr.endsWith(".JPEG") ||
            requr.endsWith(".CSS") ||
            requr.endsWith(".ICO") ||
            requr.endsWith(".SWF") ||
            requr.endsWith("PLATFORMCSS.JSP") ||
            requr.endsWith("MENUPLUSCSS.JSP")
            ){                     // in seconds
      setCacheExpireDate(response, 720000);
    }
    // pass the request/response on
    filterChain.doFilter(request , response);
  }

  public void destroy() {
  }


  public static void setCacheExpireDate(HttpServletResponse response, int seconds) {
    if (response != null) {
      response.setHeader("Cache-Control", "PUBLIC, max-age=" + seconds + ", must-revalidate");
      response.setHeader("Expires", htmlExpiresDateFormat().format(new Date(System.currentTimeMillis() + seconds * 1000)));
    }
  }

  public static DateFormat htmlExpiresDateFormat() {
    DateFormat httpDateFormat = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US);
    httpDateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
    return httpDateFormat;
  }

}