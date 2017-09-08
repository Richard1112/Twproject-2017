package org.jblooming.http.gzip;

import org.jblooming.tracer.Tracer;
import org.jblooming.waf.settings.ApplicationState;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class GZIPFilter implements Filter {

  public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain) throws IOException, ServletException {

    if (req instanceof HttpServletRequest) {
      HttpServletRequest request = (HttpServletRequest) req;
      HttpServletResponse response = (HttpServletResponse) res;
      String ae = request.getHeader("accept-encoding");
      //Wonderfully disgusting hack: the load in background of settings seems to alter the response length, hence we filter if ApplicationState.loaded
      if (ae != null && ae.toLowerCase().indexOf("gzip") != -1 && ApplicationState.loaded) {
        if (Tracer.platformLogger != null)
          Tracer.platformLogger.debug("GZIP supported by browser, compressing.");
        GZIPResponseWrapper wrappedResponse = new GZIPResponseWrapper(response);
        chain.doFilter(req, wrappedResponse);
        wrappedResponse.finishResponse();
      } else
        chain.doFilter(req, res);
    }
  }

  public void init(FilterConfig filterConfig) {
    // noop
  }

  public void destroy() {
    // noop
  }
}
