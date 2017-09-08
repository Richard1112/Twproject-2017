package com.twproject.task;

import org.jblooming.tracer.Tracer;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageState;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

public class TaskPublicPageFilter implements Filter {
  public void init(FilterConfig filterConfig) throws ServletException {
    if (ApplicationState.platformConfiguration.development) {
      Tracer.platformLogger.info("-- Init TaskPublicPageFilter --");
    }

  }

  public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws ServletException, IOException {
    // cast to HttpServletRequest
    HttpServletRequest hRequest = (HttpServletRequest) servletRequest;
    HttpServletResponse hResponse = (HttpServletResponse) servletResponse;

    // Get Uri of request
    String uriUpper = getPathWithoutContext(hRequest).toUpperCase();

    // check if URI starts with right prefix for API url
    if (uriUpper.startsWith("/PROJECT/")) {
      hRequest.getRequestDispatcher("/applications/teamwork/publicPage/taskPublic.jsp" + generateparams(hRequest, hResponse)).forward(hRequest, hResponse);

    } else if (uriUpper.startsWith("/T/".toUpperCase()) || uriUpper.startsWith("/TASK/".toUpperCase())) {
      hRequest.getRequestDispatcher("/applications/teamwork/task/taskOverview.jsp" + generateSmartLinkParams(hRequest, hResponse)).forward(hRequest, hResponse);

    } else if (uriUpper.startsWith("/I/".toUpperCase()) || uriUpper.startsWith("/ISSUE/".toUpperCase())) {
      hRequest.getRequestDispatcher("/applications/teamwork/issue/issueList.jsp" + generateSmartLinkParams(hRequest, hResponse)).forward(hRequest, hResponse);

    } else if (uriUpper.startsWith("/R/".toUpperCase()) || uriUpper.startsWith("/RESOURCE/".toUpperCase())) {
      hRequest.getRequestDispatcher("/applications/teamwork/resource/resourceEditor.jsp" + generateSmartLinkParams(hRequest, hResponse)).forward(hRequest, hResponse);

    } else if (uriUpper.startsWith("/E/".toUpperCase()) || uriUpper.startsWith("/EVENT/".toUpperCase()) || uriUpper.startsWith("/M/".toUpperCase()) || uriUpper.startsWith("/MEETING/".toUpperCase())) {
      hRequest.getRequestDispatcher("/applications/teamwork/agenda/agendaEditor.jsp" + generateSmartLinkParams(hRequest, hResponse)).forward(hRequest, hResponse);

    } else if (uriUpper.startsWith("/B/".toUpperCase()) || uriUpper.startsWith("/BOARD/".toUpperCase())) {
      hRequest.getRequestDispatcher("/applications/teamwork/board/board.jsp" + generateSmartLinkParams(hRequest, hResponse)).forward(hRequest, hResponse);

      // check if URI starts with right prefix for API url
      //serverlUrl/widget
    } else if (uriUpper.startsWith("/WIDGET/")) {
      String[] ss = uriUpper.split("/");
      if (ss.length >= 3 && !uriUpper.contains(".") ) {
        hRequest.getRequestDispatcher("/applications/teamwork/task/gantt/widget.jsp?key=" + (ss[2])).forward(hRequest, hResponse);
      }
    } else if (uriUpper.startsWith("/TICKETS/")) {
      String[] ss = uriUpper.split("/");
      if (ss.length >= 3 && !uriUpper.contains(".") ) {
        hRequest.getRequestDispatcher("/applications/teamwork/publicPage/tickets.jsp?key=" + (ss[2])).forward(hRequest, hResponse);
      }

    } else {
      filterChain.doFilter(servletRequest, servletResponse);
    }

  }

  /**
   * Obtain decoded Uri without contextPath
   */
  private String getPathWithoutContext(HttpServletRequest request) {
    String requestUri = request.getRequestURI();
    if (requestUri == null) {
      requestUri = "";
    }
    String decodedRequestUri = decodeRequestString(request, requestUri);
    if (decodedRequestUri.startsWith(request.getContextPath()))
      decodedRequestUri = decodedRequestUri.substring(request.getContextPath().length(), decodedRequestUri.length());

    return decodedRequestUri;
  }


  private String generateparams(HttpServletRequest request, HttpServletResponse response) {
    String requestUri = request.getRequestURI();
    String params = "";
    String[] tmpSplitStr = requestUri.split("/");
    int i = 0;
    for (String s : tmpSplitStr) {
      if ("PROJECT".equals(s.toUpperCase())) {
        params = "?TASK_ID=" + tmpSplitStr[i + 1];
      }
      i++;
    }
    return params;
  }


  private String generateSmartLinkParams(HttpServletRequest request, HttpServletResponse response) {
    String params = "";

    String requestUri = getPathWithoutContext(request);

    // we should receive somthing like this /t/id - /task/id /i/id - /r/id - /e/id
    String[] tmpSplitStr = requestUri.split("/");

    String id = "undefined";

    if (tmpSplitStr.length >= 3)
      id = tmpSplitStr[2].trim();

    params = "?" + Commands.COMMAND + "=GUESS&" + Fields.OBJECT_ID + "=" + id;

    return params;
  }

  /**
   * Decode the string with a URLDecoder. The encoding will be taken
   * from the request, falling back to the default for your platform ("ISO-8859-1" on windows).
   *
   * @param request HttpServletRequest
   * @param source  String that contain URL to decode
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
    if (ApplicationState.platformConfiguration.development) {
      Tracer.platformLogger.info("-- destroy TaskPublicPageFilter --");
    }

  }
}
