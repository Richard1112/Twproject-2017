package com.opnlb.website.waf;

import org.jblooming.utilities.JSP;
import org.jblooming.waf.FrontControllerFilter;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageSeed;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * WebSiteFilter (c) 2005 - Open Lab - www.open-lab.com
 */
public class WebSiteFilter implements Filter {

  public void init(FilterConfig filterConfig) throws ServletException {
  }

  public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws ServletException, IOException {

    HttpServletRequest request = (HttpServletRequest) servletRequest;
    HttpServletResponse response = (HttpServletResponse) servletResponse;

    String resource = request.getServletPath(); //.toLowerCase();

    int lastSlash = request.getRequestURI().lastIndexOf("/");
    int pageNameEnd = request.getRequestURI().indexOf(FrontControllerFilter.page);
    if (lastSlash > -1 && pageNameEnd > -1 && pageNameEnd>lastSlash) {
      String key = request.getRequestURI().substring(lastSlash + 1, pageNameEnd + 5);
      // redirect to clean all front office urls ( /applications/webwork/pageName.page ==> /pageName.page ) 
      if(resource.endsWith(FrontControllerFilter.page) && resource.contains("applications/webwork/")) {
        String paramsString = "";
        String params = request.getQueryString();
        if(JSP.ex(params))
          paramsString = "?" + params;

        String urlPath = ApplicationState.serverURL+"/"+ key + paramsString;
        response.sendRedirect(urlPath);
      }
    }

    // to be tested:: according to servername it MUST points to a different home.page (SEO compliance, no redirect)
    String requestedPageName = null;

    if (resource.toLowerCase().endsWith(FrontControllerFilter.page)) {
      PageSeed screenPage = new PageSeed("/" + ApplicationState.platformConfiguration.defaultApplication.getRootFolder() + "/screens/defaultScreen.jsp");
      // get page name from url
      if (!JSP.ex(requestedPageName))
        requestedPageName = resource.substring(resource.lastIndexOf("/") + 1, resource.lastIndexOf(FrontControllerFilter.page));

      request.setAttribute("pageName", requestedPageName);
      String url = screenPage.toLinkToHref();
      if(url.endsWith("?"))
        url = url.substring(0, url.trim().length()-1);
        request.getRequestDispatcher(url).forward(request, response);
    } else {
      filterChain.doFilter(request, servletResponse);
    }
  }

  public void destroy() {

  }

}