package org.jblooming.security.businessLogic;

import org.jblooming.waf.ScreenArea;

import javax.servlet.http.HttpServletRequest;

public class AreaScreen extends ScreenArea {

  public AreaScreen(String urlToInclude) {
    super(urlToInclude);
  }

  public static AreaScreen getListInstance(HttpServletRequest request) {
    return new AreaScreen(request.getContextPath() + "/commons/area/partAreaList.jsp");
  }

  public static AreaScreen getEditInstance(HttpServletRequest request) {
    return new AreaScreen(request.getContextPath() + "/commons/area/partAreaEditor.jsp");
  }

}
