package org.jblooming.waf.html.core;

import javax.servlet.jsp.PageContext;
import javax.servlet.ServletException;
import java.io.IOException;


public interface JspIncluder {
  String MAIN_OBJECT_STACK = "__M_O_S__";
  String INITIALIZE = "JSPINITIALIZE";
  String ACTION = "JSPACT";
  String DOM_ID = "domId_";

  void toHtml(PageContext pageContext) throws IOException, ServletException;
}
