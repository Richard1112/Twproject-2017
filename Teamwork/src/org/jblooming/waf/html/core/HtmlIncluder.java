package org.jblooming.waf.html.core;

import javax.servlet.jsp.PageContext;
import javax.servlet.ServletException;
import java.io.IOException;


public class HtmlIncluder implements JspIncluder{
  public String htmlToInclude="";

  public HtmlIncluder(String htmlToInclude){
    this.htmlToInclude=htmlToInclude;
  }

  public void toHtml(PageContext pageContext) throws IOException, ServletException {
    pageContext.getOut().print(htmlToInclude);
  }
}
