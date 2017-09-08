package org.jblooming.waf.html.container;

import org.jblooming.waf.html.core.JspIncluder;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;
import java.io.IOException;


public class DoubleButton implements JspIncluder {
  public JspIncluder button1;
  public JspIncluder button2;


  public DoubleButton(JspIncluder b1, JspIncluder b2 ){
    this.button1=b1;
    this.button2=b2;
  }


  public void toHtml(PageContext pageContext) throws IOException, ServletException {
    pageContext.getOut().println("<div class='buttonCouple'>");
    button1.toHtml(pageContext);
    button2.toHtml(pageContext);
    pageContext.getOut().println("</div>");
  }
}