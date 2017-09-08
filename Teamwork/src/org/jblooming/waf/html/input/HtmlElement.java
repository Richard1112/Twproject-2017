package org.jblooming.waf.html.input;

import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.html.core.JspIncluderSupport;

import javax.servlet.jsp.PageContext;
import java.io.IOException;
import java.util.Stack;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public abstract class HtmlElement extends JspIncluderSupport {

  public String id = "d" + hashCode() + "";
  public String toolTip;
  public boolean translateToolTip = false;
  public boolean disabled = false;


   public void toHtml(PageContext pageContext) {
    Stack stack = getStack(pageContext.getRequest());
    stack.push(this);
    try {
      StringBuffer out = toHtmlStringBuffer();
      pageContext.getOut().write(out.toString());
    } catch (IOException e) {
      throw new PlatformRuntimeException(e);
    } finally {
      stack.pop();
    }
  }

   public String getToolTip() {
    return JSP.htmlEncodeApexes(JSP.w(toolTip));
  }


  public abstract StringBuffer toHtmlStringBuffer();

}
