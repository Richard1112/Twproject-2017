package org.jblooming.waf.html.core;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.tracer.Tracer;
import org.jblooming.waf.settings.ApplicationState;

import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.PageContext;
import java.io.IOException;
import java.io.File;
import java.util.Stack;

/**
 * @author Pietro Polsinelli : ppolsinelli@open-lab.com
 */
public class JspIncluderSupport implements JspIncluder {
  /**
   * FLD_URL of the helper jsp that layouts the html to be displayed using <code>this</code>.
   */
  public String urlToInclude = null;

  public void toHtml(PageContext pageContext) {

    if (ApplicationState.platformConfiguration!=null && ApplicationState.platformConfiguration.development && !"/".equals(urlToInclude)) {

      HttpServletRequest request = (HttpServletRequest) pageContext.getRequest();
      String realPath = request.getSession(true).getServletContext().getRealPath(urlToInclude);
      String canonicalPath = null;
      try {
        canonicalPath = new File(realPath).getCanonicalPath();
      } catch (IOException e) {
        throw new PlatformRuntimeException(e);
      }

      if (!realPath.equals(canonicalPath)) {
        Tracer.platformLogger.error(realPath + " is different from " + canonicalPath);        
        throw new RuntimeException(realPath + " is different from " + canonicalPath);
      }
    }



    Stack stack = getStack(pageContext.getRequest());

    stack.push(this);
    try {
      try {
        pageContext.include(urlToInclude);
      } catch (Throwable e) {
        throw new PlatformRuntimeException(e);
      }
    } finally {
      stack.pop();
    }
  }

  protected Stack getStack(ServletRequest request) {
    Stack stack = (Stack) request.getAttribute(MAIN_OBJECT_STACK);
    if (stack == null) {
      stack = new Stack();
      request.setAttribute(MAIN_OBJECT_STACK, stack);
    }
    return stack;
  }

  public static JspIncluder getCurrentInstance(HttpServletRequest request) {
    Stack stack = (Stack) request.getAttribute(MAIN_OBJECT_STACK);
    if (stack == null) {
      stack = new Stack();
      request.setAttribute(MAIN_OBJECT_STACK, stack);
    }
    if (stack.size()>0)
      return (JspIncluder) stack.peek();
    else
      return null;
  }



  public String toString() {
    return this.getClass().getName() + "; urlToInclude = " + urlToInclude;
  }

}
