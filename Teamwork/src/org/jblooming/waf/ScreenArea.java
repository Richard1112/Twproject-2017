package org.jblooming.waf;

import org.jblooming.utilities.HttpUtilities;
import org.jblooming.waf.html.core.JspIncluderSupport;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;

/**
 * Formalizes the area of a screen. Permits to controller to have the control of layout,
 * hence doing without redirects and forwards on the base of actions.
 *
 * @author Pietro Polsinelli : ppolsinelli@open-lab.com
 */
public class ScreenArea extends JspIncluderSupport {

  public String areaHtmlClass;
  public ActionController controller;
  public ScreenRoot parent;

  protected ScreenArea() {
    super();
  }

  public ScreenArea(String urlToInclude) {
    this.urlToInclude = urlToInclude;
  }

  public ScreenArea(ActionController ac, String urlToInclude) {
    this(urlToInclude);
    controller = ac;
  }

  public ScreenArea(ActionController ac, HttpServletRequest request) {
    this();

    this.urlToInclude = HttpUtilities.realURI(request);

    controller = ac;
  }

  public ScreenArea(HttpServletRequest request) {
    //this(request.getRequestURI().substring(request.getContextPath().length()));
    this(null,request);
  }


  public void register(PageState pageState) {
    pageState.registerPart(this);
  }

  public String toString() {
    return super.toString() + "\ncontroller = " + controller;
  }

}
