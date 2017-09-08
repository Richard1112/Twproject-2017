package org.jblooming.waf.html.container;

import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.html.core.JspHelper;

import javax.servlet.jsp.PageContext;

public class Tab extends JspHelper {

  boolean openTabCalled;
  boolean closeTabCalled;

  public String id= "d" + hashCode();
  public String caption;
  public boolean focused = false;
//  public PageSeed variationsFromForm = new PageSeed();
  public boolean enabled = true;

  public TabSet tabSet;
  public final static String START = "START";
  public final static String END = "END";

  public ButtonSupport button = null;
  public String additionalScript;
  public String additionalCssClass;

  //  public boolean doSubmit=false;

  /**
   * @deprecated give an id
   */
  public Tab(String caption) {
    this.caption=caption;
  }

  public Tab(String id, String caption) {
    this.id = id;
    this.caption=caption;
  }

  public Tab(String id,ButtonSupport bs) {
    this.id = id;
    this.button=bs;
    this.caption = bs.label;
  }


  public void start(PageContext pageContext)  {
    pageContext.getRequest().setAttribute(Commands.COMMAND, START);
    toHtml(pageContext);
    openTabCalled = true;
  }

  public void end(PageContext pageContext) {
    pageContext.getRequest().setAttribute(Commands.COMMAND, END);
    toHtml(pageContext);
    closeTabCalled = true;
  }

}
