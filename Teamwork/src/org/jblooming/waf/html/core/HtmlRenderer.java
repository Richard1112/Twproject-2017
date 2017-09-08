package org.jblooming.waf.html.core;

import org.jblooming.waf.SessionState;
import org.jblooming.waf.view.PageSeed;

/**
 * A widget differently from a <code>JspHelper</code> prints out the html produced directly.
 */
public abstract class HtmlRenderer {

  public String id = "d" + hashCode() + "";
  public boolean required = false;
  public PageSeed url;
  public SessionState sessionState;

  /**
   * key controls
   */
  public String actionListened;
  public int keyToHandle;
  public boolean checkCtrlKey=false;  
  public String launchedJsOnActionListened;


  public String toolTip;

  public String getToolTip() {
    return toolTip;
  }

  public void setToolTip(String toolTip) {
    this.toolTip = toolTip;
  }

  public abstract StringBuffer toHtmlStringBuffer();

  public void addKeyPressControl(int keyToHandle, String launchedJs, String actionListened) {
    this.keyToHandle = keyToHandle;
    this.launchedJsOnActionListened = launchedJs;
    this.actionListened = actionListened;    
  }

  public String toHtml() {
    return toHtmlStringBuffer().toString();
  }

}
