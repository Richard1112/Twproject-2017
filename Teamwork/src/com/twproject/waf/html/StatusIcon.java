package com.twproject.waf.html;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.waf.html.core.JspHelper;

import javax.servlet.jsp.PageContext;
import java.io.IOException;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 13-giu-2006 : 12.47.47
 */
public class StatusIcon extends JspHelper {
  String color;
  public int size;


  public StatusIcon(String color, int size, String tooltip) {
    this.color=color;
    this.size=size;
    this.toolTip=tooltip;
  }

  public void toHtml(PageContext pageContext){
    try {
      pageContext.getOut().println(getHtml());
    } catch (IOException e) {
      throw new PlatformRuntimeException(e);
    }
  }

  public String getHtml() {
    return "<span class=\"teamworkIcon\" style=\"color:"+color+";font-size:"+size+"px;\"  title=\""+toolTip+"\">©</span>";
  }
}
