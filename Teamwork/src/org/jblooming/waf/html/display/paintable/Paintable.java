package org.jblooming.waf.html.display.paintable;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.view.PageState;
import org.jblooming.PlatformRuntimeException;

import javax.servlet.jsp.PageContext;


/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */

public abstract class Paintable extends JspHelper implements HtmlBootstrap {


  public static final String ACTION = "JSPACT";
  public static final String INITIALIZE = "IN";
  public static final String DRAW_FOLIO = "DFL";
  public static final String DRAW_LABEL = "DLB";
  public static final String DRAW_LINE = "DLN";
  public static final String DRAW_VLINE = "DVL";
  public static final String DRAW_HLINE = "DHL";
  public static final String DRAW_DOT = "DDT";
  public static final String DRAW_RECTANGLE = "DRT";
  public static final String DRAW_IMAGE = "DRI";
  public static final String DRAW_LINK = "DLI";
  public static final String DRAW_GROUP = "DGR";
  public static final String DRAW_PERCENT = "DPRCT";
  public static final String DRAW_ROUNDED = "DRRDB";

  abstract String getPaintActionName();

  public double top = 0;
  public double left = 0;
  public String style;
  public String color = "#000000";
  public String lineStyle = "solid";
  public int lineSize = 1;
  public Folio folio;
  public String additionalOnClickScript;
  public String script;
  public boolean nowrap = true;
  public String htmlClass="";


  public void toHtml(PageContext pageContext) {
    pageContext.getRequest().setAttribute(Paintable.ACTION, getPaintActionName());

    urlToInclude = "/commons/layout/partPaintable.jsp";
    super.toHtml(pageContext);
  }

  public Paintable() {
    super();
  }


  public void bringToFront() {
    if (folio == null)
      throw new PlatformRuntimeException("Add the object to folio BEFORE, then call bringToFront.");
    folio.bringToFront(this);
  }

  public void sendToBack() {
    if (folio == null)
      throw new PlatformRuntimeException("Add the object to folio BEFORE, then call sendToBack.");
    folio.sendToBack(this);
  }

  public String getDiscriminator() {
    return Paintable.class.getName();
  }


  public boolean validate(PageState pageState) {
    return true;
  }


}
