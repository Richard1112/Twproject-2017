package org.jblooming.waf.html.display;

import org.jblooming.ApplicationException;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.view.PageState;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.PageContext;
import java.io.IOException;

public class PercentileDisplay extends JspHelper {

  public static final String init = PercentileDisplay.class.getName();

  public static final String PERC_DRAW = "PERC_DRAW";

  /**
   * size in pixel
   */
  public String height ="20px";
  public String width ="100px";

  public String backgroundColor ="#E5E5E5";
  public String percentileColor = null;
  public String percentileOverflowColor ="#FF002E";

  public double percentile;
  public int fractionDigits=2;

  public PercentileDisplay(double percentile) {
    this.urlToInclude = "/commons/layout/partPercentileDisplay.jsp";
    this.percentile = percentile;
  }

  public String getDiscriminator() {
    return id;
  }

}
