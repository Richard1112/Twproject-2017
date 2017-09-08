package org.jblooming.waf.html.display.paintable;

import org.jblooming.waf.html.core.JspIncluder;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */

public class RoundedRectangle extends Paintable {
  public double height;
  public double width;
  public JspIncluder jspIncluder;
  public String templateImage;
  public int roundCurve=20;

  public RoundedRectangle(double height, double width, double top, double left,String templateImage,int roundCurve) {
    super();
    this.height = height;
    this.width = width;
    this.top = top;
    this.left = left;
    this.templateImage=templateImage;
    this.roundCurve=roundCurve;
  }


  public String getPaintActionName() {
    return( DRAW_ROUNDED);
  }
}
