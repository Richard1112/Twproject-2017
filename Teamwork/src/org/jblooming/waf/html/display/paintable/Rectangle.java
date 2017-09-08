package org.jblooming.waf.html.display.paintable;

import org.jblooming.waf.html.core.JspIncluder;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */

public class Rectangle extends Paintable {
  public double height;
  public double width;
  public JspIncluder jspIncluder;
  public String backgroundColor;

  public Rectangle(double height, double width, double top, double left) {
    super();
    this.height = height;
    this.width = width;
    this.top = top;
    this.left = left;
  }


  public String getPaintActionName() {
    return( DRAW_RECTANGLE);
  }
}
