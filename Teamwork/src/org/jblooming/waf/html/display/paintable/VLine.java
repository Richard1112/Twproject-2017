package org.jblooming.waf.html.display.paintable;


/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */

public class VLine extends Paintable {
  public double height;

  public VLine(double height, double top, double left) {
    super();
    this.height = height;
    this.left = left;
    this.top = top;
  }

  public String getPaintActionName() {
    return(DRAW_VLINE);
  }


}
