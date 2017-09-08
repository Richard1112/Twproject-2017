package org.jblooming.waf.html.display.paintable;


/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class HLine extends Paintable {
  public String id = "d" + hashCode() + "";

  public double width;

  public HLine(double width, double top, double left) {
    super();
    this.width = width;
    this.left = left;
    this.top = top;
  }

  public String getPaintActionName() {
  return( DRAW_HLINE);
}


}
