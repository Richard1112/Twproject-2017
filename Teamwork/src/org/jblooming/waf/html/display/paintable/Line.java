package org.jblooming.waf.html.display.paintable;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class Line extends Paintable {

  public double height;
  public double width;


  public Line(double x, double y, double dx, double dy) {
    super();
    this.height = dy;
    this.width=dx;
    this.left = x;
    this.top = y;
  }

  public String getPaintActionName() {
    return(DRAW_LINE);
  }



}
