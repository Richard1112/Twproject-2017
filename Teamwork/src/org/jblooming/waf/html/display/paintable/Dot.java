package org.jblooming.waf.html.display.paintable;


/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class Dot extends Paintable {


  public Dot(double top, double left) {
    super();
    this.top = top;
    this.left=left;
  }


  public String getPaintActionName() {
    return DRAW_DOT;
  }



}

