package org.jblooming.waf.html.display.paintable;


/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class Label extends Rectangle {
  
  public String label;
  public boolean resizable = false;
  public String align="left";
  public String vAlign="top";


  public Label(double height, double width, double top, double left, String label) {
    super (height,width,top,left);
    this.label = label;
    color = null;
    lineStyle = null;
  }

  public String getPaintActionName() {
  return (DRAW_LABEL);
}



}
