package org.jblooming.waf.html.display.paintable;

import java.text.NumberFormat;
import java.util.Locale;


/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class Percentage extends Rectangle {

  public String label;
  public double percent;
  public String percentileColor ="#ff0000";
  public String fontHeight=null;


  public Percentage(double height, double width, double top, double left, double percent) {
    super (height,width,top,left);
    this.percent = percent;
    color = null;
    lineStyle = null;
  }

  public String getPaintActionName() {
  return (DRAW_PERCENT);
}



}
