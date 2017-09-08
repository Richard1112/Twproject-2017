package org.jblooming.waf.html.display.paintable;


/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class Image extends Paintable {

  public String imageUrl;

  public Image(double top, double left, String imageUrl) {
    super();
    this.top = top;
    this.left=left;
    this.imageUrl=imageUrl;
  }


  public String getPaintActionName() {
    return DRAW_IMAGE;
  }



}

