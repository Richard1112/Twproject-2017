package org.jblooming.waf.html.display.paintable;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class Folio extends Paintable {

  // real dimension on screen
  public int pixelWidth = 800;
  // real dimension on screen
  public int pixelHeight = 600;
  // real left on screen
  public int pixelLeft = 0;
  // real top on screen
  public int pixelTop = 0;

  public double top=0d;
  public double left=0d;
  public double width = 800d;
  public double height = 600d;
  public List<Paintable> paintables = new ArrayList<Paintable>();
  public List<Group> groups = new ArrayList<Group>();

  public boolean showScroll=false;
  /**
   * when trimModality=true the paintable in the folio are trimme to the bounds
   */
  public boolean trimModality=false;

  public Folio() {
    super();
    lineSize = 0;
    lineStyle = null;
    color = null;

  }

  public Folio(int pixelHeight, int pixelWidth) {
    this();
    this.pixelHeight = pixelHeight;
    this.pixelWidth = pixelWidth;
  }

  public void add(Paintable paintable) {
    paintable.folio = this;
    paintables.add(paintable);
  }

  public void add(Group group) {
    group.folio = this;
    groups.add(group);
  }


  public String getPaintActionName() {
    return (DRAW_FOLIO);
  }


  public double getPixelH() {
    return this.width / this.pixelWidth;
  }

  public double getPixelV() {
    return this.height / this.pixelHeight;
  }


  
  public void bringToFront (Paintable p){
    int pos=paintables.indexOf(p);
    if (pos>0){
      paintables.remove(p);
      paintables.add(p);
    }
  }

  public void sendToBack (Paintable p){
    int pos=paintables.indexOf(p);
    if (pos>0){
      paintables.remove(p);
      paintables.add(0,p);
    }
  }



  public void link(Rectangle rectFrom, Rectangle rectTo, double peduncolusSize, String color, String lineStyle, int lineSize, String linkTooltip, String linkLabel) {
  String col = color == null ? "#000000" : color;
  String lst = lineStyle == null ? "solid" : lineStyle;
  int lsi = lineSize <= 0 ? 1 : lineSize;
  if (paintables.contains(rectFrom) && paintables.contains(rectTo)) {

    double currentX = rectFrom.left + rectFrom.width;
    double currentY = rectFrom.height / 2 + rectFrom.top;

    boolean useThreeLine = currentX < rectTo.left;

    if (!useThreeLine) {
      // L1
      if (peduncolusSize > 0) {
        HLine l1 = new HLine(peduncolusSize, currentY, currentX);
        l1.toolTip=linkTooltip;
        currentX = currentX + peduncolusSize;
        setComEFattUnBestio(l1,col,lst,lsi,linkTooltip);
        add(l1);
      }

      // L2
      double l2_4size = ((rectTo.top + rectTo.height / 2) - (rectFrom.top + rectFrom.height / 2)) / 2;
      VLine l2;
      if (l2_4size<0){
        l2 = new VLine(-l2_4size, currentY+l2_4size, currentX);
      } else{
        l2 = new VLine(l2_4size, currentY, currentX);
      }
      setComEFattUnBestio(l2,col,lst,lsi,linkTooltip);
      add(l2);
      currentY = currentY + l2_4size;

      // L3
      double l3size = rectFrom.left + rectFrom.width + peduncolusSize - (rectTo.left - peduncolusSize);
      currentX = currentX - l3size;
      HLine l3 = new HLine(l3size, currentY, currentX);
      setComEFattUnBestio(l3,col,lst,lsi,linkTooltip);
      add(l3);

      // L4
      VLine l4;
      if (l2_4size<0){
        l4 = new VLine(-l2_4size, currentY+l2_4size, currentX);
      } else{
        l4 = new VLine(l2_4size, currentY, currentX);
      }
      setComEFattUnBestio(l4,col,lst,lsi,linkTooltip);
      add(l4);
      currentY = currentY + l2_4size;

      // L5
      if (peduncolusSize > 0) {
        HLine l5 = new HLine(peduncolusSize, currentY, currentX);
        currentX = currentX + peduncolusSize;
        setComEFattUnBestio(l5,col,lst,lsi,linkTooltip);
        add(l5);
      }
    } else {
      //L1
      double l1_3Size = (rectTo.left - currentX) / 2;
      HLine l1 = new HLine(l1_3Size, currentY, currentX);
      currentX = currentX + l1_3Size;
      setComEFattUnBestio(l1,col,lst,lsi,linkTooltip);
      add(l1);

      //L2
      double l2Size = ((rectTo.top + rectTo.height / 2) - (rectFrom.top + rectFrom.height / 2));
      VLine l2;
      if (l2Size<0){
        l2 = new VLine(-l2Size, currentY+l2Size, currentX);
      } else{
        l2 = new VLine(l2Size, currentY, currentX);
      }
      setComEFattUnBestio(l2,col,lst,lsi,linkTooltip);
      add(l2);
      currentY = currentY + l2Size;

      //L3
      HLine l3 = new HLine(l1_3Size, currentY, currentX);
      currentX = currentX + l1_3Size;
      setComEFattUnBestio(l3,col,lst,lsi,linkTooltip);
      add(l3);

    }


  }
}

  private void setComEFattUnBestio (Paintable bestio, String color, String lineStyle, int lineSize, String toolTip){
    bestio.color=color;
    bestio.lineStyle=lineStyle;
    bestio.lineSize=lineSize;
    bestio.toolTip=toolTip;
  }


}
