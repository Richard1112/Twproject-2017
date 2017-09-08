package org.jblooming.waf.html.display.paintable;

import org.jblooming.PlatformRuntimeException;

import javax.servlet.jsp.PageContext;


/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */

public class Link extends Paintable {
  public Rectangle rectFrom;
  public Rectangle rectTo;
  public double peduncolusSize = 0;
  public String color;
  public String lineStyle;
  public int lineSize;
  public String linkTooltip;
  public String linkLabel;


  public Link(){
  }
  
  public String getPaintActionName() {
    return (DRAW_LINK);
  }

  public Link(Rectangle from, Rectangle to) {
    this(from, to, 0, null, null, 1);
  }

  public Link(Rectangle from, Rectangle to, double peduncolusSize, String color, String lineStyle, int lineSize) {
    this(from, to, peduncolusSize, color, lineStyle, lineSize, null, null);
  }

  public Link(Rectangle from, Rectangle to, double peduncolusSize, String color, String lineStyle, int lineSize, String linkTooltip, String linkLabel) {
    if (from!=null && to!=null ){
    rectFrom = from;
    rectTo = to;
    this.peduncolusSize = peduncolusSize;
    this.color = color;
    this.lineStyle = lineStyle;
    this.lineSize = lineSize;
    this.linkTooltip = linkTooltip;
    this.linkLabel = linkLabel;
    } else {
      throw new PlatformRuntimeException("Cannot link 'null' rectangles.");
    }

  }

  public void toHtml(PageContext pageContext) {

    // befor to paint it

    String col = color == null ? "#000000" : color;
    String lst = lineStyle == null ? "solid" : lineStyle;
    int lsi = lineSize <= 0 ? 1 : lineSize;

    double currentX = rectFrom.left + rectFrom.width;
    double currentY = rectFrom.height / 2 + rectFrom.top;

    boolean useThreeLine = (currentX+2*peduncolusSize) < rectTo.left;

    if (!useThreeLine) {
      // L1
      if (peduncolusSize > 0) {
        HLine l1 = new HLine(peduncolusSize, currentY, currentX);
        l1.toolTip = linkTooltip;
        currentX = currentX + peduncolusSize;
        setComEFattUnBestio(l1, col, lst, lsi, linkTooltip);
        l1.folio = folio;
        l1.toHtml(pageContext);
      }

      // L2
      double l2_4size = ((rectTo.top + rectTo.height / 2) - (rectFrom.top + rectFrom.height / 2)) / 2;
      VLine l2;
      if (l2_4size < 0) {
        l2 = new VLine(-l2_4size, currentY + l2_4size, currentX);
      } else {
        l2 = new VLine(l2_4size, currentY, currentX);
      }
      setComEFattUnBestio(l2, col, lst, lsi, linkTooltip);
      l2.folio = folio;
      l2.toHtml(pageContext);
      currentY = currentY + l2_4size;

      // L3
      double l3size = rectFrom.left + rectFrom.width + peduncolusSize - (rectTo.left - peduncolusSize);
      currentX = currentX - l3size;
      HLine l3 = new HLine(l3size, currentY, currentX);
      setComEFattUnBestio(l3, col, lst, lsi, linkTooltip);
      l3.folio = folio;
      l3.toHtml(pageContext);

      // L4
      VLine l4;
      if (l2_4size < 0) {
        l4 = new VLine(-l2_4size, currentY + l2_4size, currentX);
      } else {
        l4 = new VLine(l2_4size, currentY, currentX);
      }
      setComEFattUnBestio(l4, col, lst, lsi, linkTooltip);
      l4.folio = folio;
      l4.toHtml(pageContext);

      currentY = currentY + l2_4size;

      // L5
      if (peduncolusSize > 0) {
        HLine l5 = new HLine(peduncolusSize, currentY, currentX);
        currentX = currentX + peduncolusSize;
        setComEFattUnBestio(l5, col, lst, lsi, linkTooltip);
        l5.folio = folio;
        l5.toHtml(pageContext);

      }
    } else {
      //L1
      double l1_3Size = (rectTo.left - currentX) / 2;
      HLine l1 = new HLine(l1_3Size, currentY, currentX);
      currentX = currentX + l1_3Size;
      setComEFattUnBestio(l1, col, lst, lsi, linkTooltip);
      l1.folio = folio;
      l1.toHtml(pageContext);

      //L2
      double l2Size = ((rectTo.top + rectTo.height / 2) - (rectFrom.top + rectFrom.height / 2));
      VLine l2;
      if (l2Size < 0) {
        l2 = new VLine(-l2Size, currentY + l2Size, currentX);
      } else {
        l2 = new VLine(l2Size, currentY, currentX);
      }
      setComEFattUnBestio(l2, col, lst, lsi, linkTooltip);
      l2.folio = folio;
      l2.toHtml(pageContext);

      currentY = currentY + l2Size;

      //L3
      HLine l3 = new HLine(l1_3Size, currentY, currentX);
      currentX = currentX + l1_3Size;
      setComEFattUnBestio(l3, col, lst, lsi, linkTooltip);
      l3.folio = folio;
      l3.toHtml(pageContext);

    }

  }

  private void setComEFattUnBestio(Paintable bestio, String color, String lineStyle, int lineSize, String toolTip) {
    bestio.color = color;
    bestio.lineStyle = lineStyle;
    bestio.lineSize = lineSize;
    bestio.toolTip = toolTip;
  }


}
