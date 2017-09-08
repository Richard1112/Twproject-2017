package org.jblooming.waf.html.display.paintable;

import org.jblooming.PlatformRuntimeException;

import javax.servlet.jsp.PageContext;


/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 * vertical link
 */

public class VLink extends Link{

  public VLink(Rectangle from, Rectangle to) {
    this(from, to, 0, null, null, 1);
  }

  public VLink(Rectangle from, Rectangle to, double peduncolusSize, String color, String lineStyle, int lineSize) {
    this(from, to, peduncolusSize, color, lineStyle, lineSize, null, null);
  }

  public VLink(Rectangle from, Rectangle to, double peduncolusSize, String color, String lineStyle, int lineSize, String linkTooltip, String linkLabel) {
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

    double currentX = rectFrom.left + rectFrom.width/2;
    double currentY = rectFrom.height + rectFrom.top;

    boolean useThreeLine = (currentY+2*peduncolusSize) < rectTo.top;

    if (!useThreeLine) {
      // L1
      if (peduncolusSize > 0) {
        VLine l1 = new VLine(peduncolusSize, currentY, currentX);
        l1.toolTip = linkTooltip;
        currentY = currentY + peduncolusSize;
        setComEFattUnBestio(l1, col, lst, lsi, linkTooltip);
        l1.folio = folio;
        l1.toHtml(pageContext);
      }

      // L2
      double l2size = (((rectTo.left + rectTo.width/ 2) - (rectFrom.left + rectFrom.width / 2)) / 4)*3;
      double l4size = (((rectTo.left + rectTo.width/ 2) - (rectFrom.left + rectFrom.width / 2)) / 4)*1;
      HLine l2;
      if (l2size < 0) {
        l2 = new HLine(-l2size, currentY, currentX+l2size);
      } else {
        l2 = new HLine(l2size, currentY, currentX);
      }
      setComEFattUnBestio(l2, col, lst, lsi, linkTooltip);
      l2.folio = folio;
      l2.toHtml(pageContext);
      currentX = currentX + l2size;

      // L3
      double l3size = rectFrom.top + rectFrom.height + peduncolusSize - (rectTo.top - peduncolusSize);
      currentY = currentY - l3size;
      VLine l3 = new VLine(l3size, currentY, currentX);
      setComEFattUnBestio(l3, col, lst, lsi, linkTooltip);
      l3.folio = folio;
      l3.toHtml(pageContext);

      // L4
      HLine l4;
      if (l4size < 0) {
        l4 = new HLine(-l4size, currentY, currentX+l4size);
      } else {
        l4 = new HLine(l4size, currentY, currentX);
      }
      setComEFattUnBestio(l4, col, lst, lsi, linkTooltip);
      l4.folio = folio;
      l4.toHtml(pageContext);

      currentX = currentX + l4size;

      // L5
      if (peduncolusSize > 0) {
        VLine l5 = new VLine(peduncolusSize, currentY, currentX);
        currentY = currentY + peduncolusSize;
        setComEFattUnBestio(l5, col, lst, lsi, linkTooltip);
        l5.folio = folio;
        l5.toHtml(pageContext);

      }
    } else {
      //L1
      double l1Size = (rectTo.top - currentY-peduncolusSize) ;
      VLine l1 = new VLine(l1Size, currentY, currentX);
      currentY = currentY + l1Size;
      setComEFattUnBestio(l1, col, lst, lsi, linkTooltip);
      l1.folio = folio;
      l1.toHtml(pageContext);

      //L2
      double l2Size = ((rectTo.left + rectTo.width / 2) - (rectFrom.left + rectFrom.width / 2));
      HLine l2;
      if (l2Size < 0) {
        l2 = new HLine(-l2Size, currentY, currentX+l2Size);
      } else {
        l2 = new HLine(l2Size, currentY, currentX);
      }
      setComEFattUnBestio(l2, col, lst, lsi, linkTooltip);
      l2.folio = folio;
      l2.toHtml(pageContext);

      currentX = currentX + l2Size;

      //L3
      VLine l3 = new VLine(peduncolusSize, currentY, currentX);
      currentY = currentY + peduncolusSize;
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