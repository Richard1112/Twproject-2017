package org.jblooming.waf.html.display;

import org.jblooming.ontology.PersistentFile;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.html.input.HtmlElement;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.jsp.PageContext;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class Img extends HtmlElement {

  public boolean required = false;
  public String script;
  public String imageUrl;
  public String width;
  public String height;
  public String style;
  public String align = "";


  public Img(PersistentFile pf, String title) {
    PageSeed imgPs = pf.getPageSeed(false);
    imgPs.disableCache=false;
    this.imageUrl= imgPs.toLinkToHref();
    this.toolTip = title;
    this.translateToolTip = false;
  }


  public Img(String imageUrl, String title) {
    this.imageUrl = imageUrl;
    this.toolTip = title;
    this.translateToolTip = false;
  }

  public Img(String imageUrl, String title, String width, String heigth) {
    this.imageUrl = imageUrl;
    this.toolTip = title;
    this.width = width;
    this.height = heigth;
  }


  public StringBuffer toHtmlStringBuffer() {

    StringBuffer sb = new StringBuffer();

    sb.append("<img src=\"").append(imageUrl).append('\"');

    imgFill(sb);
    if (JSP.ex(align))
      sb.append(" align=\"").append(align).append('\"');

    if (disabled)
      sb.append(" style=\"filter: progid:DXImageTransform.Microsoft.Alpha(opacity=50); -moz-opacity:0.5\" ");

    //MUST be last otherwise ButtonSubmit.getPDFPrintButton gets sputtanated
    if (script != null)
      sb.append(' ').append(script);

    sb.append(">");

    return sb;
  }


  private StringBuffer imgFill(StringBuffer sb) {
    if (JSP.ex(width))
      sb.append(" width=\"").append(width).append("\" ");

    if (JSP.ex(height))
      sb.append(" height=\"").append(height).append("\" ");

    sb.append(" border=\"0\"");

    sb.append(" name=\"").append(id).append("\"");

    if (JSP.ex(style))
      sb.append(" style=\"").append(style).append("\" ");

    if (JSP.ex(toolTip)) {
      sb.append(" title=\"").append(JSP.ex(getToolTip()) ? toolTip : "").append("\"");
      sb.append(" alt=\"").append(JSP.ex(getToolTip()) ? toolTip : "").append("\"");
    }

    sb.append(" id=\"").append(id).append("\" ");

    return sb;
  }


  public void toHtml(PageContext pageContext) {
    PageState ps = PageState.getCurrentPageState(pageContext);
    fixPathAndTranslateTooltip(ps);
    super.toHtml(pageContext);
  }

  private void fixPathAndTranslateTooltip(PageState pageState) {
    if (!imageUrl.startsWith(ApplicationState.contextPath+"/img") && !imageUrl.startsWith("http:") && !imageUrl.startsWith("https:") && !imageUrl.contains("partUploaderView.jsp")) {
      imageUrl = ApplicationState.contextPath+"/img"+ (imageUrl.startsWith("/")?"":"/")+imageUrl;
    }

    if (translateToolTip) {
      toolTip = pageState.getI18n(toolTip);
      translateToolTip = false;
    }
  }


}
