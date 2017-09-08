package org.jblooming.waf.html.button;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.utilities.JSP;

import javax.servlet.jsp.PageContext;
import java.io.IOException;


public class AHref extends JspHelper {
  public String href;
  public String label;
  public String target;


  public AHref(String label, String href) {
    this(label, href, null);
  }

  public AHref(String label, PageSeed ps) {
    this(label, ps.toLinkToHref(), null);
  }


  public AHref(String label, String href, String target) {
    this.href = href;
    this.label = label;
    this.target = target;
    this.urlToInclude = "NOTUSED";
  }

  public void toHtml(PageContext pageContext) {

    try {
      pageContext.getOut().println(getHtml());
    } catch (IOException e) {
    }
  }


  public String getHtml(){
    StringBuffer bu=new StringBuffer();

    bu.append("<a href=\"");

    if (!JSP.ex(href))
      bu.append("#");
    else
      bu.append(href);
    bu.append("\" id=\"" + id + "\"");

    if (JSP.ex(target))
      bu.append(" target=\"" + target + "\"");


    bu.append(">" + label + "</a>");

    return bu.toString();
  }


  public ButtonSupport getButton(){
    ButtonSupport ret;
    if (href.startsWith("javascript:")){
      ret= new ButtonJS(label,href.substring(11));
    } else {
      ret= new ButtonLink(label,new PageSeed(href));
    }
    return ret;
  }


}
