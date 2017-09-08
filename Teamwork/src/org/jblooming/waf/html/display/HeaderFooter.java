package org.jblooming.waf.html.display;

import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.view.PageState;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;
import java.io.IOException;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class HeaderFooter extends JspHelper implements HtmlBootstrap {

  private boolean headerCalled;
  private boolean footerCalled;
  public static String HEADER = "H";
  public boolean printBody = true;

  /**
   * if isPartFooter is true doesn't print debug info and so on. Should be used when a footer is printed on components (like SmartCombo in the iframe part)
   */
  public boolean isPartFooter=false;
  public boolean keepAsTop = false;
  public boolean includePlatformCss = true;

  /**
   * this is written in the head part below title; may be used for meta keywords
   */
  public String meta;

  /**
   * this is written in the head part below title; may be used for additional CSS file inclusion
   */
  public String css;


  public HeaderFooter(PageState pageState) {
    this(null, pageState);
  }

  public HeaderFooter(String id, PageState pageState) {
    this(id, "partHeaderFooter.jsp", pageState);
  }

  public HeaderFooter(String id, String urlToInclude, PageState pageState) {
    this.urlToInclude = "/commons/layout/" + urlToInclude;
    if (id != null)
      this.id = id;
    if (pageState!=null)
      pageState.htmlBootstrappers.add(this);
  }

  public void header(PageContext pageContext) {
    footerCalled = true;
    pageContext.getRequest().setAttribute(HEADER, Fields.TRUE);
    super.toHtml(pageContext);
  }

  public void footer(PageContext pageContext)  {
    headerCalled = true;
    pageContext.getRequest().setAttribute(HEADER, Fields.FALSE);
    super.toHtml(pageContext);
  }

  public boolean validate(PageState pageState)  {
    return footerCalled && headerCalled;
  }

  public String getId() {
    return id;
  }

  public String getDiscriminator() {
    return getId();
  }

  /**
   * @deprecated
   */
  public void toHtml(PageContext pageContext) {
    throw new RuntimeException("Call start and end");
  }


}
