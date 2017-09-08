package org.jblooming.waf.html.core;


import org.jblooming.waf.view.PageState;
import javax.servlet.jsp.PageContext;


public class OneTimeIncluder extends JspHelper {

  public OneTimeIncluder(String absoluteUrlToInclude) {
    this.urlToInclude = absoluteUrlToInclude;
  }

  public void toHtml(PageContext pageContext) {
    PageState ps = PageState.getCurrentPageState(pageContext);
    if (!ps.initedElements.contains(urlToInclude)) {
      ps.initedElements.add(urlToInclude);
      super.toHtml(pageContext);
    }
  }


}
