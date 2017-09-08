package org.jblooming.waf.html.container;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.core.JspIncluder;


public class Embedder extends JspHelper {

  public JspIncluder embeddInContainer;

  public Embedder(String absoluteUrlToInclude) {
    this.urlToInclude = absoluteUrlToInclude;
  }


}
