package org.jblooming.waf.html.button;

import org.jblooming.waf.html.core.UrlComposer;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.PlatformRuntimeException;

public class Link extends UrlComposer {

  public Link(PageSeed url) {
    super(url);
  }

  public StringBuffer toHtmlStringBuffer() {
    throw new PlatformRuntimeException("Do not use this method in Link");
  }
  
}
