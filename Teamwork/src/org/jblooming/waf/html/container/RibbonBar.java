package org.jblooming.waf.html.container;

import org.jblooming.waf.html.core.JspHelper;

import java.util.List;



public abstract class RibbonBar extends ButtonBar {

  public RibbonBar() {
    super();
    align="left";
  }

  public void addButtons(List<? extends JspHelper> bs) {
    buttonList.addAll(bs);
  }

 
}