package org.jblooming.waf.html.container;

import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.html.core.JspIncluder;

import java.util.List;


public class SideBar extends RibbonBar {

  public SideBar() {
    super();
    this.urlToInclude = "/commons/layout/partSideBar.jsp";
  }

  
  public void addButtonCouple(ButtonSupport b1, JspIncluder b2){
    addButton(new DoubleButton(b1,b2));
  }

}