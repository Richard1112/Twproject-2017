package org.jblooming.waf.html.core;

import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.JSP;

import java.util.Map;


public class JspHelper extends JspIncluderSupport {

  public String id = DOM_ID + hashCode();

  public Map parameters = new HashTable();

  public String toolTip;


  public JspHelper() {
  }

  public JspHelper(String urlToInclude) {
    this.urlToInclude = urlToInclude;
  }

  public String getToolTip() {
   return JSP.htmlEncodeApexes(toolTip);
 }

  public String generateToolTip() {
   return (JSP.ex(getToolTip()) ? "title=\""+getToolTip()+"\"":"");
 }

  public void setToolTip(String toolTip) {
    this.toolTip = toolTip;
  }


}
