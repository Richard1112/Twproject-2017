package org.jblooming.waf.html.container;

public class ButtonBox extends ButtonBar{
  public String title;
  public String boxClass="buttonBox";
  public boolean useButtonTextual=false; 


  public ButtonBox() {
    super();
    this.urlToInclude = "/commons/layout/partButtonBox.jsp";
  }


}
