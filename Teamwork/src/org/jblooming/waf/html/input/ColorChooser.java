package org.jblooming.waf.html.input;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.view.PageState;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;
import java.io.IOException;


public class ColorChooser extends JspHelper  {

  public String fieldName;
  public String label;
  public String separator = "";
  /*public String palette = "[" +
    "        [\"#000\",\"#444\",\"#666\",\"#999\",\"#ccc\",\"#eee\",\"#f3f3f3\",\"#fff\"]," +
    "        [\"#f00\",\"#f90\",\"#ff0\",\"#0f0\",\"#0ff\",\"#00f\",\"#90f\",\"#f0f\"]," +
    "        [\"#f4cccc\",\"#fce5cd\",\"#fff2cc\",\"#d9ead3\",\"#d0e0e3\",\"#cfe2f3\",\"#d9d2e9\",\"#ead1dc\"]," +
    "        [\"#ea9999\",\"#f9cb9c\",\"#ffe599\",\"#b6d7a8\",\"#a2c4c9\",\"#9fc5e8\",\"#b4a7d6\",\"#d5a6bd\"]," +
    "        [\"#e06666\",\"#f6b26b\",\"#ffd966\",\"#93c47d\",\"#76a5af\",\"#6fa8dc\",\"#8e7cc3\",\"#c27ba0\"]," +
    "        [\"#c00\",\"#e69138\",\"#f1c232\",\"#6aa84f\",\"#45818e\",\"#3d85c6\",\"#674ea7\",\"#a64d79\"]," +
    "        [\"#900\",\"#b45f06\",\"#bf9000\",\"#38761d\",\"#134f5c\",\"#0b5394\",\"#351c75\",\"#741b47\"]," +
    "        [\"#600\",\"#783f04\",\"#7f6000\",\"#274e13\",\"#0c343d\",\"#073763\",\"#20124d\",\"#4c1130\"]" +
    "    ]";
  */
  public String palette="[[\"#464646\",\"#696969\",\"#8C8C8C\",\"#AFAFAF\",\"#D2D2D2\",\"#F5F5F5\"],[\"#CF0B0B\",\"#F22E2E\",\"#FF5151\",\"#FF7474\",\"#FF9797\",\"#FFBABA\"],[\"#FD9812\",\"#FFBB35\",\"#FFDE58\",\"#FFFF7B\",\"#FFFF9E\",\"#FFFFC1\"],[\"#18A106\",\"#3BC429\",\"#5EE74C\",\"#81FF6F\",\"#A4FF92\",\"#C7FFB5\"],[\"#0C40A3\",\"#2F63C6\",\"#5286E9\",\"#75A9FF\",\"#98CCFF\",\"#BBEFFF\"],[\"#9929ED\",\"#BC4CFF\",\"#DF6FFF\",\"#FF92FF\",\"#FFB5FF\",\"#FFD8FF\"]]";

  public int fieldSize = 3;
  public int colorSquareHeight = 22;
  public int colorSquareWidth = 22;
  public boolean showTextField=false;
  public boolean disabled=false;
  public boolean showOnlyPalette = true;
  public boolean showPalette = false;

  public ColorChooser(String fieldName) {
    this.urlToInclude = "/commons/layout/partColorChooser.jsp";
    this.fieldName = fieldName;

  }


  public String getDiscriminator() {
    return ColorChooser.class.getName();
  }

  public boolean validate(PageState pageState) throws IOException, ServletException {
    return true;
  }


  public void init(PageContext pageContext) {
    /*PageState ps = PageState.getCurrentPageState(pageContext);
    if (!ps.initedElements.contains(ColorValueChooser.class.getName())) {
      pageContext.getRequest().setAttribute(ACTION, INITIALIZE);
      super.toHtml(pageContext);
      ps.initedElements.add(ColorValueChooser.class.getName());
    }*/
  }

  public void toHtml(PageContext pageContext) {
    //init(pageContext);
    pageContext.getRequest().setAttribute(ACTION, "VAI");
    super.toHtml(pageContext);
  }


}
