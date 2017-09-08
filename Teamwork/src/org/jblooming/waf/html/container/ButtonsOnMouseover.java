package org.jblooming.waf.html.container;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.core.JspIncluder;
import org.jblooming.waf.html.core.TextEmbedder;
import org.jblooming.waf.view.PageState;

import javax.servlet.jsp.PageContext;
import java.util.LinkedList;
import java.util.List;

public class ButtonsOnMouseover extends JspHelper {

public List<JspIncluder> buttonList = new LinkedList<JspIncluder>();
public JspIncluder opener;

public boolean openerInTextOnly=false;

  public ButtonsOnMouseover(JspIncluder rolloverElement) {
    this.opener = rolloverElement;
    this.urlToInclude = "/commons/layout/partButtonsOnMouseover.jsp";
  }


  public void addSeparator(){
    buttonList.add(new TextEmbedder("<hr>"));
  }

  public void addTextSeparator(String text){
    buttonList.add(new TextEmbedder(text));
  }

  public void addButton(JspIncluder element){
    buttonList.add(element);
  }

  public void addButtons(List<? extends JspHelper> bs) {
    buttonList.addAll(bs);
  }

  public void toHtml(PageContext pc) {
    if (buttonList.size()>0 ){
      pc.getRequest().setAttribute(ACTION, "DRAW");
      super.toHtml(pc);
    }
  }

  public void toHtmlInTextOnlyModality(PageContext pc) {
    openerInTextOnly=true;
    toHtml(pc);
  }

}
