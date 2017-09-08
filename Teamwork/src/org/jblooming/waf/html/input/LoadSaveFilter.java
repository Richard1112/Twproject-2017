package org.jblooming.waf.html.input;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.view.PageState;

import javax.servlet.jsp.PageContext;
import javax.servlet.ServletException;
import java.io.IOException;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class LoadSaveFilter extends JspHelper implements HtmlBootstrap {

  public String category;
  public String label;
  public Form form;

  public boolean drawButtons =true; 
  public boolean drawEditor =true;

  public LoadSaveFilter( String category, Form form) {
    this(null, null, category, form);
  }

  public LoadSaveFilter(String label, String category, Form form) {
    this(null, label, category, form);
  }

  @Deprecated
  public LoadSaveFilter(String id, String label, String category, Form form) {
    super("/commons/layout/loadSaveFilter/partLoadSaveFilter.jsp");
    this.category = category;
    this.label = label;
    this.form = form;
    if (id != null)
      this.id = id;
  }


  public String getDiscriminator() {
    return this.getClass().getName();
  }

  public boolean validate(PageState pageState) throws IOException, ServletException {
    return true;
  }

  public void toHtml(PageContext pageContext) {
    if (drawButtons) {
      pageContext.getRequest().setAttribute(ACTION, "DRAW_BUTTONS");
      super.toHtml(pageContext);
    }
    if (drawEditor) {
      pageContext.getRequest().setAttribute(ACTION, "DRAW_EDITOR");
      super.toHtml(pageContext);
    }

  }

  public void drawEditor(PageContext pageContext) {
    pageContext.getRequest().setAttribute(ACTION, "DRAW_EDITOR");
    super.toHtml(pageContext);
  }

  public void drawButtons(PageContext pageContext) {
    pageContext.getRequest().setAttribute(ACTION, "DRAW_BUTTONS");
    super.toHtml(pageContext);
  }

}