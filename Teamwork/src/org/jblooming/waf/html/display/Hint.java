package org.jblooming.waf.html.display;

import org.jblooming.operator.Operator;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.settings.I18n;

import javax.servlet.jsp.PageContext;

public class Hint extends JspHelper implements HtmlBootstrap {


  public String message = null;
  public String placementSelector = null;
  public String type = null;
  public int width = 300;
  public int height = 200;

  public static final String init = Hint.class.getName();


  private Hint(String type, String placementSelector) {
    this(I18n.get(type), placementSelector, type);
  }

  private Hint(String message, String placementSelector, String type) {
    this.message = message;
    this.placementSelector = placementSelector;
    this.type = type;

    urlToInclude = "/commons/layout/hint/partHint.jsp";
  }

  public String getDiscriminator() {
    return init;
  }

  public boolean validate(PageState pageState) {
    return true;
  }


  public void toHtml(PageContext pageContext) {
    PageState ps = PageState.getCurrentPageState(pageContext);
    //si disegna solo se non fatto e non skippati tutti
    if (ps != null) {
      Operator logged = ps.getLoggedOperator();
      if (logged != null && (JSP.ex(logged.getOption("HINT_SKIP_ALL")) || JSP.ex(logged.getOption(type))))
        return;
    }

    pageContext.getRequest().setAttribute(ACTION, "DRAW");
    super.toHtml(pageContext);
  }

  public static class HintWriter {
    private boolean alreadyAdded = false;
    private Hint lastHint = null;

    public boolean addHint(Hint hint, boolean conditionToAdd, PageState pageState) {
      if (!conditionToAdd || alreadyAdded)
        return false;

      if (pageState != null) {
        Operator logged = pageState.getLoggedOperator();
        if (logged != null && (JSP.ex(logged.getOption("HINT_SKIP_ALL")) || JSP.ex(logged.getOption(hint.type))))
          return false;
      }

      lastHint = hint;
      alreadyAdded = true;
      return true;

    }


    public boolean addHint(String type, String placementSelector, int width, int height, boolean conditionToAdd, PageState pageState) {
      Hint hint = new Hint(type, placementSelector);
      hint.width = width;
      hint.height = height;
      return addHint(hint, conditionToAdd, pageState);
    }


    public void toHtml(PageContext pageContext) {
      if (lastHint != null)
        lastHint.toHtml(pageContext);
    }

  }


}