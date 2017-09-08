package org.jblooming.waf;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.waf.settings.Application;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.PageContext;
import java.lang.reflect.Constructor;


public abstract class ScreenBasic extends ScreenRoot {

  public boolean showHeaderAndFooter = true;
  public ScreenArea menu;
  public ScreenArea partPathToObject;

  protected ScreenBasic() {
  }


  /**
   * @param body
   */
  public ScreenBasic(ScreenArea body) {
    super();
    this.setBody(body);
  }


  public String toString() {
    return super.toString() + "\nbody = " + body + "\nmenu = " + menu + "\npartPathToObject = " + partPathToObject + "\n\n";
  }

  public abstract void initialize(ScreenArea body);


  public static ScreenBasic getInstance(Application current, ScreenArea body, PageContext pageContext) {
    try {
      Constructor constructor = null;
      try {
        constructor = current.getDefaultScreenClass().getConstructor(ScreenArea.class, PageContext.class);
        return (ScreenBasic) constructor.newInstance(body, pageContext);
      } catch (NoSuchMethodException e) {
        return (ScreenBasic) current.getDefaultScreenClass().newInstance();
      }

    } catch (Exception e) {
      throw new PlatformRuntimeException(e);
    }

  }

  public static ScreenBasic preparePage(PageContext pageContext) {
    return preparePage(null, pageContext);
  }

  public static ScreenBasic preparePage(ActionController ac, PageContext pageContext) {
    HttpServletRequest request = (HttpServletRequest) pageContext.getRequest();
    final ScreenArea body = new ScreenArea(ac, request);
    PageState pageState = PageState.getCurrentPageState(request);
    pageState.screenRunning = true;
    ScreenBasic lw = ScreenBasic.getInstance(pageState.sessionState.getApplication(), body, pageContext);
    lw.initialize(body);
    lw.register(pageState);
    return lw;
  }

}
