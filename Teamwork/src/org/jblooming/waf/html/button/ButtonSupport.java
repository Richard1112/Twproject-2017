package org.jblooming.waf.html.button;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.settings.I18n;

import javax.servlet.jsp.PageContext;
import java.io.Serializable;

/**
 * @author Pietro Polsinelli : ppolsinelli@open-lab.com
 */
public abstract class ButtonSupport extends JspHelper{

  public String label;
  public String target;
  public String iconChar;

  public boolean hasFocus = false;
  public boolean enabled = true;
  public String style;
  public String additionalCssClass;

  public String width;

  public String outputModality = GRAPHICAL;
  public static String TEXT_ONLY = "TEXT_ONLY";
  public static String GRAPHICAL = "GRAPHICAL";


  /**
   * key control
   */
  private String actionListened;
  private int keyToHandle;
  private String launchedJsOnActionListened;
  public boolean checkCtrlKey=false;

  public boolean noPrint=true; //by default buttons are hidden when in print


  public ButtonSupport() {
    //this("partButtonResizable.jsp");
    this("partButton.jsp");
  }



  public ButtonSupport(String part) {
    this.urlToInclude = "/commons/layout/" + part;
  }

  public String getActionListened() {
    return actionListened;
  }

  public void setActionListened(String actionListened) {
    this.actionListened = actionListened;
  }

  public int getKeyToHandle() {
    return keyToHandle;
  }

  public void setKeyToHandle(int keyToHandle) {
    this.keyToHandle = keyToHandle;
  }

  public String getLaunchedJsOnActionListened() {
    return launchedJsOnActionListened;
  }

  public void setLaunchedJsOnActionListened(String launchedJsOnActionListened) {
    this.launchedJsOnActionListened = launchedJsOnActionListened;
  }


  public void addKeyPressControl(int keyToHandle, String actionListened) {
    this.setKeyToHandle(keyToHandle);
    this.setLaunchedJsOnActionListened(generateJs().toString());
    this.setActionListened(actionListened);
  }

  /**
   * Handling of DOM events relatively to this control
   *
   * @param keyToHandle    the int code of the key
   * @param launchedJs     on pressing the key, this js will be launched
   * @param actionListened the code is the event listened e.g. keypress
   */
  public void addKeyPressControl(int keyToHandle, String launchedJs, String actionListened) {
   addKeyPressControl(keyToHandle, launchedJs, actionListened, false);
  }

  public void addKeyPressControl(int keyToHandle, String launchedJs, String actionListened, boolean usingCtrl) {
    this.setKeyToHandle(keyToHandle);
    this.setLaunchedJsOnActionListened(launchedJs);
    this.setActionListened(actionListened);
    this.checkCtrlKey = usingCtrl;
  }

  public abstract StringBuffer generateJs();


  public void toHtml(PageContext pageContext) {
    this.addKeyPressControl(13, "onkeyup");
    super.toHtml(pageContext);
  }

  public String getId() {
    return id;
  }

  public void toHtmlInTextOnlyModality(PageContext pageContext) {
    this.outputModality = TEXT_ONLY;
    toHtml(pageContext);
  }

   public void toHtmlI18n(PageContext pageContext) {
     label = I18n.get(label);
     toHtml(pageContext);
  }


  public abstract void setMainObjectId(Serializable id);

  public abstract String generateLaunchJs();

  public abstract String getLabel();

}
