package org.jblooming.waf.html.container;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.core.JspIncluder;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.button.ButtonJS;
import org.jblooming.waf.view.PageState;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

public class Container  extends JspHelper implements HtmlBootstrap{

  public static final String init = Container.class.getName();

  public static final String DEFAULT = "DEFAULT";
  public static final String COLLAPSED ="COLLAPSED";
  public static final String ICONIZED = "ICONIZED";
  public static final String MAXIMIZED ="MAXIMIZED";
  public static final String HIDDEN = "HIDDEN";

  public static final String BOX_START = "BOX_START";
  public static final String BOX_END = "BOX_END";
  public static final String BOX_INITIALIZE = "BOX_INITIALIZE";

  public boolean draggable = false;
  public boolean collapsable = false;
  public boolean resizable = false;
  public boolean closeable = false;
  public boolean iconizable=false;

  public boolean absolutePosition = false;
  public boolean centeredOnScreen = false;  //implies absolutePosition

  public boolean saveStatus=false;  // if true, save status, size, position on server

  public String containment=null; //jquery selector in case of draggable

  /**
   * Level of nestedness in page layout. If > 0, extends the style with postfix _[level]
   */
  public int level = 0;

  public String overflow = null;
  public int top = 0;
  public int left = 0;
  public String width = "100%";
  public String height = null;
  public String contentAlign;
  public String title;
  public String status = DEFAULT;
  private boolean closeContainerCalled;
  private boolean openContainerCalled;

  protected String css_postfix;
  /**
   * used to insert complex content on titlebar
   */
  public JspIncluder embeddedInTitle;

  /**
   * used to insert elements, as buttons, on titlebar' right side
   */
  public List<JspIncluder> titleRightElements = new LinkedList<JspIncluder>();


  /**
   * used to insert an element, as icon or buttons, on titlebar' left side
   */
  public JspIncluder icon;

  public String commandSuffix=""; // in case of sticky should be "_STICKY". is appended to every call to commandcontroller
  public String color=null;

  public Container() {
    this(null);
  }

  public Container(String id) {
    this(id, 0);
  }

  public Container(String id, int level) {
    this.urlToInclude = "/commons/layout/container/partContainer.jsp";
    if (id != null)
      this.id = id;
    this.level = level;

    if (id!=null)  // if id is specified by default save status on server
      saveStatus=true;

  }

  public void start(PageContext pageContext) {
    if (resizable)
      draggable = true;

    openContainerCalled = true;
    pageContext.getRequest().setAttribute(ACTION, BOX_START);
    super.toHtml(pageContext);
  }

  public void end(PageContext pageContext) {
    if (!openContainerCalled)
      throw new PlatformRuntimeException("Call start before end. Container id:" + id);
    closeContainerCalled = true;
    pageContext.getRequest().setAttribute(ACTION, BOX_END);
    super.toHtml(pageContext);
  }

  public String getId() {
    return id+"";
  }

  public String getDiscriminator() {
    return getId();
  }

  public boolean validate(PageState pageState) throws IOException, ServletException {
    return openContainerCalled && closeContainerCalled;
  }

  /**
   * @deprecated
   */
  public void toHtml(PageContext pageContext) {
    throw new PlatformRuntimeException("Call start and end");
  }


  public String getCssLevel() {
    return level != 0 ? "level_"+level : "";
  }


  public String getContainerId() {
    return getId();
  }

  public String getContainerTitleId() {
    return "cttitid_" + getId();
  }

  public String getContainerBodyId() {
    return "ctdivbdid_" + getId();
  }

  /**
   * @return a button JS thats open/close the container
   */

  public ButtonJS getOpenerButton(boolean placeNearButton) {
    ButtonJS bjs = new ButtonJS(getOpenerScript(placeNearButton));
    return bjs;
  }

  public String getOpenerScript(boolean placeNearButton){
    String script = "$('#" + getContainerId() + "').trigger('toggle');";
    if (placeNearButton)
      script+="nearBestPosition($(this),'" + getContainerId() + "');";
    else if (centeredOnScreen)
      script+="$('#"+getContainerId()+"').centerOnScreen();";
    return script;
  }

  public static Container getPopupInstance(String title) {
    Container container = new Container();
    container.title = title;
    container.status = Container.HIDDEN;
    container.width = "300px";
    container.draggable = true;
    return container;
  }



  public String getCssPostfix() {
    return JSP.w(css_postfix);
  }

  /**
   * @param css_postfix added as postfix to container css classes
   *                    Example: Css.containerContent+container.getCssPostfix()
   */
  public void setCssPostfix(String css_postfix) {
    this.css_postfix = css_postfix;
  }
}
