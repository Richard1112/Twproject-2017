package org.jblooming.waf;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.SecuredLoggableHideableSupport;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.operator.Operator;
import org.jblooming.operator.User;
import org.jblooming.security.Permission;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.button.ButtonJS;
import org.jblooming.waf.html.core.JspIncluder;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.PageContext;
import java.io.IOException;
import java.util.*;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: May 24, 2007
 * Time: 10:24:55 AM
 */

public class PageQuark extends SecuredLoggableHideableSupport implements JspIncluder {
  
  private String name;
  private String description;
  private PersistentFile file;
  private int pixelWidth = 0;
  private int pixelHeight = 0;
  private boolean scrollbar = true;
  private boolean installed = true;

  protected Set<Permission> permissions = new HashSet<Permission>();
  private String permissionIds;
  
  private SerializedMap<String, String> params = new SerializedMap();
  public static final String init = PageQuark.class.getName();
  public static final String PORTLET = "PRLT";
  public static final String FLD_PT_PARAM_KEY_ = "PT_PARAM_KEY_";
  /**
   * FLD_URL of the helper jsp that layouts the html to be displayed using <code>this</code>.
   */
  public String urlToInclude;

  public void init(PageContext pageContext)  {
    PageState ps = PageState.getCurrentPageState(pageContext);
    if (!ps.initedElements.contains(init)) {
      pageContext.getRequest().setAttribute(ACTION, INITIALIZE);
      try {
        toHtml(pageContext);
      } catch (IOException e) {
        throw new PlatformRuntimeException(e);
      } catch (ServletException e) {
        throw new PlatformRuntimeException(e);
      }
      ps.initedElements.add(init);
    }
  }

  public void toHtml(PageContext pageContext) throws IOException, ServletException {
    Stack stack = getStack(pageContext.getRequest());
    stack.push(this);
    try {
      //pageContext.include(this.urlToInclude);
      pageContext.include(this.urlToInclude!=null ? this.urlToInclude : "/"+this.getFile().getFileLocation());
    } finally {
      stack.pop();
    }
  }

  protected Stack getStack(ServletRequest request) {
    Stack stack = (Stack) request.getAttribute(MAIN_OBJECT_STACK);
    if (stack == null) {
      stack = new Stack();
      request.setAttribute(MAIN_OBJECT_STACK, stack);
    }
    return stack;
  }

  public static JspIncluder getCurrentInstance(HttpServletRequest request) {
    Stack stack = (Stack) request.getAttribute(MAIN_OBJECT_STACK);
    if (stack == null) {
      stack = new Stack();
      request.setAttribute(MAIN_OBJECT_STACK, stack);
    }
    return (JspIncluder) stack.peek();
  }

  /**
   * getters and setters
   */
  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public PersistentFile getFile() {
    return file;
  }

  public void setFile(PersistentFile file) {
    this.file = file;
  }

  @Deprecated
  /**
   * @deprecated
   */
  public boolean getScrollbar() {
    return scrollbar;
  }

  @Deprecated
  /**
   * @deprecated
   */
  public void setScrollbar(boolean scrollbar) {
    this.scrollbar = scrollbar;
  }

  public boolean getInstalled() {
    return installed;
  }

  public void setInstalled(boolean installed) {
    this.installed = installed;
  }

  /**
   * SECURABLE
   */
  public boolean hasPermissionFor(User u, Permission p) {
    return u.hasPermissionFor(p);
  }

  public int getPixelWidth() {
    return pixelWidth;
  }

  public void setPixelWidth(int pixelWidth) {
    this.pixelWidth = pixelWidth;
  }

  public int getPixelHeight() {
    return pixelHeight;
  }

  public void setPixelHeight(int pixelHeight) {
    this.pixelHeight = pixelHeight;
  }

  /**
   * PERMISSIONS
   */
  public void addPermission(Permission p) {
    permissions.add(p);
    refreshPermissionIds();
  }

  public void removePermission(Permission p) {
    if (getPermissions() != null) {
      getPermissions().remove(p);
      refreshPermissionIds();
    }
  }

  public Set<Permission> getPermissions() {
    return permissions;
  }

  public void setPermissions(Set<Permission> permissions) {
    this.permissions = permissions;
  }

  public boolean containsPermission (Permission p) {
    return permissions.contains(p);
  }

  protected void refreshPermissionIds() {
    StringBuffer sb = new StringBuffer(512);
    for (Iterator<Permission> iterator = permissions.iterator(); iterator.hasNext();) {
      Permission permission = iterator.next();
      sb.append(permission.getName());
      if (iterator.hasNext())
        sb.append('|');
    }
    permissionIds = sb.toString();
  }

  private void refreshPermissions() {
    permissions = new HashSet();
    if (permissionIds != null && permissionIds.trim().length() > 0) {
      Set<String> ps = StringUtilities.splitToSet(permissionIds, "|");
      for (String s : ps) {
        /*Permission perm = ApplicationState.getPermissions().get(s);
        if (perm!=null)
          permissions.add(perm);*/
        permissions.add(new Permission(s));
      }
    }
  }

  public String getPermissionIds() {
    return permissionIds;
  }

  public void setPermissionIds(String permissionIds) {
    this.permissionIds = permissionIds;
    refreshPermissions();
  }

  public SerializedMap<String, String> getParams() {
    if (params!=null)
      return params;
    else
      return new SerializedMap<String, String>();
  }

  public void setParams(SerializedMap<String, String> parameters) {
    this.params = parameters;
  }


  /**
   * can the logged operator see this pageQuark?
   *
   * @param logged if is null return true only when there is no permission required
   * @return true when the logged operator have at least one permission required from the pageQuark. If there is no permissions required return true.
   */
  public boolean isVisibleFor(Operator logged) {
    boolean ret = false;
    if (permissions.size() > 0) {
      if (logged != null) {

        for (Permission p : permissions) {
          if (logged.hasPermissionFor(p)) {
            ret = true;
            break;
          }
        }
      }
    } else {
      ret = true;
    }
    return ret;
  }


  public boolean isVisibleInThisContext(PageState pageState) {
    boolean ret = false;
    Operator logged = null;
    logged = pageState.getLoggedOperator();
    ret = isVisibleFor(logged);
    return ret;
  }

   public  PageSeed getPageSeedForPlugin(PageState pageState) {
    PageSeed printCustom = new PageSeed(ApplicationState.contextPath+getFile().getFileLocation());
    printCustom.mainObjectId = pageState.mainObjectId;
    printCustom.setCommand(Commands.EDIT);
    printCustom.setPopup(true);
    return printCustom;
  }


  public ButtonSupport getPluginLinkButton(PageState pageState){
    PageSeed seedForPlugin = getPageSeedForPlugin(pageState);
    boolean isPopup = seedForPlugin.isPopup();
    ButtonSupport bl;

    if (pixelHeight>0 || pixelWidth>0 ) {
      bl = ButtonLink.getBlackInstance(I18n.get(getName()), pixelHeight > 0 ? pixelHeight : 600, pixelWidth > 0 ? pixelWidth : 800, seedForPlugin);
    }
    else {
      bl = new ButtonLink(I18n.get(getName()), seedForPlugin);
      bl.target="_blank";
    }
    return bl;
  }

  public ButtonSupport getPluginJSButton(){
    ButtonSupport bl;

    if (pixelHeight>0 || pixelWidth>0 ) {
      pixelHeight =pixelHeight > 0 ? pixelHeight : 600;
      pixelWidth = pixelWidth > 0 ? pixelWidth : 800;
      bl = new ButtonJS(I18n.get(getName()), "generatePluginLink('"+ApplicationState.contextPath+getFile().getFileLocation()+"','"+pixelHeight+"','"+pixelWidth+"' )");
    }
    else {
      bl = new ButtonJS(I18n.get(getName()),"generatePluginLink('"+ApplicationState.contextPath+getFile().getFileLocation()+"')");
      bl.target="_blank";
    }
    return bl;
  }

   public  String getHrefForInclude() {
     return getFile().getFileLocation();
   }


}
