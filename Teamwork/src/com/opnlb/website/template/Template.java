package com.opnlb.website.template;

import com.opnlb.website.template.businessLogic.TemplateAction;
import org.jblooming.ApplicationException;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.SecuredLoggableHideableSupport;
import org.jblooming.operator.User;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.security.Permission;
import org.jblooming.security.PermissionCache;
import org.jblooming.security.Securable;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.core.JspIncluder;

import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.jsp.PageContext;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Stack;


public class Template extends SecuredLoggableHideableSupport implements JspIncluder, Securable, PermissionCache.PermissionCacheEnabled {

  private String name;
  private String description;
  private PersistentFile templateFile;
  private String jspFileLocation;

  private Map<String, String> aree = new HashMap();

  public String urlToInclude; //this.getFile().getFileLocation();

  public void toHtml(PageContext pageContext) throws IOException, ServletException {
    Stack stack = getStack(pageContext.getRequest());
    stack.push(this);
    try {
      pageContext.include(this.urlToInclude!=null ? this.urlToInclude : "/"+this.jspFileLocation);
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

  /**
   * CONSTRUCTORS
   */
  public Template () {
  }

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

  public PersistentFile getTemplateFile() {
    return templateFile;
  }

  public void setTemplateFile(PersistentFile templateFile) {
    this.templateFile = templateFile;
  }

  /**
   * SECURABLE
   */


  @Override
  public boolean hasPermissionForUnCached(User user, Permission permission) {
    //return u.hasPermissionFor(p);
    if (getOwner() != null && getOwner().equals(user))
      return true;
    else
      return user.hasPermissionFor(permission);
  }
  public boolean hasPermissionFor(User u, Permission p) {
    return PermissionCache.hasPermissionFor(u,this,p);
  }

  public static Template load(String mainObjectId) throws FindByPrimaryKeyException {
      return (Template) PersistenceHome.findByPrimaryKey(Template.class, mainObjectId);
  }

}