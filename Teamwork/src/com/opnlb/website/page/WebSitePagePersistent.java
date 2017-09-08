package com.opnlb.website.page;

import com.opnlb.website.content.Content;
import com.opnlb.website.template.Template;
import org.jblooming.ontology.Node;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.SecuredNodeWithAreaSupport;
import org.jblooming.ontology.SecuredSupportWithArea;
import org.jblooming.operator.Operator;
import org.jblooming.security.Permission;
import org.jblooming.security.Role;
import org.jblooming.security.SecurableWithArea;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.settings.ApplicationState;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * WebSitePagePersistent (c) 2005 - Open Lab - www.open-lab.com
 */
public abstract class WebSitePagePersistent extends SecuredSupportWithArea {

  private String frontOfficeTitle;  // name viewed in navigation menu
  private String name;              // name viewed in admin IT MUST BE UNIQUE
  private String relativeUrl;
  private String description;       // "internal" description
  private String metaKeywords;      // the metatag metaKeywords
  private Template defaultTemplate;
  private boolean active = true;
  private boolean customizable = true;
  private Operator owner;

  private Set<Permission> permissions = new HashSet<Permission>();
  private String permissionIds;

  private Set<Content> contents = new HashSet<Content>();

  public static final String WEBSITEPAGE = "WSPG";


  public String getRelativeUrl() {
    return relativeUrl;
  }

  public void setRelativeUrl(String relativeUrl) {
    this.relativeUrl = relativeUrl;
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

  public Template getDefaultTemplate() {
    return defaultTemplate;
  }

  public void setDefaultTemplate(Template defaultTemplate) {
    this.defaultTemplate = defaultTemplate;
  }

  public boolean isCustomizable() {
    return customizable;
  }

  public void setCustomizable(boolean customizable) {
    this.customizable = customizable;
  }

  /*
  * SECURABLE
  */
  public Operator getOwner() {
    return owner;
  }

  public void setOwner(Operator operator) {
    this.owner = operator;
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

  public Set getPermissions() {
    return permissions;
  }

  public void setPermissions(Set permissions) {
    this.permissions = permissions;
  }

  public boolean containsPermission (Permission p) {
    return permissions.contains(p);
  }


  public boolean hasPermissionFor(Permission p) {
    if (permissionIds==null)
      return false;
    if (permissions == null)
      refreshPermissionIds();
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

  public String getPermissionIds() {
    return permissionIds;
  }

  public void setPermissionIds(String permissionIds) {
    this.permissionIds = permissionIds;
    refreshPermissions();
  }

  private void refreshPermissions() {
    permissions = new HashSet();
    if (JSP.ex(permissionIds)) {
      Set<String> ps = StringUtilities.splitToSet(permissionIds, "|");
      for (String s : ps) {
//        if (JSP.ex(s)) {
//          Permission o = ApplicationState.getPermissions().get(s);
//          if (o!=null)
//            permissions.add(o);
//        }
          permissions.add(new Permission(s));
      }
    }
  }

  public Set<Content> getContents() {
    return contents;
  }

  private void setContents(Set<Content> contents) {
    this.contents = contents;
  }

  public boolean isActive() {
    return active;
  }

  public void setActive(boolean active) {
    this.active = active;
  }

  public String getFrontOfficeTitle() {
    return frontOfficeTitle;
  }

  public void setFrontOfficeTitle(String frontOfficeTitle) {
    this.frontOfficeTitle = frontOfficeTitle;
  }

  public String getMetaKeywords() {
    return metaKeywords;
  }

  public void setMetaKeywords(String metaKeywords) {
    this.metaKeywords = metaKeywords;
  }
}
