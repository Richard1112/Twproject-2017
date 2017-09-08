package org.jblooming.security;

import org.jblooming.ontology.LoggableIdentifiableSupport;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.logging.Auditable;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * @author Pietro Polsinelli, Roberto Bicchierai
 * @version 2 alpha
 * @since JDK 1.3.01 02
 */
public class Role extends LoggableIdentifiableSupport implements Comparable, Auditable {

  protected String name;
  private String description;
  private String permissionIds;

  private Set<Permission> permissions = new HashSet<Permission>();

  // read only collections
  private Set<OperatorRole> operatorRoles = new HashSet<OperatorRole>();
  private Set<GroupRole> groupRoles = new HashSet<GroupRole>();


  public Role() {
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public boolean hasPermissionFor(Permission p) {
    if (permissionIds == null)
      return false;
    if (permissions == null)
      refreshPermissionIds();
    return permissions.contains(p);
  }

  /**
   * add a permission to the permission's list
   *
   * @param p a  Permission
   */
  public void addPermission(Permission p) {
    permissions.add(p);
    refreshPermissionIds();
  }

  /**
   * remove a permission from the permission's list
   *
   * @param p permission to be removed
   */
  public void removePermission(Permission p) {
    if (getPermissions() != null) {
      getPermissions().remove(p);
      refreshPermissionIds();
    }
  }

  protected void refreshPermissionIds() {
    permissionIds = "";

    for (Permission permission : getPermissions()) {
      permissionIds = permissionIds + permission.getName() + "|";
    }
    if (permissionIds!=null && permissionIds.endsWith("|"))
      permissionIds = permissionIds.substring(0, permissionIds.length() - 1);
  }

  public Set<Permission> getPermissions() {
    return permissions;
  }

  public void setPermissions(Set<Permission> permissions) {
    this.permissions = permissions;
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

  public Iterator<OperatorRole> getOperatorsIterator() {
    return operatorRoles.iterator();
  }

  public int operatorsSize() {
    return operatorRoles.size();
  }

  public Iterator<GroupRole> getGroupIterator() {
    return groupRoles.iterator();
  }

  public int groupsSize() {
    return groupRoles.size();
  }

  private Set<OperatorRole> getOperatorRoles() {
    return operatorRoles;
  }

  private void setOperatorRoles(Set<OperatorRole> operatorRoles) {
    this.operatorRoles = operatorRoles;
  }

  private Set<GroupRole> getGroupRoles() {
    return groupRoles;
  }

  private void setGroupRoles(Set<GroupRole> groupRoles) {
    this.groupRoles = groupRoles;
  }

  public String getDisplayName() {
    return JSP.w(getName());
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }
}
