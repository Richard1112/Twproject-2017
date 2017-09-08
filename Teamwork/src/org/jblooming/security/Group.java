package org.jblooming.security;

import org.jblooming.agenda.ScheduleSupport;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.operator.Operator;
import org.jblooming.operator.OperatorAggregator;
import org.jblooming.operator.User;
import org.jblooming.persistence.exceptions.StoreException;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 1-apr-2005 : 13.10.54
 */
public class Group extends IdentifiableSupport implements OperatorAggregator, Securable, SecurityCarrier {

  private Operator owner;

  private String description;
  private String ldapName;

  private boolean administrator;
  private boolean enabled = true;
  private ScheduleSupport enabledOnlyOn;

  // read only colls
  private Set<GroupRole> groupRoles = new HashSet<GroupRole>();
  private Set<OperatorGroup> operatorGroups = new HashSet<OperatorGroup>();
  private Set<GroupContainsGroup> children = new HashSet<GroupContainsGroup>();
  private Set<GroupContainsGroup> parents = new HashSet<GroupContainsGroup>();

  // flattened collections
  private Set<Role> inheritedRoles;
  private Set<Group> inheritedGroups;
  private Set<Operator> operators;



  public Iterator getOperatorGroupsIterator() {
    return getOperatorGroups().iterator();
  }

  public int operatorGroupsSize() {
    return getOperatorGroups().size();
  }

  private Set<GroupRole> getGroupRoles() {
    return groupRoles;
  }

  public int groupRolesSize() {
    return groupRoles.size();
  }

  public Iterator getChildrenIterator() {
    return children.iterator();
  }

  public int childrenSize() {
    return children.size();
  }

  public Iterator getParentsIterator() {
    return parents.iterator();
  }

  public int parentsSize() {
    return parents.size();
  }


  public Collection<Operator> getOperators() {
    if (operators == null) {

      operators = new HashSet<Operator>();

      for (OperatorGroup operatorGroup : getOperatorGroups()) {
        operators.add(operatorGroup.getOperator());
      }

      for (GroupContainsGroup gcg : children) {

        Collection<Operator> operators = gcg.getSlave().getOperators();
        if (operators!=null)
          this.operators.addAll(operators);
      }
    }
    return operators;
  }



  public boolean isOperatorIn(Operator o) {
    return getOperators().contains(o);
  }


  // -------------------------------- Securable implementation --------------------------------


  public boolean hasPermissionFor(User u, Permission p) {
    if (getOwner() != null && getOwner().equals(u))
      return true;
    else
      return u.hasPermissionFor(p);
  }

  public void testPermission(User u, Permission p) throws SecurityException {
    SecurityException.riseExceptionIfNoPermission(hasPermissionFor(u, p), p, this);
  }

  public String getDescription() {
    return description;
  }
  public String getName() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public boolean isEnabled() {
    return enabled;
  }

  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  public boolean isAdministrator() {
    return administrator;
  }

  public void setAdministrator(boolean administrator) {
    this.administrator = administrator;
  }

  public ScheduleSupport getEnabledOnlyOn() {
    return enabledOnlyOn;
  }

  public void setEnabledOnlyOn(ScheduleSupport enabledOnlyOn) {
    this.enabledOnlyOn = enabledOnlyOn;
  }

  public Operator getOwner() {
    return owner;
  }

  public void setOwner(Operator owner) {
    this.owner = owner;
  }

  public Set<Group> getInheritedGroups() {
    if (inheritedGroups == null) {
      inheritedGroups = new HashSet<Group>();
      for (GroupContainsGroup groupContainsGroup : getParents()) {
        Group master = groupContainsGroup.getMaster();
        if (!inheritedGroups.contains(master)) {
          inheritedGroups.add(master);
          inheritedGroups.addAll(master.getInheritedGroups());
        }
      }
    }
    return inheritedGroups;
  }

  public Set<Role> getInheritedRoles() {
    if (inheritedRoles == null) {
      inheritedRoles = new HashSet<Role>();
      for (GroupRole groupRole : getGroupRoles()) {
        Role role = groupRole.getRole();
        if (!inheritedRoles.contains(role)) {
          inheritedRoles.add(role);
        }
      }
      for (Group group : getInheritedGroups()) {
        inheritedRoles.addAll(group.getInheritedRoles());
      }
    }
    return inheritedRoles;
  }

   public Iterator<Role> getInheritedRoleIterator() {
    return getInheritedRoles().iterator();
  }

  public int rolesSize() {
    return getInheritedRoles().size();
  }


  public void addRoleAndPersist(Role role) throws StoreException {
    GroupRole gr = new GroupRole(this,role);
    groupRoles.add(gr);
    gr.store();
    inheritedRoles = null;
  }


  public void addOperatorAndPersist(Operator op1) throws StoreException {
    OperatorGroup og = new OperatorGroup(op1,this);
    getOperatorGroups().add(og);
    og.store();
  }

  public void addGroupAndPersist(Group group) throws StoreException {
    GroupContainsGroup gcg = new GroupContainsGroup(this,group);
    children.add(gcg);
    group.parents.add(gcg);
    gcg.store();
    group.inheritedGroups =null;
  }

  public String getLdapName() {
    return ldapName;
  }

  public void setLdapName(String ldapName) {
    this.ldapName = ldapName;
  }

  public Set<OperatorGroup> getOperatorGroups() {
    return operatorGroups;
  }

  private void setGroupRoles(Set<GroupRole> groupRoles) {
    this.groupRoles = groupRoles;
  }

  public Iterator<GroupRole> getGroupRolesIterator() {
    return this.groupRoles.iterator();
  }

  private void setOperatorGroups(Set<OperatorGroup> operatorGroups) {
    this.operatorGroups = operatorGroups;
  }


  private void setParents(Set<GroupContainsGroup> parents) {
    this.parents = parents;
  }

   private Set<GroupContainsGroup> getParents() {
    return parents;
  }

  private void setInheritedRoles(Set<Role> inheritedRoles) {
    this.inheritedRoles = inheritedRoles;
  }

  private void setInheritedGroups(Set<Group> inheritedGroups) {
    this.inheritedGroups = inheritedGroups;
  }

  private void setOperators(Set<Operator> operators) {
    this.operators = operators;
  }

  public Set<GroupContainsGroup> getChildren() {
    return children;
  }

  private void setChildren(Set<GroupContainsGroup> children) {
    this.children = children;
  }

  /**
   * remove roles from memory
   * @param groupRole
   */
  public void removeRoleFromMemory(GroupRole groupRole) {
    groupRoles.remove(groupRole);
  }

}
