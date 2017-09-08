package org.jblooming.security;

import org.jblooming.ontology.IdentifiableSupport;

import java.util.HashSet;
import java.util.Set;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class GroupRole extends IdentifiableSupport {

  
  private Group group;
  private Role role;

  public GroupRole() {
  }

  public GroupRole(Group group, Role role) {
    this.group = group;
    this.role = role;
  }

  public Group getGroup() {
    return group;
  }

  public void setGroup(Group group) {
    this.group = group;
  }

  public Role getRole() {
    return role;
  }

  public void setRole(Role role) {
    this.role = role;
  }

  public static Set<GroupRole> removeGroupRoleByMembers(Set<GroupRole> groupRoles, GroupRole groupRole) {
    Set<GroupRole> grs = new HashSet(groupRoles);
    for (GroupRole gr : groupRoles) {
      if (gr.getRole().equals(groupRole.getRole()) && gr.getGroup().equals(groupRole.getGroup())) {
        grs.remove(gr);
        break;
      }
    }
    return grs;
  }
}
