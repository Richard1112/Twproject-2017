package org.jblooming.security;

import org.jblooming.ontology.IdentifiableSupport;

import java.util.Set;
import java.util.HashSet;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class GroupContainsGroup extends IdentifiableSupport {

  private Group master;
  private Group slave;

   public GroupContainsGroup() {
   }

  public GroupContainsGroup(Group master, Group slave) {
    this.master=master;
    this.slave=slave;
  }

  public Group getMaster() {
    return master;
  }

  public void setMaster(Group master) {
    this.master = master;
  }

  public Group getSlave() {
    return slave;
  }

  public void setSlave(Group slave) {
    this.slave = slave;
  }
  public static Set<GroupContainsGroup> removeGroupContainsGroupByMembers(Set<GroupContainsGroup> groupContainsGroups, GroupContainsGroup groupContainsGroup) {
    Set<GroupContainsGroup> gcgs = new HashSet(groupContainsGroups);
    for (GroupContainsGroup gcg : groupContainsGroups) {
      if (gcg.getMaster().equals(groupContainsGroup.getMaster()) && gcg.getSlave().equals(groupContainsGroup.getSlave())) {
        gcgs.remove(gcg);
        break;
      }
    }
    return gcgs;
  }
}
