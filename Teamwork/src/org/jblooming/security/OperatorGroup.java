package org.jblooming.security;

import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.operator.Operator;

import java.util.Set;
import java.util.HashSet;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class OperatorGroup extends IdentifiableSupport {

  private Operator operator;
  private Group group;

  public OperatorGroup() {
  }

  public OperatorGroup(Operator operator, Group group) {
    this.operator=operator;
    this.group=group;
  }

  public Operator getOperator() {
    return operator;
  }

  public void setOperator(Operator operator) {
    this.operator = operator;
  }

  public Group getGroup() {
    return group;
  }

  public void setGroup(Group group) {
    this.group = group;
  }

  public static Set<OperatorGroup> removeOperatorGroups(Set<OperatorGroup> operatorGroups, OperatorGroup operatorGroup) {
    Set<OperatorGroup> sog = new HashSet(operatorGroups);
    for (OperatorGroup og : operatorGroups) {
      if (og.getOperator().equals(operatorGroup.getOperator())) {
        sog.remove(og);
        break;
      }
    }
    return sog;
  }
}
