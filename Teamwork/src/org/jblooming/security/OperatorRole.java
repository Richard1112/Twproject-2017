package org.jblooming.security;

import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.operator.Operator;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class OperatorRole extends IdentifiableSupport {

  private Operator operator;
  private Role role;

  public OperatorRole() {
  }
 
  public Operator getOperator() {
    return operator;
  }

  public void setOperator(Operator operator) {
    this.operator = operator;
  }

  public Role getRole() {
    return role;
  }

  public void setRole(Role role) {
    this.role = role;
  }

}
