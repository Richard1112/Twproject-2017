package org.jblooming.security;

import org.jblooming.ontology.Identifiable;
import org.jblooming.operator.Operator;
import org.jblooming.operator.User;


public interface Securable {

  public boolean hasPermissionFor(User u, Permission p);
  public void testPermission(User u, Permission p) throws SecurityException;

  public Operator getOwner();
  public void setOwner(Operator operator);

}
