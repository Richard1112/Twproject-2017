package org.jblooming.ontology;

import org.jblooming.security.*;
import org.jblooming.operator.Operator;
import org.jblooming.operator.User;
import org.jblooming.security.SecurityException;

import javax.persistence.Transient;
import javax.persistence.MappedSuperclass;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Oct 18, 2007
 * Time: 10:14:42 AM
 */
@MappedSuperclass
public abstract class SecuredLoggableSupport extends LoggableIdentifiableSupport implements Securable {

  private Operator owner;

 /**
   * default implementation; should be extended with the business logic
   */
  public boolean hasPermissionFor(User u, Permission p) {
    if (getOwner() != null && getOwner().equals(u))
      return true;
    else
      return u.hasPermissionFor(p);
  }

  // notice that these cannot be persisted here as we want to specify fk and idx which are specific to the implementation
  //@ManyToOne(targetEntity = Operator.class)
  //@JoinColumn(name="ownerx")
  @Transient
  public Operator getOwner() {
    return owner;
  }

  public void setOwner(Operator owner) {
    this.owner = owner;
  }

  public void testPermission(User u, Permission p) throws SecurityException {
    SecurityException.riseExceptionIfNoPermission(hasPermissionFor(u, p), p, this);
  }

}
