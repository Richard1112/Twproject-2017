package org.jblooming.ontology;

import org.jblooming.security.SecurityException;
import org.jblooming.security.Permission;
import org.jblooming.security.Securable;
import org.jblooming.operator.User;
import org.jblooming.operator.Operator;

import javax.persistence.MappedSuperclass;
import javax.persistence.Transient;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
@MappedSuperclass
public abstract class SecuredLoggableHideableSupport extends HideableIdentifiableSupport implements Hideable, Loggable, Securable {

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

  public void testPermission(User u, Permission p) throws org.jblooming.security.SecurityException {
    SecurityException.riseExceptionIfNoPermission(hasPermissionFor(u, p), p, this);
  }

}
