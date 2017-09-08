package org.jblooming.ontology;

import org.jblooming.security.*;
import org.jblooming.operator.User;
import org.jblooming.operator.Operator;
import org.hibernate.search.annotations.IndexedEmbedded;

import javax.persistence.MappedSuperclass;
import javax.persistence.ManyToOne;
import javax.persistence.JoinColumn;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
//@MappedSuperclass
public abstract class SecuredSupportWithArea extends SecuredLoggableHideableSupport implements SecurableWithArea {

  private Area area;

  //@ManyToOne(targetEntity = Area.class)
  //@JoinColumn(name="areax")
  @IndexedEmbedded
  public Area getArea() {
    return area;
  }

  public void setArea(Area area) {
    this.area = area;
  }

  public boolean hasPermissionFor(User u, Permission p) {

    if (getOwner() != null && getOwner().equals(u))
      return true;

    if (getArea() == null)
       return u.hasPermissionAsAdmin();

    return area.hasPermissionFor(u, p);

  }

}
