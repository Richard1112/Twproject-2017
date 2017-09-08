package org.jblooming.security;

import org.hibernate.search.annotations.Field;
import org.hibernate.search.annotations.Store;
import org.hibernate.search.annotations.Analyze;
import org.hibernate.search.annotations.Index;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.operator.Operator;
import org.jblooming.operator.OperatorAggregator;
import org.jblooming.operator.User;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.QueryException;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.constants.SecurityConstants;

import javax.persistence.Transient;
import java.util.Collection;
import java.util.HashSet;

public class Area extends IdentifiableSupport implements OperatorAggregator, Comparable {

  private String name;
  private Operator owner;

  public AreaBricks bricks = new AreaBricks(this);


  public static final String AREA = "AR";


  public Area() {
  }

  @Transient
  @Field(name = "id", analyze = Analyze.NO, index = Index.YES, store = Store.YES)
  public String getStringId() {
    return super.getId()+"";
  }

  /**
   * Method hasPermissionFor
   * Allows to check the <code>Permission</code> "p" for the <code>User</code> u on all the roles relative to this security area.
   * If <code>User</code> u is owner this rolls to true.
   *
   * @param u an User
   * @param p a  Permission
   * @return a boolean
   */
  public boolean hasPermissionFor(User u, Permission p) {

    if(u==null)
      return false;
    
    if ((owner != null && u.getId().equals(owner.getId())) || u.hasPermissionAsAdmin()) {
      return true;

    } else {
      for (Role role : u.getInheritedRoles()) {
        role = (Role) ReflectionUtilities.getUnderlyingObject(role);
        if (role instanceof RoleWithArea) {
          RoleWithArea rt = (RoleWithArea) role;
          if (this.equals(rt.getArea())) {
            if (role.hasPermissionFor(p))
              return true;
          }
        }
      }
    }
    return false;
  }


  /**
   * @param u
   * @param p
   * @throws java.lang.SecurityException
   */
  public void testPermission(User u, Permission p) throws org.jblooming.security.SecurityException {
    if (!hasPermissionFor(u, p))
      throw new SecurityException(SecurityConstants.I18N_PERMISSION_LACKING);
  }


  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public org.jblooming.operator.Operator getOwner() {
    return owner;
  }

  public void setOwner(org.jblooming.operator.Operator owner) {
    this.owner = owner;
  }

  public Collection getOperators()  {
    try {
      OqlQuery oql = new OqlQuery("from " + org.jblooming.operator.Operator.class.getName() + " as op " +
        "where op.area.id = :id ");
      oql.getQuery().setInteger("id", this.getIntId());
      return oql.list();
    } catch (PersistenceException e) {
      return new HashSet();
    }
  }

  /**
   * deprecated
   */
  public boolean isOperatorIn(org.jblooming.operator.Operator o) throws FindException, QueryException {
    return true;


  }

  public String toString() {
    return getId()+" "+getName()+" "+hashCode();
  }

}
