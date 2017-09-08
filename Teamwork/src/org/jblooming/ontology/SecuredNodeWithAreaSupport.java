package org.jblooming.ontology;

import org.jblooming.operator.User;
import org.jblooming.security.*;
import org.hibernate.search.annotations.IndexedEmbedded;

import java.io.Serializable;

/**
 * This class represents the Tree structure for many TW'objects<br>
 * Each node contains two different list: the children and the associated documents.<br>
 * The children list is used to create the "tree".<br>
 * The document list is used to contains document related to the node<br>
 * The security is implemented using CredentialWallet<br>
 * <br>
 *
 * @author Roberto Bicchierai & Pietro Polsinelli
 * @version 2 alpha<br>
 * @since JDK 1.4
 */
public abstract class SecuredNodeWithAreaSupport extends SecuredNodeSupport implements SecurableWithArea {

  protected Area area;


  public SecuredNodeWithAreaSupport() {
  }

  protected SecuredNodeWithAreaSupport(Serializable snId) {
    setId(snId);
  }

  @IndexedEmbedded
  public Area getArea() {
    return area;
  }

  public void setArea(Area area) {
    this.area = area;
  }

  public boolean hasPermissionFor(User u, Permission p) {


    if (getOwner() != null && getOwner().equals(u) || (u!=null && u.hasPermissionAsAdmin()))
      return true;

    boolean result = false;

    if (getArea()!=null)
      result = area.hasPermissionFor(u, p);

    if (!result) {
      SecuredNodeWithAreaSupport parent = (SecuredNodeWithAreaSupport) getParentNode();
      if (parent != null && (isInherit() || parent.isPropagate()))
      result = parent.hasPermissionFor(u, p);
    }
    return result;

  }



}