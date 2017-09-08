package org.jblooming.accounting;

import org.jblooming.ontology.SecuredNodeSupport;
import org.jblooming.ontology.Node;
import org.jblooming.operator.User;
import org.jblooming.security.Permission;


public class CenterCost  extends SecuredNodeSupport  {
  private String code;
  private String description;

  public Node getParentNode() {
   return getParent();
 }


 public void setParentNode(Node node) {
   setParent((CenterCost) node);
 }

  public void setParent(CenterCost n) {
   parent = n;
  }

 private CenterCost getParent() {
   return (CenterCost) parent;
 }

 public boolean hasPermissionFor(User u, Permission p) {
   return false;
 }

  public String getCode() {
    return code;
  }

  public void setCode(String code) {
    this.code = code;
  }

  public String getName() {
    return code;
  }
  
  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }
}
