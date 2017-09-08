package com.twproject.task.financial;

import com.twproject.resource.Person;
import org.jblooming.accounting.CenterCost;
import org.jblooming.ontology.Node;
import org.jblooming.operator.User;
import org.jblooming.security.Area;
import org.jblooming.security.Permission;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class CostAggregator extends CenterCost {

//  private String code;
//  private String description;
  private String type;
  private Person manager;
  private Area area;

  public Area getArea() {
    return area;
  }

  public void setArea(Area area) {
    this.area = area;
  }

  public CostAggregator() {
    super();
  }

  public Node getParentNode() {
    return getParent();
  }

  public void setParentNode(Node node) {
    setParent((com.twproject.task.financial.CostAggregator) node);
  }

  public void setParent(CostAggregator n) {
    parent = n;
  }

  private CostAggregator getParent() {
    return (CostAggregator) parent;
  }

  public boolean hasPermissionFor(User u, Permission p) {
    return false;
  }


/*  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }*/

  public String getType() {
    return type;
  }

  public void setType(String type) {
    this.type = type;
  }


 /* public String getCode() {
    return code;
  }

  public void setCode(String code) {
    this.code = code;
  }

  public String getName() {
    return code;
  }*/

  public Person getManager() {
    return manager;
  }

  public void setManager(Person manager) {
    this.manager = manager;
  }

}
