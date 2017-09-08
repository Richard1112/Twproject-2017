package org.jblooming.ontology;

import javax.persistence.MappedSuperclass;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Feb 2, 2007
 * Time: 5:03:48 PM
 */
@MappedSuperclass
public abstract class PerformantNodeBean extends HideableIdentifiableSupport {
  protected String ancestorIds;

  public PerformantNodeBean() {
    super();
  }

  public String getAncestorIds() {
    return ancestorIds;
  }

  public void setAncestorIds(String ancestorIds) {
    this.ancestorIds = ancestorIds;
  }
}
