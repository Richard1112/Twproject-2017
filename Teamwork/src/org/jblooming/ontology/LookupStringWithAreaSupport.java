package org.jblooming.ontology;

import org.jblooming.security.Area;

import javax.persistence.MappedSuperclass;
import javax.persistence.Transient;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Mar 8, 2007
 * Time: 7:04:39 PM
 */
@MappedSuperclass
public class LookupStringWithAreaSupport extends LookupStringSupport implements LookupWithArea{
  private Area area;

  @Transient
  public Area getArea() {
    return area;
  }

  public void setArea(Area area) {
    this.area = area;
  }
}
