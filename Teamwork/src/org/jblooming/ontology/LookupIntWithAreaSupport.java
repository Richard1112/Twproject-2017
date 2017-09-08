package org.jblooming.ontology;

import org.jblooming.security.Area;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Mar 8, 2007
 * Time: 7:03:29 PM
 */
public class LookupIntWithAreaSupport extends LookupIntSupport implements LookupWithArea{

  private Area area;

  public Area getArea() {
    return area;
  }

  public void setArea(Area area) {
    this.area = area;
  }
}
