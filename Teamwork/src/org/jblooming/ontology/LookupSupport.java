package org.jblooming.ontology;

import javax.persistence.MappedSuperclass;
import javax.persistence.Transient;


/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
@MappedSuperclass
public abstract class LookupSupport extends LoggableIdentifiableSupport implements Lookup {

  private String description;

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  @Transient
  public String getName() {
    return getDescription();
  }

}
