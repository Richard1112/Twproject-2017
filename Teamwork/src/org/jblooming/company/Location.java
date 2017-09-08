package org.jblooming.company;

import org.jblooming.anagraphicalData.AnagraphicalData;
import org.jblooming.ontology.IdentifiableSupport;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class Location extends IdentifiableSupport {

  private String code;
  private String name;
  private AnagraphicalData anagraphicalData;

  public String getCode() {
    return code;
  }

  public void setCode(String code) {
    this.code = code;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public AnagraphicalData getAnagraphicalData() {
    return anagraphicalData;
  }

  public void setAnagraphicalData(AnagraphicalData anagraphicalData) {
    this.anagraphicalData = anagraphicalData;
  }
}
