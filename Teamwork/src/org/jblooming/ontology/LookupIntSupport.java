package org.jblooming.ontology;

/**
 * LookupSupport still uses by default id as value; this handles cases where you have to carry a specific int as value
 *
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class LookupIntSupport extends LookupSupport implements LookupInt {

  private int intValue;
  
  public int getIntValue() {
    return intValue;
  }

  public void setIntValue(int intValue) {
    this.intValue = intValue;
  }

}
