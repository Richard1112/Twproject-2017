package org.jblooming.flowork;

import org.jblooming.ontology.*;
import org.jblooming.utilities.StringUtilities;

import java.util.Set;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class Starter extends LoggableIdentifiableSupport {

  private String definitionName;
  private String swimlaneNames;

  public String getDefinitionName() {
    return definitionName;
  }

  public void setDefinitionName(String definitionName) {
    this.definitionName = definitionName;
  }

  public String getSwimlaneNames() {
    return swimlaneNames;
  }

  public void setSwimlaneNames(String swimlaneNames) {
    this.swimlaneNames = swimlaneNames;
  }

  public Set<String> getSwimlanes() {
    if (swimlaneNames != null && swimlaneNames.length() > 2)
      return StringUtilities.splitToSet(swimlaneNames.substring(1, swimlaneNames.length()), "*");
    else
      return null;
  }


}
