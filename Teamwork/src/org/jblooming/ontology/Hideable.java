package org.jblooming.ontology;

import java.util.Date;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 22-lug-2005 : 10.05.42
 */
public interface Hideable {
  boolean isHidden();

  void setHidden(boolean hidden);

  Date getHiddenOn();

  void setHiddenOn(Date hiddenOn);

  String getHiddenBy();

  void setHiddenBy(String hiddenBy);
}
