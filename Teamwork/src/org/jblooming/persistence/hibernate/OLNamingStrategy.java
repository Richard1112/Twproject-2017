package org.jblooming.persistence.hibernate;

import org.hibernate.cfg.DefaultNamingStrategy;
import org.hibernate.cfg.ImprovedNamingStrategy;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class OLNamingStrategy  extends DefaultNamingStrategy {

  public String tableName(String tableName) {
    if (tableName.startsWith("_"))
      return "olpl" + tableName;
    else if (tableName.startsWith("JBPM_"))
      return "flow" + tableName.substring("JBPM".length()).toLowerCase();
    else
      return tableName;
  }
}