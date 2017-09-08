package com.twproject.task.financial;

import org.jblooming.ontology.LookupStringWithAreaSupport;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class CostClassification extends LookupStringWithAreaSupport {

  public static CostClassification load(int id) throws FindByPrimaryKeyException {
    return (CostClassification) PersistenceHome.findByPrimaryKey(CostClassification.class, id);  //To change body of created methods use File | Settings | File Templates.
  }
}
