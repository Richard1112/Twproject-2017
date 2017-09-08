package com.twproject.task;

import org.jblooming.ontology.LookupIntWithAreaSupport;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;

import java.io.Serializable;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class IssueImpact extends LookupIntWithAreaSupport {

  public static IssueImpact load(Serializable id) throws FindByPrimaryKeyException {
    return (IssueImpact) PersistenceHome.findByPrimaryKey(IssueImpact.class, id);
  }

}
