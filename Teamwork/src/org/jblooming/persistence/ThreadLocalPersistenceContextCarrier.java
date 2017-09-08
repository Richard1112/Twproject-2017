package org.jblooming.persistence;

import org.jblooming.operator.Operator;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.utilities.HashTable;

import java.util.Stack;
import java.util.Map;
import java.util.Iterator;
import java.util.Hashtable;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class ThreadLocalPersistenceContextCarrier {

  private Operator operator;
  public PersistenceContext currentPC;

  public Map<String,PersistenceContext> persistenceContextMap=new Hashtable();

  public ThreadLocalPersistenceContextCarrier() {
  }

  public Operator getOperator() {
    return operator;
  }

  public void setOperator(Operator operator) {
    this.operator = operator;
  }

  public PersistenceContext getPersistenceContext(String persistenceConfigurationName){
    return persistenceContextMap.get(persistenceConfigurationName);
  }

  public void putPersistenceContext(PersistenceContext persistenceContext){
    persistenceContextMap.put(persistenceContext.persistenceConfiguration.name,persistenceContext);
  }

}
