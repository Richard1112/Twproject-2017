package com.twproject.task;

import org.jblooming.ontology.LookupStringWithAreaSupport;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */


public class TaskType extends LookupStringWithAreaSupport {
  public enum Type {FEASIBILITY, PRODUCTION }

  public static TaskType getDefaultTaskType(Type code) throws FindException {
    String hql = "from "+TaskType.class.getName() + " as dt where dt.stringValue = :typ";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("typ",code.toString());
    return (TaskType) oql.uniqueResult();
  }

    public static TaskType load(String mainObjectId) throws FindByPrimaryKeyException {
        return (TaskType) PersistenceHome.findByPrimaryKey(TaskType.class, mainObjectId);
    }


  public static TaskType loadByCode(String code) {
    return (TaskType) PersistenceHome.findUniqueNullIfEmpty(TaskType.class, "stringValue", code);
  }


  public static String decode(String code)  {
    TaskType tt = TaskType.loadByCode(code);
    if (tt!=null)
      return tt.getDescription();
    else
      return "";

  }
}
