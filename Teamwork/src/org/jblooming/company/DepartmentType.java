package org.jblooming.company;

import org.jblooming.ontology.LookupStringSupport;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.FindException;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 1-apr-2005 : 11.43.53
 */
public class DepartmentType extends LookupStringSupport {

  public enum Code {COMPANY, DIVISION, UNIT }

  public static DepartmentType getDefaultDepartment(Code code) throws FindException {
    String hql = "from "+DepartmentType.class.getName() + " as dt where dt.stringValue = :dept";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("dept",code.toString());
    return (DepartmentType) oql.uniqueResult();
  }

}
