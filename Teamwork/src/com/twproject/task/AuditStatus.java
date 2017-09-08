/**
 * 
 */
package com.twproject.task;

import java.io.Serializable;

import org.jblooming.ontology.LookupIntWithAreaSupport;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.FindException;

/**
 * @author x-wang
 *
 */
public class AuditStatus extends LookupIntWithAreaSupport {
	public static AuditStatus load(Serializable id) throws FindByPrimaryKeyException {
		return (AuditStatus) PersistenceHome.findByPrimaryKey(AuditStatus.class, id);
	}

	public static AuditStatus getDefaultAuditStatus(AuditStatus code) throws FindException {
		String hql = "from " + AuditStatus.class.getName() + " as dt where dt.intValue = :typ";
		OqlQuery oql = new OqlQuery(hql);
		oql.getQuery().setString("typ", code.toString());
		return (AuditStatus) oql.uniqueResult();
	}

	public static AuditStatus loadByCode(Integer code) {
		return (AuditStatus) PersistenceHome.findUniqueNullIfEmpty(AuditStatus.class, "intValue", code);
	}

}
