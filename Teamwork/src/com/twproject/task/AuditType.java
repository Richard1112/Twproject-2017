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
public class AuditType extends LookupIntWithAreaSupport {
	public static AuditType load(Serializable id) throws FindByPrimaryKeyException {
		return (AuditType) PersistenceHome.findByPrimaryKey(AuditType.class, id);
	}

	public static AuditType getDefaultAuditType(AuditType code) throws FindException {
		String hql = "from " + AuditType.class.getName() + " as dt where dt.intValue = :typ";
		OqlQuery oql = new OqlQuery(hql);
		oql.getQuery().setString("typ", code.toString());
		return (AuditType) oql.uniqueResult();
	}

	public static AuditType loadByCode(String code) {
		return (AuditType) PersistenceHome.findUniqueNullIfEmpty(AuditType.class, "intValue", code);
	}

	public static String decode(String code) {
		AuditType tt = AuditType.loadByCode(code);
		if (tt != null)
			return tt.getDescription();
		else
			return "";

	}
}
