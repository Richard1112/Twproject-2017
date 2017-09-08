/**
 * 
 */
package com.twproject.task;

import java.io.Serializable;

import org.jblooming.ontology.LookupStringWithAreaSupport;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.FindException;

import com.twproject.task.TaskType.Type;

/**
 * @author x-wang
 *
 */
public class DataType extends LookupStringWithAreaSupport {
	public static DataType load(Serializable id) throws FindByPrimaryKeyException {
		return (DataType) PersistenceHome.findByPrimaryKey(DataType.class, id);
	}

	public static DataType getDefaultTaskType(Type code) throws FindException {
		String hql = "from " + DataType.class.getName() + " as dt where dt.stringValue = :typ";
		OqlQuery oql = new OqlQuery(hql);
		oql.getQuery().setString("typ", code.toString());
		return (DataType) oql.uniqueResult();
	}

	public static DataType loadByCode(String code) {
		return (DataType) PersistenceHome.findUniqueNullIfEmpty(DataType.class, "stringValue", code);
	}

	public static String decode(String code) {
		DataType tt = DataType.loadByCode(code);
		if (tt != null)
			return tt.getDescription();
		else
			return "";

	}
}
