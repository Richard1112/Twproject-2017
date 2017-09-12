/**
 * 
 */
package com.twproject.task;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.Type;
import org.hibernate.search.annotations.FieldBridge;
import org.hibernate.search.bridge.builtin.IntegerBridge;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.FindException;

/**
 * @author x-wang
 *
 */
@Entity
@Table(name = "twk_task_audit_type")
public class TaskAuditType extends IdentifiableSupport {

	private int intValue;
	private String name;

	@Override
	@Id
	@Type(type = "int")
	@FieldBridge(impl = IntegerBridge.class)
	public Serializable getId() {
		return super.getId();
	}

	public void setId(int id) {
		this.id = id;
	}

	public int getIntValue() {
		return intValue;
	}

	public void setIntValue(int intValue) {
		this.intValue = intValue;
	}

	@Override
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	public static TaskAuditType load(Serializable id) throws FindByPrimaryKeyException {
		return (TaskAuditType) PersistenceHome.findByPrimaryKey(TaskAuditType.class, id);
	}

	public static TaskAuditType getDefaultAuditType(TaskAuditType code) throws FindException {
		String hql = "from " + TaskAuditType.class.getName() + " as dt where dt.intValue = :typ";
		OqlQuery oql = new OqlQuery(hql);
		oql.getQuery().setString("typ", code.toString());
		return (TaskAuditType) oql.uniqueResult();
	}

	public static TaskAuditType loadByCode(String code) {
		return (TaskAuditType) PersistenceHome.findUniqueNullIfEmpty(TaskAuditType.class, "intValue", code);
	}

	public static String decode(String code) {
		TaskAuditType tt = TaskAuditType.loadByCode(code);
		if (tt != null)
			return tt.getName();
		else
			return "";

	}
}
