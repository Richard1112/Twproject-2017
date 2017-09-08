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
@Table(name = "twk_task_audit_status")
public class TaskAuditStatus extends IdentifiableSupport {

	private int intValue;
	private String description;

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

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public static TaskAuditStatus load(Serializable id) throws FindByPrimaryKeyException {
		return (TaskAuditStatus) PersistenceHome.findByPrimaryKey(TaskAuditStatus.class, id);
	}

	public static TaskAuditStatus getDefaultAuditStatus(TaskAuditStatus code) throws FindException {
		String hql = "from " + TaskAuditStatus.class.getName() + " as dt where dt.intValue = :typ";
		OqlQuery oql = new OqlQuery(hql);
		oql.getQuery().setString("typ", code.toString());
		return (TaskAuditStatus) oql.uniqueResult();
	}

	public static TaskAuditStatus loadByCode(Integer code) {
		return (TaskAuditStatus) PersistenceHome.findUniqueNullIfEmpty(TaskAuditStatus.class, "intValue", code);
	}

}
