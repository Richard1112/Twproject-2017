/**
 * 
 */
package com.twproject.task;

import java.io.Serializable;

import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.FindException;

/**
 * @author x-wang
 *
 */
// @Entity
// @Table(name = "twk_task_audit_subject")
public class TaskAuditSubject extends IdentifiableSupport {

	private String description;
	private int auditLevel;
	private PersistentFile picture;

	public TaskAuditSubjectBricks bricks = new TaskAuditSubjectBricks(this);

	// @Override
	// @Id
	// @Type(type = "int")
	// @GeneratedValue(strategy = GenerationType.AUTO)
	// @Override
	// public Serializable getId() {
	// return super.getId();
	// }
	//
	// public void setId(int id) {
	// this.id = id;
	// }

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public int getAuditLevel() {
		return auditLevel;
	}

	public void setAuditLevel(int auditLevel) {
		this.auditLevel = auditLevel;
	}

	public PersistentFile getPicture() {
		return picture;
	}

	public void setPicture(PersistentFile picture) {
		this.picture = picture;
	}

	public static TaskAuditSubject load(Serializable id) throws FindByPrimaryKeyException {
		return (TaskAuditSubject) PersistenceHome.findByPrimaryKey(TaskAuditSubject.class, id);
	}

	public static TaskAuditSubject getDefaultAuditStatus(TaskAuditSubject code) throws FindException {
		String hql = "from " + TaskAuditSubject.class.getName() + " as dt where dt.intValue = :typ";
		OqlQuery oql = new OqlQuery(hql);
		oql.getQuery().setString("typ", code.toString());
		return (TaskAuditSubject) oql.uniqueResult();
	}

	public static TaskAuditSubject loadByCode(Integer code) {
		return (TaskAuditSubject) PersistenceHome.findUniqueNullIfEmpty(TaskAuditSubject.class, "intValue", code);
	}

}
