/**
 * 
 */
package com.twproject.task;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;
import org.hibernate.search.annotations.FieldBridge;
import org.hibernate.search.bridge.builtin.IntegerBridge;
import org.jblooming.ontology.LoggableIdentifiableSupport;

import com.twproject.resource.Resource;

/**
 * 项目变更审核记录entity
 * 
 * @author x-wang
 *
 */
@Entity
@Table(name = "twk_task_audit_log")
public class TaskAuditLog extends LoggableIdentifiableSupport {
	private String content;
	private TaskAuditStatus auditStatus;
	private TaskAudit mainAudit;
	private Date submitdate;
	private Resource submituser;
	private Date auditdate;
	private Resource audituser;
	private int auditLevel;

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

	public String getContent() {
		return content == null ? "" : content;
	}

	public void setContent(String content) {
		this.content = content;
	}

	@ManyToOne(targetEntity = TaskAuditStatus.class, fetch = FetchType.LAZY)
	@ForeignKey(name = "fk_auditlog_status")
	@Index(name = "idx_auditlog_status")
	public TaskAuditStatus getAuditStatus() {
		return auditStatus;
	}

	public void setAuditStatus(TaskAuditStatus auditStatus) {
		this.auditStatus = auditStatus;
	}


	public Date getSubmitdate() {
		return submitdate;
	}

	@ManyToOne(targetEntity = TaskAudit.class)
	@ForeignKey(name = "fk_audit_log")
	@Index(name = "idx_audit_log")
	@JoinColumn(name = "mainAudit")
	public TaskAudit getMainAudit() {
		return mainAudit;
	}

	public void setMainAudit(TaskAudit mainAudit) {
		this.mainAudit = mainAudit;
	}

	public void setSubmitdate(Date submitdate) {
		this.submitdate = submitdate;
	}

	@ManyToOne(targetEntity = Resource.class, fetch = FetchType.LAZY)
	@ForeignKey(name = "fk_auditlog_submit")
	@Index(name = "idx_auditlog_submit")
	@JoinColumn(name = "submituser")
	public Resource getSubmituser() {
		return submituser;
	}

	public void setSubmituser(Resource submituser) {
		this.submituser = submituser;
	}

	public Date getAuditdate() {
		return auditdate;
	}

	public void setAuditdate(Date auditdate) {
		this.auditdate = auditdate;
	}

	@ManyToOne(targetEntity = Resource.class, fetch = FetchType.LAZY)
	@ForeignKey(name = "fk_auditlog_audituser")
	@Index(name = "idx_auditlog_audituser")
	@JoinColumn(name = "Audituser")
	public Resource getAudituser() {
		return audituser;
	}

	public void setAudituser(Resource audituser) {
		this.audituser = audituser;
	}

	public int getAuditLevel() {
		return auditLevel;
	}

	public void setAuditLevel(int auditLevel) {
		this.auditLevel = auditLevel;
	}

}
