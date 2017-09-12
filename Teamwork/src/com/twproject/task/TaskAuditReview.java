/**
 * 
 */
package com.twproject.task;

import java.io.Serializable;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.hibernate.HibernateException;
import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;
import org.hibernate.search.annotations.FieldBridge;
import org.hibernate.search.bridge.builtin.IntegerBridge;
import org.jblooming.ontology.LoggableIdentifiableSupport;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.PersistenceException;

import com.twproject.resource.Resource;

/**
 * @author x-wang
 *
 */
@Entity
@Table(name = "twk_task_audit_review")
public class TaskAuditReview extends LoggableIdentifiableSupport {

	private TaskAudit mainAudit;
	private TaskAuditStatus auditStatus;
	private Resource reviewer;
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

	@ManyToOne(targetEntity = TaskAudit.class)
	@ForeignKey(name = "fk_audit_reviewers")
	@Index(name = "idx_audit_reviewers")
	@JoinColumn(name = "mainAudit")
	public TaskAudit getMainAudit() {
		return mainAudit;
	}

	public void setMainAudit(TaskAudit mainAudit) {
		this.mainAudit = mainAudit;
	}

	@ManyToOne(targetEntity = TaskAuditStatus.class, fetch = FetchType.LAZY)
	@ForeignKey(name = "fk_auditr_status")
	@Index(name = "idx_auditr_status")
	public TaskAuditStatus getAuditStatus() {
		return auditStatus;
	}

	public void setAuditStatus(TaskAuditStatus auditStatus) {
		this.auditStatus = auditStatus;
	}

	@ManyToOne(targetEntity = Resource.class, fetch = FetchType.LAZY)
	@ForeignKey(name = "fk_auditr_reviewer")
	@Index(name = "idx_auditr_reviewer")
	@JoinColumn(name = "reviewer")
	public Resource getReviewer() {
		return reviewer;
	}

	public void setReviewer(Resource reviewer) {
		this.reviewer = reviewer;
	}

	public int getAuditLevel() {
		return auditLevel;
	}

	public void setAuditLevel(int auditLevel) {
		this.auditLevel = auditLevel;
	}

	public static TaskAuditReview loadByReviewer(String id, String auditId) throws PersistenceException {
		TaskAuditReview result = null;
		final OqlQuery oqlQuery = new OqlQuery("from " + TaskAuditReview.class.getName()
				+ " as obj where obj.reviewer.id = :aparam and obj.mainAudit.id=:audit order by obj.auditStatus");
		try {
			result = (TaskAuditReview) oqlQuery.getQuery().setParameter("aparam", id)
					.setParameter("audit", Integer.parseInt(auditId))
					.uniqueResult();
		} catch (HibernateException e) {
			throw new PersistenceException(oqlQuery.doDebug(new Object[] { id }), e);
		}
		return result;
	}

	public static TaskAuditReview loadByReviewer2(int id, String auditId) throws PersistenceException {
		TaskAuditReview result = null;
		final OqlQuery oqlQuery = new OqlQuery("from " + TaskAuditReview.class.getName()
				+ " as obj where obj.reviewer.id = :aparam and obj.reviewer.id=:audit order by obj.auditStatus");
		try {
			result = (TaskAuditReview) oqlQuery.getQuery().setParameter("aparam", id)
					.setParameter("audit", auditId).uniqueResult();
		} catch (HibernateException e) {
			throw new PersistenceException(oqlQuery.doDebug(new Object[] { id }), e);
		} catch (Exception e) {
			e.printStackTrace();
			throw e;
		}
		return result;
	}
	

	public static TaskAuditReview loadByReviewer3(int id, String auditId, int level) throws PersistenceException {
		TaskAuditReview result = null;
		final OqlQuery oqlQuery = new OqlQuery("from " + TaskAuditReview.class.getName()
				+ " as obj where obj.reviewer.id = :aparam and obj.reviewer.id=:audit and obj.auditLevel=:auditLevel");
		try {
			result = (TaskAuditReview) oqlQuery.getQuery().setParameter("aparam", id).setParameter("audit", auditId)
					.setParameter("auditLevel", level).uniqueResult();
		} catch (HibernateException e) {
			throw new PersistenceException(oqlQuery.doDebug(new Object[] { id }), e);
		} catch (Exception e) {
			e.printStackTrace();
			throw e;
		}
		return result;
	}

	public static List<TaskAuditReview> loadByReviewer3(String auditId, int level) throws PersistenceException {
		List<TaskAuditReview> result = null;
		final OqlQuery oqlQuery = new OqlQuery("from " + TaskAuditReview.class.getName()
				+ " as obj where obj.mainAudit.id=:audit and obj.auditLevel=:auditLevel");
		try {
			result = oqlQuery.getQuery().setParameter("audit", Integer.parseInt(auditId))
					.setParameter("auditLevel", level).list();
		} catch (HibernateException e) {
			throw new PersistenceException(oqlQuery.doDebug(new Object[] { auditId }), e);
		} catch (Exception e) {
			e.printStackTrace();
			throw e;
		}
		return result;
	}

	public static TaskAuditReview loadLastByAudit(String auditId) throws PersistenceException {
		final OqlQuery oqlQuery = new OqlQuery("from " + TaskAuditReview.class.getName()
				+ " as obj where obj.mainAudit.id=:audit and obj.auditStatus.intValue=3 order by obj.lastModified desc, obj.id desc");
		try {
			List<TaskAuditReview> rl = oqlQuery.getQuery().setParameter("audit", Integer.parseInt(auditId)).list();
			if (rl != null && rl.size() > 0) {
				return rl.get(0);
			}
		} catch (HibernateException e) {
			throw new PersistenceException(oqlQuery.doDebug(new Object[] { auditId }), e);
		}
		return null;
	}

	public static String getAllStatusByAudit(String auditId) throws PersistenceException {
		final OqlQuery oqlQuery = new OqlQuery("from " + TaskAuditReview.class.getName()
				+ " as obj where obj.mainAudit.id=:audit order by obj.lastModified desc, obj.id desc");
		try {
			List<TaskAuditReview> rl = oqlQuery.getQuery().setParameter("audit", Integer.parseInt(auditId)).list();
			StringBuilder sbd = new StringBuilder();
			if (rl != null && rl.size() > 0) {
				for (TaskAuditReview tr : rl) {
					sbd.append(tr.getReviewer().getName() + ":");
					sbd.append(tr.getAuditStatus().getDescription());
					if (rl.indexOf(tr) < rl.size() - 1) {
						sbd.append("、");
					}
				}
			}
			return sbd.toString();
		} catch (HibernateException e) {
			throw new PersistenceException(oqlQuery.doDebug(new Object[] { auditId }), e);
		}
	}
}
