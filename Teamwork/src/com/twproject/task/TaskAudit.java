/**
 * 
 */
package com.twproject.task;

import java.io.Serializable;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import org.hibernate.HibernateException;
import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.OrderBy;
import org.hibernate.annotations.Type;
import org.hibernate.search.annotations.FieldBridge;
import org.hibernate.search.bridge.builtin.IntegerBridge;
import org.jblooming.ontology.LoggableIdentifiableSupport;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Resource;
import com.twproject.security.TeamworkPermissions;

import net.sf.json.JSONObject;

/**
 * 项目变更审核entity
 * 
 * @author x-wang
 *
 */
@Entity
@Table(name = "twk_task_audit")
public class TaskAudit extends LoggableIdentifiableSupport {

	private TaskAuditSubject title;
	private String content;
	private AuditType type;
	private TaskAuditStatus auditStatus;
	private Task task;
	private Resource reviewer;
	private Resource reportor;
	private List<TaskAuditLog> logs;
	private String preAudit;
	private String auditFlow;
//	private List<AuditPType> types;
	private int isClosed;
	private int auditLevel;

	private List<TaskAuditReview> reviewers;

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

	@ManyToOne(targetEntity = TaskAuditSubject.class, fetch = FetchType.LAZY)
	@ForeignKey(name = "fk_audit_subject")
	@Index(name = "idx_audit_subject")
	public TaskAuditSubject getTitle() {
		return title != null ? title : new TaskAuditSubject();
	}

	public void setTitle(TaskAuditSubject title) {
		this.title = title;
	}

	public String getContent() {
		return content;
	}

	public void setContent(String content) {
		this.content = content;
	}

	@ManyToOne(targetEntity = AuditType.class, fetch = FetchType.LAZY)
	@ForeignKey(name = "fk_audit_type")
	@Index(name = "idx_audit_type")
	public AuditType getType() {
		return type;
	}

	public void setType(AuditType type) {
		this.type = type;
	}

	@ManyToOne(targetEntity = TaskAuditStatus.class, fetch = FetchType.LAZY)
	@ForeignKey(name = "fk_audit_status")
	@Index(name = "idx_audit_status")
	public TaskAuditStatus getAuditStatus() {
		return auditStatus;
	}

	public String selectStatusForDisplay() throws PersistenceException {
		TaskAuditReview adr = TaskAuditReview.loadLastByAudit(getId().toString());
		return adr != null ? adr.getLastModifier() + adr.getAuditStatus().getDescription() : auditStatus.getDescription();
	}

	public void setAuditStatus(TaskAuditStatus auditStatus) {
		this.auditStatus = auditStatus;
	}

	@OneToMany(cascade = {
			CascadeType.REMOVE }, targetEntity = TaskAuditLog.class, mappedBy = "mainAudit", fetch = FetchType.LAZY)
	@OrderBy(clause = "auditLevel asc, submitdate desc")
	public List<TaskAuditLog> getLogs() {
		return logs;
	}

	public void setLogs(List<TaskAuditLog> logs) {
		this.logs = logs;
	}

//	@OneToMany(cascade = {
//			CascadeType.REMOVE }, targetEntity = TaskAuditLog.class, mappedBy = "mainAudit", fetch = FetchType.LAZY)
//	public List<AuditPType> getTypes() {
//		return types;
//	}
//
//	public void setTypes(List<AuditPType> types) {
//		this.types = types;
//	}

	public void addLog(TaskAuditLog log) {
		logs.add(log);
	}

	@ManyToOne(targetEntity = Task.class, fetch = FetchType.LAZY)
	@ForeignKey(name = "fk_audit_task")
	@Index(name = "idx_audit_task")
	public Task getTask() {
		return task;
	}

	public void setTask(Task task) {
		this.task = task;
	}

	@ManyToOne(targetEntity = Resource.class, fetch = FetchType.LAZY)
	@ForeignKey(name = "fk_audit_reviewer")
	@Index(name = "idx_audit_reviewer")
	@JoinColumn(name = "reviewer")
	public Resource getReviewer() {
		return reviewer;
	}

	public void setReviewer(Resource reviewer) {
		this.reviewer = reviewer;
	}

	@ManyToOne(targetEntity = Resource.class, fetch = FetchType.LAZY)
	@ForeignKey(name = "fk_audit_reportor")
	@Index(name = "idx_audit_reportor")
	@JoinColumn(name = "reportor")
	public Resource getReportor() {
		return reportor;
	}

	public void setReportor(Resource reportor) {
		this.reportor = reportor;
	}

	public String listReviewers() {
		StringBuilder rs = new StringBuilder();
		if (reviewers != null) {
			for (TaskAuditReview tr : reviewers) {
				if (tr.getAuditLevel() == this.auditLevel)
					rs.append(tr.getReviewer().getName()).append(",");
			}
		}
		String s = rs.toString();
		if (!org.apache.commons.lang.StringUtils.isEmpty(s)) {
			s = s.substring(0, s.length() - 1);
		}
		return s;
	}

	public static TaskAudit loadByReviewer(String id) throws PersistenceException {
		return (TaskAudit) PersistenceHome.findFirst(TaskAudit.class, "reviewer.id", id);
	}

	public static TaskAudit loadByTask(String id) throws PersistenceException {
		return (TaskAudit) PersistenceHome.findFirst(TaskAudit.class, "task.id", id);
	}

	public static TaskAudit load(String id) throws FindByPrimaryKeyException {
		TaskAudit audit = (TaskAudit) PersistenceHome.findByPrimaryKey(TaskAudit.class, id);
		return audit;
	}

	public static List<TaskAudit> findList(String taskId)
			throws PersistenceException {
		final OqlQuery oqlQuery = new OqlQuery(
				"from " + TaskAudit.class.getName() + " as obj where obj.task.id = :aparam");

		try {
			List<TaskAudit> list = oqlQuery.getQuery().setParameter("aparam", taskId).list();
			if (JSP.ex(list))
				return list;
			else
				return null;
		} catch (HibernateException e) {
			throw new PersistenceException(oqlQuery.doDebug(new Object[] { taskId }), e);
		}
	}

	public static TaskAuditReview getCurrentReviewer(List<TaskAuditReview> reviewers, String reviewerId) {
		if (reviewers == null) {
			return null;
		}
		for (TaskAuditReview v : reviewers) {
			if (reviewerId.equals(v.getReviewer().getId())) {
				return v;
			}
		}
		return null;
	}

	public String getPreAudit() {
		return preAudit;
	}

	public void setPreAudit(String preAudit) {
		this.preAudit = preAudit;
	}

	public String getAuditFlow() {
		return auditFlow;
	}

	public void setAuditFlow(String auditFlow) {
		this.auditFlow = auditFlow;
	}

	@OneToMany(cascade = {
			CascadeType.REMOVE }, targetEntity = TaskAuditReview.class, mappedBy = "mainAudit", fetch = FetchType.LAZY)
	public List<TaskAuditReview> getReviewers() {
		return reviewers;
	}

	public void setReviewers(List<TaskAuditReview> reviewers) {
		this.reviewers = reviewers;
	}

	public int getIsClosed() {
		return isClosed;
	}

	public void setIsClosed(int isClosed) {
		this.isClosed = isClosed;
	}

	public int getAuditLevel() {
		return auditLevel;
	}

	public void setAuditLevel(int auditLevel) {
		this.auditLevel = auditLevel;
	}

	public boolean showReSubmitBtn(TeamworkOperator logged) {
		if (getAuditStatus().getIntValue() == 3) {
			if (task.hasPermissionFor(logged, TeamworkPermissions.task_audit_canRCreate)) {
				try {
					List<TaskAuditReview> rl = TaskAuditReview.loadByReviewer3(
							getId().toString(), getAuditLevel());
					if(rl!=null && rl.size()>0) {
						for (TaskAuditReview r : rl) {
							
						}
					}
					TaskAuditReview tr = TaskAuditReview.loadByReviewer3(logged.getPerson().getIntId(),
							getId().toString(), getAuditLevel());
					if (tr == null) {
						return true;
					} else {
						if (tr.getAuditStatus().getIntValue() == 0 || tr.getAuditStatus().getIntValue() == 2) {

						}
					}

				} catch (PersistenceException e) {
					e.printStackTrace();
					return false;
				}
			}
		}
		return false;
	}

	public boolean showCallBackBtn(TeamworkOperator logged) {
		try {
			TaskAuditReview tr = TaskAuditReview.loadByReviewer3(logged.getPerson().getIntId(), getId().toString(),
					getAuditLevel());
			if (tr == null) {
				return true;
			} else {
				if (tr.getAuditStatus().getIntValue() == 0 || tr.getAuditStatus().getIntValue() == 2) {
					return true;
				}
			}
		} catch (PersistenceException e) {
			e.printStackTrace();
			return false;
		}

		return false;
	}

	@Override
	public JSONObject jsonify() {
		JSONObject jso = super.jsonify();

		jso.element("id", getId());
		jso.element("content", JSP.encode(getContent()));
		jso.element("reviewer", JSP.encode(getReviewer().getName()));
		jso.element("reporter", JSP.encode(getReportor().getName()));
		jso.element("type", JSP.encode(getType().getName()));
		jso.element("creation", DateUtilities.dateToString(getCreationDate(), "yyyy-MM-dd"));
		jso.element("status", JSP.encode(getAuditStatus().getName()));
		jso.element("taskId", getTask().getId());
		jso.element("taskName", JSP.encode(getTask().getName()));

		return jso;
	}
}
