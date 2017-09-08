package com.twproject.task.businessLogic;

import org.hibernate.Query;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.view.RestState;

import com.twproject.operator.TeamworkOperator;
import com.twproject.task.TaskAudit;
import com.twproject.task.TaskAuditReview;

public class TaskAuditAction extends ActionSupport {

	public TeamworkOperator logged;

	public TaskAuditAction(RestState pageState) {
		super(pageState);
		this.logged = (TeamworkOperator) pageState.getLoggedOperator();
	}

	public void cmdPrepareDefaultFind() throws ActionException, PersistenceException {

	}

	public void cmdFind() throws ActionException, PersistenceException {
		// defaults
		// cmdPrepareDefaultFind();

		String hql = "from " + TaskAudit.class.getName()
				+ " as audit where 1=1";
		// /audit.task.id=:task and isClosed=:isClosed
		QueryHelper qhelp = new QueryHelper(hql);
		String taskId = restState.getEntry("TASKID").stringValueNullIfEmpty();
		if (JSP.ex(taskId)) {
			ActionUtilities.addOQLClause("TASKID", "audit.task.id", "task", qhelp, QueryHelper.TYPE_CHAR, restState);
		}

		System.out.println(taskId);
		String isClosed = restState.getEntry("ISCLOSED").stringValueNullIfEmpty();
		if (JSP.ex(isClosed)) {
			ActionUtilities.addOQLClause("ISCLOSED", "audit.isClosed", "isClosed", qhelp, QueryHelper.TYPE_INT,
					restState);
		}
		System.out.println(isClosed);
		String reportorId = restState.getEntry("REPORTID").stringValueNullIfEmpty();
		if (JSP.ex(reportorId)) {
			ActionUtilities.addOQLClause("REPORTID", "audit.reportor.id", "reportorId", qhelp, QueryHelper.TYPE_CHAR,
					restState);
		}
		System.out.println(reportorId);
		DataTable.orderAction(qhelp, "ADTLST", restState, " audit.creationDate desc");

		OqlQuery oqlQuery = qhelp.toHql();
		Query query = oqlQuery.getQuery();
		HibernatePage page = HibernatePage.getHibernatePageInstance(query, Paginator.getWantedPageNumber(restState),
				Paginator.getWantedPageSize("ADTLST", restState));
		System.out.println("page:"+page.getLastPageNumber());
		restState.setPage(page);
	}

	public void cmdFindMineAsReviewer() throws ActionException, PersistenceException {
		// defaults
		// cmdPrepareDefaultFind();

		String hql = "select distinct audit from " + TaskAudit.class.getName()
				+ " as audit," + TaskAuditReview.class.getName() + " as r where r.mainAudit.id = audit.id";
		// /audit.task.id=:task and isClosed=:isClosed
		QueryHelper qhelp = new QueryHelper(hql);

		 String taskId =
		 restState.getEntry("TASKID").stringValueNullIfEmpty();
		 if (JSP.ex(taskId)) {
		 ActionUtilities.addOQLClause("TASKID", "audit.task.id", "task",
		 qhelp, QueryHelper.TYPE_CHAR, restState);
		 }

		String isClosed = restState.getEntry("ISCLOSED").stringValueNullIfEmpty();
		if (JSP.ex(isClosed)) {
			ActionUtilities.addOQLClause("ISCLOSED", "audit.isClosed", "isClosed", qhelp, QueryHelper.TYPE_INT,
					restState);
		}

		String reviewerId = restState.getEntry("REVIEWERID").stringValueNullIfEmpty();
		if (JSP.ex(reviewerId)) {
			ActionUtilities.addOQLClause("REVIEWERID", "r.reviewer.id", "reviewerId", qhelp, QueryHelper.TYPE_CHAR,
					restState);
		}

		DataTable.orderAction(qhelp, "ADTLST", restState, " audit.creationDate desc");

		OqlQuery oqlQuery = qhelp.toHql();
		Query query = oqlQuery.getQuery();
		HibernatePage page = HibernatePage.getHibernatePageInstance(query, Paginator.getWantedPageNumber(restState),
				Paginator.getWantedPageSize("ADTLST", restState));

		restState.setPage(page);
	}

}