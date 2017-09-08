package com.twproject.task.businessLogic;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

public class TaskAuditController implements ActionController {

	@Override
	public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException,
			ActionException, org.jblooming.security.SecurityException, ApplicationException, IOException {

		PageState pageState = PageState.getCurrentPageState(request);

		TaskAuditAction taskAuditAction = new TaskAuditAction(pageState);
		String command = pageState.getCommand();

		if (Commands.FIND.equals(command)) { // FIND, PRINT, ETC
			taskAuditAction.cmdFind();
		} else if (Commands.FINDMR.equals(command)) { // FIND, PRINT, ETC
			taskAuditAction.cmdFindMineAsReviewer();
		} else if ("CREATE_SNAPSHOT".equals(command)) {
			// taskAction.cmdSnapshot();
			PageSeed ps = pageState.pageFromRoot("task/taskDocumentList.jsp");
			ps.command = "LIST_DOCS";
			ps.addClientEntry("TASK_ID", pageState.getMainObject());
			pageState.redirect(ps);

		} else {
			String id = pageState.getEntry("TASK_ID").stringValueNullIfEmpty();
			pageState.addClientEntry("TASKId", id);
			taskAuditAction.cmdPrepareDefaultFind();
		}

		try {
			ReflectionUtilities.invoke(ApplicationState.applicationParameters.get("get"), "doFilter", request,
					response);
		} catch (Throwable e) {
			throw new PlatformRuntimeException(e);
		}
		return pageState;
	}
}

