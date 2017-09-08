package com.teamwork.expand;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.jblooming.ApplicationException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import com.twproject.document.businessLogic.DocumentAction;

public class ReportController implements ActionController{

	@Override
	public PageState perform(HttpServletRequest request, HttpServletResponse response)
			throws PersistenceException, ActionException, SecurityException, ApplicationException, IOException {
		// TODO Auto-generated method stub
		PageState pageState = PageState.getCurrentPageState(request);
		ReportAction reportAction = new ReportAction(pageState);
	    String command = pageState.getCommand();
	    if (Commands.ADD.equals(command) || "ADD_REPORT".equals(command))
	    	reportAction.cmdAdd();
	    else if (Commands.EDIT.equals(command) || Commands.DELETE_PREVIEW.equals(command))
	    	reportAction.cmdEdit();
	    else if ("AUTH".equals(command))
	    	reportAction.cmdAuthAndAdd(pageState,request);
	    else if ("MKDIR".equals(command))
	    	reportAction.cmdMkdir();
	    else if (Commands.SAVE.equals(command)){
	      try {
	    	  reportAction.cmdSave();

	      } catch (ActionException e) {}



	    }else if (Commands.DELETE.equals(command)) {
	      try {
	    	  reportAction.cmdDelete();
	        PageSeed ps;
	        if (reportAction.task!=null) {
	          ps = pageState.pageFromRoot("task/taskReportList.jsp");
	          ps.addClientEntry("TASK_ID",reportAction.task);
	          ps.command = "LIST_REPORT";
	        } else {
	          ps = pageState.pageFromRoot("task/taskReportList.jsp");
	          ps.setCommand(Commands.FIND);
	        }

	        if (JSP.ex(pageState.getEntry("DO_NOT_REDIR")))
	          pageState.addClientEntry("REDIRECT_TO",ps.toLinkToHref());
	        else
	          pageState.redirect(ps);



	      } catch (RemoveException ex) {
	        // in order to feedback operator in partDelete.jsp
	        pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);
	      }
	    } else if ("TAKE_OWNERSHIP".equals(command)) {
	    //	reportAction.cmdTakeOwnership();

	    } else if ("LIST_REPORTS".equals(command)) {
	    	reportAction.editNoMake();


	    } else {
	    	reportAction.cmdFind();
	    }
	    return pageState;
	}

}
