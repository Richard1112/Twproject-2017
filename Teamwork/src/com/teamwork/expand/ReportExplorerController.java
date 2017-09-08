package com.teamwork.expand;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.SecurityException;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageState;

public class ReportExplorerController implements ActionController{

	@Override
	public PageState perform(HttpServletRequest request, HttpServletResponse response)
			throws PersistenceException, ActionException, SecurityException, ApplicationException, IOException {
		// TODO Auto-generated method stub
		  PageState pageState = PageState.getCurrentPageState(request);
		  ReportExplorerAction action = new ReportExplorerAction();
		  String command = pageState.getCommand();

		  if ("ZIP".equals(command)) {
		      try {
		        action.cmdZip(request, response, pageState);
		      } catch (IOException e) {
		        throw new PlatformRuntimeException(e);
		      }
		  } else if ("MKDIR".equals(command)) {
		      action.mkdir(pageState);

		   } else if (Commands.DELETE.equals(command)) {
		      action.cmdDelete(pageState);

		   } else if ("UPLOAD".equals(command)) {
		      action.upload(pageState);
		   } 
		   return pageState;
	}

}
