package com.twproject.worklog.businessLogic;

import org.jblooming.security.License;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;


/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 5-ott-2005 : 14.15.06
 */
public class WorklogController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException, IOException {
    PageState pageState = PageState.getCurrentPageState(request);
    WorklogAction action = new WorklogAction(pageState);

    String command = pageState.getCommand();


    if (Commands.EDIT.equals(command)){
      action.cmdEdit();

    } else if ("WORKLOG_MOVE".equals(pageState.command)) {
      action.cmdMove();  
      action.cmdFind();

    }else if ("WORKLOG_DELETE".equals(pageState.command)) {
      action.deleteWorklogs();
      action.cmdFind();
    }else if ("WORKLOG_CHANGESTATUS".equals(pageState.command)) {
      action.changeStatus();
      action.cmdFind();

    } else if ("BULK_SET_STATUS".equals(command)) {
      action.bulkSetStatus();
    } else if ("BULK_MOVE_TO_TASK".equals(command)) {
      action.bulkMoveToTask();
    } else if ("BULK_MOVE_TO_DATE".equals(command)) {
      action.bulkSetDate();
    } else if (Commands.FIND.equals(command)){
      if (License.assertLevel(10))
        action.cmdFind();
    } else if ("dummy".equalsIgnoreCase(command)){
    } else{
      action.cmdPrepareDefaultFind();
    }

    try {
      ReflectionUtilities.invoke(ApplicationState.applicationParameters.get("get"), "doFilter", request, response);
    } catch (Throwable e) {
      throw new PlatformRuntimeException(e);
    }


    return pageState;
  }

}



