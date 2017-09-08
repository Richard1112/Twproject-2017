package com.twproject.task.businessLogic;

import com.twproject.task.Assignment;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class AssignmentController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException, IOException {

    PageState pageState = PageState.getCurrentPageState(request);

    AssignmentAction assigAction = new AssignmentAction(pageState);
    String command = pageState.getCommand();

    if (Commands.ADD.equals(command))
      assigAction.cmdAdd();

    else if (Commands.EDIT.equals(command) || Commands.DELETE_PREVIEW.equals(command)) {
      assigAction.cmdEdit();

    } else if (Commands.SAVE.equals(command)){
      try {
        assigAction.cmdSave();
        PageSeed ps = pageState.pageFromRoot("task/taskAssignmentList.jsp");
        ps.addClientEntry(pageState.getEntry("TASK_ID"));
        pageState.redirect(ps);

      } catch (PersistenceException e) {
        throw new PlatformRuntimeException(e);
      } catch (ActionException e) {
      }
    }else if (Commands.DELETE.equals(command)){
       try {
        assigAction.cmdDelete();
        PageSeed ps = pageState.pageFromRoot("task/taskAssignmentList.jsp");
        ps.addClientEntry(pageState.getEntry("TASK_ID"));

        if (JSP.ex(pageState.getEntry("DO_NOT_REDIR")))
          pageState.addClientEntry("REDIRECT_TO",ps.toLinkToHref());
        else
          pageState.redirect(ps);

      } catch (Throwable ex) {
        // in order to feedback operator in partDelete.jsp
        pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);

      }



    } else if ("ASSIG_MOVE".equals(command)) {
      assigAction.cmdMoveAssignment();
      PageSeed ps = pageState.pageFromRoot("task/taskAssignmentList.jsp");
      ps.addClientEntry("TASK_ID",((Assignment) pageState.getMainObject()).getTask().getId());
      pageState.redirect(ps);

    } else if ("SAVE_SUBSCRIPTIONS".equals(command)) {
      assigAction.cmdSaveSubs(assigAction.logged, assigAction.logged);

    } else { // FIND
       assigAction.loadTask();
    }

    try {
      ReflectionUtilities.invoke(ApplicationState.applicationParameters.get("get"), "doFilter", request, response);
    } catch (Throwable e) {
      throw new PlatformRuntimeException(e);
    }


    return pageState;
  }


}