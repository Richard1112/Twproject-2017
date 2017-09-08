package com.twproject.task.businessLogic;

import com.twproject.task.Task;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class TaskController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException, IOException {

    PageState pageState = PageState.getCurrentPageState(request);

    TaskAction taskAction = new TaskAction(pageState);
    IssueAction issueAction = new IssueAction(pageState);
    String command = pageState.getCommand();

    if (Commands.ADD.equals(command))
      taskAction.cmdAdd();

    else if (Commands.DELETE_PREVIEW.equals(command)) {
      taskAction.cmdEdit();

    } else if (
      Commands.EDIT.equals(command) || "TASK_EDIT_SECURITY".equals(command)) {
      taskAction.cmdEdit();

    } else if ("GUESS".equals(command)) {
      String oldId = pageState.mainObjectId + "";
      try {
        taskAction.cmdGuess();
      } catch (ActionException ae) {
        PageSeed newSeed = pageState.pageFromRoot("tools/invalidReference.jsp");
        newSeed.addClientEntry("CAUSE", ae.getMessage());
        newSeed.addClientEntry("TYPE", "TASK");
        newSeed.addClientEntry("REF", oldId);
        pageState.redirect(newSeed);

      }

    } else if (Commands.SAVE.equals(command))
      taskAction.cmdSave();


    else if ("GANTT".equals(command)) {
      taskAction.editNoMake();

    } else if (Commands.DELETE.equals(command)) {
      try {
        taskAction.cmdDelete();
        PageSeed ps = pageState.pageFromRoot("task/taskList.jsp");
        Task parent = (Task) pageState.attributes.get("PARENT_TASK_DELETED");
        if (parent != null) {
          ps = pageState.pageFromRoot("task/taskOverview.jsp");
          ps.command = Commands.EDIT;
          ps.mainObjectId = parent.getId();
        }
        if (JSP.ex(pageState.getEntry("DO_NOT_REDIR")))
          pageState.addClientEntry("REDIRECT_TO",ps.toLinkToHref());
        else
          pageState.redirect(ps);
      } catch (Exception ex) {
        // in order to feedback operator in partDelete.jsp
        pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);
      }

    //  --------------------------------------------------------------- WORKFLOW ------------------------------------------------------------
    } else if ("CREATE_PROCESS".equals(command)) {
      try {
        taskAction.cmdCreateProcess();

        PageSeed ps = pageState.pageFromRoot("task/taskOverview.jsp");
        ps.command=Commands.EDIT;
        ps.mainObjectId=pageState.mainObjectId;

        response.sendRedirect(ps.toLinkToHref());

      } catch (ActionException e) {
        Tracer.platformLogger.error(e);
      }


    } else if ("SHOW_GRAPH".equals(command)) {
      taskAction.editNoMake();

    } else if ("DO_STEP".equals(command)) {
      try {
        taskAction.cmdDoProcessStep();
      } catch (ActionException e) {
      }

    //------------------------------------------------ OTHERS -----------------------------------------------------------------------------------------------
    } else if ("TASK_SAVE_PUBLICPAGE".equals(command)){
      taskAction.cmdSavePublicPage();
    } else if ("TASK_SAVE_SECURITY".equals(command)){
      taskAction.cmdSaveSecurity();
    } else if ("TAKE_OWNERSHIP".equals(command)) {
      taskAction.cmdTakeOwnership();

    }else if ("CREATE_SNAPSHOT".equals(command)) {
      taskAction.cmdSnapshot();
      PageSeed ps = pageState.pageFromRoot("task/taskDocumentList.jsp");
      ps.command="LIST_DOCS";
      ps.addClientEntry("TASK_ID", pageState.getMainObject());
      pageState.redirect(ps);


    }else if ("EXPORT".equals(command)) {
      taskAction.cmdExport();

    } else if (Commands.FIND.equals(command)) { // FIND, PRINT, ETC
      taskAction.cmdFind();

    } else {
      taskAction.cmdPrepareDefaultFind();
    }

    try {
      ReflectionUtilities.invoke(ApplicationState.applicationParameters.get("get"), "doFilter", request, response);
    } catch (Throwable e) {
      throw new PlatformRuntimeException(e);
    }


    return pageState;
  }


}

