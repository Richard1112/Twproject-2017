package com.twproject.task.businessLogic;

import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
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

public class IssueController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response)
          throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException {
    PageState pageState = PageState.getCurrentPageState(request);

    IssueAction issueAction = new IssueAction(pageState);

    String command = pageState.getCommand();


    if (Commands.ADD.equals(command)) {
      issueAction.cmdAdd(false);

    } else if (Commands.EDIT.equals(command) || Commands.DELETE_PREVIEW.equals(command)) {
      issueAction.cmdEdit();

    } else if ("GUESS".equals(command)) {
      String oldId=pageState.mainObjectId+"";
      try{
        issueAction.cmdGuess();
      } catch (ActionException ae) {
        PageSeed newSeed = pageState.pageFromRoot("tools/invalidReference.jsp");
        newSeed.addClientEntry("CAUSE", ae.getMessage());
        newSeed.addClientEntry("TYPE", "ISSUE");
        newSeed.addClientEntry("REF", oldId);
        newSeed.setPopup(true);
        pageState.redirect(newSeed);

      }


    } else if (Commands.SAVE.equals(command) ) {
      issueAction.cmdSave();

     } else if ("SAVE_DUR".equals(command)) {
      issueAction.cmdSaveDur();

    } else if ("SAVE_AND_ADD_NEW".equals(command)) {
      issueAction.cmdSaveAndAdd();

    } else if ("CLONE".equals(command)) {
      issueAction.cmdClone();

    } else if (Commands.DELETE.equals(command)) {
      try {
        issueAction.cmdDelete();
      } catch (RemoveException ex) {
        // in order to feedback operator in partDelete.jsp
        pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);
      }

    } else if ("TRANSFORM".equals(command)) {
      issueAction.cmdUpgrade();

    } else if ("CLOSE".equals(command)) {
      issueAction.cmdClose();

    } else if ("BULK_MOVE_TO_TASK".equals(command)) {
      issueAction.cmdBulkMoveToTask();

     } else if ("BULK_MOVE_TO_RES".equals(command)) {
      issueAction.cmdBulkMoveToRes();

    } else if ("BULK_SET_STATUS".equals(command)) {
      issueAction.cmdBulkSetStatus();

    } else if ("BULK_SET_GRAVITY".equals(command)) {
      issueAction.cmdBulkSetGravity();

    } else if ("BULK_SET_IMPACT".equals(command)) {
      issueAction.cmdBulkSetImpact();

    } else if ("BULK_ADD_COMMENT".equals(command)) {
      issueAction.cmdBulkAddComment();

    } else if ("BULK_ADD_TAGS".equals(command)) {
      issueAction.cmdBulkAddTags();

    } else if ("BULK_DEL_ISSUES".equals(command)) {
      issueAction.cmdBulkDelIssues(false);

    } else if ("BULK_CLOSE_ISSUES".equals(command)) {
      issueAction.cmdBulkCloseIssues();

    } else if ("BULK_MERGE_ISSUES".equals(command)) {
      issueAction.cmdBulkMergeIssues();

    } else if ("BULK_SET_NEW_DATE".equals(command)) {
      issueAction.cmdBulkSetNewDate();

    } else if ("BULK_PRINT".equals(command)) {
      issueAction.cmdBulkPrint();

    } else if ("EXPORT".equals(command)) {
      issueAction.cmdExport();

    } else if (Commands.FIND.equals(command)) {
      issueAction.cmdFind();
    } else {
      issueAction.cmdPrepareDefaultFind();
    }

    try {
      ReflectionUtilities.invoke(ApplicationState.applicationParameters.get("get"), "doFilter", request, response);
    } catch (Throwable e) {
      throw new PlatformRuntimeException(e);
    }

    return pageState;
  }


}
