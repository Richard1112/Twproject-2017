package com.twproject.resource.businessLogic;

import com.twproject.resource.Resource;
import com.twproject.task.businessLogic.IssueAction;
import org.jblooming.ApplicationException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class ResourceController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException {
    PageState pageState = PageState.getCurrentPageState(request);
    ResourceAction action = new ResourceAction(pageState);
    IssueAction issueAction = new IssueAction(pageState);


    String command = pageState.getCommand();

    if (Commands.EDIT.equals(command) || Commands.DELETE_PREVIEW.equals(command))
      action.cmdEdit();
    else if ("GUESS".equals(command)) {
      String oldId = pageState.mainObjectId + "";
      try {
        action.cmdGuess();
      } catch (ActionException ae) {
        PageSeed newSeed = pageState.pageFromRoot("tools/invalidReference.jsp");
        newSeed.addClientEntry("CAUSE", ae.getMessage());
        newSeed.addClientEntry("TYPE", "RESOURCE");
        newSeed.addClientEntry("REF", oldId);
        pageState.redirect(newSeed);

      }

    } else if ("EDIT_OPT".equals(command) || "EDIT_LISTENERS".equals(command))
      action.cmdEditOptions();
    else if ("SAVE_OPT".equals(command))
      action.saveOptions();
    else if (Commands.ADD.equals(command))
      action.cmdAdd();
    else if (Commands.SAVE.equals(command))
      try {
        action.cmdSave();
      } catch (ActionException e) {
      }

    else if ("SV_SECURITY".equals(command))
      try {
        action.cmdSaveSecurity();
      } catch (ActionException e) {
      }

    else if (Commands.DELETE.equals(command))
      delete(pageState, action);


    else if ("CREATE_SNAPSHOT".equals(command)) {
      action.cmdSnapshot();
      PageSeed ps = pageState.pageFromRoot("resource/resourceDocumentList.jsp");
      ps.command = "LIST_DOCS";
      ps.addClientEntry("RES_ID", pageState.getMainObject());
      pageState.redirect(ps);


    } else if ("EXPORT_RESOURCES".equals(command)) {
      pageState.sessionState.setAttribute("EXPORT_RESOURCES", action.cmdFind());

    } else if (Commands.FIND.equals(command)){
      action.cmdFind();
    } else{
      action.cmdPrepareDefaultFind();
    }

    return pageState;
  }

  private void delete(PageState pageState, ResourceAction ra) throws org.jblooming.security.SecurityException, PersistenceException {
    //is it an assig ?

    String docId = pageState.getEntry("DOC_ID").stringValueNullIfEmpty();
    if (docId != null) {
      try {
        ra.cmdPerformDocumentAction();
        pageState.setCommand(Commands.EDIT);
      } catch (Exception ex) {
        // in order to feedback operator in partDelete.jsp
        pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);

      }
      ra.cmdEdit();
    } else {
      try {
        ra.cmdDelete();
        PageSeed ps = pageState.pageFromRoot("resource/resourceList.jsp");

        if (JSP.ex(pageState.getEntry("DO_NOT_REDIR")))
          pageState.addClientEntry("REDIRECT_TO",ps.toLinkToHref());
        else
          pageState.redirect(ps);

      } catch (Exception ex) {
        // in order to feedback operator il partDelete.jsp
        pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);
        ra.cmdEdit();
      }
    }
  }


}

