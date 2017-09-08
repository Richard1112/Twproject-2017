package com.twproject.document.businessLogic;

import org.jblooming.ApplicationException;
import org.jblooming.utilities.JSP;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class DocumentController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException {
    PageState pageState = PageState.getCurrentPageState(request);
    DocumentAction documentAction = new DocumentAction(pageState);
    String command = pageState.getCommand();
    if (Commands.ADD.equals(command) || "ADD_DOCUMENT".equals(command))
      documentAction.cmdAdd();
    else if ("ADD_VERSION".equals(command))
      documentAction.cmdAddVersion();
    else if (Commands.EDIT.equals(command) || Commands.DELETE_PREVIEW.equals(command))
      documentAction.cmdEdit();
    else if (Commands.SAVE.equals(command)){
      try {
        documentAction.cmdSave();

      } catch (ActionException e) {}



    }else if (Commands.DELETE.equals(command)) {
      try {
        documentAction.cmdDelete();
        PageSeed ps;
        if (documentAction.task!=null) {
          ps = pageState.pageFromRoot("task/taskDocumentList.jsp");
          ps.addClientEntry("TASK_ID",documentAction.task);
          ps.command = "LIST_DOCS";
        } else if (documentAction.resource!=null) {
          ps = pageState.pageFromRoot("resource/resourceDocumentList.jsp");
          ps.addClientEntry("RES_ID",documentAction.resource);
          ps.command = "LIST_DOCS";
        } else {
          ps = pageState.pageFromRoot("document/documentList.jsp");
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
      documentAction.cmdTakeOwnership();

    } else if ("LIST_DOCS".equals(command)) {
      documentAction.editNoMake();


    } else {
      documentAction.cmdFind();
    }
    return pageState;
  }


}

