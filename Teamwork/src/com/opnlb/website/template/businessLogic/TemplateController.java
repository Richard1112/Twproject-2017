package com.opnlb.website.template.businessLogic;

import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * TemplateController (c) 2005 - Open Lab - www.open-lab.com
 */
public class TemplateController implements ActionController  {


  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, ApplicationException, org.jblooming.security.SecurityException {

    PageState pageState = PageState.getCurrentPageState(request);

    Operator loggedOp = pageState.getLoggedOperator();
    TemplateAction templAction = new TemplateAction ();
    String command = pageState.getCommand();

    if (Commands.ADD.equals(command)) {
      templAction.cmdAdd(pageState);

    } else if (Commands.SAVE.equals(command)) {
      try {
        templAction.cmdSave(pageState);
      } catch (ActionException e) {
      } catch(PlatformRuntimeException a) {
        ClientEntry ceNull = new ClientEntry("TEMPLATE_FILE", pageState.getEntry("TEMPLATE_FILE").stringValueNullIfEmpty());
        ceNull.errorCode = a.getMessage();
        pageState.addClientEntry(ceNull);
      }

    } else if (Commands.EDIT.equals(command)|| Commands.DELETE_PREVIEW.equals(command)) {
        templAction.cmdEdit(pageState, request);

    } else if ( Commands.DELETE.equals(command)) {

      try {
        templAction.cmdDelete(pageState, request);
        PageSeed ps = pageState.pageFromApplications("website/admin/templateList.jsp");

        if (JSP.ex(pageState.getEntry("DO_NOT_REDIR")))
          pageState.addClientEntry("REDIRECT_TO",ps.toLinkToHref());
        else
          pageState.redirect(ps);

      } catch (RemoveException ex) {
        // in order to feedback operator il partDelete.jsp
        pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);
      }

    } else if (Commands.FIND.equals(command)){
      templAction.cmdFind(pageState, loggedOp);
    }

    return pageState;
  }

}
