package com.opnlb.website.portlet.businessLogic;

import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.input.Collector;
import org.jblooming.waf.html.input.Uploader;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * PortletController (c) 2005 - Open Lab - www.open-lab.com
 */
public class PortletController implements ActionController {

  /*
  public Class portletClass;
  public String hrefAfterDelete = "/applications/website/admin/portletList.jsp";

  public PortletController() {
    this.portletClass = Portlet.class;
  }

  public PortletController(Class portletClass) {
    this.portletClass = portletClass;
  }
 */

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, SecurityException, ApplicationException {

    PageState pageState = PageState.getCurrentPageState(request);
    PortletAction wpAction = new PortletAction();
    String command = pageState.getCommand();

    if (Commands.ADD.equals(command)) {
      wpAction.cmdAdd(pageState);

    } else if (Commands.SAVE.equals(command)) {
      try {
        wpAction.cmdSave(pageState);
      } catch (ActionException a) {
      }

    } else if (Commands.EDIT.equals(command) || Commands.DELETE_PREVIEW.equals(command)) {
      //wpAction.cmdEdit(pageState,false,true,true, request);
      wpAction.cmdEdit(pageState, request);

    } else if (Commands.DELETE.equals(command)) {
      try {
        wpAction.cmdDelete(pageState);
        PageSeed ps = pageState.pageFromApplications("website/admin/portletList.jsp");
        if (JSP.ex(pageState.getEntry("DO_NOT_REDIR")))
          pageState.addClientEntry("REDIRECT_TO",ps.toLinkToHref());
        else
          pageState.redirect(ps);
      } catch (RemoveException ex) {
        // in order to give a feedback in partDelete.jsp
        pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);
      }

    } else if (Commands.INSTALL.equals(command)) {
      pageState.initializeEntries("row");
      wpAction.cmdInstall(pageState);
      wpAction.cmdFind(pageState);

    } else if (Commands.UNINSTALL.equals(command)) {
      pageState.initializeEntries("row");
      wpAction.cmdUnInstall(pageState);
      wpAction.cmdFind(pageState);

    } else if (Collector.isCollectorCommand("wpPermColl", command)) {
      try {
        wpAction.cmdMove("wpPermColl", pageState);
      } catch (PersistenceException a) {
        throw new PlatformRuntimeException(a);
      }

    } else if (Commands.FIND.equals(command)){
      wpAction.cmdFind(pageState);
    }

    return pageState;
  }

}
