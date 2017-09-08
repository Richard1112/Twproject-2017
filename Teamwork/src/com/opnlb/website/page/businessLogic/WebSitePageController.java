package com.opnlb.website.page.businessLogic;

import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.html.input.Collector;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.ApplicationException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.opnlb.website.page.WebSitePage;

/**
 * WebSitePageController (c) 2005 - Open Lab - www.open-lab.com
 */
public class WebSitePageController implements ActionController  {

  public Class pageClass;


  public WebSitePageController() {
    this.pageClass = WebSitePage.class;
  }

  public WebSitePageController(Class pageClass) {
    this.pageClass = pageClass;
  }

  public PageState perform(HttpServletRequest request, HttpServletResponse response)
          throws PersistenceException, org.jblooming.security.SecurityException, ActionException, ApplicationException {
    WebSitePageAction pageAction = new WebSitePageAction();
    return perform(request, response, pageAction );
  }

  public PageState perform(HttpServletRequest request, HttpServletResponse response, WebSitePageAction wsa)  throws PersistenceException, ActionException, ApplicationException {
    PageState pageState = PageState.getCurrentPageState(request);
    String command = pageState.getCommand();

    if (Commands.ADD.equals( command )) {
      wsa.cmdAdd(pageState, pageClass);

    } else if ( Commands.EDIT.equals( command ) || Commands.DELETE_PREVIEW.equals( command )) {
      wsa.cmdEdit( pageState, pageClass );

    } else if (Commands.DELETE.equals( command )) {
      try {
        wsa.cmdDelete(pageState);
        PageSeed ps = pageState.pageFromApplications("website/admin/pageList.jsp");
        pageState.setCommand(Commands.FIND);

        if (JSP.ex(pageState.getEntry("DO_NOT_REDIR")))
          pageState.addClientEntry("REDIRECT_TO",ps.toLinkToHref());
        else
          pageState.redirect(ps);

      } catch (Exception ex) {
        // in order to give a feedback in partDelete.jsp
        pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);
      }

    } else if (Commands.SAVE.equals( command )) {
      try {
        pageState.initializeEntries("row");
        wsa.cmdSave(pageState, pageClass );
      } catch(ActionException a) {}

    } else if (Collector.isCollectorCommand("pagePermColl", command)) {
      try {
        wsa.cmdMove(pageState);
      } catch (PersistenceException a) { }


    } else if ("RESET_DEFAULTS".equals(command)) {
      wsa.cmdResetDefault(pageState);


    } else if (Commands.FIND.equals(command)){
      wsa.cmdFind(pageState);
    }

    return pageState;
  }
}
