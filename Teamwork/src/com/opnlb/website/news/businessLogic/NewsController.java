package com.opnlb.website.news.businessLogic;

import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.ApplicationException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * NewsController (c) 2005 - Open Lab - www.open-lab.com
 */
public class NewsController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException {

    PageState pageState = PageState.getCurrentPageState(request);

    NewsAction na = new NewsAction();
    final String command = pageState.getCommand();

    if (Commands.EDIT.equals(command) || Commands.DELETE_PREVIEW.equals(command) ) {
      na.cmdEdit(pageState);

    } else if (Commands.FIND.equals(command) ) {
      na.cmdFind(pageState);

    } else if (Commands.SAVE.equals(command)) {
      try {
      na.cmdSave(request, pageState);
      } catch (ActionException e){ }

    } else if (Commands.ADD.equals(command) ) {
      na.cmdAdd(pageState);

    } else if ("NEWS_VISIBLE".equals(command) ) {
      na.cmdMakeVisible(pageState);
      na.cmdFind(pageState);

    } else if ("NEWS_INVISIBLE".equals(command) ) {
      na.cmdMakeInvisible(pageState);
      na.cmdFind(pageState);

    } else if (Commands.DELETE.equals(command) ) {
      try {
        na.cmdDelete(pageState, request);
        PageSeed ps = pageState.pageFromApplications("website/admin/news/newsList.jsp");
        if (JSP.ex(pageState.getEntry("DO_NOT_REDIR")))
          pageState.addClientEntry("REDIRECT_TO",ps.toLinkToHref());
        else
          pageState.redirect(ps);

      } catch (RemoveException ex) {
        // in order to give a feedback in partDelete.jsp
        pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);
      }

    } else if (Commands.FIND.equals(command)){
      na.cmdFind(pageState);
    }

    return pageState;
  }


}
