package com.twproject.security.businessLogic;

import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.html.input.Collector;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class TeamworkRoleController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, org.jblooming.security.SecurityException {

    PageState pageState = PageState.getCurrentPageState(request);

    TeamworkRoleAction roleAction = new TeamworkRoleAction(pageState);
    String command = pageState.getCommand();

    if (Collector.isCollectorCommand("permColl", command)) {
      try {
        roleAction.cmdMove("permColl");
      } catch (PersistenceException e) {
      }
    } else if (Commands.ADD.equals(command)) {
      roleAction.cmdAdd();

    } else if ("CLONE".equals(command)) {
      roleAction.cmdClone();

    } else if (Commands.EDIT.equals(command) || Commands.DELETE_PREVIEW.equals(command)) {
      roleAction.cmdEdit();

    } else if (Commands.SAVE.equals(command)) {
      roleAction.cmdSave();

    } else if ("CROWN_ME_WITH_ROLE".equals(command)) {
        roleAction.cmdCrownMe();


    } else if (Commands.DELETE.equals(command)) {
      try {
        roleAction.cmdDelete();
        PageSeed ps = pageState.pageFromRoot("security/roleList.jsp");
        if (JSP.ex(pageState.getEntry("DO_NOT_REDIR")))
          pageState.addClientEntry("REDIRECT_TO",ps.toLinkToHref());
        else
          pageState.redirect(ps);
      } catch (Exception ex) {
        // in order to feedback operator in partDelete.jsp
        pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);
      }




    } else {
      roleAction.cmdFind();
    }

     return pageState;

  }
}
