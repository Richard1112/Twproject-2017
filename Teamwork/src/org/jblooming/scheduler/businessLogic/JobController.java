package org.jblooming.scheduler.businessLogic;

import org.jblooming.ApplicationException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.scheduler.Job;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.PageSeed;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class JobController implements ActionController {
  public PageState perform(HttpServletRequest request, HttpServletResponse response)
          throws PersistenceException, ActionException, SecurityException, ApplicationException, IOException {

    PageState pageState = PageState.getCurrentPageState(request);
    final String command = pageState.getCommand();
    JobAction joba = new JobAction();

      if (Commands.SAVE.equals(command)) {
        try {
          joba.cmdSave(pageState);

        } catch (ActionException e) {
        }
      } else if ("ONOFF".equals(pageState.command)) {

        joba.cmdEdit(pageState);
        pageState.addClientEntry("enabled",!((Job) pageState.getMainObject()).isEnabled()); // invert value
        joba.cmdSave(pageState);
        joba.cmdFind(pageState);

      } else if ("RUN_NOW".equals(command)) {
        joba.cmdRunNow(pageState);

      } else if (Commands.EDIT.equals(command) || Commands.DELETE_PREVIEW.equals(command)) {
        joba.cmdEdit(pageState);

      } else if (Commands.DELETE.equals(command)) {
        try {
          joba.cmdDelete(pageState);
          PageSeed ps = pageState.pageFromCommonsRoot("scheduler/jobList.jsp");
          pageState.setCommand(Commands.FIND);
          String appName = pageState.getEntry(Fields.APPLICATION_NAME).stringValueNullIfEmpty();
          if (appName != null)
            ps.addClientEntry(Fields.APPLICATION_NAME, appName);

          if (JSP.ex(pageState.getEntry("DO_NOT_REDIR")))
            pageState.addClientEntry("REDIRECT_TO",ps.toLinkToHref());
          else
            pageState.redirect(ps);

          //pageState.redirect(ps);
        } catch (RemoveException ex) {
          // in order to feedback operator il partDelete.jsp
          pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);
        }

      } else if (Commands.ADD.equals(command)) {
        joba.cmdAdd(pageState);

      } else if (Commands.FIND.equals(command)) {
        joba.cmdFind(pageState);
      }
    return pageState;
  }
}
