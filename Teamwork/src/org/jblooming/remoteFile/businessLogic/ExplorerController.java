package org.jblooming.remoteFile.businessLogic;

import org.jblooming.waf.view.PageState;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import java.io.IOException;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class ExplorerController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response)
          throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException, IOException {

    PageState pageState = PageState.getCurrentPageState(request);
    ExplorerAction action = new ExplorerAction();
    String command = pageState.getCommand();

    if ("ZIP".equals(command)) {
      try {
        action.cmdZip(request, response, pageState);
      } catch (IOException e) {
        throw new PlatformRuntimeException(e);
      }
    } else if ("MKDIR".equals(command)) {
      action.mkdir(pageState);

     } else if (Commands.DELETE.equals(command)) {
      action.cmdDelete(pageState);

    } else if ("UPLOAD".equals(command)) {
      action.upload(pageState);
    }

    return pageState;
  }
}
