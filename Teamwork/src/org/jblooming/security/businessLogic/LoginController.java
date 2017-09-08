package org.jblooming.security.businessLogic;

import org.jblooming.ApplicationException;
import org.jblooming.system.SystemConstants;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * @author Pietro Polsinelli : ppolsinelli@open-lab.com
 */
public class LoginController implements ActionController {

  public String redirectUrl;
  protected LoginAction loginAction;

  /**
   * @param redirectUrl used in case of any action if no pendingn url is present
   *                    notice that it is always used in cse of logoff
   */
  public LoginController(String redirectUrl) {
    this.redirectUrl = redirectUrl;
    this.loginAction = new LoginAction();
  }

  public PageState perform(HttpServletRequest request, HttpServletResponse response)
          throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException, IOException {


    PageState pageState = PageState.getCurrentPageState(request);
    SessionState sessionState = pageState.sessionState;
    final String command = pageState.getCommand();

    if (Commands.LOGOUT.equals(command)) {
       loginAction.logout(pageState, request,response);
     } else if ("DO_NOTHING".equals(command)) {
     } else {
      // modified by robik 8/3/07 in order to avoid error message when loginname and password pre-loaded
        request.getSession().removeAttribute("CMD_LOG_OUT_PARAM_SESSION_KEY");
      loginAction.login(pageState, request, response);
    }

    String contextPath = request.getContextPath();

    if (!Commands.LOGOUT.equals(command) && sessionState != null && sessionState.isOperatorLogged()) {

      if (!Fields.FALSE.equals(ApplicationState.getApplicationSetting(SystemConstants.ENABLE_REDIR_AFTER_LOGIN))) {
      PageSeed loginPendingUrl = sessionState.getLoginPendingUrl();
      if (loginPendingUrl != null) {
        //no need to show them at redirect
        loginPendingUrl.removeEntry(OperatorConstants.FLD_LOGIN_NAME);
        loginPendingUrl.removeEntry(OperatorConstants.FLD_PWD);
        String url = contextPath + loginPendingUrl.toLinkToHref();
        sessionState.setLoginPendingUrl(null);
        redirectUrl = url;
      }
      }
      response.sendRedirect(redirectUrl);

    }
    return pageState;
  }

}
