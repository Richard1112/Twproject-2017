package com.twproject.security.businessLogic;

import org.jblooming.security.businessLogic.LoginController;
import org.jblooming.security.businessLogic.LoginAction;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.constants.Fields;
import com.twproject.operator.TeamworkOperator;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Sep 29, 2008
 * Time: 3:43:40 PM
 */
public class TeamworkLoginController extends LoginController {

  public TeamworkLoginController(String redirectUrl) {

    super(redirectUrl);
    this.loginAction = new TeamworkLoginAction();

  }

  

}
