package com.twproject.task.businessLogic;

import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;


public class AdditionalCostController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException, IOException {
    PageState pageState = PageState.getCurrentPageState(request);
    AdditionalCostAction action = new AdditionalCostAction(pageState);

    String command = pageState.getCommand();

    if (Commands.FIND.equals(command)){
      action.cmdFind();
    } else if ("dummy".equalsIgnoreCase(command)){
    } else{
      action.cmdPrepareDefaultFind();
    }

    try {
      ReflectionUtilities.invoke(ApplicationState.applicationParameters.get("get"), "doFilter", request, response);
    } catch (Throwable e) {
      throw new PlatformRuntimeException(e);
    }


    return pageState;
  }

}



