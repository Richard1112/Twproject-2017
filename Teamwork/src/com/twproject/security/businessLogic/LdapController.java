package com.twproject.security.businessLogic;

import org.jblooming.waf.ActionController;
import org.jblooming.waf.html.input.Collector;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageState;
import org.jblooming.persistence.exceptions.PersistenceException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


public class LdapController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException {
    PageState pageState = PageState.getCurrentPageState(request);
    LdapAction action = new LdapAction(pageState);


    String command = pageState.getCommand();
    if ("IMPORT".equals(command)) {
      action.cmdImport();

    } else if (Collector.isCollectorCommand("ldapUsers", command)) {
      Collector.move("ldapUsers", pageState);

    } else {
      action.cmdFind();
    }
    return pageState;
  }
}                                                     
