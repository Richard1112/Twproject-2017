<%@page pageEncoding="UTF-8" %><%@ page buffer="16kb" %><%@ page import="org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  PageSeed ps= new PageSeed(com.twproject.setup.WizardSupport.getHomePage(pageState));
  ps.addClientEntries(pageState.getClientEntries());
  response.sendRedirect(ps.toLinkToHref());

  //case admin first entered -> home PM, create company -> set personal data and image

  //case user first entered -> set personal data and image -> if permissions, home PM oth. home WRKR

  //response.sendRedirect(request.getContextPath()+"/applications/teamwork/);
  

  %>