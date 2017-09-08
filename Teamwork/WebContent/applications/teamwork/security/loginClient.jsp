<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.security.businessLogic.TeamworkLoginAction, net.sf.json.JSONObject,  org.jblooming.utilities.JSP, org.jblooming.waf.JSONHelper, org.jblooming.waf.view.PageState, com.twproject.security.TeamworkPermissions" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);

  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;

  try {
    // --------------------------------------------- GETTOKEN ------------------------------------------
    if ("GTK".equals(pageState.command)) {
      String name=pageState.getEntry("TKN").stringValueNullIfEmpty();
      if (JSP.ex(name))
        json.element(name,pageState.tokenCreate(name));


    // --------------------------------------------- LOGOUT ------------------------------------------
    } else if ("LO".equals(pageState.command)) {
      new TeamworkLoginAction().logout(pageState, request,response);


      // --------------------------------------------- TEST IF LOGGED AND EVENTUALLY TRY A COOKIES LOGIN ------------------------------------------
    } else if ("TST".equals(pageState.command)) {
      //si guarda se c'è già
      TeamworkOperator operator = (TeamworkOperator) pageState.getLoggedOperator();
      if (operator!=null) {
        json.element("loginOk", true);
      } else {
        //si prova a fare un login con i cookies
        new TeamworkLoginAction().login(pageState, request, response);
        operator = (TeamworkOperator) pageState.getLoggedOperator();
        if (operator != null) {
          json.element("loginOk", true);
          json.element("newSession", true); //si dice che c'è una nuova sessione
          /*JSONObject jsonifyOP = operator.jsonify();
          jsonifyOP.element("canCreateProject", operator.hasPermissionFor(TeamworkPermissions.project_canCreate));
          jsonifyOP.element("canCreateIssue", operator.hasPermissionFor(TeamworkPermissions.issue_canCreate));
          jsonifyOP.element("canCreateResource", operator.hasPermissionFor(TeamworkPermissions.resource_canCreate));
          json.element("user", jsonifyOP);*/

        } else {
          json.element("loginOk", false);
        }
      }

      // --------------------------------------------- LOGIN WITH USER & PASSWORD ------------------------------------------
    } else if ("LI".equals(pageState.command)) {
      new TeamworkLoginAction().login(pageState, request, response);
      TeamworkOperator operator = (TeamworkOperator) pageState.getLoggedOperator();

      if (operator!= null ) {
        json.element("loginOk", true);
        JSONObject jsonifyOP = operator.jsonify();
        jsonifyOP.element("canCreateProject", operator.hasPermissionFor(TeamworkPermissions.project_canCreate));
        jsonifyOP.element("canCreateIssue", operator.hasPermissionFor(TeamworkPermissions.issue_canCreate));
        jsonifyOP.element("canCreateResource", operator.hasPermissionFor(TeamworkPermissions.resource_canCreate));
        json.element("user", jsonifyOP);

      } else {
        json.element("loginOk", false);
      }
    }

  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);

%>