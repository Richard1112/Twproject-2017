<%@page import="com.teamwork.expand.ReportExplorerAction"%>
<%@ page import="net.sf.json.JSONObject, org.jblooming.operator.Operator, org.jblooming.remoteFile.RemoteFile, org.jblooming.remoteFile.businessLogic.ExplorerAction, org.jblooming.waf.JSONHelper, org.jblooming.waf.view.PageState" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);
  Operator logged = pageState.getLoggedOperator();

  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;

  try {

    ReportExplorerAction ea= new ReportExplorerAction();
      // ---------------------------------------- DOCUMENT DROP MANAGEMENT ----------------------------------------
    if ("UPLOAD".equals(pageState.command)) {
      RemoteFile rfs = ea.upload(pageState);

      json.element("remoteFile",rfs.jsonify());
    }


  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);

%>