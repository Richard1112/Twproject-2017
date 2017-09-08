<%@ page import="com.twproject.operator.TeamworkOperator, net.sf.json.JSONObject, org.jblooming.messaging.Message, org.jblooming.oql.QueryHelper, org.jblooming.waf.JSONHelper, org.jblooming.waf.view.PageState, java.util.Date" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;

  try {
    // --------------------------- SET ALL MESSAGES AS READ --------------------------------
    if ("READALL".equals(pageState.command)) {
      String hql="update "+Message.class.getName()+" as m set readOn=:now where toOperator=:op";
      QueryHelper qhelp = new QueryHelper(hql);
      qhelp.addParameter("op", logged);
      qhelp.addParameter("now", new Date());
      qhelp.addOQLClause("m.media=:mm","mm","LOG");
      qhelp.addOQLClause("m.readOn is null");
      int messagesRead = qhelp.toHql().getQuery().executeUpdate();

      json.element("messagesRead",messagesRead);

    }
  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);

%>