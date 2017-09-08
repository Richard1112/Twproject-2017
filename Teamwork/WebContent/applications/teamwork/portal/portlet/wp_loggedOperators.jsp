<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Person,
                 com.twproject.security.TeamworkPermissions,org.jblooming.utilities.DateUtilities, org.jblooming.waf.SessionState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  %><div class="portletBox small">
  <h1><%=I18n.get("LOGGED_OPERATORS")%></h1><%

  if (SessionState.totalOpIds.size() > 1) {

%>

<table class="table"><tr>
  <th class="tableHead"><%=I18n.get("LOGGED_IN_AS")%></th>
  <th class="tableHead"><%=I18n.get("LAST_LOGIN")%></th>
  <th class="tableHead"><%=I18n.get("LAST_REQUEST")%></th>
  </tr>
<!--th>&nbsp;</th-->
    <%
      try {
        for (int op : SessionState.totalOpIds) {

          Person p = Person.getPersonFromOperatorId(op + "");

          if (!p.hasPermissionFor(logged, TeamworkPermissions.resource_canRead))
            continue;
    %>
    <tr class="alternate">
    <td><%=p.getDisplayName()%></td>
      <td align="center"><%=DateUtilities.dateToHourMinutes(p.getMyself().getLastLoggedOn())%></td>
      <td align="center"><%=DateUtilities.dateToHourMinutes(p.getMyself().getLastRequestOn())%></td>
    </tr>
    <%
    }
      } catch (Exception e) {
       //may be in session broken
      }


    %></table><%

    } else {
    %><%=I18n.get("ALONE")%><%
    }

    %></div>

