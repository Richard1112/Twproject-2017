<%@page pageEncoding="UTF-8" %>
<%@ page import="com.twproject.operator.TeamworkOperator,
                 org.jblooming.messaging.Listener,
                 org.jblooming.oql.QueryHelper,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 java.util.List"
  %>
<div class="portletBox">
  <%
    PageState pageState = PageState.getCurrentPageState(request);
    TeamworkOperator op = (TeamworkOperator) pageState.getLoggedOperator();
    String hql = "from " + Listener.class.getName();
    QueryHelper query = new QueryHelper(hql);
    query.addQBEClause("ownerx", "owner", op.getId().toString(), QueryHelper.TYPE_CHAR);
    List<Listener> listeners = query.toHql().list();
  %><h1><%=I18n.get("MYSUBSCRIPTIONS")%>
</h1>
  <table class="table" height="100%" width="100%" border="0">
    <tr>
      <th><%=I18n.get("LISTENER_THE_CLASS")%>
      </th>
      <th><%=I18n.get("LISTENER_IDENTIFIABLE_ID")%>
      </th>
      <th><%=I18n.get("LISTENER_COMMAND")%>
      </th>
      <th><%=I18n.get("LISTENER_MEDIA")%>
      </th>
      <th><%=I18n.get("DELETE_SHORT")%>
      </th>
    </tr>
    <%


      for (Listener listener : listeners) {

    %>
    <tr class="alternate">
      <td id="messageList">
        <%=listener.getTheClass().substring(listener.getTheClass().lastIndexOf(".") + 1)%>
      </td>
      <td>
        <%=listener.getIdentifiableId()%>
      </td>
      <td>
        <%=I18n.get(listener.getEventType())%>
      </td>
      <td>
        <%=listener.getMedia().toLowerCase()%>
      </td>
      <td align="center"><%
        PageSeed deleteMessage = pageState.thisPage(request);
        deleteMessage.setMainObjectId(listener.getId());
        deleteMessage.setCommand(Commands.DELETE);
        ButtonLink delLink = new ButtonLink(deleteMessage);
        delLink.iconChar = "d";
        delLink.label = "";
        delLink.toolTip = I18n.get("DELETE");
        delLink.toHtmlInTextOnlyModality(pageContext);
      %></td>
    </tr>
    <%
      }
    %></table>
</div>

