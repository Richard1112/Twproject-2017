<%@ page import="com.twproject.operator.TeamworkOperator,
                com.twproject.resource.Person,
                org.jblooming.operator.Operator,
                org.jblooming.security.Role, org.jblooming.utilities.JSP, org.jblooming.waf.SessionState,
                 org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  Person resource = (Person) pageState.getMainObject();
  TeamworkOperator operator = resource.getMyself();

  ButtonBar buttonBar = pageState.getButtonBar();
  if (operator.getId().equals(pageState.getLoggedOperator().getId())) {
    buttonBar.addSeparator(10);
    PageSeed opt = pageState.pageInThisFolder("options.jsp", request);
    ButtonLink optBL = new ButtonLink(pageState.getI18n("SEE_YOUR_OPTIONS"), opt);
    buttonBar.addButton(optBL);
  }

%>
<table class="table">
          <tr>
            <th>
            <%=pageState.getI18n("INHERITED_ROLES")%>
            </th>
          </tr>
<%
      for (Role role : operator.getInheritedRoles()) {
%>
          <tr>
            <td>
            <%=JSP.w(role.getDisplayName())%>&nbsp;<%=operator.directRolesContain(role) ? "("+pageState.getI18n("DIRECT")+")" : ""%>
            </td>
          </tr>
<%
      }

  Operator owner = resource.getOwner();%>
  <tr>
            <th>
            <%=pageState.getI18n("OWNER")%>
            </th>
          </tr>

   <tr>
            <td>
            owner: (id: <%=owner !=null ? owner.getId() : "-"%>) <%=owner !=null ? owner.getDisplayName() : "-"%>
            </td>
          </tr>


  </table> <%

%>