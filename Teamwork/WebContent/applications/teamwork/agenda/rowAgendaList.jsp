<%@ page import="com.twproject.agenda.Event, com.twproject.resource.Person, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.display.Paginator" %>
<%

  JspHelper rowDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  Object[] obj= (Object[]) rowDrawer.parameters.get("ROW_OBJ");


  PageState pageState = PageState.getCurrentPageState(request);

  if ("DRAW_PAGE_FOOTER".equals(request.getAttribute(Paginator.ACTION))) {

%><tfoot><tr><td id="bulkPlace" colspan="99"></td></tr></tfoot><%

  } else {

  Person loggedPerson = Person.getLoggedPerson(pageState);


  PageSeed e = new PageSeed("agendaEditor.jsp");
  e.setCommand(Commands.EDIT);

    Event event = (Event)obj[0];
    boolean canSeeDetail =
      (event.getAuthor() != null && event.getAuthor().equals(loggedPerson))||
        (event.isPersonal() && event.getTargets().contains(loggedPerson))||
        (!event.isPersonal() );// || logged.hasPermissionAsAdmin()));
    e.setMainObjectId(event.getId());
    ButtonLink editLink = new ButtonLink(e);
    editLink.enabled=canSeeDetail;
    editLink.iconChar="e";
    editLink.label="";

%>
<tr  class="alternate"  evId="<%=event.getId()%>">
  <td class="ck"><input type="checkbox" onclick="refreshBulk($(this));"></td>
  <td align="center"><%editLink.toHtmlInTextOnlyModality(pageContext);%></td>
  <td>
    <%=event.getSchedule()!=null ? JSP.w(event.getSchedule().getStartDate()) : "!!! EVENT WITHOUT SCHEDULE !!!"%>
  </td>
  <td>
    <%=event.getSchedule()!=null ? JSP.w(event.getSchedule().getEndDate()) : "!!! EVENT WITHOUT SCHEDULE !!!"%>
  </td>
  <td>
    <%=event.getSchedule()!=null ? DateUtilities.getMillisInHoursMinutes(event.getSchedule().getStartTimeInMillis()) : "!!! EVENT WITHOUT SCHEDULE !!!"%>
  </td>
  <td>
    <%=event.getSchedule()!=null ? DateUtilities.getMillisInHoursMinutes(event.getSchedule().getStartTimeInMillis() + event.getSchedule().getDurationInMillis()) : "!!! EVENT WITHOUT SCHEDULE !!!"%>
  </td>
  <td style="max-width: 300px;"><%=canSeeDetail ? JSP.w(event.getSummary()) : I18n.get("IS_PERSONAL")%> </td>
  <td style="max-width: 300px;"><%=canSeeDetail ? JSP.w(event.getDescription()): I18n.get("IS_PERSONAL")%> </td>

  <td nowrap>
    <%=(event.getAuthor() != null ? event.getAuthor().getDisplayName() : "")%>
  </td>
  <td>
    <%=canSeeDetail ? (event.getLocation() != null ? event.getLocation() : ""): I18n.get("IS_PERSONAL")%>
  </td>
  <td>
    <%=canSeeDetail ? (event.getType()!=null ? event.getType().getDescription() : ""): I18n.get("IS_PERSONAL")%>
  </td>


  <td align="center">
    <%
      PageSeed del = new PageSeed("agendaEditor.jsp");
      del.setCommand(Commands.DELETE_PREVIEW);
      del.setMainObjectId(event.getId());
      ButtonLink delLink = new ButtonLink(del);
      if (!canSeeDetail)
        delLink.isEnabled();
      delLink.iconChar="d";
      delLink.label="";
      delLink.additionalCssClass="delete";
      delLink.toHtmlInTextOnlyModality(pageContext);%>
  </td>
</tr><%
  }
%>
