<%@ page import=" com.twproject.agenda.Event,
                 com.twproject.resource.Person,
                 com.twproject.resource.Resource,
                 com.twproject.waf.html.EventWriter,
                 org.jblooming.persistence.PersistenceHome,
                 org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  Person loggedPerson = Person.getLoggedPerson(pageState);


  EventWriter eventWriter = (EventWriter) JspIncluderSupport.getCurrentInstance(request);

  Event event = eventWriter.event;
  String toolTip ="";// JSP.w(JSP.convertLineFeedToBR(event.getDescription())) + "<br>";

  boolean first=true;

  for (Resource res:event.getTargets()){
    toolTip +=(first?"":"<br>")+JSP.makeTag("small","",res.getDisplayName());
    first=false;
  }


  PageSeed ps = new PageSeed(request.getContextPath() + "/applications/teamwork/agenda/agendaEditor.jsp");
  ps.setCommand(Commands.EDIT);
  ps.setMainObjectId(event.getId());

%><div onmouseover="obj('<%=eventWriter.rectangle.id%>').btf=true;" onmouseout="obj('<%=eventWriter.rectangle.id%>').btf=false;
setTimeout( 'if (!obj(\'<%=eventWriter.rectangle.id%>\').btf){obj(\'<%=eventWriter.rectangle.id%>\').style.zIndex=\'\'}',1000);"><%
if (eventWriter.showTargetLegenda ){
  %>
  <table border="0" cellspacing="1" cellpadding="1">
    <tr>
      <%
        for (Resource resource : event.getTargets()) {
          %><td bgcolor="<%=eventWriter.colorInfo.get(resource)%>" title="<%=resource.getDisplayName()%>"><div style="position:relative;width:10px;height:10px;"></div></td><%
        }
      %>
    </tr>
  </table>
  <%
 }
  %>
  <table border="0" cellspacing="1" cellpadding="1" >
    <tr>
      <td >
        <b>
          <small><%=DateUtilities.dateToHourMinutes(event.getSchedule().getValidityStartDate())%>-<%=DateUtilities.dateToHourMinutes(event.getSchedule().getValidityEndDate()) %></small>
        </b>
        <%

          boolean canSeeDetail = !event.isPersonal() || event.getTargets().contains(loggedPerson);
          boolean isNew = PersistenceHome.NEW_EMPTY_ID.equals(event.getId());
          String body = null;
          if (!canSeeDetail) {
            body = pageState.getI18n("IS_PERSONAL");
          } else {
            body = event.getSummary();
          }
          ButtonLink bEdit = ButtonLink.getTextualInstance(JSP.w(body), ps);
          bEdit.enabled = canSeeDetail && !isNew;
          bEdit.toolTip = toolTip;
          bEdit.toHtml(pageContext);

          if (canSeeDetail) {
        %>
        <br>
        <small class='linkEnabled'><%=JSP.encode(event.getDescription())%><br>
          <small class='linkEnabled'><i><%=JSP.encode(event.getLocation())%>
          </i></small></small>
        <%
          }
        %></td>
    </tr>
  </table>
</div>