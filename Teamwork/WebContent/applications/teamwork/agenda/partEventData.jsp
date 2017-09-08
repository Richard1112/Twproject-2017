<%@ page
    import="com.twproject.agenda.AgendaBricks, com.twproject.agenda.Event, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.waf.html.MeetingDrawer, org.jblooming.agenda.CompanyCalendar, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.JSP, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.input.*, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.util.List, org.jblooming.operator.Operator" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  JspHelper ped= (JspHelper)JspIncluderSupport.getCurrentInstance(request);

  List<Resource> resources=(List<Resource>)ped.parameters.get("resources");
  Event event = (Event) pageState.getMainObject();
  Person loggedP = Person.getLoggedPerson(pageState);
  boolean isNew = PersistenceHome.NEW_EMPTY_ID.equals(event.getId());
  boolean isAuthor = true;
  if (!isNew && event.getAuthor() != loggedP)
    isAuthor = false;

  boolean isExternal= JSP.ex(event.getIcalId())&& !event.getIcalId().startsWith("TW_");
  Operator loggedOperator = pageState.getLoggedOperator();
  boolean canEdit = isAuthor || !event.isPersonal() && loggedOperator.hasPermissionAsAdmin();
  boolean readOnly= !canEdit || isExternal;

%>
<%--*************************************************************  event  *********************************************************************/--%>
<div id="agenda_event_data" class="inlineContainerWrapper">
  <div class="container">
<table cellpadding="5" cellspacing="0" class="table" border="0">
  <tr>
    <td valign="top" colspan="3">
      <%--<span class="teamworkIcon" style="font-size: 40px;">m</span><br>--%>
      <%
        ScheduleComposer c1 = new ScheduleComposer("SCHEDULE");
        c1.isMinute = false;
        c1.readOnly=readOnly;
        c1.toHtml(pageContext);
      %>

    </td>
  </tr>

  <tr>
     <td colspan="3">

        <div class="groupRow">
            <div class="groupCell col6">

                <%
                    TextField tf = new TextField("text", "AGENDA_SUMMARY", "<br>", 40);
                    tf.required = true;
                    tf.script="style=\"width:100%;\"";
                    tf.readOnly=readOnly;
                    tf.fieldClass="formElements bold";
                    tf.toHtmlI18n(pageContext);
                %><hr class="formElementsSpacer"><%
                    tf = new TextField("text", "LOCATION", "<br>", 40);
                    tf.script="style=\"width:100%;\"";
                    tf.readOnly=readOnly;
                    tf.toHtmlI18n(pageContext);
                %>

            </div>
            <div class="groupCell col6 offset-left">
                <label><%=I18n.get("AGENDA_DESCRIPTION")%></label>
                <%
                    TextArea ta = new TextArea("AGENDA_DESCRIPTION", "", 38, 4, "");
                    ta.label = "";
                    ta.maxlength = 2000;
                    ta.setAutosize(60,300,20);
                    ta.script="style=\"width:100%;\"";
                    ta.readOnly=readOnly;
                    ta.toHtml(pageContext);
                %>
            </div>

        </div>



    </td>
  </tr>
  <tr>
     <td colspan="3">

      <%
    if (event.getMeeting() == null) {
  %><div><%


        CheckField cf = new CheckField("IS_REMINDER", "", false);
        cf.disabled=readOnly;
        cf.toHtmlI18n(pageContext);
        %>&nbsp;&nbsp;&nbsp;&nbsp;<%
        cf = new CheckField("IS_PERSONAL", "", false);
        cf.disabled=readOnly;
        cf.toHtmlI18n(pageContext);
        %>&nbsp;&nbsp;&nbsp;&nbsp;<%
        cf = new CheckField("IS_UNAVAILABLE", "", false);
        cf.disabled=readOnly;
        cf.toHtmlI18n(pageContext);


      %></div><%
    }
  %>
          <hr class="formElementsSpacer">
          <div class="groupRow">
              <div class="groupCell col2 offset-right">

    <%
      SmartCombo cbb = AgendaBricks.getAgendaTypesCombo("TYPE",pageState);
      cbb.separator="<br>";
      cbb.readOnly=readOnly;
      cbb.toHtml(pageContext);
    %>

  </div>
      <div class="groupCell"><%//=I18n.get("REMAINDERS")%><%
    pageState.getEntryOrDefault("ALERT_BEFORE_X_HOURS");
    ComboBox ambnh = ComboBox.getTimeInstance("ALERT_BEFORE_X_HOURS", I18n.get("ALERT_BEFORE_X_HOURS"),"ALRTBF",(int) (CompanyCalendar.MILLIS_IN_MINUTE * 15), pageState);
    ambnh.separator = "<br>";
    ambnh.readOnly=readOnly;
    ambnh.toHtmlI18n(pageContext);

  %><%=JSP.wHelp(I18n.get("HELP_HOUR_MINUTE_FORMAT_SHORT"))%></div>

</div>
      </td>
  </tr>
</table>
</div>


  <div class="container">
    <%--/*************************************************************  attendee  *********************************************************************/--%>
      <h2><%=I18n.get("WORK_GROUP")%></h2><%

      if (isAuthor) {
        if (resources.size() > 0) {
    %><div><%
      for (Resource res: resources) {
    %><div resId="<%=res.getId()%>" class="workgroupElement" ><img src="<%=res.bricks.getAvatarImageUrl()%>" class="face" title="<%=res.getDisplayName()%>"></div><%
      }
    %></div><%
      }
    } else {
      if ( event.getTargets().size() > 0) {
    %><div><%
      for (Resource res:  event.getTargets()) {
    %><div resId="<%=res.getId()%>" class="workgroupElement" ><img src="<%=res.bricks.getAvatarImageUrl()%>" class="face" title="<%=res.getDisplayName()%>"></div><%
      }
    %></div><%
        }

      }


    %><div class="workgroupElement"><%
      ButtonSupport wp = ButtonSubmit.getSubmitInstanceInBlack(pageState.getForm(), request.getContextPath() + "/applications/teamwork/workgroup/workgroupPopup.jsp", 700, 550);
      wp.iconChar = "r";
      wp.label = "";
      wp.toolTip = I18n.get("CHANGE_WORK_GROUP");
      wp.enabled=!readOnly;
      wp.toHtml(pageContext);
    %></div>
  </div>
</div>
