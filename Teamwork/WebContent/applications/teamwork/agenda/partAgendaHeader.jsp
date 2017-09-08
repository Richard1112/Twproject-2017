<%@ page import="com.twproject.agenda.Event, com.twproject.agenda.IcalUtilities, com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions,
 org.jblooming.agenda.CompanyCalendar, org.jblooming.ontology.SerializedList, org.jblooming.security.Permission, org.jblooming.utilities.JSP, org.jblooming.waf.SessionState,
 org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink,
  org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.core.JST,
  org.jblooming.waf.html.display.Img, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.io.IOException, java.net.URL, java.util.ArrayList, java.util.List" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  String type = (String) pageState.sessionState.getAttribute("AGENDA_TYPE");
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  long focusMillis = pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
  focusMillis = focusMillis ==0?System.currentTimeMillis(): focusMillis;
  pageState.addClientEntry("FOCUS_MILLIS", focusMillis);

  CompanyCalendar cal = new CompanyCalendar(SessionState.getLocale());
  cal.setTimeInMillis(focusMillis);

  Form form=pageState.getForm();


  /****************************************** Calendar Part by silvia chelazzi & bicch after a while *******************************************************************************/

//------------------------------ ADD CALENDAR
  if ("ADDEXTERNALCAL".equals(pageState.command)) {
    String linkss = pageState.getEntry("EXTERNAL_CAL_LINKS").stringValueNullIfEmpty();

    if (JSP.ex(linkss)) {
      try {
        List<Event> iCalEvents=IcalUtilities.getEventsFromURLs(linkss);
        pageState.sessionState.setAttribute("EXTERNAL_CAL_LINKS", linkss);
        pageState.sessionState.setAttribute("EXTERNAL_CAL_EVENTS", iCalEvents);
        pageState.sessionState.setAttribute("EXTERNAL_CAL_REFRESH_TIME", System.currentTimeMillis());

      } catch (IOException e) {  // errore lanciato se l'url non è scritto corretamente
        pageState.getEntry("EXTERNAL_CAL_LINKS").errorCode = I18n.get("CALENDAR_LINK_NOT_VALID");
      } catch (Exception e) { // errori lanciati in generale se il file è corrotto(formato ical non valido)---> ho messo questo perche lancia tanti tipi diversi errore
        pageState.getEntry("EXTERNAL_CAL_LINKS").errorCode = I18n.get("CALENDAR_FILE_MALFORMED");
      }

    } else {
      pageState.sessionState.getAttributes().remove("EXTERNAL_CAL_LINKS");
      pageState.sessionState.getAttributes().remove("EXTERNAL_CAL_EVENTS");
    }


    //------------------------------ REMOVE CALENDAR
  } else if ("REMOVEEXTERNALCAL".equals(pageState.command)) {
    pageState.removeEntry("EXTERNAL_CAL_LINKS");
    pageState.sessionState.getAttributes().remove("EXTERNAL_CAL_LINKS");
    pageState.sessionState.getAttributes().remove("EXTERNAL_CAL_EVENTS");
  }
  /*************************************************************************************************************************/



%>
<script>
  $("#AGENDA_MENU").addClass('selected');
</script>

<div class="tools"><%
  {
    PageSeed psAdd = new PageSeed("agendaEditor.jsp");
    psAdd.setCommand(Commands.ADD);

    ButtonSubmit add = new ButtonSubmit(form);
    add.label = I18n.get("AGEADD");
    add.iconChar = "P";
    add.variationsFromForm = psAdd;
    add.additionalCssClass = "first";
   %><div class="toolsElement"><% add.toHtml(pageContext);%></div><%
  }


     /* ------------------- Calendar Address created by Silvia Chelazzi the great ------------------- */
  Container address = new Container();
  address.draggable = true;
  address.closeable = true;
  address.width = "600";
  address.centeredOnScreen = true;
  if (!JSP.ex(pageState.getEntry("EXTERNAL_CAL_LINKS").errorCode)) {
    address.status = Container.HIDDEN;
  }
  ButtonJS calendarContainerButt = address.getOpenerButton(false);
  calendarContainerButt.label = "";
  calendarContainerButt.toolTip = I18n.get("CALENDAR_ADDRESS");
  calendarContainerButt.iconChar = "I";

  %><div class="toolsElement"><% calendarContainerButt.toHtml(pageContext);%></div><%

  //search
  PageSeed ps = new PageSeed("agendaList.jsp");

  ps.command=Commands.FIND;

  ButtonSubmit sub = new ButtonSubmit(form);
  sub.iconChar = "L";
  sub.label = "";
  sub.toolTip = I18n.get("SEARCH");
  sub.variationsFromForm = ps;
  sub.hasFocus = pageState.href.contains(ps.href);
%><div class="toolsElement"><% sub.toHtml(pageContext);%></div><%

%></div>

<h1><%=I18n.get("AGENDA")%></h1>



<div class="optionsBar clearfix">
  <div class="filterElement centered"><%
  TextField.hiddenInstanceOfFormToHtml("FILTER", form, pageContext);
  TextField.hiddenInstanceToHtml("SHOW_LIST", JSP.w(pageState.getEntry("SHOW_LIST").stringValueNullIfEmpty()), pageContext);


  //filters


  String label ="";
  if ("NONE".equals(pageState.getEntry("FILTER").stringValueNullIfEmpty())) {

  label = I18n.get("FILTER_NONE");
  } else if ("WORK".equals(pageState.getEntry("FILTER").stringValueNullIfEmpty())) {
  label = I18n.get("FILTER_WORK");
  } else if ("PERSONAL".equals(pageState.getEntry("FILTER").stringValueNullIfEmpty())) {
  label = I18n.get("FILTER_IS_PERSONAL");
  } else if ("ONLY_ME_IN_IT".equals(pageState.getEntry("FILTER").stringValueNullIfEmpty())) {
  label=I18n.get("FILTER_ONLY_ME_IN_IT");
  } else if ("AUTHOR".equals(pageState.getEntry("FILTER").stringValueNullIfEmpty())) {
  label=I18n.get("FILTER_AUTHOR");
  } else if ("UNAVAIL".equals(pageState.getEntry("FILTER").stringValueNullIfEmpty())) {
  label=I18n.get("FILTER_UNAVAIL");
  }

  %><%=label%>&nbsp;<%
  ButtonJS filter = new ButtonJS("");
  filter.label = "";
  filter.iconChar="[";
  filter.additionalCssClass="arrowSmall";
  filter.toolTip = I18n.get("MY_SAVED_FILTERS");
  DivOnMouseover domo = new DivOnMouseover(filter);

  {
    ButtonSubmit bs = new ButtonSubmit(form);
    bs.variationsFromForm.addClientEntry("FILTER", "NONE");
    bs.hasFocus = "NONE".equals(pageState.getEntry("FILTER").stringValueNullIfEmpty());
    bs.label = I18n.get("FILTER_NONE");
    domo.addButton(bs);
  }
  {
    ButtonSubmit bs = new ButtonSubmit(form);
    bs.variationsFromForm.addClientEntry("FILTER", "WORK");
    bs.hasFocus = "WORK".equals(pageState.getEntry("FILTER").stringValueNullIfEmpty());
    bs.label = I18n.get("FILTER_WORK");
    domo.addButton(bs);
  }
  {
    ButtonSubmit bs = new ButtonSubmit(form);
    bs.variationsFromForm.addClientEntry("FILTER", "PERSONAL");
    bs.hasFocus = "PERSONAL".equals(pageState.getEntry("FILTER").stringValueNullIfEmpty());
    bs.label = I18n.get("FILTER_IS_PERSONAL");
    domo.addButton(bs);
  }

  {
    ButtonSubmit bs = new ButtonSubmit(form);
    bs.variationsFromForm.addClientEntry("FILTER","ONLY_ME_IN_IT");
    bs.hasFocus="ONLY_ME_IN_IT".equals(pageState.getEntry("FILTER").stringValueNullIfEmpty());
    bs.label=I18n.get("FILTER_ONLY_ME_IN_IT");
    domo.addButton(bs);
  }

  {
    ButtonSubmit bs = new ButtonSubmit(form);
    bs.variationsFromForm.addClientEntry("FILTER","AUTHOR");
    bs.hasFocus="AUTHOR".equals(pageState.getEntry("FILTER").stringValueNullIfEmpty());
    bs.label=I18n.get("FILTER_AUTHOR");
    domo.addButton(bs);
  }
  {
    ButtonSubmit bs = new ButtonSubmit(form);
    bs.variationsFromForm.addClientEntry("FILTER","UNAVAIL");
    bs.hasFocus="UNAVAIL".equals(pageState.getEntry("FILTER").stringValueNullIfEmpty());
    bs.label=I18n.get("FILTER_UNAVAIL");
    domo.addButton(bs);
  }


  // filter by type two on the bar the rest in the more
  SerializedList<Permission> permissions = new SerializedList();
  permissions.add(TeamworkPermissions.resource_canRead);
  //permissions.add(TeamworkPermissions.assignment_manage);
  TextField.hiddenInstanceToHtml("PERM_REQUIRED", permissions.serialize(), pageContext);


  domo.toHtmlInTextOnlyModality(pageContext);



%></div>
<div class="filterElement centered">
    <div style="float:left;" class="legendaBox">

      <div id="workgroupLegenda" style="display: inline"></div>
      <div class="workgroupElement"><%
        ButtonSupport wp = ButtonSubmit.getSubmitInstanceInBlack(form, request.getContextPath() + "/applications/teamwork/workgroup/workgroupPopup.jsp", 700, 550);
        wp.iconChar = "r";
        wp.label = "";
        wp.toolTip = I18n.get("CHANGE_WORK_GROUP");
        wp.toHtml(pageContext);
      %></div>


    </div>

    <div class="agendaTemplates" style="display:none;">
      <%=JST.start("WORKGROUPELEMENT")%>
      <div class="workgroupElement">
        <img src="(#=obj.resAvatarUrl#)" title="(#=obj.resName#)" class='face' align='absmiddle'>
      </div>
      <%=JST.end()%>


    </div>
</div>
  <div class="filterElement centered"><%
  //------------------ show task/issues on week and day
  if (!"MONTH".equalsIgnoreCase(type)){
    pageState.getEntryOrDefault("SHOW_TASK_ISSUES", Fields.FALSE);
    CheckField showTaskIssues = new CheckField(I18n.get("SHOW_TASK_ISSUES"),"SHOW_TASK_ISSUES","&nbsp;",false);
    showTaskIssues.additionalOnclickScript="$('#"+form.id+"').submit()";
    showTaskIssues.toHtml(pageContext);

  }
%></div>
</div>


<table width="100%" cellspacing="0" cellpadding="0" border="0">
  <tr>
    <td class="calHeader left">
        <span class="button textual noprint" onclick="goToMillis(new Date().getTime());"><%=I18n.get("TODAY")%></span>
        <div style="float:right">
          <span class="button textual icon noprint" onclick="goToMillis(new Date(focusMillis).add(agendaType=='DAY'?'d':(agendaType=='WEEK'?'w':'M'),-1).getTime());"><span class="teamworkIcon" style="font-size:18px">{</span></span>
          <span class="button textual icon noprint" onclick="goToMillis(new Date(focusMillis).add(agendaType=='DAY'?'d':(agendaType=='WEEK'?'w':'M'),1).getTime());"><span class="teamworkIcon" style="font-size:18px">}</span></span>
        </div>
    </td>
    <td class="calHeader">
      <h2 style="margin:0">
        <div style="position:relative;height:30px;">
          <div id="topHeaderCentral"></div>
          <div class="headerCalendarOpener" title="<%=I18n.get("DATEFIELDCALENDAR")%>" id="openCal" onclick="$(this).dateField({inputField:$('#dummy'),callback:function(date){goToMillis(date.getTime());}}); " ><input type="hidden" id="dummy"><span class="teamworkIcon">m</span></div>
        </div>
      </h2>
    </td>
    <td class="calHeader right">
      <%
        ps = new PageSeed("agendaWeekDay.jsp");
        ps.addClientEntry("AGENDA_TYPE","DAY");
        sub = new ButtonSubmit(form);
        sub.label = I18n.get("DAY");
        sub.variationsFromForm = ps;
        sub.hasFocus =  "DAY".equals(pageState.sessionState.getAttribute("AGENDA_TYPE"));
        sub.toHtmlInTextOnlyModality(pageContext);
      %>&nbsp;&nbsp;&nbsp;<%
      ps = new PageSeed("agendaWeekDay.jsp");
      ps.addClientEntry("AGENDA_TYPE","WEEK");
      sub = new ButtonSubmit(form);
      sub.label = I18n.get("WEEK");
      sub.variationsFromForm = ps;
      sub.hasFocus =  "WEEK".equals(pageState.sessionState.getAttribute("AGENDA_TYPE"));
      sub.toHtmlInTextOnlyModality(pageContext);
    %>&nbsp;&nbsp;&nbsp;<%
      ps = new PageSeed("agendaMonth.jsp");
      ps.addClientEntry("AGENDA_TYPE","MONTH");
      sub = new ButtonSubmit(form);
      sub.label = I18n.get("MONTH");
      sub.variationsFromForm = ps;
      sub.hasFocus = pageState.href.contains(ps.href);
      sub.toHtmlInTextOnlyModality(pageContext);

    %>
    </td>
  </tr>
</table>


<%-- ------------------------------------   EXTERNAL ICAL ADDRESS CONTAINER  ---------------------------------------- --%>    <%
  address.start(pageContext);

  String href = ApplicationState.serverURL + "/agendaInIcal.ics?" + Commands.COMMAND + "=EXTERNAL&USR=" + logged.getId() + "&CK=" + logged.getPassword();

%><%=I18n.get("TW_CALENDAR_%%_LINK_DESCRIPTION", href)%>
<textarea class="formElements" rows="2" cols="60" style="width:90%; font-size:12pt;" readonly><%=JSP.encode(href)%></textarea>

<hr>
<br><%new Img("ical.gif", "ICAL format").toHtml(pageContext);%> <%=I18n.get("VIEW_OTHER_CAL_DESCR")%>
 <br><%=JSP.wHelp(I18n.get("TW_CALENDAR_GOOGLE_ICAL_EXAMPLE"))%>

<%
  //make
  pageState.addClientEntry("EXTERNAL_CAL_LINKS",JSP.w(pageState.sessionState.getAttribute("EXTERNAL_CAL_LINKS")));
  TextField extLink = new TextField("EXTERNAL_CAL_LINKS", "");
  extLink.fieldSize = 55;
  extLink.label = "";//"VIEW_OTHER_CAL_DESCR";
  extLink.toHtmlI18n(pageContext);

  ButtonSubmit importcal = new ButtonSubmit(form);
  importcal.label = I18n.get("ADD");
  importcal.variationsFromForm.command="ADDEXTERNALCAL";
  importcal.additionalCssClass="first small";
  importcal.toHtml(pageContext);

  ButtonSubmit hidecal = new ButtonSubmit(form);
  hidecal.label = I18n.get("HIDE_EXTERNAL_CALENDAR_EVENTS");
  hidecal.variationsFromForm.command="REMOVEEXTERNALCAL";

  hidecal.toHtmlInTextOnlyModality(pageContext);

%>&nbsp;&nbsp;&nbsp;<%

  PageSeed importIcalFile = pageState.pageInThisFolder("importIcal.jsp", request);
  ButtonLink imp = new ButtonLink(I18n.get("IMPORT_FROM_ICAL_FILE"), importIcalFile);
  imp.toHtmlInTextOnlyModality(pageContext);
%><br> <br> <%
  address.end(pageContext);
/**
 * if there is an error the container stay opened
 */
  if (JSP.ex(pageState.getEntry("EXTERNAL_CAL_LINKS").errorCode)) {
%>
<script type="text/javascript">
  initialize(contextPath + "/commons/layout/container/partContainer.js", "script");
  <%=calendarContainerButt.generateJs()%>
</script>
<%
  }



%>



