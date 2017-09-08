<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Resource, com.twproject.resource.ResourceBricks,
                 com.twproject.security.TeamworkPermissions, com.twproject.task.AssignmentPriority,
                 com.twproject.waf.TeamworkHBFScreen, org.jblooming.agenda.CompanyCalendar,
                 org.jblooming.agenda.Period, org.jblooming.agenda.Scale,
                 org.jblooming.ontology.SerializedList, org.jblooming.security.Permission,
                 org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    body.areaHtmlClass="lreq20 lreqPage";
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {


    // get the focused millis
    long focusedMillis= pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
    focusedMillis= focusedMillis==0?System.currentTimeMillis():focusedMillis;
    pageState.addClientEntry("FOCUS_MILLIS",focusedMillis);

    // set the current scale factor to 1WEEK
    Scale.ScaleType scaleType = Scale.ScaleType.SCALE_1WEEK;

    // define the scale and increment of ticks
    Scale scale = Scale.getScaleAndSynch(scaleType, focusedMillis, true, SessionState.getLocale());

    // compute the startMillis and endMillis using focusedMillis and scaleType
    final long minMillisInBar = scale.startPointTime;
    final long maxMillisInBar = scale.endPointTime;

    PageSeed pageSeed = pageState.thisPage(request);

    // add the focused time client entries;
    pageSeed.addClientEntry("FOCUS_MILLIS", minMillisInBar + "");
    
    List<Resource> resources= ResourceBricks.fillWorkGroup(pageState);
    pageSeed.addClientEntry(pageState.getEntry("WG_IDS"));
    pageSeed.addClientEntry("WG_IDS", JSP.w(pageState.getEntry("WG_IDS").stringValueNullIfEmpty()));

    SerializedList<Permission> permissions = new SerializedList();
    permissions.add(TeamworkPermissions.resource_manage);
    pageSeed.addClientEntry("PERM_REQUIRED", permissions);

    CompanyCalendar cc = new CompanyCalendar();
    cc.setTimeInMillis(minMillisInBar);

    Form form = new Form(pageSeed);
    form.id = "ASSIG_PRIOR_FORM";
    form.alertOnChange = true;
    pageState.setForm(form);
    form.start(pageContext);

    // BEGIN HEAD ----------------------------------------------

%><script>$("#PLAN_MENU").addClass('selected');</script>
<h1><%=I18n.get("TASK_WORKGROUP_PRIORITY")%></h1>


<style type="text/css">
  .radioButtons{
    display:none;
  }

  .table.edged > tbody > tr > td {
      padding: 0;
  }

  .PRIORITY_LOW {background-color: <%=AssignmentPriority.getPriorityColor(AssignmentPriority.PRIORITY_LOW)%>;}
  .PRIORITY_MEDIUM {background-color: <%=AssignmentPriority.getPriorityColor(AssignmentPriority.PRIORITY_MEDIUM)%>}
  .PRIORITY_HIGH {background-color: <%=AssignmentPriority.getPriorityColor(AssignmentPriority.PRIORITY_HIGH)%>}

</style>
<div class="optionsBar clearfix">
<%
  TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();
  //check if logged can inspect more see more resources
  boolean canInspectMoreResources = loggedOperator.hasPermissionFor(TeamworkPermissions.resource_manage);
  canInspectMoreResources = canInspectMoreResources || JSP.ex(loggedOperator.getPerson().getMyStaff());


  ButtonSupport wp = ButtonSubmit.getSubmitInstanceInBlack(pageState.getForm(), request.getContextPath() + "/applications/teamwork/workgroup/workgroupPopup.jsp", 700, 550);
  wp.label = "";
  wp.toolTip = I18n.get("CHANGE_WORK_GROUP");
  wp.iconChar="r";
  wp.enabled=canInspectMoreResources;

%><div class="filterElement centered">
  <div class="facesBox"><%

    for (Resource res : resources) {
      Img img = res.bricks.getAvatarImage("");
  %><div class="workgroupElement"><img src="<%=res.bricks.getAvatarImageUrl()%>" title="<%=res.getDisplayName()%>" class='face' align='absmiddle'></div>
  <%
    }
  %>


    <div class="workgroupElement"><%wp.toHtml(pageContext);%></div>
</div></div>


<div class="filterElement centered">
  <div class="filterOptions"><%

      pageState.getEntryOrDefault("SHOW_NOTACTIVE_ASSIGS");
      pageState.getEntryOrDefault("PRIORITY_SHOW_ROUTINARY");
      pageState.getEntryOrDefault("WORKLOG_CLOSED_TASKS");


      ButtonSubmit oms = new ButtonSubmit(pageState.getForm());

      CheckField snaaCF = new CheckField("SHOW_NOTACTIVE_ASSIGS", "", false);
      snaaCF.preserveOldValue = false;
      snaaCF.additionalOnclickScript=oms.generateJs().toString();
      snaaCF.label=I18n.get("ASS_SHOW_DISABLED");
      snaaCF.toHtml(pageContext);
    %>&nbsp;<%

      CheckField phr = new CheckField("PRIORITY_SHOW_ROUTINARY", "", false);
      phr.preserveOldValue = false;
      phr.additionalOnclickScript=oms.generateJs().toString();
      phr.toHtmlI18n(pageContext);

    %>&nbsp;<%
      CheckField sac = new CheckField("WORKLOG_CLOSED_TASKS", "", false);
      sac.preserveOldValue = false;
      sac.additionalOnclickScript=oms.generateJs().toString();
      sac.toHtmlI18n(pageContext);

    %></div>
</div>
</div>

<jsp:include page="../part/partCalendarHeader.jsp"/>

<table class="table edged fixHead" cellpadding="4" cellspacing="0" border="0" id="multi" >
    <thead>
  <tr>
    <%
        cc.setTimeInMillis(minMillisInBar);
        for (int i = 1; i <= 7; i++) {
      %><th class="dayHeader <%=cc.isWorkingDay()?"":"dayHHeader"%> <%=cc.isToday()?"dayTHeader":""%>" style="width: calc(100% / 7)"><%=DateUtilities.dateToString(cc.getTime(), "EEE dd MMM")%></th><%
        cc.add(CompanyCalendar.DAY_OF_MONTH, 1);
      }
    %>
    </tr>
    </thead>
    <%
      boolean swap = true;

      // iterate for all resources selected
      for (Resource person : resources) {

        JspHelper rowDrawer =new JspHelper("partPriorityRow.jsp");
        rowDrawer.parameters.put("person",person);
        rowDrawer.parameters.put("minMillisInBar",minMillisInBar);

        rowDrawer.toHtml(pageContext);
        swap=!swap;
      }
  %>
  </table>
<script type="text/javascript">
  function priorityChanged(colorChooser) {
    showSavingMessage();
    var tr = colorChooser.closest(".rowPrio");

    var req = {
      CM:"CHANGEPRIO",
      ASSID:colorChooser.attr("ass"),
      TIME:colorChooser.attr("time"),
      PRIO:colorChooser.val(),
      MINMILLISINBAR:<%=minMillisInBar%>,
      WORKLOG_CLOSED_TASKS:$("#WORKLOG_CLOSED_TASKS").val(),
      PRIORITY_SHOW_ROUTINARY:$("#PRIORITY_SHOW_ROUTINARY").val(),
      SHOW_NOTACTIVE_ASSIGS:$("#SHOW_NOTACTIVE_ASSIGS").val()
    };
    $.get('priorityAjaxController.jsp', req, function(ret) {
      tr.prev().remove();
      tr.replaceWith(ret);
      hideSavingMessage();
    });
  }

  function clearFromNowOn(button, assId, time) {
    if (confirm("<%=I18n.get("PRIORITY_RESET_FROM_NOW")%>")) {
      showSavingMessage();
      var tr = $(button).parents(".rowPrio").eq(0);
      var req = {
        CM:"CLEAR",
        ASSID:assId,
        TIME:time,
        MINMILLISINBAR:<%=minMillisInBar%>,
        WORKLOG_CLOSED_TASKS:$("#WORKLOG_CLOSED_TASKS").val(),
        PRIORITY_SHOW_ROUTINARY:$("#PRIORITY_SHOW_ROUTINARY").val(),
        SHOW_NOTACTIVE_ASSIGS:$("#SHOW_NOTACTIVE_ASSIGS").val()
      };
      $.get('priorityAjaxController.jsp', req, function(ret) {
        tr.prev().remove();
        tr.replaceWith(ret);
        hideSavingMessage();
      });
    }
  }

  function goToMillis(time) {
    $("[name=FOCUS_MILLIS]").val(time);
    $("#<%=form.id%>").submit();
  }

</script>
<%
pageState.attributes.put("FOCUSED_PERIOD", new Period(minMillisInBar, maxMillisInBar));// used in the bar
%> <jsp:include page="../../parts/timeBar.jsp"/><%
  form.end(pageContext);
  }
%>
