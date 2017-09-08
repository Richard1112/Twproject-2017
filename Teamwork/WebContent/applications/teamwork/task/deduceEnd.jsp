<%@ page import="com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.businessLogic.TaskController,
com.twproject.waf.TeamworkPopUpScreen, org.jblooming.agenda.CompanyCalendar, org.jblooming.oql.OqlQuery, org.jblooming.utilities.DateUtilities,
org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS,
org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar,
org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form,
org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState,
java.util.Date, java.util.List"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TaskController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    Task task = (Task) pageState.getMainObject();

    int percOfTimeEmployedForThisTask = pageState.getEntryOrDefault("PERC_EMPLOY", "100").intValue();
    int percOfScopeCreep = pageState.getEntryOrDefault("PERC_CREEP", "10").intValue();

    Date startDate = task.getSchedule().getStartDate();
    CompanyCalendar cc = new CompanyCalendar();
    cc.setTime(startDate);
    List descs = task.getDescendants(Task.class);
    descs.add(task);

    long sumOfEstimatedDuration=task.getTotalWorklogEstimated();

    if (percOfScopeCreep > 0) {
      sumOfEstimatedDuration = Math.round(sumOfEstimatedDuration * (1D + (percOfScopeCreep / 100D)));
    }

    String hql = "select count(assig.resource) from " + Assignment.class.getName() + " as assig where assig.task in (:tasks)";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setParameterList("tasks", descs);
    Long totalWorkers = (Long) oql.uniqueResultNullIfEmpty();

    if (totalWorkers == null)
      totalWorkers = 0L;
    long dedicatedHours = totalWorkers * 8;
    dedicatedHours = (int) (dedicatedHours * (percOfTimeEmployedForThisTask / 100D));

    int totalDays = dedicatedHours > 0 ? (int) Math.round( (sumOfEstimatedDuration/CompanyCalendar.MILLIS_IN_HOUR) / dedicatedHours) : 0;

    if (totalDays > 0)
      cc.addWorkingDays(totalDays);


    String result = DateUtilities.dateToString(cc.getTime());

    //controller

    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = task.getId();
    self.setCommand(Commands.EDIT);
    self.setPopup(true);
    Form f = new Form(self);
    f.start(pageContext);



%><h2><%=I18n.get("DEDUCE_TASK_END")%></h2>

<%=I18n.get("DEDUCE_TASK_END_TEXT1", "<b>" + DateUtilities.dateToString(startDate) + "</b>")%>,
    <%=I18n.get("DEDUCE_TASK_END_TEXT2","<b>"+(int)(sumOfEstimatedDuration/CompanyCalendar.MILLIS_IN_HOUR)+""+"</b>")%>,
    <%=I18n.get("DEDUCE_TASK_END_TEXT3","<b>"+totalWorkers+""+"</b>")%>, <%

  TextField pe = new TextField("PERC_EMPLOY","");pe.fieldSize=3;pe.label="";pe.setValue(pageState.getEntry("PERC_EMPLOY"));

  %> <%=I18n.get("DEDUCE_TASK_END_TEXT4",pe.toHtmlStringBuffer()+"")%>, <%

  TextField pc = new TextField("PERC_CREEP","");pc.fieldSize=3;pc.label="";pc.setValue(pageState.getEntry("PERC_CREEP"));

  %><%=I18n.get("DEDUCE_TASK_END_TEXT5",pc.toHtmlStringBuffer()+"")%>,
  <%=I18n.get("DEDUCE_TASK_END_TEXT6","<b>"+totalDays+"</b>")%>,
  <%=I18n.get("DEDUCE_TASK_END_TEXT7","<b>"+result+"</b>")%>. <%


    ButtonBar bb2 = new ButtonBar();

    ButtonSubmit cals = new ButtonSubmit(f);
    cals.label= I18n.get("CALCULATE");
    bb2.addButton(cals);

    ButtonJS insert = new ButtonJS();
    insert.additionalCssClass = "first";
    insert.label = I18n.get("INSERT");

    insert.onClickScript = "copyValues();";
    bb2.addButton(insert);

    bb2.toHtml(pageContext);

    pageState.setFocusedObjectDomId(pe.id);

    f.end(pageContext);


%>
<script type="text/javascript">
  function copyValues(){
    var targetWin =getBlackPopupOpener();

    var twin = targetWin.$("body");
    twin.find("[name=END]").val("<%=result%>");
    twin.find("[name=TASK_DURATION]").val("<%=totalDays%>");
    closeBlackPopup();

  }
</script>
<%
  }
%>
