<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %><%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Person,
                 com.twproject.task.Task,
                 com.twproject.task.TaskBricks,
                 com.twproject.task.TaskScheduleHistory,
                 com.twproject.task.businessLogic.TaskController,
                 com.twproject.waf.TeamworkHBFScreen,
                 com.twproject.waf.html.TaskHeaderBar,
                 org.jblooming.agenda.CompanyCalendar,
                 org.jblooming.designer.DesignerField, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.*, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState, org.jbpm.graph.def.ProcessDefinition, org.jbpm.graph.exe.ProcessInstance, java.util.Set, com.twproject.security.TeamworkPermissions"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TaskController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

    Task task = (Task) pageState.getMainObject();

    // Form -----------------------------------------------------------------------------------------------
    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = task.getId();
    self.setCommand(Commands.EDIT);
    String parId = pageState.getEntry(Fields.PARENT_ID).stringValueNullIfEmpty();
    self.addClientEntry(Fields.PARENT_ID,JSP.w(parId));

    Form f = new Form(self);
    f.encType=Form.MULTIPART_FORM_DATA;
    f.alertOnChange = true;
    pageState.setForm(f);

    f.start(pageContext);


    boolean isProcessDriven=task.isProcessDriven();
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Person loggedPerson=logged.getPerson();
%>
<div class="mainColumn"><%


  //-----------------HEAD BAR START
  pageState.addClientEntry("TASK_TABSET","TASK_GENERAL_TAB");
  TaskHeaderBar head = new TaskHeaderBar(task);
  //head.pathToObject.pageTitle=I18n.get("TASK_GENERAL_TAB");
  //head.pathToObject.mainClass = Task.class;
  head.toHtml(pageContext);
  //-----------------HEAD BAR END
%>

<div class="inlineContainerWrapper">

<div class="container  <%=(task.isNew() ? "new" : "" )%>">
<% //---------------------------------------------------------------------------------  START PROCESS MANAGEMENT ------------------------------------------------------------------------------------
  if (isProcessDriven) {
    %><div class="box"> <%
    ProcessInstance processInstance = task.getProcess();
    ProcessDefinition def = processInstance.getProcessDefinition();
    %><span class="teamworkIcon">&pound;</span> <b><%=I18n.get("PROCESS_DRIVEN_TASK")%></b>:&nbsp;<%=def.getName()%>&nbsp;<%--small>(ver. <%=def.getVersion()%> id:"<%=def.getId()%>)</small--%>&nbsp;&nbsp;<%

  PageSeed seeProcess=pageState.pageFromRoot("task/taskProcess.jsp");
  seeProcess.command= "SHOW_GRAPH";
  seeProcess.mainObjectId=task.getId();
  seeProcess.setPopup(true);

  ButtonSupport process = ButtonLink.getBlackInstance(I18n.get("SEE_PROCESS"), seeProcess);
  process.toHtmlInTextOnlyModality(pageContext);%><%




%></div><%
  }
  //---------------------------------------------------------------------------------  END PROCESS MANAGEMENT ------------------------------------------------------------------------------------

  //notification and note input are generated only if task is not new and it is created 6 hours before in order to not annoy users
  boolean notifyStatusAndDateChanges = !task.isNew() && (System.currentTimeMillis()-task.getCreationDate().getTime()> CompanyCalendar.MILLIS_IN_6_HOUR);

%>

<table border="0" class="table" cellpadding="5">

<tr>
  <td valign="top" style="height: 80px; width: 25%"><% TextField tf = new TextField("TASK_CODE", "<br>");
    tf.fieldSize = 15;
    boolean canWrite = task.hasPermissionFor(logged,TeamworkPermissions.task_canWrite);
    tf.readOnly = !canWrite;
    tf.script="style='width:100%'";
    tf.toHtmlI18n(pageContext); %>
    <small><%=task.isNew() ? "" : "(id:&nbsp;" + task.getId() + ")"%></small>
  </td>
  <td colspan="3" valign="top" style="width: 75%"><%
    TextField tfn = new TextField("TASK_NAME", "<br>");
    tfn.fieldSize = 50;
    tfn.readOnly = !canWrite;
    tfn.required = true;
    tfn.fieldClass = "formElements bold";
    tfn.script="style='width:100%'";
    tfn.toHtmlI18n(pageContext);
  %></td>
</tr>

<tr class="dateRow">
  <td nowrap width="25">
    <div style="position:relative"><%

      DateDurationInput ddi = new DateDurationInput("START", "STARTISMILESTONE", "END", "ENDISMILESTONE", "TASK_DURATION", pageState);

    %><%=JSP.makeTag("label", "for=\"START\"",I18n.get("START"))%>&nbsp;&nbsp;&nbsp;&nbsp;<%
      CheckField cbx = ddi.startMilestoneField;
      cbx.disabled = !canWrite;
      cbx.label =I18n.get("ISMILESTONE");
      ddi.drawStartMiles(pageContext);
    %><%
      DateField df = ddi.startField;
      df.readOnly = !canWrite || task.getPreviousesSize() > 0; //task.getChildrenSize()>0 ||
      df.labelstr = "";
      df.size = 10;
      df.separator = "<br>";
      if (notifyStatusAndDateChanges) {
        df.script = "onFocus = \" if ($('#" + df.fieldName + "').isValueChanged()){$('#" + df.fieldName + "').after($('#LOG_DATE_CHANGE_CONTAINER').show());} \"" +
            "onKeydown=\" if ($('#" + df.fieldName + "').isValueChanged()){$('#" + df.fieldName + "').after($('#LOG_DATE_CHANGE_CONTAINER').show());} \"";
      }

      ddi.drawStart(pageContext);

    %><div id="LOG_DATE_CHANGE_CONTAINER" class="microEdit mbBalloon" style="display:none; width: 300px">
      <div style="top: -12px; left: 10px;" class="arrow n"></div>
      <div style="top: -12px; left: 10px;border-color:#fff" class="arrow border n"></div>

      <div style="float:right"><span class="teamworkIcon meClose" title="Close" onclick="$('#LOG_DATE_CHANGE_CONTAINER').hide();" style="cursor: pointer; font-size: 15px">x</span></div>
      <h3><%=I18n.get("LOG_DATE_CHANGE")%></h3>

      <%
        TextArea ta = new TextArea("LOG_DATE_CHANGE", "", 25, 3, null);
        ta.readOnly = !canWrite;
        ta.label = "";
        ta.maxlength=2000;
        ta.script = "style='width:100%;'";
        ta.toHtml(pageContext);


      %></div>
    </div>
  </td><td nowrap width="25">
  <%=JSP.makeTag("label", "for=\"END\"",I18n.get("END"))%>&nbsp;&nbsp;&nbsp;&nbsp;<%
  CheckField cby = ddi.endMilestoneField;
  cby.disabled = !canWrite;
  cby.label =  I18n.get("ISMILESTONE");
  ddi.drawEndMiles(pageContext);
%><%
  df = ddi.endField;
  df.readOnly = !canWrite;
  df.labelstr = "";
  df.size = 10;
  df.separator = "<br>";
  if (!task.isNew()) {
    df.script = "onFocus = \" if ($('#" + df.fieldName + "').isValueChanged()){$('#" + df.fieldName + "').after($('#LOG_DATE_CHANGE_CONTAINER').show());} \"" +
        "onKeydown=\" if ($('#" + df.fieldName + "').isValueChanged()){$('#" + df.fieldName + "').after($('#LOG_DATE_CHANGE_CONTAINER').show());} \"";

  }
  ddi.drawEnd(pageContext);
%></td><td nowrap width="25"><%

  TextField td = ddi.durationField;
  td.separator = "<br>";
  td.readOnly = !canWrite || (task.isStartIsMilestone() && task.isEndIsMilestone()); //|| task.getChildrenSize()>0
  if (notifyStatusAndDateChanges) {
    td.script = td.script +" onFocus = \" if ($('#" + td.fieldName + "').isValueChanged()){$('#" + td.fieldName + "').after($('#LOG_DATE_CHANGE_CONTAINER').show());} \"" +
        "onKeydown=\" if ($('#" + td.fieldName + "').isValueChanged()){$('#" + td.fieldName + "').after($('#LOG_DATE_CHANGE_CONTAINER').show());} \"";

  }
  td.label = I18n.get("DAYS");
  td.toolTip = I18n.get("HELP_DURATION");
  ddi.drawDuration(pageContext);


  PageSeed ded = pageState.pageInThisFolder("deduceEnd.jsp", request);
  ded.setCommand(Commands.EDIT);
  ded.mainObjectId = task.getId();
  ButtonSupport dedPop = ButtonLink.getBlackInstance(JSP.wHelp("&nbsp;"+I18n.get("DEDUCE_END")), 400, 600, ded);
  dedPop.enabled = canWrite && !task.isNew() && task.getSchedule() != null && task.getSchedule().getStartDate() != null;
  dedPop.toHtmlInTextOnlyModality(pageContext);

  %></td><td valign="top" width="25">
  <div style="margin:0; position: relative;">
    <%

      ColorValueChooser cvc = task.bricks.getStatusChooser("STATUS");
      cvc.showOpener = true;
      cvc.id = "taskGetStatusChooser";
      cvc.width = 120;
      cvc.height = 26;
      cvc.readOnly = !task.hasPermissionFor(logged, TeamworkPermissions.task_canChangeStatus)  || isProcessDriven;
      cvc.label = I18n.get(cvc.fieldName);
      cvc.separator = "<br>";
      if (notifyStatusAndDateChanges) {
        cvc.onChangeScript = "$('#LOG_STATUS_CHANGE_CONTAINER').show();obj('LOG_STATUS_CHANGE').focus();warnOnClosing();";
      } else
        cvc.preserveOldValue = false;
      cvc.toHtml(pageContext);
    %>

    <div id="LOG_STATUS_CHANGE_CONTAINER" class="microEdit mbBalloon" style="display:none; width: 300px">
      <div style="top: -12px; left: 10px;" class="arrow n"></div>
      <div style="top: -12px; left: 10px;border-color:#fff" class="arrow border n"></div>

      <div style="float:right"><span class="teamworkIcon meClose" title="Close" onclick="$('#LOG_STATUS_CHANGE_CONTAINER').hide();" style="cursor: pointer; font-size: 15px">x</span></div>
      <h3><%=I18n.get("STATUS_CHANGE_LOG")%></h3>

      <%
        ta = new TextArea("LOG_STATUS_CHANGE", "", 25, 3, null);
        ta.readOnly = !canWrite;
        ta.label = "";
        ta.script = "style='width:100%;'";
        ta.maxlength=2000;
        ta.toHtml(pageContext);

      %></div>
  </div><%
  Set<TaskScheduleHistory> tsh = task.getScheduleHistory();
  boolean hasCommentedScheduleChanges = false;
  for (TaskScheduleHistory taskScheduleHistory : tsh) {
    if (JSP.ex(taskScheduleHistory.getChangeLog())) {
      hasCommentedScheduleChanges = true;
      break;
    }
  }
  if (hasCommentedScheduleChanges) {

    PageSeed seeChang = pageState.pageInThisFolder("taskLogs.jsp", request);
    seeChang.mainObjectId = task.getId();
    ButtonSupport tlog = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("DEDUCE_END")), seeChang);
    tlog.label = JSP.wHelp(I18n.get("THERE_HAVE_BEEN_SCHEDULE_CHANGES"));
    tlog.additionalCssClass ="small";
    tlog.toHtmlInTextOnlyModality(pageContext);

  }
%>
</td>
</tr><tr>
  <td nowrap><%
    SmartCombo sc = TaskBricks.getTaskTypeCombo("TASK_TYPE", task, pageState);
    sc.separator = "<br>";
    sc.fieldSize=15;
    sc.readOnly = !canWrite;
    sc.toHtmlI18n(pageContext);
  %></td><td valign="top">
  <%
    tf = TextField.getPercentileInstance("RELEVANCE");
    tf.separator="<br>";
    tf.readOnly = !canWrite;
    tf.fieldSize = 7;

    tf.toHtmlI18n(pageContext); %>&nbsp;<%
%></td><td valign="top" nowrap>
  <label><%=I18n.get("PROGRESS")%>&nbsp;&nbsp;&nbsp;&nbsp;</label>
  <%
    CheckField pbw = new CheckField("PROGRESS_BY_WORKLOG", "", false);
    pbw.label=I18n.get("PROGRESS_BY_WORKLOG");
    pbw.toolTip = I18n.get("HELP_PROGRESS_BY_WORKLOG");
    pbw.disabled = !canWrite;
    if (!pbw.disabled) {
      pbw.additionalOnclickScript="$(this).attr('checked') ? disableProgress(this) : enableProgress(this)";
    }

    pbw.toHtmlI18n(pageContext);

    tf = TextField.getPercentileInstance("PROGRESS");
    tf.separator="<br>";
    tf.readOnly = !canWrite || task.isProgressByWorklog();
    tf.fieldSize = 7;
    tf.label="";
    tf.toHtmlI18n(pageContext);

  %>

</td><td  valign="top" nowrap><%
  //from assig
  PageSeed editA = pageState.pageInThisFolder("taskAssignmentList.jsp",request);
  editA.addClientEntry("TASK_ID",task.getId());
  ButtonLink editAssig = new ButtonLink(editA);
  editAssig.toolTip = I18n.get("HELP_ASSIGNMENT_WORKLOG_ESTIMATION")+" - "+I18n.get("UNIT_DAY_HOUR");
  editAssig.label = DateUtilities.getMillisInDaysWorkHoursMinutes( task.getTotalWorklogEstimated());
  editAssig.enabled=!task.isNew();


%><label title="<%=I18n.get("HELP_ASSIGNMENT_WORKLOG_ESTIMATION")%>">
  <%=I18n.get("FROM_ASSIG_SHORT")%>:</label> <%editAssig.toHtmlInTextOnlyModality(pageContext);%><br>

  <%
    editAssig.label = DateUtilities.getMillisInDaysWorkHoursMinutes(task.getTotalWorklogDone());
  %><label title="<%=I18n.get("HELP_ASSIGNMENT_WORKLOG_ESTIMATION")+" - "+I18n.get("UNIT_DAY_HOUR")%>"><%=I18n.get("WORKLOG_DONE")%>:&nbsp;</label>
  <%editAssig.toHtmlInTextOnlyModality(pageContext);%>
</td>
</tr><tr>
  <td colspan="2" style="width: 50%"><label><%=I18n.get("TASK_DESCRIPTION")%></label>
    <div class="linkEnabled formElements" style="font-size:14px;overflow:auto; word-break:break-all;color:black;font-weight:normal;-moz-border-radius:3px 3px 3px 3px; max-height: 800px; min-height: 80px; " onclick="$(this).hide().next().show().focus()"><%=JSP.encode(pageState.getEntry("DESCRIPTION").stringValueNullIfEmpty())%></div>
    <%

      ta = new TextArea("DESCRIPTION", "", 50, 10, null);
      ta.readOnly = !canWrite;
      ta.label = "";
      ta.script = "style='width:100%;display:none;'";
      ta.maxlength=2000;
      ta.setAutosize(80,800,20);
      ta.toHtml(pageContext);



      if (I18n.isActive("CUSTOM_FEATURE_USE_PROJECT_COLOR")){
        ColorChooser colorChooser = new ColorChooser("COLOR");
        colorChooser.label=I18n.get("COLOR");
        colorChooser.separator="<br>";
        colorChooser.showTextField=false;

        %><div style="float: left; width: 10%;"><% colorChooser.toHtml(pageContext);%> </div><%
      }
    %>

    <div style="float: right; width: <%=I18n.isActive("CUSTOM_FEATURE_USE_PROJECT_COLOR")?90:100%>%;">
    <%
      TagBox tags= new TagBox("TASK_TAGS",Task.class,task.getArea());
      tags.label=I18n.get("TAGS");
      tags.separator="<br>";
      tags.fieldSize=36;
      tags.script="style='width:100%'";
      tags.toHtml(pageContext);

    %></div></td><td colspan="2" valign="top" style="width: 50%"><%

  ta = new TextArea("NOTES", "", 50, 10, null);
  ta.readOnly = !canWrite;
  ta.label = "";
  ta.script = "style='width:100%;display:none;'";
  ta.maxlength=2000;
  ta.setAutosize(130,800,20);

%><label><%=I18n.get("NOTES_DELIVERABLES")%></label>
  <div class="linkEnabled formElements" style="overflow:auto; word-break:break-all; color:black;font-weight:normal;-moz-border-radius:3px 3px 3px 3px; max-height: 800px; min-height:130px;" onclick="$(this).hide().next().show().focus()"><%=JSP.encode(pageState.getEntry("NOTES").stringValueNullIfEmpty())%></div>
  <%ta.toHtml(pageContext); %>
</td>
</tr>
</table>
<%


%>


<br><%

  //try to know if there is custom forms or custom field
  if (TaskBricks.hasCustomField() ){

    //---------------------------------------------------- CUSTOM FIELDS ------------------------------------------------------------------------
%><div class="clearfix"><%
  for (int i=1; i<7; i++) {
    DesignerField dfStr = DesignerField.getCustomFieldInstance( "TASK_CUSTOM_FIELD_",i, task,!canWrite, false, false, pageState);
    if (dfStr!=null){
      dfStr.separator="<br>";
%><div style="float:left;margin-right: 10px"><%dfStr.toHtml(pageContext);%></div><%
    }
  }

%></div><%

  }

  ButtonBar buttonBar = new ButtonBar();

  ButtonSubmit save = ButtonSubmit.getSaveInstance(pageState.getForm(), I18n.get("SAVE"),false);
  save.additionalCssClass="first big";
  save.enabled = canWrite;
  buttonBar.addButton(save);

  if (task.isNew())
    pageState.setFocusedObjectDomId(tfn.id);

  DeletePreviewer deletePreviewer = new DeletePreviewer("TASK_DEL",TaskController.class, pageState);

  ButtonSupport delPrev = deletePreviewer.getDeleteButton(I18n.get("DELETE"), task.getId() );
  delPrev.enabled = !task.isNew() &&task.hasPermissionFor(logged,TeamworkPermissions.task_canDelete) && !(isProcessDriven && task.getTaskProcess()==null);
  delPrev.additionalCssClass="big delete";
  buttonBar.addButton(delPrev);

  buttonBar.loggableIdentifiableSupport = task;
  buttonBar.toHtml(pageContext);
%>
</div>

<%
  //---------------------------------------------------- WORKGROUP /DOCS START ------------------------------------------------------------------------
%>
<jsp:include page="part/partTaskColumnRight.jsp"/>

</div>

</div>
<%------------------------------------------------ MAIN COLUMN END ---------------------------------------------------------%>

<script type="text/javascript">
  function enableProgress(o) {
    $('#<%=tf.id%>').removeAttr('readonly');
    $('#<%=tf.id%>').css('backgroundColor','#ffffff');
  }
  function disableProgress(o) {
    $('#<%=tf.id%>').attr('readonly','readonly');
    $('#<%=tf.id%>').css('backgroundColor','#f3f3f3');
  }

  function taskTypeAddedCallback(taskType){
    //console.debug("taskTypeAddedCallback",taskType)
    if (taskType) {
      $("#TASK_TYPE").val(taskType.id);
      $("#TASK_TYPE_txt").val(taskType.name).blur();
    }

  }
</script>


<%---------------------------------------------- RIGHT COLUMN START ---------------------------------------------------------%><%
    if (!task.isNew()) {
      JspHelper side = new JspHelper("part/partTaskSideBar.jsp");
      side.parameters.put("TASK", task);
      side.toHtml(pageContext);
    }

//---------------------------------------------- RIGHT COLUMN END ---------------------------------------------------------

    f.end(pageContext);
  }
%>
