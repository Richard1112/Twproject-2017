<%@ page import="com.twproject.rank.EntityGroupRank, com.twproject.rank.RankUtilities, com.twproject.security.TeamworkPermissions, com.twproject.task.businessLogic.TaskController, com.twproject.waf.settings.ReportBricks, net.sf.json.JSONObject, org.jblooming.ontology.Identifiable, org.jblooming.utilities.JSP, org.jblooming.waf.Bricks, org.jblooming.waf.EntityViewerBricks, org.jblooming.waf.PluginBricks, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.*, org.jblooming.waf.html.container.ButtonsOnMouseover, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date, java.util.List, java.util.TreeSet, com.twproject.operator.TeamworkOperator, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.LoadSaveFilter, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.state.Form, org.jblooming.designer.DesignerField, com.twproject.resource.Resource, com.twproject.worklog.Worklog, com.twproject.resource.Person, com.twproject.task.*" %>

<div class="rightColumn noprint">
<div class="tools">

  <%
    JspHelper incl = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
    LoadSaveFilter lsfb = (LoadSaveFilter) incl.parameters.get("loadSaveFilter");

    PageState pageState = PageState.getCurrentPageState(request);
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Form form = pageState.getForm();

    if (logged.hasPermissionFor(TeamworkPermissions.project_canCreate)) {

      ButtonJS opener = new ButtonJS("");
      opener.iconChar = "P";
      opener.additionalCssClass = "first";
      opener.label = I18n.get("ADD")+"...";
      opener.iconChar="P";


      DivOnMouseover add = new DivOnMouseover(opener);

      {
        PageSeed newTask = new PageSeed(request.getContextPath() + "/applications/teamwork/task/taskNew.jsp");
        newTask.addClientEntry("ADD_TYPE", "ADD_TASK");
        //PageSeed newTask = new PageSeed(request.getContextPath() + "/applications/teamwork/task/gantt/gantt.jsp");
        newTask.setCommand(Commands.ADD);
        ButtonSupport bl = ButtonLink.getBlackInstance("+ " + I18n.get("ADD_TASK"), 720, 800, newTask);
        add.addButton(bl);
      }

      /*{
        PageSeed newTask = new PageSeed(request.getContextPath() + "/applications/teamwork/task/taskNew.jsp");
        newTask.addClientEntry("ADD_TYPE", "ADD_TASKTREE");
        //PageSeed newTask = new PageSeed(request.getContextPath() + "/applications/teamwork/task/gantt/gantt.jsp");
        newTask.setCommand(Commands.ADD);
        ButtonSupport bl = ButtonLink.getBlackInstance("+ " + I18n.get("ADD_TASKTREE"), 720, 800, newTask);
        add.addButton(bl);
      }*/


      if (!Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("DISABLEWORKFLOW"))) {
        PageSeed newProcess = pageState.pageFromRoot("processes/createProcess.jsp");
        newProcess.setCommand(Commands.ADD);
        ButtonLink bl = new ButtonLink("+ " + I18n.get("ADD_TASK_PROCESS"), newProcess);
        bl.additionalCssClass="lreq30 lreqLabel lreqActive";
        add.addButton(bl);
      }

      ButtonLink blScrum = new ButtonLink("+ " + I18n.get("SCRUM_PROJECT_CREATION"), pageState.pageFromRoot("task/scrum/projectWizard.jsp"));
      add.addButton(blScrum);

      //aggiungo i WIZARD
      for (ButtonSupport bs:PluginBricks.createPageQuarkItemsButtons("WIZARDS", pageState) ){
        bs.additionalCssClass="special";
        add.addButton(bs);
      }


  %> <div class="toolsElement"><%add.toHtml(pageContext);%></div><%
  }

  //------------------------------------- IMPORT EXPORT -------------------------------------------------------------
  ButtonJS opener = new ButtonJS("");
  opener.iconChar = "I";
  opener.additionalCssClass = "menuArrow ";
  opener.toolTip = I18n.get("IMPORT_EXPORT");
  opener.label = "";
  DivOnMouseover impExp = new DivOnMouseover(opener);

  if (logged.hasPermissionFor(TeamworkPermissions.project_canCreate) && logged.hasPermissionFor(TeamworkPermissions.resource_canCreate)) {

    //import from project
    PageSeed importTaskMPX = pageState.pageFromRoot("task/taskImportMPX.jsp");
    ButtonSupport itp = ButtonLink.getBlackInstance(I18n.get("IMPORT_FROM_MSPROJECT"), 500,750,importTaskMPX);
    itp.additionalCssClass="lreq20 lreqLabel lreqActive";
    impExp.addButton(itp);

    PageSeed importFromBasecamp = pageState.pageFromRoot("basecamp/importFromBasecamp.jsp");
    ButtonLink ifb = new ButtonLink(I18n.get("IMPORT_FROM_BASECAMP"), importFromBasecamp);
    impExp.addButton(ifb);

    PageSeed importFromJSON = pageState.pageFromRoot("task/taskImportJSON.jsp");
    ButtonSupport bl = ButtonLink.getBlackInstance(I18n.get("IMPORT_FROM_JSON"),500,500, importFromJSON);
    bl.additionalCssClass="lreq10 lreqLabel";
    impExp.addButton(bl);

    PageSeed importFromGantt = pageState.pageFromRoot("task/taskImportGantt.jsp");
    ButtonLink bli = new ButtonLink(I18n.get("IMPORT_FROM_GANTT_SERVICE"), importFromGantt);
    impExp.addButton(bli);
  }

  impExp.addSeparator();

  ButtonSubmit exportJson= new ButtonSubmit(I18n.get("EXPORT_TO_JSON"),Commands.FIND,form);
  exportJson.preserveFormStatus = true;
  exportJson.target = "_blank";
  exportJson.variationsFromForm.href=pageState.pageFromRoot("task/taskListExportJSON.jsp").toLinkToHref();
  impExp.addButton(exportJson);


  ButtonExportXLS excel = new ButtonExportXLS(pageState.getForm(), TaskController.class, "EXPORT");
  excel.addImportClasses(Task.class.getName(), Assignment.class.getName(), Resource.class.getName(), Worklog.class.getName(), Person.class.getName(), Issue.class.getName());
  excel.outputFileName = "taskList";
  excel.label = I18n.get("EXPORT_EXCEL");
  excel.additionalCssClass="lreq20 lreqLabel";

  if (I18n.isActive("CUSTOM_EXPORT_EXCEL_TASK")) {
    JSONObject properties = JSONObject.fromObject(I18n.get("CUSTOM_EXPORT_EXCEL_TASK"));
    excel.addFieldsToExport(properties);
  } else {
    excel.addBSHFieldToExport("obj.id", I18n.get("TASK_ID"));
    excel.addBSHFieldToExport("obj.externalCode", "ext code");
    excel.addBSHFieldToExport("obj.code", I18n.get("TASK_CODE") );
    excel.addBSHFieldToExport("obj.getPath(\"/\",false)", "");
    excel.addBSHFieldToExport("obj.name", I18n.get("TASK_NAME"));
    excel.addBSHFieldToExport("obj.description", I18n.get("TASK_DESCRIPTION"));
    excel.addBSHFieldToExport("obj.notes", I18n.get("NOTES_DELIVERABLES"));

    excel.addBSHFieldToExport("obj.getSchedule().getStartDate()",I18n.get("START"));
    excel.addBSHFieldToExport("obj.isStartIsMilestone()",I18n.get("MILESTONE"));
    excel.addBSHFieldToExport("obj.getSchedule().getEndDate()", I18n.get("END"));
    excel.addBSHFieldToExport("obj.isEndIsMilestone()",I18n.get("MILESTONE"));
    excel.addBSHFieldToExport("obj.getDuration()", I18n.get("DAYS"));
    excel.addBSHFieldToExport("obj.getStatus()", I18n.get("STATUS"));
    excel.addBSHFieldToExport("obj.getType().getDisplayName()", I18n.get("TYPE"));
    excel.addBSHFieldToExport("obj.relevance", I18n.get("RELEVANCE"));
    excel.addBSHFieldToExport("obj.progress", I18n.get("PROGRESS"));

    excel.addBSHFieldToExport("obj.getWorklogEstimated()/3600000D", I18n.get("FROM_ASSIG_SHORT"));
    excel.addBSHFieldToExport("obj.getWorklogDone()/3600000D",  I18n.get("WORKLOG_DONE"));

    excel.addFieldToExport("lastModified", I18n.get("LAST_MODIFIED_ON"));
    excel.addFieldToExport("lastModifier", I18n.get("LAST_MODIFIED_BY"));

    for (int i = 1; i < 7; i++) {
      if (I18n.isActive("TASK_CUSTOM_FIELD_" + i)) {
        excel.addFieldToExport("customField" + i, DesignerField.getCustomFieldInstance("TASK_CUSTOM_FIELD_", i, null, true, false, false, null).label);
      }
    }

  }
  impExp.addButton(excel);

%><div class="toolsElement"><%impExp.toHtml(pageContext);%></div><%

  //------------------------------------- PRINT  -------------------------------------------------------------
  ButtonJS print = new ButtonJS("");
  print.iconChar = "p";
  print.toolTip = I18n.get("PRINT");
  print.label = "";
  print.id = "btn_print";
  print.enabled = false;
  DivOnMouseover pr = new DivOnMouseover(print);

  String gantt = pageState.getEntry("VIEW_AS_GANTT").stringValueNullIfEmpty();
  {
    ButtonJS pri;
    if (Fields.TRUE.equals(gantt)) {
      pri = ButtonSubmit.getBlackInstance(pageState.getForm(), pageState.pageFromRoot("task/taskListPrintAsGantt.jsp").href);
    } else {
      pri = ButtonSubmit.getBlackInstance(pageState.getForm(), pageState.pageFromRoot("task/taskListPrint.jsp").href);
    }
    pri.label = I18n.get("PRINT");
    pri.id = "btn_gnt_print";
    //pri.enabled=false;
    pr.addButton(pri);
  }


  {
    ButtonJS pri = ButtonSubmit.getBlackInstance(pageState.getForm(), pageState.pageFromRoot("task/taskTreeListPrint.jsp").href);
    pri.label = I18n.get("PRINT_WITH_WORKLOG_DETAIL");
    pri.id = "btn_ttlp";
    pr.addButton(pri);
  }


  //reports
  TreeSet<ReportBricks.Report> reports = ReportBricks.getReports("task");
  if (reports.size() > 0) {
    pr.addSeparator();
    form.url.addClientEntry("reportName", "");
    form.url.addClientEntry("outType", "");
    for (ReportBricks.Report r : reports) {
      ButtonSubmit pri = new ButtonSubmit(pageState.getForm());
      pri.variationsFromForm.command = Commands.FIND;
      pri.variationsFromForm.href = pageState.pageFromRoot("task/taskIReportHelper.jsp").href;
      pri.variationsFromForm.addClientEntry("reportName", r.name);
      pri.target = "_blank";
      pri.label = r.getBestName();
      pri.preserveFormStatus = true;
      pri.additionalCssClass="lreq30 lreqLabel";
      pr.addButton(pri);
    }
  }

  //inject custom reports
  pr.addButtons(PluginBricks.createPageQuarkItemsButtons("REPORTS", pageState));


%><div class="toolsElement"><%pr.toHtml(pageContext);%></div><%

%></div><%

  //----------------------------------------------------------------------------  START RIGHT COLUMN INNER ----------------------------------------------------------------------------

%>
<div class="rightColumnInner">
  <div class="noprint filters">
    <h2><%=I18n.get("MY_SAVED_FILTERS")%></h2>
    <%

      lsfb.drawEditor = false;
      lsfb.drawButtons = true;
      lsfb.toHtml(pageContext);

    %><div class="separator"></div><%

    String cmd = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_OPEN_PROJECT");
      ButtonLink bl = new ButtonLink(I18n.get("PF_MY_OPEN_PROJECT"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }


    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_OPEN_TASK");
      ButtonLink bl = new ButtonLink(I18n.get("PF_MY_OPEN_TASK"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_OVERDUE_TASK");
      ButtonLink bl = new ButtonLink(I18n.get("PF_MY_OVERDUE_TASK"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_FORTHCOMING_MILESTONES");
      ButtonLink bl = new ButtonLink(I18n.get("PF_MY_FORTHCOMING_MILESTONES"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_FORTHCOMING_STARTS");
      ButtonLink bl = new ButtonLink(I18n.get("PF_MY_FORTHCOMING_STARTS"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_FORTHCOMING_ENDS");
      ButtonLink bl = new ButtonLink(I18n.get("PF_MY_FORTHCOMING_ENDS"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }


    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_JUST_CLOSED");
      ButtonLink bl = new ButtonLink(I18n.get("PF_MY_JUST_CLOSED"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }


    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_OPEN_PROJECT");
      ButtonLink bl = new ButtonLink(I18n.get("PF_OPEN_PROJECT"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);

    }


    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_OPEN_TASK");
      ButtonLink bl = new ButtonLink(I18n.get("PF_OPEN_TASK"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);

    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_FORTHCOMING_MILESTONES");
      ButtonLink bl = new ButtonLink(I18n.get("PF_FORTHCOMING_MILESTONES"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MILESTONES_OVERDUE");
      ButtonLink bl = new ButtonLink(I18n.get("PF_MILESTONES_OVERDUE"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_BUDGET_OVERFLOW");
      ButtonLink bl = new ButtonLink(I18n.get("PF_BUDGET_OVERFLOW"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }


    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_FORTHCOMING_STARTS");
      ButtonLink bl = new ButtonLink(I18n.get("PF_FORTHCOMING_STARTS"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_FORTHCOMING_ENDS");
      ButtonLink bl = new ButtonLink(I18n.get("PF_FORTHCOMING_ENDS"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_OVERDUE_TASK");
      ButtonLink bl = new ButtonLink(I18n.get("PF_OVERDUE_TASK"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_JUST_CLOSED");
      ButtonLink bl = new ButtonLink(I18n.get("PF_JUST_CLOSED"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }


    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_LAST_MODIFIED");
      ButtonLink bl = new ButtonLink(I18n.get("PF_LAST_MODIFIED"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }
    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_RECENTLY_USED");
      ButtonLink bl = new ButtonLink(I18n.get("PF_RECENTLY_USED"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }
  %></div><%

    List<EntityGroupRank> ranks = RankUtilities.getRanked(RankUtilities.computeWeightForEntities(RankUtilities.getRecentHitsOfOperator(logged.getIntId(), Task.class.getName(), new Date())), 5);

    if (ranks != null && ranks.size() > 0) {

  %>
  <div class="noprint recent">
    <h2><%=I18n.get("INTRO_PROJ_VISITED")%></h2>
    <%

      for (EntityGroupRank egr : ranks) {
        Identifiable is = egr.getEntity();
        EntityViewerBricks.EntityLinkSupport edi = Bricks.getLinkSupportForEntity(is, pageState);
        edi.getButton().iconChar = "";

    %><% edi.bs.toHtmlInTextOnlyModality(pageContext);%><%

    }

  %></div><%
    }

  %>
</div>
</div>

