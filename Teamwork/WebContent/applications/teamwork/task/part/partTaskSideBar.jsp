<%@ page
    import="com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, com.twproject.task.Task, com.twproject.waf.settings.ReportBricks, org.jblooming.system.SystemConstants, org.jblooming.utilities.JSP, org.jblooming.waf.Bricks, org.jblooming.waf.PluginBricks, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.Hint, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, java.util.TreeSet, com.twproject.document.TeamworkDocument, org.jblooming.security.License" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  JspHelper jspHelper = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  Task task = (Task) jspHelper.parameters.get("TASK");
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
 
 
%> <div class="rightColumn noprint"> <%
 
 
  boolean isNew = task.isNew();
  boolean assignment_canCRW =  task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW);
  boolean task_canWrite = task.hasPermissionFor(logged,TeamworkPermissions.task_canWrite);
  if (!isNew) {
 
%><div class="tools"><%
 
  boolean taskMain = request.getRequestURI().indexOf("taskOverview.jsp") > 0 || request.getRequestURI().indexOf("taskEditor.jsp") > 0;
  boolean taskGantt = request.getRequestURI().indexOf("/gantt.jsp") > 0;
  boolean taskAssig = request.getRequestURI().indexOf("/taskAssignment") > 0;
  boolean taskCost = request.getRequestURI().indexOf("/taskCost") > 0;
  boolean taskIssue = request.getRequestURI().indexOf("/taskIssue") > 0;
  boolean taskDoc = request.getRequestURI().indexOf("/taskDocument") > 0;
  boolean taskDiscussion = request.getRequestURI().indexOf("/taskForum") > 0;
  boolean taskReport = request.getRequestURI().indexOf("/taskReportList.jsp") > 0;
  //--- ADD MENU ------------------------------------------------------------------------------------------------------------------
  if (!task.isNew()) {
    JspHelper addButton = null;
 
    // ----------------------------- OVERVIEW AND  EDIT  -----------------------------
    if ((taskMain)) {
 
      ButtonJS addOpener = new ButtonJS("");
      addOpener.toolTip = I18n.get("ADD");
      addOpener.label = addOpener.toolTip + "...";
      addOpener.additionalCssClass = "first";
      addOpener.iconChar = "P";
      DivOnMouseover add = new DivOnMouseover(addOpener);
 
      addButton = add;
 
      // ADD TASKS
      if (task.hasPermissionFor(logged,TeamworkPermissions.task_canCreate)) {
 
        //SCRUM
        if (JSP.w(task.getTags()).indexOf("SCRUM") >= 0) {
          PageSeed pageSeed = pageState.pageFromRoot("task/scrum/sprintWizard.jsp");
          pageSeed.addClientEntry(Fields.PARENT_ID, task.getId());
          pageSeed.command = "NEW_SPRINT";
          ButtonLink buttonLink = new ButtonLink("+ " + I18n.get("NEW_SPRINT"), pageSeed);
          add.addButton(buttonLink);
 
        }
 
        // ADD CHILD --
        PageSeed newChildTask = new PageSeed(request.getContextPath() + "/applications/teamwork/task/taskNew.jsp");
        newChildTask.setCommand(Commands.ADD);
        newChildTask.addClientEntry(Fields.PARENT_ID, task.getId());
        newChildTask.addClientEntry("ADD_TYPE", "ADD_CHILD_TASK");
        ButtonSupport buttonLink = ButtonLink.getBlackInstance("+ " + I18n.get("ADD_CHILD_TASK"), 720, 800, newChildTask);
        buttonLink.id = "btnAddSubtask";
        add.addButton(buttonLink);
 
        // add brother
        if (task.getParent() != null && task.getParent().hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_canCreate)) {
          ButtonSupport addBrother;
          PageSeed newBrotherTask = new PageSeed(request.getContextPath() + "/applications/teamwork/task/taskNew.jsp");
          newBrotherTask.setCommand(Commands.ADD);
          newBrotherTask.addClientEntry(Fields.PARENT_ID, task.getParent().getId());
          newBrotherTask.addClientEntry("ADD_TYPE", "ADD_BROTHER_TASK");
          addBrother = ButtonLink.getBlackInstance("+ " + I18n.get("ADD_BROTHER_TASK"), 720, 800, newBrotherTask);
          add.addButton(addBrother);
        }
      }
 
      //add assignment if in taskAssignList
      if (assignment_canCRW) {
        //PageSeed psAddAssignment = pageState.pageFromRoot("task/taskAssignmentEditor.jsp");
        PageSeed psAddAssignment = pageState.pageFromRoot("task/taskAssignmentNew.jsp");
        psAddAssignment.addClientEntry("TASK_ID", task.getId());
        psAddAssignment.command = Commands.ADD;
        ButtonSupport buttonLink = ButtonLink.getBlackInstance(I18n.get("ADD_ASSIGNMENT"), 600, 800, psAddAssignment);
        buttonLink.enabled = assignment_canCRW;
        buttonLink.label = "+ " + I18n.get("ADD_ASSIGNMENTS");
        add.addButton(buttonLink);
      }
 
 
      // add issue
      ButtonSupport addIssue;
      PageSeed issues = pageState.pageFromRoot("task/taskIssueList.jsp");
      issues.mainObjectId = task.getId();
      issues.command = "ADDISSUE";
      addIssue = new ButtonLink("", issues);
      addIssue.enabled = task.hasPermissionFor(logged,TeamworkPermissions.issue_canCreate);
      addIssue.label = "+ " + I18n.get("ADD_ISSUE");
      add.addButton(addIssue);
 
 
      //inject wizards  solo su overview, edit
      List<ButtonSupport> bss2 = PluginBricks.createPageQuarkItemsButtons("WIZARDS", pageState);
      for (ButtonSupport bs : bss2) {
        bs.additionalCssClass = "special";
        add.addButton(bs);
      }
 
 
      // ----------------------------- GANTT TAB -----------------------------
    } else if (taskGantt) {
 
      ButtonJS addOpener = new ButtonJS("");
      addOpener.toolTip = I18n.get("ADD");
      addOpener.label = addOpener.toolTip + "...";
      addOpener.additionalCssClass = "first";
      addOpener.iconChar = "P";
      DivOnMouseover add = new DivOnMouseover(addOpener);
 
      addButton = add;
 
 
      // add child
      ButtonJS addChildTask = new ButtonJS("+ " + I18n.get("ADD_CHILD_TASK"), "$('#workSpace').trigger('addBelowCurrentTask.gantt');");
      add.addButton(addChildTask);
 
      // add brother
      ButtonJS addBrother = new ButtonJS("+ " + I18n.get("ADD_BROTHER_TASK"), "$('#workSpace').trigger('addAboveCurrentTask.gantt');");
      add.addButton(addBrother);
 
 
      // ----------------------------- ASSIG TAB -----------------------------
    } else if (taskAssig) {
 
      //add assignment if in taskAssignList
      if (assignment_canCRW) {
        //PageSeed psAddAssignment = pageState.pageFromRoot("task/taskAssignmentEditor.jsp");
        PageSeed psAddAssignment = pageState.pageFromRoot("task/taskAssignmentNew.jsp");
        psAddAssignment.addClientEntry("TASK_ID", task.getId());
        psAddAssignment.command = Commands.ADD;
        ButtonSupport buttonLink = ButtonLink.getBlackInstance(I18n.get("ADD_ASSIGNMENT"), 600, 800, psAddAssignment);
        buttonLink.enabled = assignment_canCRW;
        buttonLink.label =I18n.get("ADD_ASSIGNMENTS");
        buttonLink.iconChar="P";
        buttonLink.additionalCssClass="first";
        addButton = buttonLink;
      }
 
      // ----------------------------- ISSUE TAB -----------------------------
    } else if (taskIssue) {
 
      ButtonSupport addIssue=new ButtonJS("addIssue();");
      addIssue.enabled = task.hasPermissionFor(logged,TeamworkPermissions.issue_canCreate);
      addIssue.label = I18n.get("ADD_ISSUE");
      addIssue.iconChar="P";
      addIssue.additionalCssClass="first";
 
      addButton = addIssue;
 
      // ----------------------------- DOCUMENT TAB -----------------------------
    } else if (taskDoc) {
 
      // document list
      if (task.hasPermissionFor(logged,TeamworkPermissions.document_canCreate)) {
 
        ButtonJS addOpener = new ButtonJS("");
        addOpener.toolTip = I18n.get("ADD");
        addOpener.label = addOpener.toolTip + "...";
        addOpener.additionalCssClass = "first";
        addOpener.iconChar = "P";
        DivOnMouseover add = new DivOnMouseover(addOpener);
 
        addButton = add;
 
        // ADD DOCUMENT UPLOAD
        PageSeed ps = pageState.pageFromRoot("task/taskDocumentEditor.jsp");
        ps.setCommand(Commands.ADD);
        ps.addClientEntry("TASK_ID", task.getId());
        ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_UPLOAD);
        ButtonSupport addDoc = ButtonLink.getBlackInstance("+ "+I18n.get("ADD_DOCUMENT_UPLOAD"),700,1000, ps);
        add.addButton(addDoc);
 
        // ADD DOCUMENT FILE STORAGE solo se hai permessi globali
        //if (logged.hasPermissionFor(TeamworkPermissions.fileStorage_canRead)) {
        ps = pageState.pageFromRoot("task/taskDocumentEditor.jsp");
        ps.setCommand(Commands.ADD);
        ps.addClientEntry("TASK_ID", task.getId());
        ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_FILE_STORAGE);
        addDoc = ButtonLink.getBlackInstance("+ " + I18n.get("ADD_DOCUMENT_REMOTE"), 700, 1000, ps);
        addDoc.enabled=logged.hasPermissionFor(TeamworkPermissions.fileStorage_canRead);  // messo il bottono per far vedere che esiste questa possibilità
        if (License.assertLevel(30))
          add.addButton(addDoc);
        //}
 
        // ADD DOCUMENT URL
        ps = pageState.pageFromRoot("task/taskDocumentEditor.jsp");
        ps.setCommand(Commands.ADD);
        ps.addClientEntry("TASK_ID", task.getId());
        ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_URL);
        addDoc = ButtonLink.getBlackInstance("+ "+I18n.get("ADD_DOCUMENT_URL"),600,700, ps);
        add.addButton(addDoc);
 
        // ADD CONTENT
        ps = pageState.pageFromRoot("task/taskDocumentEditor.jsp");
        ps.setCommand(Commands.ADD);
        ps.addClientEntry("TASK_ID", task.getId());
        ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_CONTENT);
        addDoc = ButtonLink.getBlackInstance("+ "+I18n.get("ADD_DOCUMENT_CONTENT"),700,700, ps);
        add.addButton(addDoc);
 
      }
 
      //------------------------------TASK REPORT TAB------------------------------------
    }else if(taskReport){
        String systemPath=ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL);
        if(JSP.ex(systemPath)){
            ButtonJS addOpener = new ButtonJS("");
            addOpener.toolTip = I18n.get("ADD");
            addOpener.label = addOpener.toolTip + "...";
            addOpener.additionalCssClass = "first";
            addOpener.iconChar = "P";
            DivOnMouseover add = new DivOnMouseover(addOpener);
 
            addButton = add;
 
            // ADD REPORT
            PageSeed ps = pageState.pageFromRoot("task/taskReportEditor.jsp");
            ps.setCommand(Commands.ADD);
            ps.addClientEntry("TASK_ID", task.getId());
           // ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_UPLOAD);
            ButtonSupport addDoc = ButtonLink.getBlackInstance("+ "+I18n.get("ADD_REPORT"),700,1000, ps);
            add.addButton(addDoc);
            
            PageSeed ps2 = pageState.pageFromRoot("task/taskReportUpload.jsp");
            ps2.setCommand(Commands.ADD);
            ps2.addClientEntry("TASK_ID", task.getId());
            ps2.addClientEntry("PATH", task.getCode());
           // ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_UPLOAD);
            ButtonSupport addDoc2 = ButtonLink.getBlackInstance("+ "+I18n.get("UPLOAD_REPORT_FILE"),700,1050, ps2,"function(response){ $('#reportMultiEditor').submit();}");
            add.addButton(addDoc2);
            
            
            // ADD REPORT
            PageSeed ps3 = pageState.pageFromRoot("task/taskReportEditorWithAuth.jsp");
            ps3.setCommand(Commands.ADD);
            ps3.addClientEntry("TASK_ID", task.getId());
           // ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_UPLOAD);
            ButtonSupport addDoc3 = ButtonLink.getBlackInstance("+ "+I18n.get("ADD_AUTH_REPORT"),700,1000, ps3);
            add.addButton(addDoc3);
            
            
        }
      // ----------------------------- DISCUSSION TAB -----------------------------
    } else if (taskDiscussion) {
 
      //add discussion
      PageSeed newTh = pageState.pageFromRoot("task/taskForumEditor.jsp");
      newTh.addClientEntry("TASK_ID", task.getId());
      newTh.command = Commands.ADD;//"ADD_DOCUMENT";
      ButtonLink addPost = new ButtonLink(I18n.get("POST_NEW_THREAD"), newTh);
      addPost.iconChar="P";
      addPost.additionalCssClass="first lreq10 lreqLabel";
      addButton = addPost;
 
 
      // ----------------------------- COST TAB -----------------------------
    } else if (taskCost) {
      // add cost
      if (task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canCreate)) {
        ButtonJS add_cost = new ButtonJS(I18n.get("ADD_COST"), "$('#tableAddCosts .addAddCostBtn:first').click();");
        add_cost.iconChar="P";
        add_cost.additionalCssClass="first lreq20 lreqLabel";
        addButton = add_cost;
      }
    }
 
 
    //-----------------------DRAW ADD BUTTON IF SET-----------------------------------------------------------------------------------------------------
    if (addButton != null) {
      %><div class="toolsElement"><%addButton.toHtml(pageContext);%></div><%
    }
  }
 
 
  //------------------------------------- IMPORT EXPORT  -------------------------------------------------------------
  ButtonJS impExp = new ButtonJS("");
  impExp.iconChar = "I";
  impExp.toolTip = I18n.get("IMPORT_EXPORT");
  impExp.label = "";
  DivOnMouseover imp = new DivOnMouseover(impExp);
 
  if (task.hasPermissionFor(logged,TeamworkPermissions.task_canRead)) {
    ButtonJS export = new ButtonJS("");
    export.label = I18n.get("EXPORT_TO_MSPROJECT");
    DivOnMouseover edomo = new DivOnMouseover(export);
 
 
    //export to project
    PageSeed exportTaskMPX = pageState.pageFromRoot("task/taskExportMPX.jsp");
    exportTaskMPX.mainObjectId = task.getId();
    exportTaskMPX.command = Commands.EDIT;
    ButtonLink jtp=new ButtonLink(I18n.get("EXPORT_TO_MSPROJECT_MPX"),  exportTaskMPX);
    jtp.target="_blank";
    jtp.additionalCssClass="lreq20 lreqLabel";
    imp.addButton(jtp);
 
 
    //export to xml
    PageSeed exportTaskXML = pageState.pageFromRoot("task/taskExportXML.jsp");
    exportTaskXML.mainObjectId = task.getId();
    exportTaskXML.command = Commands.EDIT;
    ButtonSupport jtp1 = new ButtonLink(I18n.get("EXPORT_TO_MSPROJECT_XML"), exportTaskXML);
    jtp1.additionalCssClass="lreq20 lreqLabel";
    jtp1.target="_blank";
    imp.addButton(jtp1);
 
    //export to JSON
    PageSeed exportJson = pageState.pageFromRoot("task/taskExportJSON.jsp");
    exportJson.mainObjectId = task.getId();
    exportJson.command = Commands.EDIT;
 
    ButtonSupport jtp2 = new ButtonLink(I18n.get("EXPORT_TO_JSON"), exportJson);
    jtp2.target="_blank";
    imp.addButton(jtp2);
 
    //export excel
    if (taskGantt){
      PageSeed exportGanttExcel = pageState.pageFromRoot("task/ganttExportExcel.jsp");
      exportGanttExcel.mainObjectId = task.getId();
      exportGanttExcel.command = Commands.EDIT;
      ButtonSupport expGantt = new ButtonLink(I18n.get("EXPORT_GANTT_TO_EXCEL"), exportGanttExcel);
      expGantt.target="_blank";
      imp.addButton(expGantt);
    }
 
 
    imp.addButton(edomo);
  }
 
 
 
%><div class="toolsElement"><%imp.toHtml(pageContext);%></div><%
 
  //------------------------------------- PRINT  -------------------------------------------------------------
  ButtonJS opener = new ButtonJS("");
  opener.iconChar = "p";
  opener.toolTip = I18n.get("PRINT");
  opener.label = "";
  DivOnMouseover printButtons = new DivOnMouseover(opener);
 
  if (pageState.href.indexOf("taskCost")>-1){
    PageSeed print = pageState.pageFromRoot("task/taskCostEditorPrint.jsp");
    print.mainObjectId = task.getId();
    print.setCommand(Commands.EDIT);
    ButtonSupport is = ButtonLink.getBlackInstance(I18n.get("TASK_PRINT"), print);
    is.enabled = !task.isNew();
    printButtons.addButton(is);
 
  } else {
    PageSeed printLink = pageState.pageFromRoot("task/taskEditorPrint.jsp");
    printLink.mainObjectId = task.getId();
    printLink.setCommand(Commands.EDIT);
 
    if((pageState.href.contains("Forum"))) {
      printLink.addClientEntry("PRINT_TASK_ASSIGS_SUBTASKS", Fields.FALSE);
      printLink.addClientEntry("PRINT_TASK_DIARY_DETAIL", Fields.TRUE);
    }
 
    ButtonSupport print = ButtonLink.getBlackInstance(I18n.get("TASK_PRINT"), printLink);
    print.enabled = !task.isNew();
    printButtons.addButton(print);
  }
 
 
  PageSeed redirTo = pageState.pageFromRoot("task/taskEditor.jsp");
  redirTo.mainObjectId = task.getId();
  redirTo.setCommand(Commands.EDIT);
  redirTo.command = "CREATE_SNAPSHOT";
 
 
  PageSeed printFreeze = new PageSeed("/applications/teamwork/task/taskEditorPrint.jsp");
  printFreeze.mainObjectId = task.getId();
  printFreeze.setCommand(Commands.EDIT);
  printFreeze.addClientEntry("PRINT_TASK_ASSIGS_SUBTASKS", Fields.TRUE);
  printFreeze.addClientEntry("PRINT_TASK_DESCENDANTS", Fields.TRUE);
  printFreeze.addClientEntry("PRINT_TASK_DIARY_DETAIL", Fields.TRUE);
 
  ButtonLink freeze = ButtonLink.getPDFFreezeButton(printFreeze, redirTo, "task_" + task.getId());
  String spa = ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL);
  freeze.enabled = !task.isNew() && task_canWrite && org.jblooming.utilities.JSP.ex(spa);
  freeze.label = I18n.get("TASK_FREEZE");
  printButtons.addButton(freeze);
 
  //reports
  TreeSet<ReportBricks.Report> reports = ReportBricks.getReports("task");
  if (reports.size()>0){
    printButtons.addSeparator();
    pageState.getForm().url.addClientEntry("reportName","");
    pageState.getForm().url.addClientEntry("outType","");
    for (ReportBricks.Report r:reports){
      ButtonSubmit pri = new ButtonSubmit(pageState.getForm());
      pri.variationsFromForm.command = Commands.EDIT;
      pri.variationsFromForm.href = pageState.pageFromRoot("task/taskIReportHelper.jsp").href;
      pri.variationsFromForm.addClientEntry("reportName",r.name);
      pri.target = "_blank";
      pri.label = r.getBestName();
      pri.preserveFormStatus = true;
      pri.additionalCssClass="lreq30 lreqLabel";
      printButtons.addButton(pri);
    }
  }
 
  //inject custom reports
  printButtons.addButtons(PluginBricks.createPageQuarkItemsButtons("REPORTS", pageState));
 
 
 
 
%><div class="toolsElement"><%printButtons.toHtml(pageContext);%></div><%
 
 
%></div><%
  }
 
  //----------------------------------------------------------------------------  START RIGHT COLUMN INNER ----------------------------------------------------------------------------
 
%><div class="rightColumnInner">
<div class="" style="display:<%=(!isNew ? "block" : "none" )%>">
  <br><%
 
 
  // se hai un permesso locale o globale
  if (task.hasPermissionFor(logged,TeamworkPermissions.task_canCreate)|| logged.hasPermissionFor(TeamworkPermissions.project_canCreate)|| logged.hasPermissionFor(TeamworkPermissions.task_canCreate)) {
 
    PageSeed taskCopy = pageState.pageFromRoot("task/taskClone.jsp");
    taskCopy.mainObjectId = task.getId();
    taskCopy.command = Commands.EDIT;
    taskCopy.setPopup(true);
    ButtonSupport act = ButtonLink.getBlackInstance(I18n.get("TASK_CLONE"), 650, 800, taskCopy);
    act.additionalCssClass = "bolder lreq20 lreqLabel lreqActive";
    act.iconChar="D";
    act.toHtmlInTextOnlyModality(pageContext);
 
 
    PageSeed taskMove = pageState.pageFromRoot("task/taskMove.jsp");
    taskMove.mainObjectId = task.getId();
    taskMove.command = Commands.EDIT;
 
    taskMove.setPopup(true);
    act = ButtonLink.getBlackInstance(I18n.get("TASK_MOVE"), 450,700,taskMove);
    act.additionalCssClass = "bolder";
    act.iconChar="y";
    act.toHtmlInTextOnlyModality(pageContext);
  }
 
%></div>
 
 
 
<%--------------------------------------- MORE -------------------------------------------------------------%>
<div class="" style="display:<%=(!isNew ? "block" : "none" )%>" id="tskSidBarMoreDiv">
  <%
    if (task.hasPermissionFor(logged,TeamworkPermissions.task_canCreate)) {
  %>
  <div class="separator"></div><%
 
 
 
  //create a sprint
  if (JSP.w(task.getTags()).indexOf("SCRUM")<0) {
    PageSeed pageSeed = pageState.pageFromRoot("task/scrum/sprintWizard.jsp");
    pageSeed.addClientEntry(Fields.PARENT_ID,task.getId());
    pageSeed.command="NEW_SPRINT";
    ButtonLink buttonLink = new ButtonLink("<span class=\"teamworkIcon withLabel\" style=\"\">P</span> " +I18n.get("NEW_SPRINT"), pageSeed);
    buttonLink.toHtmlInTextOnlyModality(pageContext);
  }
 
  //add child process
  if (!task.isProcessDriven()) {
    PageSeed newChildProcess = new PageSeed(request.getContextPath() + "/applications/teamwork/processes/createProcess.jsp");
    newChildProcess.setCommand(Commands.ADD);
    newChildProcess.addClientEntry(Fields.PARENT_ID, task.getId());
    ButtonLink buttonLink = new ButtonLink("<span class=\"teamworkIcon withLabel\" style=\"\">P</span> " +I18n.get("ADD_CHILD_PROCESS"), newChildProcess);
    buttonLink.additionalCssClass="lreq30 lreqLabel lreqHide";
    buttonLink.toHtmlInTextOnlyModality(pageContext);
  }
 
  //add child from template
  PageSeed addFromTemplate = new PageSeed(request.getContextPath() + "/applications/teamwork/task/taskAddFromTemplate.jsp");
  addFromTemplate.setCommand(Commands.EDIT);
  addFromTemplate.mainObjectId= task.getId();
  //ButtonLink buttonLink = new ButtonLink("<span class=\"teamworkIcon withLabel\" style=\"\">P</span> " +I18n.get("ADD_CHILD_TEMPLATE"), addFromTemplate);
  ButtonSupport buttonLink = ButtonLink.getBlackInstance("<span class=\"teamworkIcon withLabel\" style=\"\">P</span> " +I18n.get("ADD_CHILD_TEMPLATE"), 650, 800, addFromTemplate);
 
  buttonLink.toHtmlInTextOnlyModality(pageContext);
 
%><div class="separator"></div><%
 
  }
 
  //worklog 
  if (task.hasPermissionFor(logged,TeamworkPermissions.worklog_manage)) {
    PageSeed pl = pageState.pageFromRoot("task/worklog/approval/worklogApprovalByTask.jsp");
    pl.addClientEntry("TASK_ID",task);
    pl.command="dummy";
    ButtonLink jtp = new ButtonLink(I18n.get("WORKLOG_APPROVAL"), pl);
    jtp.additionalCssClass="lreq20 lreqLabel lreqActive";
    jtp.toHtmlInTextOnlyModality(pageContext);
  }
 
  {
    PageSeed wlL = pageState.pageFromRoot("task/worklog/worklogList.jsp");
    wlL.addClientEntry("TASK",task.getId());
    wlL.addClientEntry("TASK_WORKLOG_SHOW_CHILDREN", Fields.TRUE);
    wlL.command= Commands.FIND;
    ButtonLink jtp = new ButtonLink(I18n.get("WORKLOG_FIND_MENU"), wlL);
    jtp.toHtmlInTextOnlyModality(pageContext);
  }
 
 
 
  //------------------------------------- CHECK WORKLOG
  {
    PageSeed wip = pageState.pageFromRoot("task/worklog/worklogInPeriodForTask.jsp");
    wip.mainObjectId = task.getId();
    wip.setCommand(Commands.EDIT);
    ButtonSupport jtp = ButtonLink.getBlackInstance(I18n.get("WORKLOGS_IN_PERIOD"), 800 , 800, wip);
 
    jtp.enabled = !isNew;
    jtp.toHtmlInTextOnlyModality(pageContext);
  }
 
%><div class="separator"></div><%
 
 
  //more
 
  {
    boolean existsScrumMaster = false;
    boolean existsScrumTeam = false;
    for (com.twproject.task.Assignment ass : task.getAssignments()) {
      String ROLE_SCRUM_MASTER_NAME = ApplicationState.getApplicationSetting("ROLE_SCRUM_MASTER_NAME", "Scrum Master");
      String ROLE_SCRUM_TEAM_NAME = ApplicationState.getApplicationSetting("ROLE_SCRUM_TEAM_NAME", "Scrum Team");
 
      if (ass.getRole().getName().equalsIgnoreCase(ROLE_SCRUM_TEAM_NAME))
        existsScrumTeam = true;
      else if (ass.getRole().getName().equalsIgnoreCase(ROLE_SCRUM_MASTER_NAME))
        existsScrumMaster = true;
    }
    if (existsScrumMaster && existsScrumTeam){
      if (task.hasPermissionFor(logged,TeamworkPermissions.task_canCreate) && assignment_canCRW) {
        PageSeed pageSeed = pageState.pageFromRoot("task/scrum/sprintWizard.jsp");
        pageSeed.addClientEntry(Fields.PARENT_ID, task.getId());
        pageSeed.command = "NEW_SPRINT";
        ButtonLink buttonLink = new ButtonLink(I18n.get("NEW_SPRINT"), pageSeed);
        buttonLink.toHtmlInTextOnlyModality(pageContext);
      }
      PageSeed scru=pageState.pageFromRoot("task/taskScrum.jsp");
      scru.command=Commands.EDIT;
      scru.mainObjectId=task.getId();
      new ButtonJS(I18n.get("ISSUES_BURN_DOWN"),"openBlackPopup('"+scru.toLinkToHref()+"','95%','95%')").toHtmlInTextOnlyModality(pageContext);
 
      %><div class="separator"></div><%
    }
  }
 
    if (JSP.ex(task.getAssignments())) {
 
    {
      PageSeed pl = pageState.pageFromRoot("task/plan/planByTask.jsp");
      pl.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
      pl.mainObjectId = task.getId();
      pl.command = Commands.EDIT;
      ButtonLink buttonLink = new ButtonLink(I18n.get("PLAN_BY_RESOURCE"), pl);
      buttonLink.toolTip = I18n.get("PLAN_BY_RESOURCE");
      buttonLink.additionalCssClass="lreq30 lreqLabel lreqActive";
      buttonLink.toHtmlInTextOnlyModality(pageContext);
    }
    {
      PageSeed ol = pageState.pageFromRoot("task/plan/operatorLoad.jsp");
      ol.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
      Bricks.addReferral(task.getId(), Task.class, ol);
      ol.setCommand("FIND_BY_ENTITY");
      ButtonLink buttonLink = new ButtonLink(I18n.get("TASK_WORKGROUP_OPERATOR_LOAD"), ol);
      buttonLink.toolTip = I18n.get("TASK_WORKGROUP_OPERATOR_LOAD");
      buttonLink.additionalCssClass="lreq20 lreqLabel lreqActive";
      buttonLink.toHtmlInTextOnlyModality(pageContext);
    }
 
    //if (task.bricks.canManageAssignment) {
    if (assignment_canCRW || task.hasPermissionFor(logged,TeamworkPermissions.resource_manage)) {
      PageSeed ap = pageState.pageFromRoot("task/assignment/assignmentPriority.jsp");
      ap.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
      Bricks.addReferral(task.getId(), Task.class, ap);
      ap.setCommand("FIND_BY_ENTITY");
      ButtonLink buttonLink = new ButtonLink(I18n.get("TASK_WORKGROUP_PRIORITY"), ap);
      buttonLink.toolTip = I18n.get("TASK_WORKGROUP_PRIORITY");
      buttonLink.additionalCssClass="lreq20 lreqLabel lreqActive";
      buttonLink.toHtmlInTextOnlyModality(pageContext);
    }
 
%><div class="separator"></div><%
 
    }
 
 
    {
      PageSeed ps=pageState.pageFromRoot("task/taskPublicPageEditor.jsp");
      ps.mainObjectId=task.getId();
      ps.command="TASK_EDIT_SECURITY";
      ButtonSupport pubPag = ButtonLink.getBlackInstance(I18n.get("PROJECT_PUBLIC_PAGE"),600,800,ps);
      pubPag.enabled = !isNew;
      pubPag.additionalCssClass = "lreq20 lreqLabel lreqActive";
 
      pubPag.toHtmlInTextOnlyModality(pageContext);
    }
 
  if (!isNew && task_canWrite){
    ButtonJS choose = new ButtonJS(I18n.get("CHOOSE_PROJECT_IMAGE"),"openTaskImageEditor('"+task.getId()+"')");
    choose.toHtmlInTextOnlyModality(pageContext);
 
    %>
 
  <script type="text/javascript">
    function openTaskImageEditor(resId) {
      $(".uploadizeDrop").attr("disabled", "true");
      openBlackPopup(contextPath + "/applications/teamwork/task/taskImageUploader.jsp?TASK_ID=" +<%=task.getId()%>, '500px', '550px', function (response) {
 
      });
    }
 
  </script>
  <%
 
  }
 
  if (task.getChildrenSize()>0 && task_canWrite){
    PageSeed ps=pageState.pageFromRoot("task/recodeTaskTree.jsp");
    ps.mainObjectId=task.getId();
    ps.command="ED";
    ButtonSupport logs = ButtonLink.getBlackInstance(I18n.get("RECODE_TASK_TREE"), 400,500,ps);
    logs.enabled = !isNew;
    logs.toHtmlInTextOnlyModality(pageContext);
  }
 
 
  %><div class="separator"></div><%
 
    {
      PageSeed ps=pageState.pageFromRoot("task/taskLogs.jsp");
      ps.mainObjectId=task.getId();
      ButtonSupport logs = ButtonLink.getBlackInstance(I18n.get("TASK_LOGS_TAB"), ps);
      logs.enabled = !isNew;
      logs.toHtmlInTextOnlyModality(pageContext);
    }
 
    {
      PageSeed ps=pageState.pageFromRoot("task/taskArea.jsp");
      ps.mainObjectId=task.getId();
      ps.command="TASK_EDIT_SECURITY";
      ButtonSupport logs = ButtonLink.getBlackInstance(I18n.get("SECURITY"), 600,600,ps);
      logs.enabled = !isNew;
      logs.toHtmlInTextOnlyModality(pageContext);
    }
 
    //------------------------------------------ END MORE ---------------------------
%></div><%
 
  if (!isNew) {
 
    if (I18n.isActive("CUSTOM_FEATURE_SHOW_NOTES_ON_SIDEBAR") && JSP.ex(task.getNotes()) && pageState.href.indexOf("taskEditor")<0){
%><div class="buttonBox">
  <h2><%=I18n.get("NOTES_DELIVERABLES")%></h2>
  <%=JSP.convertLineFeedToBR(task.getNotes())%>
</div><%
    }
 
    //-------------------------------  HINTS MANAGEMENT ---------------------------------------------------
 
    Hint.HintWriter hintWriter = new Hint.HintWriter();
    if (pageState.href.indexOf("taskOverview") >= 0) {
      hintWriter.addHint("HINT_FIRST_SUBTASK", ".tools div.toolsElement:first .button.first", 400,270, task_canWrite, pageState);
      hintWriter.addHint("HINT_FIRST_ASSIG", "#hintFirstAssig .button", 400,270, assignment_canCRW, pageState);
      hintWriter.addHint("HINT_FIRST_DOC", "#btnAddDoc", 350,420,  task.hasPermissionFor(logged,TeamworkPermissions.document_canCreate), pageState);
      hintWriter.addHint("HINT_FIRST_COST", "#btnCosts", 200, 150, task_canWrite, pageState);
      hintWriter.addHint("HINT_FIRST_MEETING", "#btnAgenda", 200,150, task_canWrite, pageState);
    } else  if (pageState.href.indexOf("taskAssignmentList") >= 0) {
      hintWriter.addHint("HINT_FIRST_TASK_BACK", "#btn_taskGeneralData",  200,150, task_canWrite, pageState);
    } else  if (pageState.href.indexOf("gantt") >= 0) {
      hintWriter.addHint("HINT_USE_GANTT", "#btnGantt",  200,150, task_canWrite && task.getParent()==null && task.getChildrenSize()==0, pageState);
    }
 
    hintWriter.toHtml(pageContext);
 
 
  }
 
%>
</div>
</div>

<%---------------------------------------------- RIGHT COLUMN END ---------------------------------------------------------%>