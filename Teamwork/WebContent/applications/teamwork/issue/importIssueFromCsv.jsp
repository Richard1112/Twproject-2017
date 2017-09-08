<%@ page
    import="com.twproject.operator.TeamworkOperator,
            com.twproject.resource.Resource,
            com.twproject.security.TeamworkPermissions,
            com.twproject.task.Task,
            com.twproject.task.TaskBricks,
            com.twproject.task.businessLogic.IssueImportControllerAction,
            com.twproject.waf.TeamworkHBFScreen,
            org.jblooming.page.ListPage,
            org.jblooming.persistence.PersistenceHome,
            org.jblooming.utilities.HashTable,
            org.jblooming.utilities.JSP,
            org.jblooming.utilities.SmileyUtilities,
            org.jblooming.waf.ScreenArea,
            org.jblooming.waf.SessionState,
            org.jblooming.waf.constants.Commands,
            org.jblooming.waf.html.button.ButtonLink,
            org.jblooming.waf.html.button.ButtonSubmit,
            org.jblooming.waf.html.container.ButtonBar,
            org.jblooming.waf.html.display.Paginator,
            org.jblooming.waf.html.display.PathToObject,
            org.jblooming.waf.html.input.CheckField,
            org.jblooming.waf.html.input.SmartCombo,
            org.jblooming.waf.html.input.Uploader,
            org.jblooming.waf.html.state.Form,
            org.jblooming.waf.html.table.ListHeader,
            org.jblooming.waf.settings.I18n,
            org.jblooming.waf.view.PageSeed,
            org.jblooming.waf.view.PageState,
            java.util.Iterator,
            java.util.List,
            java.util.Map, com.twproject.waf.html.StatusIcon" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new IssueImportControllerAction(pageState), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    SessionState sessionState = pageState.getSessionState();

%>
<script type="text/javascript">
  $(document).ready(function () {  //this function check all the checkboxin the table an if there is one filled enables the bar
    refreshBAR();
    $(document).on("click","#allissues :checkbox", refreshBAR);

    function refreshBAR() {
      if ($("#allissues :checked").length > 0) {
        $("#addworklog").fadeIn();
      } else {
        $("#addworklog").fadeOut();
      }
    }



  });

</script><script>$("#ISSUES_MENU").addClass('selected');</script>
<%

  Map<String, Integer> columnsPositions = new HashTable();
  List<String[]> lines = null;
  PageSeed self = pageState.thisPage(request);
  self.setCommand(Commands.FIND);
  Form f = new Form(self);
  f.encType = Form.MULTIPART_FORM_DATA;
  pageState.setForm(f);
  f.start(pageContext);

  String taskId = pageState.getEntry("TASK_IMPORT_ISSUE_ALL").stringValueNullIfEmpty();
  Task task = null;
  if (JSP.ex(taskId))
    task = (Task)PersistenceHome.findByPrimaryKey(Task.class, taskId) ;
  if (task != null) {
    /*
    ________________________________________________________________________________________________________________________________________________________________________


          path to object

    ________________________________________________________________________________________________________________________________________________________________________

    */



      PathToObject pto = new PathToObject(task);
      pto.canClick = TeamworkPermissions.task_canRead;

      PageSeed back = pageState.pageFromRoot("task/taskList.jsp");
      back.setCommand(Commands.FIND);
      ButtonLink taskList = new ButtonLink(I18n.get("TASK_LIST") + " /", back);
      pto.rootDestination = taskList;
      pto.destination = pageState.pageFromRoot("task/taskOverview.jsp");
      pto.destination.setCommand(Commands.EDIT);
      pto.toHtml(pageContext);

    StatusIcon statusIcon = task.bricks.getStatusIcon(10, pageState);

    %>
<div class="pathCodeWrapper"><%statusIcon.toHtml(pageContext);%><span class='pathCode' title='<%=I18n.get("REFERENCE_CODE")%>'><%=(task.isNew() ? "" : "T#" + task.getMnemonicCode())%>#</span></div>
<%
} else {
%>
<h1><%=I18n.get("IMPORT_ISSUE")%></h1>
<%
  }

%><hr>
<table class="table" cellspacing="0" cellpadding="5">
  <%
    pageState.removeEntry("ISSUE_FILE_TO_IMPORT");
  %>
            <tr>
            <td valign="top">
             <span class="hint"><%=I18n.get("ISSUE_FILE_IMPORT_DESCRIPTION")%></span>
            <code class="import">bug id, bug severity, priority, op sys, assigned to, bug status, resolution, short desc</code>

             <br>
              <%
                  Uploader u = new Uploader("ISSUE_FILE_TO_IMPORT", pageState);
                  u.label = I18n.get("ISSUE_FILE_TO_IMPORT");
                  u.separator = "<br>";
                  u.size = 30;
                  u.toHtml(pageContext);
                %>
            </td>
              </tr><tr>
            <td>

              <table width="100%" class="table">
              <tr>
                <th class="tableHead">
                  Bugzilla
                </th>
                <th class="tableHead">
                  Twproject
                </th>
              </tr>
                <tr class="alternate" >
                <td width="50%" valign="top">bug_id
                </td>
                <td width="50%">Not mapped but required by the import procedure. It can be an incremental numeric value.
                </td>
              </tr>
              <tr class="alternate" >
                <td width="50%" valign="top">bug_severity
                </td>
                <td width="50%">Not mapped, as severity in Twproject is a fixed set (as it is used from some refined business logic,
                    differently from apps where everything is a lookup and the application does nothing <%=SmileyUtilities.getTextWithSmileys(":-)",pageContext)%> ). Always set to "high".
                </td>
              </tr>
                <tr class="alternate" >
                <td width="50%" valign="top">priority
                </td>
                <td width="50%">Mapped to "impact".
                </td>
              </tr>
                 <tr class="alternate" >
                <td width="50%" valign="top">op_sys
                </td>
                <td width="50%">Added to the end of the issue description.
                </td>
              </tr>
                <tr class="alternate" >
                <td width="50%" valign="top">assigned_to
                </td>
                <td width="50%">Mapped to "assignee".
                </td>
              </tr>
                <tr class="alternate" >
                  <td width="50%" valign="top">bug_status
                  </td>
                  <td width="50%">Mapped to "open" or "closed" depending on wehether the issue is marked as "resolved" or not in Bugzilla.
                  </td>
                </tr>
              <tr class="alternate" >
                <td width="50%" valign="top">resolution
                </td>
                <td width="50%">Mapped to "notes".
                </td>
              </tr>
                <tr class="alternate" >
                <td width="50%" valign="top">short_desc
                </td>
                <td width="50%">Mapped to "description".
                </td>
              </tr>
            </table>
         </td>
        </tr>
  </table>
<%
  if (lines == null) {
    lines = (List<String[]>) sessionState.getAttribute("ISSUE_IMPORT_LINES");
    columnsPositions = (Map<String, Integer>) sessionState.getAttribute("ISSUE_IMPORT_CP");
  }

  ButtonBar bb2 = new ButtonBar();

  ButtonSubmit saveInstance = ButtonSubmit.getSaveInstance(f, I18n.get("READ_ISSUES_FROM_FILE"), false);
  saveInstance.variationsFromForm.command = "READ_FILE";
  saveInstance.additionalCssClass = "big first";
  bb2.addButton(saveInstance);

  if (lines != null) {

    ButtonSubmit importSel = ButtonSubmit.getSaveInstance(f, I18n.get("IMPORT_ISSUES"));
    importSel.variationsFromForm.command = "IMPORT_ISSUES";
    importSel.additionalCssClass = "big";
    bb2.addButton(importSel);
  }
  pageState.getEntryOrDefault("TASK_IMPORT_ISSUE_ALL").stringValueNullIfEmpty();

  bb2.toHtml(pageContext);
  if (lines != null) {
    ListPage listPage = new ListPage(lines, Paginator.getWantedPageNumber(pageState), Paginator.getWantedPageSize("RESIMPORT", pageState));
    pageState.setPage(listPage);

%>
<table border="0" cellpadding="0" class="table">
  <tr>
    <td><%new Paginator("RESIMP", f).toHtml(pageContext);%></td>
  </tr>
</table>
<table border="0" class="table" id="allissues">
  <tr>
    <%
      CheckField toCheckAll = CheckField.getMasterCheckField("CHECK_ALL", "CHECK_ISSUES_", null);
      toCheckAll.label = "";
      ListHeader lh = new ListHeader("RESIMPLH", f);
    %>
    <th width="1%"><%toCheckAll.toHtml(pageContext);%></th>
    <%

      lh.addHeaderFitAndCentered(I18n.get("DESCRIPTION"));
      lh.addHeaderFitAndCentered(I18n.get("ISSUE_GRAVITY"));
      lh.addHeaderFitAndCentered(I18n.get("STATUS"));
      lh.addHeaderFitAndCentered(I18n.get("ISSUE_PRIORITY"));
      lh.addHeaderFitAndCentered(I18n.get("ASSIGNEE"));
      lh.addHeaderFitAndCentered(I18n.get("TASK"));
      lh.toHtml(pageContext);
    %>
  </tr>
  <%

    int lineCounterInPage = 0;
    PageSeed ps = pageState.pageFromRoot("/resource/resourceEditor.jsp");
    ps.setCommand(Commands.EDIT);
    for (Iterator iterator = listPage.getThisPageElements().iterator(); iterator.hasNext();) {
      String[] line = (String[]) iterator.next();
      String product = "";
      String id = line[columnsPositions.get(IssueImportControllerAction.COLUMNS.bug_id.name())];
      String descr = line[columnsPositions.get(IssueImportControllerAction.COLUMNS.short_desc.name())];
      String sev = line[columnsPositions.get(IssueImportControllerAction.COLUMNS.bug_severity.name())];
      String prio = line[columnsPositions.get(IssueImportControllerAction.COLUMNS.priority.name())];
      String assign = line[columnsPositions.get(IssueImportControllerAction.COLUMNS.assigned_to.name())];
      String status = line[columnsPositions.get(IssueImportControllerAction.COLUMNS.bug_status.name())];
      if(columnsPositions.get(IssueImportControllerAction.COLUMNS.product.name()) != null)
       product = line[columnsPositions.get(IssueImportControllerAction.COLUMNS.product.name())];
  %>
  <tr class="alternate" >
    <td width="1%" align="center">
      <%
        CheckField check = new CheckField("", "CHECK_ISSUES_" + id, "", false);
        check.toHtml(pageContext);
      %>
    </td>
    <td width="20%"><%=JSP.w(descr)%>&nbsp;</td>
    <td width="10%"><%=JSP.w(sev)%>&nbsp;</td>
    <td width="10%"><%=JSP.w(status)%>
    </td>
    <td width="10%"><%=JSP.w(prio)%>
    </td>
    <td nowrap width="10%">
      <%
        List<Resource> found = IssueImportControllerAction.findResource(assign, pageState);
        if (found.size() == 0) {
      %><%=JSP.w(assign)%> (<%=I18n.get("THIS_WILL_BE_CREATED")%>)<%

    } else if (found.size() == 1) {

      Resource resource = found.get(0);
      ps.mainObjectId = resource.getId();
      ButtonLink edit = new ButtonLink(JSP.w(resource.getDisplayName()), ps);
      edit.toHtmlInTextOnlyModality(pageContext);
    } else {
    %>Multiple resources found: non determined update.<br><%
      for (Resource resource : found) {
        ButtonLink edit = new ButtonLink(JSP.w(resource.getDisplayName()), ps);
        edit.toHtmlInTextOnlyModality(pageContext);
    %><br><%
        }
      }
    %>&nbsp;</td>
    <td width="15%" align="left"><%
      SmartCombo comboAssig = TaskBricks.getTaskCombo("TASK_IMPORT_ISSUE" + id, false, TeamworkPermissions.issue_canCreate, pageState);
      comboAssig.label = I18n.get("");
      comboAssig.separator = "";
      comboAssig.fieldSize = 30;
      comboAssig.toHtml(pageContext);
      %><br><%
        if(JSP.ex(product)){
         CheckField ck = new CheckField(product + " (create a task with this name)", "CHECK_CREATE_TASK_" + id, "", false);
         ck.toHtml(pageContext);
        }
    %></td>
  </tr>
  <%
      lineCounterInPage++;
    }
  %></table>
<br>

<%
  }
%>
<table class="table" id="addworklog" style="display:none;width:100%;position:fixed;bottom: 0;">
  <tr>
    <td colspan="4" style="padding-left:30px;">
      <%
        pageState.getEntryOrDefault("TASK_IMPORT_ISSUE_ALL").stringValueNullIfEmpty();
        SmartCombo comboAssig = TaskBricks.getTaskCombo("TASK_IMPORT_ISSUE_ALL", false, TeamworkPermissions.issue_canCreate, pageState);
        comboAssig.label = I18n.get("");
        comboAssig.separator = I18n.get("IMPORT_ISSUE_ON_THIS_TASK");
        comboAssig.fieldSize = 30;
        comboAssig.toHtml(pageContext);
      %>
    </td>
    <td align="right" style="padding-right:5px;">
      <%
        ButtonSubmit addWorklog = new ButtonSubmit(f);
        addWorklog.alertOnChange = false;
        addWorklog.variationsFromForm.setCommand("IMPORT_ISSUES");
        addWorklog.label = I18n.get("IMPORT_ISSUES");
        addWorklog.alertOnRequired = true;
        addWorklog.toHtml(pageContext);
      %>
    </td>
  </tr>
</table>
<%

    f.end(pageContext);

  }


%>
