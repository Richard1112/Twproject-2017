<%@ page import="org.jblooming.waf.settings.I18n, org.jblooming.utilities.JSP, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSupport, com.twproject.task.Task, org.jblooming.waf.constants.Fields, org.jblooming.utilities.CodeValueList, org.jblooming.waf.html.input.*, com.twproject.security.TeamworkPermissions, org.jblooming.security.Area, com.twproject.resource.ResourceBricks, java.util.Set, org.jblooming.waf.view.PageState, com.twproject.task.TaskBricks, com.twproject.security.SecurityBricks, org.jblooming.designer.DesignerField, org.jblooming.waf.html.display.DataTable, com.twproject.operator.TeamworkOperator, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.core.JspIncluder, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.view.PageSeed, org.jblooming.waf.constants.Commands, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.PluginBricks, org.jblooming.utilities.CollectionUtilities" %><%

  JspHelper incl = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  ButtonSupport searchButton= (ButtonSupport) incl.parameters.get("searchButton");
  LoadSaveFilter lsfb= (LoadSaveFilter) incl.parameters.get("loadSaveFilter");
  lsfb.label=I18n.get("WANT_TO_SAVE_FILTER");

  PageState pageState= PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  //hidden field per portarsi dietro il valore passato dalla ricerca T:
  TextField.hiddenInstanceToHtml("NAME_DESCRIPTION_NOTE_CODE",pageContext);


  TextField tfCode = new TextField("TEXT", I18n.get("TASK_CODE"), "CODE", "<br>", 8, false);
  tfCode.searchField = true;
  tfCode.preserveOldValue=false;

  TextField tfNotes = new TextField("TEXT", I18n.get("NOTES_DELIVERABLES"), "NOTES", "<br>", 15, false);
  tfNotes.searchField = true;
  tfNotes.preserveOldValue=false;

  TextField tfName = new TextField("TEXT", I18n.get("NAME_DESCRIPTION"), "NAME_DESCRIPTION", "<br>", 25, false);
  tfName.searchField = true;
  tfName.preserveOldValue=false;


  ColorValueChooser ccv = TaskBricks.getStatusChooser("STATUS", "TASKSTATUSFILTER", true, true);
  ccv.height = 30;
  ccv.showOpener = true;
  ccv.label = I18n.get("ISSUE_TASK_STATUS");
  ccv.separator = "<br>";
  ccv.init(pageContext);
  ccv.preserveOldValue=false;


  TextField taskId = new TextField("TASK_ID", "<br>");
  taskId.fieldSize = 3;
  taskId.searchField = true;
  taskId.preserveOldValue=false;


  SmartCombo type = TaskBricks.getTaskTypeCombo("TYPE", pageState);
  type.label = I18n.get(type.fieldName);
  type.separator = "<br>";
  type.fieldSize = 15;
  type.addAllowed = true;
  type.preserveOldValue=false;


  TextField progress = new TextField("PROGRESS", "<br>");
  progress.fieldSize = 4;
  progress.searchField = true;
  progress.preserveOldValue=false;

  TextField relevance = new TextField("RELEVANCE", "<br>");
  relevance.fieldSize = 4;
  relevance.searchField = true;
  relevance.preserveOldValue=false;


  DateField start = new DateField("START");
  start.separator = "<br>";
  start.setSearchField(true);
  start.preserveOldValue=false;


  DateField end = new DateField("END");
  end.separator = "<br>";
  end.setSearchField(true);
  end.preserveOldValue=false;


  SmartCombo assignee = ResourceBricks.getInspectableResourcesCombo("ASSIGNEE", CollectionUtilities.toList(TeamworkPermissions.task_canRead), pageState);
  assignee.separator = "<br>";
  assignee.fieldSize = 15;
  assignee.addAllowed=true;
  assignee.preserveOldValue=false;

  SmartCombo roles = SecurityBricks.getRoleComboForAssignments("ROLE", null, pageState);
  roles.separator = "<br>";
  roles.classic = false;
  roles.fieldSize = 15;
  roles.addAllowed=true;
  roles.preserveOldValue=false;


  DateField lastModified = new DateField("LAST_MODIFIED");
  lastModified.separator = "<br>";
  lastModified.setSearchField(true);
  lastModified.preserveOldValue=false;

  DateField createdOn = new DateField("CREATED_ON");
  createdOn.separator = "<br>";
  createdOn.setSearchField(true);
  createdOn.preserveOldValue=false;

  DateField assCreatedOn = new DateField("ASSIG_CREATED_ON");
  assCreatedOn.separator = "<br>";
  assCreatedOn.setSearchField(true);
  assCreatedOn.preserveOldValue=false;

  DateField stsChangeDate = new DateField("STATUS_CHANGE_DATE");
  stsChangeDate.separator = "<br>";
  stsChangeDate.setSearchField(true);
  stsChangeDate.preserveOldValue=false;

  SmartCombo subtaskOf = TaskBricks.getTaskCombo("SUBTASK_OF", false, TeamworkPermissions.task_canRead, pageState);
  subtaskOf.label = I18n.get("SUBTASK_OF");
  subtaskOf.separator = "<br>";
  subtaskOf.fieldSize = 15;
  subtaskOf.preserveOldValue=false;

  SmartCombo assignmentCreator = ResourceBricks.getPersonCombo("ASSIGNMENT_CREATOR", false, "", pageState);
  assignmentCreator.label = I18n.get("ASSIGNMENT_CREATOR");
  assignmentCreator.separator = "<br>";
  assignmentCreator.fieldSize = 15;
  assignmentCreator.preserveOldValue=false;


  Set<Area> areas = logged.getAreasForPermission(TeamworkPermissions.task_canRead);
  CodeValueList cvl = CodeValueList.getI18nInstanceForIdentifiables(areas, pageState);
  cvl.addChoose(pageState);
  Combo cbbArea = new Combo("AREA", "<br>", null, 15, null, cvl, "");
  cbbArea.preserveOldValue=false;


  DateField milestone = new DateField("MILESTONE");
  milestone.setSearchField(true);
  milestone.separator = "<br>";
  milestone.preserveOldValue=false;


  cvl= new CodeValueList();
  cvl.add("",I18n.get("ALL"));
  cvl.add(Fields.TRUE,I18n.get("PROJECT"));
  cvl.add(Fields.FALSE,I18n.get("TASK"));
  Combo roots= new Combo("ROOT_OR_STANDALONE","<br>",null,10,cvl,"");
  roots.label=I18n.get("PROJECT_OR_TASK");
  roots.preserveOldValue=false;

  SmartCombo customersCombo = ResourceBricks.getInspectableCustomersCombo("CUST_ID", pageState);
  customersCombo.label = I18n.get("CUSTOMERS");
  customersCombo.fieldSize = 30;
  customersCombo.preserveOldValue = false;
  customersCombo.separator = "<br>";

  TagBox tags = new TagBox("TASK_TAGS", Task.class, logged.getPerson().getArea());
  tags.label = I18n.get("TAGS");
  tags.separator = "<br>";
  tags.fieldSize = 20;
  tags.preserveOldValue=false;

  CheckField milestonesOverdued= new CheckField(I18n.get("MILES_OVER"),"MILES_OVER","&nbsp;",false);

  CheckField budgetOverflow= new CheckField(I18n.get("BUDGET_OVERFLOW"),"BUDGET_OVERFLOW","&nbsp;",false);

  CheckField recentlyUsed= new CheckField("RECENTLY_USED","&nbsp;",false);
  recentlyUsed.preserveOldValue=false;

  DateField lastPostOnThread = new DateField("LAST_POST");
  lastPostOnThread.separator = "<br>";
  lastPostOnThread.setSearchField(true);
  lastPostOnThread.preserveOldValue=false;

  ColorChooser taskColor = new ColorChooser("COLOR");
  taskColor.label=I18n.get("COLOR");
  taskColor.separator="<br>";
  taskColor.showTextField=false;

  ButtonSupport qbe = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("HELP")), 700, 800, pageState.pageFromCommonsRoot("help/qbe.jsp"));
  qbe.toolTip = I18n.get("HELP_QBE");

  pageState.setFocusedObjectDomId(tfCode.id);

%><div class="filterBar clearfix">
  <div class="filterActiveElements"></div>

  <div class="filterInactiveElements">
    <div class="filterElement filterDefault"><%tfCode.toHtml(pageContext);%></div>
    <div class="filterElement filterDefault"><%tfName.toHtml(pageContext);%></div>
    <div class="filterElement"><%ccv.toHtml(pageContext); %></div>
    <div class="filterElement"><%taskId.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%type.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%progress.toHtmlI18n(pageContext);%> %</div>
    <div class="filterElement"><%relevance.toHtmlI18n(pageContext);%> %</div>
    <div class="filterElement"><%start.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%end.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%assignee.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%roles.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%lastModified.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%assignmentCreator.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%subtaskOf.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%cbbArea.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%milestone.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%roots.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%tags.toHtml(pageContext);%></div>
    <div class="filterElement centered"><%milestonesOverdued.toHtml(pageContext);%></div>
    <div class="filterElement centered"><%budgetOverflow.toHtml(pageContext);%></div>
    <div class="filterElement "><%taskColor.toHtml(pageContext);%></div>
    <div class="filterElement"><%lastPostOnThread.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%createdOn.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%assCreatedOn.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%stsChangeDate.toHtmlI18n(pageContext);%></div>
    <div class="filterElement centered"><%recentlyUsed.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%customersCombo.toHtml(pageContext);%></div>
    <div class="filterElement"><%tfNotes.toHtml(pageContext);%></div>


    <%
      // ------------------------------------------------------------------------------------------- custom fields -------------------------------------------------------------------------------------------
      if (TaskBricks.hasCustomField()) {
        for (int i = 1; i < 7; i++) {
          DesignerField dfStr = DesignerField.getCustomFieldInstance("TASK_CUSTOM_FIELD_", i, null, false, false, true, pageState);
          if (dfStr != null) {
            dfStr.usedForSearch = true;
            dfStr.preserveOldValue = false;
            dfStr.separator = "<br>";
    %><div class="filterElement"><%dfStr.toHtml(pageContext);%></div> <%
        }
      }
    }
  %>
  </div>
  <div class="filterButtons">
    <div class="filterButtonsElement filterAdd"><span class="button" id="filterSelectorOpener" title="<%=I18n.get("ADD_FILTER")%>" onclick="bjs_showMenuDiv('filterSelectorBox', 'filterSelectorOpener');"><span class="teamworkIcon">f</span></span></div>
    <div class="filterButtonsElement filterSearch"><%searchButton.toHtml(pageContext);%></div>

    <div class="filterActions">
      <div class="filterButtonsElement filterSave"><%lsfb.toHtml(pageContext);%></div>
      <div class="filterButtonsElement filterHelp"><%DataTable.getQBEHelpButton(pageState).toHtmlInTextOnlyModality(pageContext);%></div>
    </div>

  </div>
</div>


<script src="<%=request.getContextPath()%>/commons/js/filterEngine.js"></script>
