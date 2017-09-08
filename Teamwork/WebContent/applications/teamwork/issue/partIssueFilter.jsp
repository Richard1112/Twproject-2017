<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.ResourceBricks, com.twproject.security.SecurityBricks, com.twproject.security.TeamworkPermissions, com.twproject.task.Issue, com.twproject.task.IssueBricks, com.twproject.task.TaskBricks, org.jblooming.designer.DesignerField, org.jblooming.security.Permission, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.input.*, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.util.HashSet, java.util.Set, com.twproject.resource.Resource, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.PluginBricks, org.jblooming.waf.html.container.DivOnMouseover, java.util.List, org.jblooming.utilities.JSP, org.jblooming.utilities.CollectionUtilities" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  boolean hideTask=request.getRequestURI().contains("taskIssueList");  //in questo caso sono nascosti;
  boolean hideAssignee=request.getRequestURI().contains("resourceIssueList");  //in questo caso sono nascosti;


  Set<Permission> perms = new HashSet();
  perms.add(TeamworkPermissions.issue_canRead);
  perms.add(TeamworkPermissions.issue_canCreate);
  perms.add(TeamworkPermissions.issue_canWrite);
  perms.add(TeamworkPermissions.issue_canDelete);


  SmartCombo taskSC = TaskBricks.getTaskCombo("FLT_ISSUE_TASK", false, perms,null, pageState);
  taskSC.fieldSize = 25;
  taskSC.separator = "";
  taskSC.preserveOldValue = false;
  taskSC.addAllowed = true;
  taskSC.label = "";

  CheckField taskChildren = new CheckField("", "FLT_TASK_ISSUE_SHOW_CHILDREN", "&nbsp;", false);
  taskChildren.preserveOldValue = false;


  TextField tfDescr = new TextField("TEXT", I18n.get("ISSUE_DESCRIPTION"), "FLT_ISSUE_DESCRIPTION", "<br>", 30, false);
  tfDescr.id = "ISSUE_DESCRIPTION";
  tfDescr.searchField = true;
  tfDescr.preserveOldValue = false;


  //SmartCombo resourceCombo = ResourceBricks.getPersonCombo("FLT_ISSUE_ASSIGNED_TO", false, "", pageState);
  SmartCombo resourceCombo = ResourceBricks.getInspectableResourcesCombo("FLT_ISSUE_ASSIGNED_TO", CollectionUtilities.toList(TeamworkPermissions.issue_canRead), pageState);
  resourceCombo.fieldSize = 30;
  resourceCombo.addAllowed = true;
  resourceCombo.separator = "";
  resourceCombo.preserveOldValue = false;
  resourceCombo.label = "";
  resourceCombo.onValueSelectedScript="$('#ck_FLT_ISSUE_UNASSIGNED').prop('checked',false);$('#FLT_ISSUE_UNASSIGNED').val('no');";

  CheckField unass = new CheckField(I18n.get("ISSUE_UNASSIGNED"), "FLT_ISSUE_UNASSIGNED", " ", false);
  unass.preserveOldValue = false;
  unass.additionalOnclickScript = "if ($(this).prop('checked')){removeSmartComboEntry('FLT_ISSUE_ASSIGNED_TO')}";
  unass.label = "";


  SmartCombo companySC = ResourceBricks.getCompanyCombo("FLT_ISSUE_ASSIGNED_COMPANY", TeamworkPermissions.resource_canRead, null, pageState);
  companySC.fieldSize = 30;
  companySC.separator = "";
  companySC.preserveOldValue = false;
  companySC.addAllowed = true;
  companySC.label = "";

  CheckField companyChildren = new CheckField("", "FLT_COMPANY_ISSUE_SHOW_CHILDREN", "&nbsp;", false);
  companyChildren.preserveOldValue = false;


  /*TextField tfNotes = new TextField("TEXT", I18n.get("ISSUE_NOTES"), "FLT_ISSUE_NOTES", "<br>", 20, false);
  tfNotes.id = "ISSUE_NOTES";
  tfNotes.searchField = true;
  tfNotes.preserveOldValue = false;*/

  CheckField inHist = new CheckField(I18n.get("ISSUE_INHISTORY"), "FLT_ISSUE_INHISTORY", " ", false);
  inHist.preserveOldValue = false;
  inHist.toolTip = I18n.get("SEARCH_A&N_IN_HISTORY");

  //SmartCombo assBy = ResourceBricks.getPersonCombo("FLT_ISSUE_ASSIGNED_BY", false, null, pageState);
  SmartCombo assBy = ResourceBricks.getResourceCombo("FLT_ISSUE_ASSIGNED_BY", TeamworkPermissions.resource_canRead, null, Resource.class, pageState);
  assBy.fieldSize = 25;
  assBy.preserveOldValue = false;
  assBy.separator = "<br>";
  assBy.label = I18n.get("ISSUE_ASSIGNED_BY");

  TextField tfEstim = new TextField("TEXT", I18n.get("ISSUE_WORKLOG_ESTIMATED_TIME"), "FLT_ISSUE_ESTIMATED", "<br>", 10, false);
  tfEstim.id = "ISSUE_ESTIMATED";
  tfEstim.searchField = true;
  tfEstim.preserveOldValue = false;

  ColorValueChooser statusChooser = IssueBricks.getStatusChooser("FLT_ISSUE_STATUS", "FLT_ISSUE_STATUS", true, true, pageState);
  statusChooser.label = I18n.get("ISSUE_STATUS");
  statusChooser.showOpener = true;
  statusChooser.separator = "<br>";
  statusChooser.preserveOldValue = false;

  DateField dfLastChange = new DateField("FLT_ISSUE_STATUS_LAST_CHANGE");
  dfLastChange.separator = "<br>";
  dfLastChange.setSearchField(true);
  dfLastChange.labelstr = I18n.get("ISSUE_STATUS_LAST_CHANGE");

  DateField dfSignalled = new DateField("FLT_ISSUE_DATE_SIGNALLED");
  dfSignalled.separator = "<br>";
  dfSignalled.labelstr = I18n.get("ISSUE_DATE_SIGNALLED");
  dfSignalled.setSearchField(true);

  DateField dfShouldClose = new DateField("FLT_ISSUE_DATE_CLOSE_BY");
  dfShouldClose.separator = "<br>";
  dfShouldClose.setSearchField(true);
  dfShouldClose.labelstr = I18n.get("ISSUE_DATE_CLOSE_BY");

  DateField dfLastMod = new DateField("FLT_ISSUE_LAST_MODIFIED");
  dfLastMod.separator = "<br>";
  dfLastMod.setSearchField(true);
  dfLastMod.labelstr = I18n.get("ISSUE_LAST_MODIFIED");

  ColorValueChooser gravity = IssueBricks.getGravityChooser("FLT_ISSUE_GRAVITY", "ISSUEGRAVITYFILTER", true, true, pageState);
  gravity.height = 30;
  gravity.showOpener = true;
  gravity.label = I18n.get("ISSUE_GRAVITY");
  gravity.separator = "<br>";
  gravity.preserveOldValue = false;

  SmartCombo impact = IssueBricks.getIssueImpactCombo("FLT_ISSUE_IMPACT", null, pageState);
  impact.separator = "<br>";
  impact.addAllowed=true;
  impact.preserveOldValue = false;
  impact.label = I18n.get("ISSUE_IMPACT");

  SmartCombo taskType = TaskBricks.getTaskTypeCombo("ISSUE_TASK_TYPE", pageState);
  taskType.label = I18n.get("ISSUE_TASK_TYPE");
  taskType.separator = "<br>";
  taskType.addAllowed=true;
  taskType.preserveOldValue = false;

  ColorValueChooser taskStatus = TaskBricks.getStatusChooser("FLT_ISSUE_TASK_STATUS", "ISSUETASKSTATUSFILTER", true, true);
  taskStatus.height = 30;
  taskStatus.showOpener = true;
  taskStatus.label = I18n.get("ISSUE_TASK_STATUS");
  taskStatus.separator = "<br>";
  taskStatus.preserveOldValue = false;

  TextField tfCode = new TextField("TEXT", I18n.get("ISSUE_CODE"), "FLT_ISSUE_CODE", "<br>", 18, false);
  tfCode.searchField = true;
  tfCode.preserveOldValue = false;

  TextField tfId = new TextField("TEXT", I18n.get("ISSUE_ID"), "ISSUE_ID", "<br>", 8, false);
  tfId.searchField = true;
  tfId.preserveOldValue = false;

  TagBox tags = new TagBox("FLT_ISSUE_TAGS", Issue.class, ((TeamworkOperator) pageState.getLoggedOperator()).getPerson().getArea());
  tags.label = I18n.get("TAGS");
  tags.separator = "<br>";
  tags.fieldSize = 20;
  tags.preserveOldValue = false;

  SmartCombo issueType = IssueBricks.getIssueTypeCombo("FLT_ISSUE_TYPE", null, pageState);
  issueType.label = I18n.get("ISSUE_TYPE");
  issueType.separator = "<br>";
  issueType.fieldSize = 15;
  issueType.required = false;
  issueType.addAllowed= true;
  issueType.preserveOldValue = false;

  CheckField haveWklg = new CheckField(I18n.get("ISSUE_HAVING_WRKLG"), "FLT_ISSUE_HAVING_WRKLG", "&nbsp;", false);
  haveWklg.preserveOldValue = false;

  Combo area = SecurityBricks.getAreaCombo("FLT_AREA", TeamworkPermissions.issue_canRead, pageState);
  area.separator = "<br>";
  area.cvl.addChoose(pageState);
  area.preserveOldValue = false;
  area.label = I18n.get("AREA");

  CheckField recentlyUsed= new CheckField("RECENTLY_USED","&nbsp;",false);
  recentlyUsed.preserveOldValue=false;

  SmartCombo customersCombo = ResourceBricks.getInspectableCustomersCombo("CUST_ID", pageState);
  customersCombo.label = I18n.get("CUSTOMERS");
  customersCombo.fieldSize = 30;
  customersCombo.preserveOldValue = false;
  customersCombo.separator = "<br>";

  TextField externalRequester = TextField.getEmailInstance("FLT_ISSUE_EXTERNAL_REQUESTER");
  externalRequester.preserveOldValue=false;
  externalRequester.label=I18n.get("EXT_REQUESTER_EMAIL");
  externalRequester.separator="<br>";


%>
  <div class="filterActiveElements"></div>

  <div class="filterInactiveElements">
    <div class="filterElement <%=hideTask?"hiddenFilterElement":""%> ">
      <label for="<%=taskSC.id%>_txt"><%=I18n.get("ISSUE_TASK")%></label> &nbsp;&nbsp;&nbsp;&nbsp;<%taskChildren.toHtml(pageContext);%><label><%=I18n.get("TASK_ISSUE_SHOW_CHILDREN")%></label><br><%taskSC.toHtmlI18n(pageContext);%>
    </div>
    <%if (hideTask){%>
    <div class="filterElement centered">
     <%taskChildren.label="TASK_ISSUE_SHOW_CHILDREN";
       taskChildren.toHtmlI18n(pageContext);%>
    </div>
    <%}%>


    <div class="filterElement filterDefault"><%tfDescr.toHtml(pageContext);%></div>
    <div class="filterElement <%=hideAssignee?"hiddenFilterElement":""%> ">
      <label for="<%=resourceCombo.id%>_txt"><%=I18n.get("ISSUE_ASSIGNED_TO")%></label> &nbsp;&nbsp;&nbsp;&nbsp; <%unass.toHtml(pageContext);%> <label><%=I18n.get("ISSUE_UNASSIGNED")%></label><br><%resourceCombo.toHtmlI18n(pageContext);%>
    </div>

    <div class="filterElement <%=hideAssignee?"hiddenFilterElement":""%> ">
      <label for="<%=companySC.id%>_txt"><%=I18n.get("ISSUE_ASSIGNED_TO")%> <%=I18n.get("COMPANY_OR_DEPARTMENT")%></label> &nbsp;&nbsp;&nbsp;&nbsp; <%companyChildren.toHtml(pageContext);%> <label><%=I18n.get("WHOLE_TEAM")%></label><br><%companySC.toHtmlI18n(pageContext);%>
    </div>


    <%--<div class="filterElement"><%tfNotes.toHtml(pageContext);%></div>--%>
    <div class="filterElement centered"><%inHist.toHtml(pageContext);%></div>
    <div class="filterElement"><%assBy.toHtml(pageContext);%></div>
    <div class="filterElement"><%tfEstim.toHtml(pageContext);%></div>
    <div class="filterElement"><%statusChooser.toHtml(pageContext);%></div>
    <div class="filterElement"><%dfLastChange.toHtml(pageContext);%></div>
    <div class="filterElement"><%dfSignalled.toHtml(pageContext);%></div>
    <div class="filterElement"><%dfShouldClose.toHtml(pageContext);%></div>
    <div class="filterElement"><%dfLastMod.toHtml(pageContext);%></div>
    <div class="filterElement"><%gravity.toHtml(pageContext);%></div>
    <div class="filterElement centered"><%recentlyUsed.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%externalRequester.toHtml(pageContext);%></div>

    <%if (Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USEIMPACTONISSUES"))) {%>
    <div class="filterElement"><%impact.toHtml(pageContext);%></div>
    <%}%>
    <div class="filterElement"><%customersCombo.toHtml(pageContext);%></div>
    <div class="filterElement"><%taskType.toHtml(pageContext);%></div>
    <div class="filterElement"><%taskStatus.toHtml(pageContext);%></div>
    <%if (Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USECODEONISSUES"))) {%>
    <div class="filterElement"><% tfCode.toHtml(pageContext);%></div>
    <%}%>
    <div class="filterElement"><%tfId.toHtml(pageContext);%></div>
    <div class="filterElement"><%tags.toHtml(pageContext);%></div>
    <%if (Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USETYPEONISSUES"))) {%>
    <div class="filterElement"><%issueType.toHtml(pageContext);%></div>
    <%}%>
    <div class="filterElement centered"><%haveWklg.toHtml(pageContext);%></div>
    <%if(!SecurityBricks.isSingleArea()){%>
    <div class="filterElement "><%area.toHtml(pageContext);%></div>
    <%}%>

    <%
      // ------------------------------------------------------------------------------------------- custom fields -------------------------------------------------------------------------------------------
      if (IssueBricks.hasCustomField()) {
        for (int i = 1; i < 7; i++) {
          DesignerField dfStr = DesignerField.getCustomFieldInstance("ISSUE_CUSTOM_FIELD_", i, null, false, false, true, pageState);
          if (dfStr != null) {
            dfStr.usedForSearch = true;
            dfStr.preserveOldValue = false;
            dfStr.separator = "<br>";
            %><div class="filterElement"><%dfStr.toHtml(pageContext);%></div><%
        }
      }
    }
  %>
  </div>



