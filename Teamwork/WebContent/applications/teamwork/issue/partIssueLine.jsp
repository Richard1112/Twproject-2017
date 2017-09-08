<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Person,
                 com.twproject.resource.Resource,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.*,
                 com.twproject.waf.html.IssueDrawer,
                 net.sf.json.JSONArray,
                 org.jblooming.agenda.CompanyCalendar,
                 org.jblooming.designer.DesignerField,
                 org.jblooming.ontology.PersistentFile,
                 org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.ReflectionUtilities,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.input.*, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.constants.Commands, org.jblooming.utilities.StringUtilities, java.util.HashSet, org.jblooming.security.Permission, java.util.Set, java.util.ArrayList, com.twproject.resource.ResourceBricks, org.jblooming.waf.html.layout.HtmlColors" %>
<%

  IssueDrawer taskIssueDrawer = (IssueDrawer) JspIncluderSupport.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  Issue issue = taskIssueDrawer.currentIssue;
  boolean canWrite = issue.hasPermissionFor(logged,TeamworkPermissions.issue_canWrite);
  boolean canChangeStatus =  issue.hasPermissionFor(logged,TeamworkPermissions.issue_canChangeStatus);

  if (!issue.hasPermissionFor(logged,TeamworkPermissions.issue_canRead))
    return;

  boolean usetypeonissues = Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USETYPEONISSUES"));
  boolean useimpactonissues = Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USEIMPACTONISSUES"));


  // make the client entries
  pageState.addClientEntry("ISSUE_DESCRIPTION", issue.getDescription());
  Task task = issue.getTask();
  pageState.addClientEntry("ISSUE_TASK", task);
  pageState.addClientEntry("ISSUE_TYPE", issue.getType());
  pageState.addClientEntry("ISSUE_CODE", issue.getCode());
  pageState.addClientEntry("ISSUE_GRAVITY", issue.getGravity());
  pageState.addClientEntry("ISSUE_TAGS", issue.getTags());
  pageState.addClientEntry("ASSIGNEE", issue.getAssignedTo());
  pageState.addClientEntry("ISSUE_STATUS", issue.getStatus());
  pageState.addClientEntryTime("ISSUE_WORKLOG_ESTIMATED_TIME", issue.getEstimatedDuration());
  pageState.addClientEntry("ISSUE_DATE_CLOSE_BY", issue.getShouldCloseBy());
  pageState.addClientEntry("ISSUE_IMPACT", issue.getImpact());


  ColorValueChooser cvc = IssueBricks.getStatusChooser("ISSUE_STATUS", "STATUSLINE", pageState);
  cvc.readOnly = !canChangeStatus;
  cvc.displayValue = false;
  cvc.height = 15;
  cvc.width = 15;
  cvc.onChangeScript = "hidden.each(changeStatus);";
  //pageState.initedElements.add(ColorValueChooser.class.getName()); //horrible hack to avoid cvc is inited
  cvc.alreadyInited=true;

  List<IssueHistory> issueComments = issue.getComments();

  ColorValueChooser cvcG = IssueBricks.getGravityChooser("ISSUE_GRAVITY", "ISSGRLIN", false, false, pageState);
  cvcG.readOnly = !canWrite;
  cvcG.height = 15;
  cvcG.width = 15;
  cvcG.displayValue = false;
  cvcG.showOpener = false;

  // questo è iniettato nella lista

  cvcG.onChangeScript = "hidden.each(changeGravity);";


  TextArea issueDescription = new TextArea("", "ISSUE_DESCRIPTION", "", 1, 1, "");
  issueDescription.maxlength = 0;
  issueDescription.readOnly = !canWrite;
  issueDescription.script = "style='width:100%;margin-bottom:5px'";
  issueDescription.setAutosize(10, 800, 30);
  issueDescription.required=true;

  ButtonJS comments = new ButtonJS("showComments($(this));");
  comments.toolTip = I18n.get("ISSUE_COMMENTS") + ": " + (JSP.ex(issueComments) ? issueComments.size() : 0);
  comments.additionalCssClass = "small";
  comments.label = issueComments.size() + "";
  comments.iconChar="Q";


  TextField issueCode = new TextField("ISSUE_CODE", "");
  issueCode.readOnly = !canWrite;
  issueCode.separator = "";
  issueCode.fieldSize = 8;
  issueCode.label = "";
  issueCode.maxlength = 20;
  issueCode.innerLabel =  I18n.get("ISSUE_CODE");
  issueCode.innerLabel = I18n.get("ADD_ISSUE_CODE");
  issueCode.script = " style='margin:0 10px 0 0'";


  TagBox tags = new TagBox("ISSUE_TAGS", Issue.class, issue.getArea());
  tags.label = "";
  tags.readOnly = !canWrite;
  tags.toolTip = I18n.get("TAGS_COMMA_SEPARATED");
  tags.innerLabel = I18n.get("ADD_TAGS");
  tags.script = " style='width:65%;display:inline;margin:0 10px 0 0'";


  boolean showActiveOnly = !JSP.ex(issue.getDescription()); // in order to see only active task when creating
  //SmartCombo taskCombo = TaskBricks.getTaskCombo("ISSUE_TASK", showActiveOnly, TeamworkPermissions.issue_canWrite, pageState);
  SmartCombo taskCombo = TaskBricks.getTaskCombo("ISSUE_TASK", showActiveOnly, TeamworkPermissions.issue_canCreate,null, pageState);

  if (task !=null) {
    taskCombo.additionalLines = new ArrayList<Object[]>();
    taskCombo.additionalLines.add(new Object[]{task.getId(), JSP.w(task.getCode()) + " " + JSP.w(task.getName())});
  }


  taskCombo.readOnly = !canWrite;
  taskCombo.label = "";
  taskCombo.separator = "";
  taskCombo.fieldSize = 20;
  taskCombo.required=true;
  taskCombo.onValueSelectedScript = "reloadAssignee($(this));";
  taskCombo.script = "style='min-width:150px'";

  SmartCombo issueType = IssueBricks.getIssueTypeCombo("ISSUE_TYPE", issue, pageState);
  issueType.readOnly = !canWrite;
  issueType.separator = "";
  issueType.label = "";
  issueType.innerLabel =  I18n.get("ISSUE_TYPE");
  issueType.fieldSize = 10;
  issueType.script = "style='min-width:100px'";

  SmartCombo issueImpact = IssueBricks.getIssueImpactCombo("ISSUE_IMPACT", issue, pageState);
  issueImpact.readOnly = !canWrite;
  issueImpact.separator = "";
  issueImpact.label = "";
  issueImpact.innerLabel =  I18n.get("ISSUE_IMPACT");
  issueImpact.fieldSize = 10;
  issueImpact.script = "style='min-width:100px'";

  Person loggedPerson = logged.getPerson();
  Assignment ass = issue.getAssignmentOnTask(loggedPerson);
  String assS = "";
  boolean showWL = ass != null;
  if (showWL) {
    assS = "assId=\"" + ass.getId() + "\"";
  }

  Resource assignee = issue.getAssignedTo();

  boolean expired=issue.getShouldCloseBy()!=null&& issue.getShouldCloseBy().getTime()<System.currentTimeMillis() && issue.getStatus().isBehavesAsOpen() ;

%>

<tr issueId="<%=issue.getId()%>" class="alternate issueRow <%=taskIssueDrawer.readMode?"inView":"inEdit"%> <%=expired?"expired":""%>" <%=assS%> gravity="<%=JSP.w(issue.getGravity())%>">

<%-------------------------------------------------------SELECT---------------------------------------------------------------------%>
<td width="1%" align="center">
  <input type="checkbox" class="selector" onclick="refreshBulk($(this));" <%=canWrite?"":"disabled"%>>
</td>

<%-------------------------------------------------------PRIORITY AND DRAG---------------------------------------------------------------------%>
<td align="center" class="priority issueDrag <%=canWrite?"dragHandler":""%>" fieldName="ISSUE_GRAVITY">
  <%if (taskIssueDrawer.readMode){%>
  <div class= "prioWrapper">
    <div class="prio up teamworkIcon" title="top" onclick="moveIssueToTop($(this));event.stopPropagation();">k</div>
    <div class="prio down teamworkIcon" title="bottom" onclick="moveIssueToBottom($(this));event.stopPropagation();">j</div>
  </div>
  <%
    }else{
      cvcG.showOpener = true;
      cvcG.toHtml(pageContext);
    }
  %>
</td>
<%-------------------------------------------------------STATUS---------------------------------------------------------------------%>
<%if (!taskIssueDrawer.readMode){
   cvc.showOpener = true;
  }
%>
<td valign="top" align="center"><%cvc.toHtml(pageContext);%></td>

<%-------------------------------------------------------DESCRIPTION (CODE) ---------------------------------------------------------------------%>
<td class="issueDescTd" valign="top" fieldName="ISSUE_DESCRIPTION">
  <%if (taskIssueDrawer.readMode){%>
  <div class="issueDesc twLink" fieldName="ISSUE_DESCRIPTION" style="word-wrap:normal; overflow: hidden;"><%=JSP.encode(issue.getDescription())%></div>
  <%} else {
    issueDescription.toHtml(pageContext);
    if (Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USECODEONISSUES"))) {
  %><div style="display: inline"><%issueCode.toHtml(pageContext);%></div><%
    }
  }
%><span class="twLink textSmall issueCode" style="<%=taskIssueDrawer.readMode?"":"display:none;"%>">I#<%=issue.getMnemonicCode()%>#</span><%


  //------------------------------------------------------- TAGS ---------------------------------------------------------------------
  if (taskIssueDrawer.readMode){
    if (JSP.ex(issue.getTags())) {
      PageSeed search = pageState.pageFromRoot("issue/issueList.jsp");
      search.command=Commands.FIND;

      %><div fieldName="ISSUE_TAGS" class="issueTags"><%
      for (String tag:StringUtilities.splitToList(issue.getTags(), ",")){
        search.addClientEntry("FLT_ISSUE_TAGS",tag);
        ButtonJS bl= new ButtonJS(tag,"location.href='"+search.toLinkToHref()+"';event.stopPropagation();");
        bl.additionalCssClass="tag";
        bl.toHtmlInTextOnlyModality(pageContext);
      }
      %></div><%

    }
  %><span class="textSmall"><%comments.toHtmlInTextOnlyModality(pageContext);%></span><%

} else {

  tags.toHtml(pageContext);
  if (! (issue.isNew() || (issue.getId()+"").startsWith("new_"))){
    %><span class="textSmall"><%comments.toHtmlInTextOnlyModality(pageContext);%></span><%
  }
}

  //------------------------------------------------------- COMMENTS ---------------------------------------------------------------------

  //mostra l'ultimo commento se non è del logged ed è più recente di 2 settimane
  //mostra l'ultimo commento se  è più recente di 2 settimane
  if (JSP.ex(issueComments)) {
    IssueHistory issueHistory = issueComments.get(issueComments.size() - 1);
    if (JSP.ex(issueHistory.getComment()) && !logged.getFullname().equals(issueHistory.getCreator())  && (issueHistory.getCreationDate().getTime() > (System.currentTimeMillis()- CompanyCalendar.MILLIS_IN_WEEK * 2 ))) {
      ButtonJS bjs = new ButtonJS(I18n.get("REPLY"), "addComment($(this));");
      bjs.additionalCssClass = "small";
      %><span class="textSmall" style="color: #999">
      <%=issueHistory.getCreator()%>:&nbsp;<%=DateUtilities.dateToRelative(issueHistory.getCreationDate())%> <%--<%=JSP.w(issueHistory.getComment())%>--%>
      <%bjs.toHtmlInTextOnlyModality(pageContext);%>
      </span><%
    }
  }


  //------------------------------------------------------- FILES ---------------------------------------------------------------------
%><div class="filesBox textSmall"><span class="teamworkIcon filesIcon" style="display:none;">n</span></div><%
  JSONArray files= new JSONArray();
  for (PersistentFile pf:issue.getFiles())
    files.add(pf.jsonify());
%>

  <script>$(function(){drawFiles("<%=issue.getId()%>",<%=files%>)});</script>

  <div class="uploadizeDrop" style="display: none;" <%=canWrite ?"":"disabled"%> ></div>

  <%
    //------------------------------------------------------- CUSTOM FIELDS ---------------------------------------------------------------------

    if (IssueBricks.hasCustomField()) {
  %><div class="issueCustomFields clearfix"><%
  for (int i=1; i<7; i++) {
    DesignerField customField = DesignerField.getCustomFieldInstance( "ISSUE_CUSTOM_FIELD_",i, issue,!canWrite, taskIssueDrawer.readMode, false, pageState);
    String fieldValue= (String)(ReflectionUtilities.getFieldValue("customField" + i, issue));
    if (customField!=null && (taskIssueDrawer.readMode && JSP.ex(fieldValue) || !taskIssueDrawer.readMode)){
      customField.separator=(taskIssueDrawer.readMode ? "&nbsp;" : "<br>");
      //customField.label=I18n.get("ISSUE_CUSTOM_FIELD_"+i);
      %><div style="display:inline-block; <%=(taskIssueDrawer.readMode ? "margin-right: 20px" : "margin-right: 5px")%>" fieldName="ISSUE_CUSTOM_FIELD_<%=i%>">
      <%customField.toHtml(pageContext); %>
      </div><%
    }
  }
%></div><%
  }

%>
</td>

<%------------------------------------------------------- TASK ---------------------------------------------------------------------%>
<td fieldName="ISSUE_TASK_txt" class="twLink issueTaskCell">
  <%
    if (taskIssueDrawer.readMode) {
      if (task != null) {
        if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")){
          %><div class="pathSmall"><%=task.getPath(" / ", false)%></div><%
        }
      String styleColor="";
      String color = task.getTaskColor();
      if (I18n.isActive("CUSTOM_FEATURE_USE_PROJECT_COLOR") && JSP.ex(color)) {
        styleColor = " style=\"border-left:5px solid " + color + "; padding-left: 5px;\"";
      }

        %><span class="textSmall" style="display: block;line-height: 130%"><span <%=styleColor%>><%=task.getName()%></span><br>T#<%=task.getMnemonicCode()%>#</span><%
      } else {
        %>&nbsp;<%
      }
    } else {
      taskCombo.toHtml(pageContext);
    }
  %>
</td>

<%------------------------------------------------------- ASSIGNEE ---------------------------------------------------------------------%>
<td fieldName="ASSIGNEE_txt" class="twLink resCombo " style="min-width: 120px;"><%
  if (taskIssueDrawer.readMode){
    if (JSP.ex(issue.getExtRequesterEmail())){
      %><img src="<%=ResourceBricks.getGravatarUrl(issue.getExtRequesterEmail(), 50)%>" align="absmiddle" class="face small" title="<%=issue.getExtRequesterEmail()%>"><span class="teamworkIcon fromTo">:</span><%
    } else if (issue.getAssignedBy()!=null && !issue.getAssignedBy().equals(assignee)){
      %><img src="<%=issue.getAssignedBy().bricks.getAvatarImageUrl()%>" align="absmiddle" class="face small" title="<%=issue.getAssignedBy().getDisplayName()%>"><span class="teamworkIcon fromTo">:</span><%
    }
    if (assignee != null) {
      %><img src="<%=assignee.bricks.getAvatarImageUrl()%>" align="absmiddle" class="face small" title="<%=assignee.getDisplayName()%>"><%
    } else {
      %><small style="opacity: .5;font-style: italic;white-space:nowrap"><%=I18n.get("ISSUE_UNASSIGNED")%></small><%
    }

  } else {
    JspHelper issueResDraw = new JspHelper("/applications/teamwork/issue/issueResourceDrawer.jsp");
    issueResDraw.parameters.put("ISSUE", issue);
    issueResDraw.toHtml(pageContext);
  }
%>
</td>


<%------------------------------------------------------- TYPE ? ---------------------------------------------------------------------%>
<%if (usetypeonissues) {%>
<td fieldName="ISSUE_TYPE_txt">
  <%if (taskIssueDrawer.readMode){%>
  <%=issue.getType() == null ? "&nbsp;" : issue.getType().getDescription()%>
  <%} else {
    issueType.toHtml(pageContext);
  }%>
</td>
<%}%>

<%------------------------------------------------------- IMPACT ? ---------------------------------------------------------------------%>
<%if (useimpactonissues) {%>
<td fieldName="ISSUE_IMPACT_txt">
  <%if (taskIssueDrawer.readMode){%>
  <%=issue.getImpact() == null ? "&nbsp;" : issue.getImpact().getDescription()%>
  <%} else {
    issueImpact.toHtml(pageContext);
  }%>
</td>
<%}%>

<%------------------------------------------------------- ISSUE DATE CLOSE BY ---------------------------------------------------------------------%>
<td fieldName="ISSUE_DATE_CLOSE_BY" nowrap>
  <%if (taskIssueDrawer.readMode){%>
  <span class="textSmall <%=(JSP.ex(issue.getShouldCloseBy()) && (issue.getShouldCloseBy().getTime()) < System.currentTimeMillis() && issue.getStatus().isBehavesAsOpen())?"warning warningIcon":""%>"> <%=JSP.w(issue.getShouldCloseBy())%></span>
  <%} else {
    DateField df = new DateField("ISSUE_DATE_CLOSE_BY");
    df.readOnly = !canWrite;
    df.separator = "";
    df.labelstr = "";
    df.size=6;
    if (task!=null) {
      df.minDate = task.getSchedule().getStartDate();
      df.maxDate = task.getSchedule().getEndDate();
    }

    df.toHtml(pageContext);

  }%>
</td>

<%------------------------------------------------------- ISSUE WORKLOG ESTIMATED TIME ---------------------------------------------------------------------%>
<td fieldName="ISSUE_WORKLOG_ESTIMATED_TIME" >
  <%if (taskIssueDrawer.readMode){%>
  <span class="textSmall" title="<%=I18n.get("ISSUE_WORKLOG_ESTIMATED_TIME")%>"><%=DateUtilities.getMillisInHoursMinutes(issue.getEstimatedDuration())%></span>
  <%
    if (issue.getWorklogDone()>0) {
      PageSeed was = pageState.pageFromRoot("/task/worklog/worklogAssignmentList.jsp");
      was.addClientEntry("ISSUE_ID",issue.getId());
      was.command= Commands.FIND;
      ButtonJS wdbutton = (ButtonJS) ButtonLink.getBlackInstance("", 768, 1180, was);
      wdbutton.additionalOnClickScript="event.stopPropagation();";

      wdbutton.label="<span class='textSmall'>" + DateUtilities.getMillisInHoursMinutes(issue.getWorklogDone()) + "</span>";
      %><br><%
      wdbutton.toHtmlInTextOnlyModality(pageContext);
    }
  } else {
    TextField et = TextField.getDurationInMillisInstance("ISSUE_WORKLOG_ESTIMATED_TIME");
    et.separator = "";
    et.label = "";
    et.readOnly = !canWrite;
    et.fieldSize = 3;
    et.toHtml(pageContext);
  }%>
</td>

<%------------------------------------------------------- BUTTONS ---------------------------------------------------------------------%>
<td nowrap align="right" class="issueButtonTd" width="1%"><%
  // --------------- EDIT BUTTON ------------
/*  if (issue.bricks.canWrite && taskIssueDrawer.readMode) {  // si va in edit di riga
    ButtonJS rowEdit = new ButtonJS("editIssue($(this));");
    rowEdit.iconChar = "e";
    rowEdit.toolTip = I18n.get("EDIT");
    rowEdit.toHtmlInTextOnlyModality(pageContext);
  } else */if (canWrite && !(issue.getId() + "").startsWith("new_")) { // si va in edit popup
  ButtonJS popupEdit = IssueBricks.getBlackEditor(issue.getId());
  popupEdit.iconChar = "e";
  popupEdit.toolTip = ButtonLink.getToolTipForIdentifiable(issue, pageState);
  //popupEdit.toolTip = I18n.get("FULL_EDIT");
  popupEdit.enabled = issue.hasPermissionFor(logged,TeamworkPermissions.issue_canRead);
  popupEdit.toHtmlInTextOnlyModality(pageContext);
}


  // --------------- WORLOG BUTTON ------------
  if (showWL) {
    ButtonJS worklog = new ButtonJS("btnShowWorklog($(this));");
    worklog.iconChar = "w";
    worklog.toolTip = I18n.get("ADD_WORKLOG");
    worklog.toHtmlInTextOnlyModality(pageContext);
  }


  // --------------- CLONE BUTTON (read only) ------------
  if (issue.hasPermissionFor(logged,TeamworkPermissions.issue_canCreate)) {
    ButtonJS clone = new ButtonJS("clone($(this));");
    clone.iconChar = "D";
    clone.toolTip = I18n.get("CLONE");
    clone.toHtmlInTextOnlyModality(pageContext);
  }


  // --------------- DELETE BUTTON ------------
  if (issue.hasPermissionFor(logged,TeamworkPermissions.issue_canDelete)){
    ButtonJS delete=new ButtonJS("delRow($(this));");
    delete.iconChar = "d";
    delete.toolTip = I18n.get("DELETE");
    delete.additionalCssClass = "delete";
    delete.toHtmlInTextOnlyModality(pageContext);
  }

  if (canWrite && !taskIssueDrawer.readMode ) {
%><div class="issueRowButtons"><%
  // --------------- SAVE BUTTON (edit only) ------------
  ButtonJS saveRow = new ButtonJS("saveIssue($(this));");
//    saveRow.iconChar = "s";
  saveRow.additionalCssClass = "btnSave first small full";
  saveRow.label = I18n.get("SAVE");
  saveRow.toHtml(pageContext);

%><br><%
  // --------------- CLOSE EDIT (edit only) ------------
  ButtonJS closeEdit= new ButtonJS("closeEditor($(this));");
  //closeEdit.iconChar="x";
  closeEdit.additionalCssClass = "small full";
  closeEdit.label = I18n.get("CLOSE");
  closeEdit.toHtml(pageContext);
%></div><%
  }

%>
</td>

</tr>
