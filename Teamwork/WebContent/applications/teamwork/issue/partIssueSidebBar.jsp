<%@ page import="org.jblooming.waf.view.PageSeed, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.settings.I18n,
org.jblooming.waf.html.display.Hint, com.twproject.security.TeamworkPermissions, org.jblooming.waf.html.container.DivOnMouseover,
com.twproject.task.businessLogic.IssueController, org.jblooming.waf.html.button.*, com.twproject.waf.settings.ReportBricks,
org.jblooming.waf.PluginBricks, java.util.List, java.util.TreeSet, org.jblooming.utilities.JSP, net.sf.json.JSONObject, org.jblooming.waf.view.PageState, com.twproject.operator.TeamworkOperator, org.jblooming.page.Page, org.jblooming.waf.html.state.Form, org.jblooming.waf.html.input.LoadSaveFilter, com.twproject.task.Issue, com.twproject.task.IssueBricks, org.jblooming.designer.DesignerField, com.twproject.resource.Person, com.twproject.worklog.Worklog, com.twproject.resource.Resource, com.twproject.task.Assignment, com.twproject.task.Task" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  Page issues = pageState.getPage();
%>

<div class="rightColumn noprint">


<div class="tools">
  <div class="toolsElement">
  <%
    // --------------- ADD ISSUE -------
    // se ci sono plugin deve mettere il dropdown, altrimenti un add stecchito
    List<ButtonSupport> wizButtons = PluginBricks.createPageQuarkItemsButtons("WIZARD", pageState);

    if (JSP.ex(wizButtons)) {
      ButtonJS opener = new ButtonJS("");
      opener.iconChar = "P";
      opener.additionalCssClass = "menuArrow first";
      opener.toolTip = I18n.get("ADD");
      opener.label = "";
      DivOnMouseover add = new DivOnMouseover(opener);

      ButtonJS addIssue = new ButtonJS("addIssue();");
      addIssue.label = I18n.get("ADD_ISSUE");
      add.addButton(addIssue);

      //inject custom Wizard
      add.addButtons(wizButtons);
      add.toHtml(pageContext);
    } else {
      ButtonJS add_issue = new ButtonJS(I18n.get("ADD_ISSUE"), "addIssue();");
      add_issue.iconChar="P";
      add_issue.additionalCssClass="first";
      add_issue.toHtml(pageContext);
    }
  %></div><%


  ButtonJS imp = new ButtonJS("");
  imp.toolTip = I18n.get("ISSUE_IMPORT_EXPORT");
  imp.label = "";
  imp.iconChar = "I";
  DivOnMouseover domo = new DivOnMouseover(imp);

  if (logged.hasPermissionFor(TeamworkPermissions.issue_canCreate)) {
    ButtonLink ii = new ButtonLink(I18n.get("ISSUE_IMPORT_FROM_CSV"), pageState.pageFromRoot("issue/importIssueFromCsv.jsp"));
    domo.addButton(ii);
  }


  ButtonExportXLS excel = new ButtonExportXLS(pageState.getForm(), IssueController.class, "EXPORT");
  excel.additionalCssClass="lreq20 lreqLabel";
  excel.addImportClasses(Task.class.getName(), Assignment.class.getName(), Resource.class.getName(), Worklog.class.getName(), Person.class.getName(), Issue.class.getName());

  excel.outputFileName = "issueList";
  excel.label = I18n.get("EXPORT_EXCEL");

  if (I18n.isActive("CUSTOM_EXPORT_EXCEL_ISSUE")) {
    JSONObject properties = JSONObject.fromObject(I18n.get("CUSTOM_EXPORT_EXCEL_ISSUE"));
    excel.addFieldsToExport(properties);
  } else {

    excel.addFieldToExport("id",  I18n.get("ISSUE_ID"));
    excel.addFieldToExport("status.description",  I18n.get("ISSUE_STATUS"));
    excel.addFieldToExport("code",  I18n.get("ISSUE_CODE"));
    excel.addFieldToExport("description",  I18n.get("ISSUE_DESCRIPTION"));
    excel.addFieldToExport("type.description",  I18n.get("TYPE"));
    excel.addFieldToExport("gravity",  I18n.get("ISSUE_GRAVITY"));
    excel.addFieldToExport("tags",  I18n.get("TAGS"));
    excel.addFieldToExport("assignedTo.name",  I18n.get("ISSUE_ASSIGNED_TO"));
    excel.addFieldToExport("assignedBy.name",  I18n.get("ISSUE_ASSIGNED_BY"));
    excel.addFieldToExport("dateSignalled",  I18n.get("ISSUE_DATE_SIGNALLED"));
    excel.addFieldToExport("shouldCloseBy",  I18n.get("ISSUE_DATE_CLOSE_BY"));
    excel.addBSHFieldToExport("obj.getEstimatedDuration()/3600000D",  I18n.get("ISSUE_WORKLOG_ESTIMATED_TIME"));
    excel.addBSHFieldToExport("obj.getWorklogDone()/3600000D",  I18n.get("ISSUE_WORKLOGS_SHORT"));

    for (int i = 1; i < 7; i++) {
      if (I18n.isActive("ISSUE_CUSTOM_FIELD_" + i)) {
        excel.addFieldToExport("customField" + i, DesignerField.getCustomFieldInstance("ISSUE_CUSTOM_FIELD_", i, null, true, false, false, null).label);
      }
    }

    excel.addFieldToExport("lastModified", I18n.get("LAST_MODIFIED_ON"));
    excel.addFieldToExport("lastModifier", I18n.get("LAST_MODIFIED_BY"));

  }
  domo.addButton(excel);

%><div class="toolsElement"><%domo.toHtml(pageContext);%></div><%


  ButtonJS print = new ButtonJS("");
  print.iconChar = "p";
  print.toolTip = I18n.get("PRINT");
  print.label = "";
  DivOnMouseover pr = new DivOnMouseover(print);

  ButtonJS p = ButtonSubmit.getBlackInstance(pageState.getForm(), pageState.pageFromRoot("issue/issueListPrint.jsp").href);
  p.label = I18n.get("PRINT");
  pr.addButton(p);

  //reports
  TreeSet<ReportBricks.Report> reports = ReportBricks.getReports("issue");
  if (reports.size() > 0) {
    pr.addSeparator();
    pageState.getForm().url.addClientEntry("reportName", "");
    pageState.getForm().url.addClientEntry("outType", "");
    for (ReportBricks.Report r : reports) {
      ButtonSubmit pri = new ButtonSubmit(pageState.getForm());
      pri.variationsFromForm.command = Commands.FIND;
      pri.variationsFromForm.href = pageState.pageFromRoot("issue/issueIReportHelper.jsp").href;
      pri.variationsFromForm.addClientEntry("reportName", r.name);
      pri.target = "_blank";
      pri.label = r.getBestName();
      pri.preserveFormStatus = true;
      pri.additionalCssClass="lreq30 lreqLabel";
      pr.addButton(pri);
    }
  }

  //inject custom reports
  pr.addButtons(PluginBricks.createPageQuarkItemsButtonsForDataTable("REPORTS", pageState));


%><div class="toolsElement"><%pr.toHtml(pageContext);%></div>

</div>
<%
  //----------------------------------------------------------------------------  START RIGHT COLUMN INNER ----------------------------------------------------------------------------
%>
<div class="rightColumnInner">
  <%

    //----------------------------------------------------------------------------  START FILTER BOX ----------------------------------------------------------------------------
  %>


  <div class="noprint filters">
    <h2><%=I18n.get("MY_SAVED_FILTERS")%></h2>
    <%

      LoadSaveFilter lsfb = new LoadSaveFilter("ISSUEFILTER", pageState.getForm());
      lsfb.drawEditor = false;
      lsfb.drawButtons = true;
      lsfb.id="issueLSF";

      lsfb.toHtml(pageContext);

    %><div class="separator"></div><%

    String cmd = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_OPEN_ISSUES");
      ButtonLink bl = new ButtonLink(I18n.get("PF_MY_OPEN_ISSUES"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_EXPIRED_ISSUES");
      ButtonLink bl = new ButtonLink(I18n.get("PF_MY_EXPIRED_ISSUES"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_OPEN_SEVERE_ISSUES");
      ButtonLink bl = new ButtonLink(I18n.get("PF_MY_OPEN_SEVERE_ISSUES"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }


    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_INSERTED_ISSUES");
      ButtonLink bl = new ButtonLink(I18n.get("PF_MY_INSERTED_ISSUES"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

           /* {
                PageSeed ps = pageState.thisPage(request);
                ps.command = Commands.FIND;
                ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_OPEN_TODOS");
                ButtonLink bl = new ButtonLink(I18n.get("PF_MY_OPEN_TODOS"), ps);
                bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
                bl.toHtmlInTextOnlyModality(pageContext);
            }*/

    //more
    cmd = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_ISSUES_OPENED_RECENTLY");
      ButtonLink bl = new ButtonLink(I18n.get("PF_ISSUES_OPENED_RECENTLY"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_ISSUES_CLOSED_RECENTLY");
      ButtonLink bl = new ButtonLink(I18n.get("PF_ISSUES_CLOSED_RECENTLY"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_LONG_STANDING_ISSUES");
      ButtonLink bl = new ButtonLink(I18n.get("PF_LONG_STANDING_ISSUES"), ps);
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_OPEN_SEVERE_ISSUES");
      ButtonLink bl = new ButtonLink(I18n.get("PF_OPEN_SEVERE_ISSUES"), ps);
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


    //----------------------------------------------------------------------------  END FILTER BOX ----------------------------------------------------------------------------


  %></div><%
  //-------------------------------  HINTS MANAGEMENT ---------------------------------------------------

  //Hint.HintWriter hintWriter = new Hint.HintWriter();
  //hintWriter.addHint("HINT_FIRST_PLAN", "#ISSUES_MENU", 300, 200, pageState.href.indexOf("IssueList") >= 0 && pageState.getPage() != null && pageState.getPage().getTotalNumberOfElements() > 5, pageState);
  //hintWriter.toHtml(pageContext);

%>
</div>
</div>
