<%@ page import="org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.I18n, org.jblooming.waf.html.input.DateField, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.view.PageState, org.jblooming.designer.DesignerField, com.twproject.task.IssueBricks, com.twproject.worklog.WorklogSupport, com.twproject.worklog.Worklog, com.twproject.task.Assignment, org.jblooming.operator.Operator, com.twproject.operator.TeamworkOperator, org.jblooming.utilities.JSP, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.input.TextArea, java.util.Date" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  Worklog worklog = null;
  Assignment ass = null;
  boolean canWrite = true;
  int wlId = pageState.getEntry("wlId").intValueNoErrorCodeNoExc();
  int assId = pageState.getEntry("assId").intValueNoErrorCodeNoExc();
  if (wlId > 0) {
    worklog = Worklog.load(wlId + "");
    if (worklog != null) {  // caso edit di un assignment
      ass = worklog.getAssig();
      //make dei parametri
      pageState.addClientEntryTime("WORKLOG_DURATION", worklog.getDuration());
      pageState.addClientEntry("WORKLOG_ACTION", worklog.getAction());
      pageState.addClientEntry("WORKLOG_INSERTIONDATE", worklog.getInserted());

      canWrite = worklog.bricks.canWrite(logged);
    }
  } else if (assId > 0) {
    ass = Assignment.load(assId + "");
  }

  if (!JSP.ex(pageState.getEntry("WORKLOG_INSERTIONDATE")))
    pageState.addClientEntry("WORKLOG_INSERTIONDATE", new Date());


%><style type="text/css">
  .buttonArea {
  margin: 10px 0 0;
}
  h3 {
    font-size: 16px;
  }
</style>

<div assId="<%=JSP.w(ass.getId())%>" issueId="<%=JSP.w(pageState.getEntry("issueId").stringValueNullIfEmpty())%>" wlId="<%=wlId>0?wlId:""%>" alertonchange=true>
  <h2 id="wlEditorTitle"><%=wlId <= 0 ? I18n.get("ADD_WORKLOG"): I18n.get("WORKLOG_EDIT")%></h2>
  <h3 id="wlEditorSubTitle"></h3>
  <%--<span class="teamworkIcon closeAddWL" title="hide" onclick="hideWorklogEditorIfYouCan();" style="cursor: pointer">x</span>--%>

  <table width="100%">
    <tr>
      <td><%

        TextField wl = TextField.getDurationInMillisInstance("WORKLOG_DURATION");
        wl.label = "WORKLOG";
        wl.separator = "<br>";
        wl.required = true;
        wl.readOnly = !canWrite;
        wl.addKeyPressControl(13, "_saveWorklog($(this));", "onkeyup");
        wl.toHtmlI18n(pageContext);

      %></td><td><span id="wlDateInput" style="display:none"><%

        DateField df = new DateField("WORKLOG_INSERTIONDATE");
        df.labelstr = "WORKLOG_INSERTIONDATE";
        df.separator = "<br>";
        df.readOnly = !canWrite || wlId > 0;
        df.size = 10;
        df.script=" tabindex=9"; // per non averlo nella seq. dei tab
        df.toHtmlI18n(pageContext);

      %></span></td>
    </tr>
    <tr><td colspan="2">
  <%

    // questa e il path local senza contextpath a questa pagina che può essere sovrascritto con una custom label
    String descriptionFormUrl = "/applications/teamwork/task/worklog/worklogDescriptionForm.jsp";
    if (I18n.isActive("CUSTOM_FEATURE_WORKLOG_FORM")) {
      descriptionFormUrl = I18n.get("CUSTOM_FEATURE_WORKLOG_FORM");
    }

    JspHelper wlDF = new JspHelper(descriptionFormUrl);
    if (ass != null)
      wlDF.parameters.put("assig", ass);
    if (worklog != null)
      wlDF.parameters.put("worklog", worklog);
    wlDF.toHtml(pageContext);

%></td></tr></table><%

    ButtonBar bb = new ButtonBar();


    ButtonJS savewl = new ButtonJS("_saveWorklog($(this))");
    savewl.label = I18n.get("SAVE_WORKLOG");
    savewl.additionalCssClass = "first small";
    savewl.enabled = canWrite;
    //savewl.toHtml(pageContext);
    bb.addButton(savewl);


    if (wlId > 0) {
      ButtonJS del = new ButtonJS(I18n.get("DELETE"), "_deleteWorklog($(this))");
      del.confirmRequire = true;
      del.enabled = canWrite;
      del.additionalCssClass = "small delete";
      //del.toHtml(pageContext);
      bb.addButton(del);
    }

    bb.toHtml(pageContext);


  %></div>