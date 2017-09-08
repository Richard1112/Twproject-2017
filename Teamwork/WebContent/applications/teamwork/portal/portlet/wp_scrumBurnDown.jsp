<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, com.twproject.task.Task, com.twproject.task.TaskBricks, org.jblooming.ontology.Identifiable, org.jblooming.utilities.JSP, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, com.twproject.waf.html.StatusIcon, org.jblooming.waf.html.display.PercentileDisplay"%>
<div id="wpScrumBDDiv"><%

  PageState pageState = PageState.getCurrentPageState(request);


  String burnDownTaskId =pageState.getEntryOrDefault("BURNDOWN_TASK").stringValueNullIfEmpty();
  Task task =null;
  if (JSP.ex(burnDownTaskId))
    task =Task.load(burnDownTaskId);


  PageSeed self = pageState.pagePart(request);
  Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);

  TeamworkOperator logged=(TeamworkOperator) pageState.getLoggedOperator();


  String bsa = ButtonSubmit.getAjaxButton(f, "wpScrumBDDiv").generateJs().toString();
  %> <div class="portletBox">

  <div style="float:right; padding-top: 5px"><%
  ButtonJS bs = new ButtonJS();
  bs.onClickScript = "$('#burnDownParams').toggle()";
  bs.iconChar="g";
  bs.label="";
  bs.toolTip= I18n.get("FILTER");
  bs.toHtmlInTextOnlyModality(pageContext);
  %></div><%
  String paramBox="none";
  
  if (task==null){
    paramBox="block";
    %><h1><%=I18n.get("SELECT_YOUR_PANIC_TASK")%></h1><%
  } else {
    PercentileDisplay progressBar = task.bricks.getProgressBar(false);
    progressBar.width="100px";
    progressBar.height="15px";
    StatusIcon statusIcon = task.bricks.getStatusIcon(10, pageState);
  %>
  <h1 style="padding-bottom: 10px"><%=task.getName()%></h1>
  <div style="width:100px;float:left;margin-right:20px"><%progressBar.toHtml(pageContext);%></div>
  <div class="pathCodeWrapper"><%statusIcon.toHtml(pageContext);%><span class='pathCode' title='<%=I18n.get("REFERENCE_CODE")%>'><%=(task.isNew() ? "" : "T#" + task.getMnemonicCode())%>#</span></div>
  <%
  }

  %><div id="burnDownParams" class="portletParams" style="display:<%=paramBox%>"><%
    SmartCombo taskCombo = TaskBricks.getTaskCombo("BURNDOWN_TASK", true, TeamworkPermissions.task_canRead, pageState);
    taskCombo.separator="&nbsp;";
    taskCombo.label=I18n.get("SELECT_YOUR_PANIC_TASK");
    taskCombo.onValueSelectedScript=bsa;
    taskCombo.toHtml(pageContext);
  %></div><%

  if (task!=null){

    Identifiable oldOb= pageState.getMainObject();
    pageState.setMainObject(task);
    %><jsp:include page="../../issue/partIssueGraphs.jsp" /><%
    pageState.setMainObject(oldOb);
  }


  f.end(pageContext);
%></div></div>





