<%@ page contentType="text/html;charset=UTF-8" language="java" %><%@ page import="com.twproject.task.Task,
                 com.twproject.task.businessLogic.TaskController, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.exceptions.PersistenceException,
                 org.jblooming.system.SystemConstants, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.DateField,
                 org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date, java.util.List" %>
<%!
  void drawTaskRecursively(Task task, Task father, int rowId, int depth, boolean showTotalDone, Date from, Date to, boolean root, boolean assig, PageState pageState, PageContext pageContext) throws PersistenceException {
    JspHelper j = new JspHelper("/applications/teamwork/task/multiline/partTaskPrintLine.jsp");
    rowId++;
    //j.parameters.put("depth",depth);
    if (father != null)
      j.parameters.put("father", father);
    if (from != null)
      j.parameters.put("from", from);
    if (to != null)
      j.parameters.put("to", to);
    
    j.parameters.put("task", task);
    j.parameters.put("rowId", rowId);
    j.parameters.put("showTotal", showTotalDone);
    j.parameters.put("root", root);
    j.parameters.put("assig", assig);

    j.toHtml(pageContext);
    if (depth == 0 || (depth != 0 && depth > rowId)){
      if (task.getChildrenSize() > 0 && !root) {
        for (Task childTask : task.getChildrenSorted()) {
          drawTaskRecursively(childTask, task, rowId, depth, showTotalDone, from, to, false, assig,pageState, pageContext);
        }
      }
    }
  }
%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  SessionState sessionState = pageState.sessionState;


  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(new TaskController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    PageSeed self = pageState.thisPage(request);
    self.setCommand("PRINT");

    // I need to preserve the damn filter
    int depth = pageState.getEntryOrDefault("DEPTH").intValueNoErrorCodeNoExc();
    Date from = pageState.getEntryOrDefault("FROM_DATE").dateValueNoErrorNoCatchedExc();
    Date to = pageState.getEntryOrDefault("TO_DATE").dateValueNoErrorNoCatchedExc();
    boolean root = pageState.getEntryOrDefault("GROUP_BY_ROOT").checkFieldValue();
    boolean showTotal = pageState.getEntryOrDefault("SHOW_TOTAL_DONE").checkFieldValue();
    boolean assig = pageState.getEntryOrDefault("SHOW_ASSIG").checkFieldValue();
    Form f = new Form(self);
    f.addKeyPressControl(13, "this.submit();", "onkeyup");
    pageState.setForm(f);

    f.start(pageContext);

    if (Commands.FIND.equals(pageState.getCommand())) {
      List<Object[]> tasksSched = pageState.getPage().getAllElements();
      sessionState.setAttribute("TASK_PRINT_PAGE", tasksSched);
    }


    List<Object[]> tasks = (List) sessionState.getAttribute("TASK_PRINT_PAGE");
    Img logo = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");
%>
<table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" class="noprint">
  <tr>
    <td align="left" width="100%"><%logo.toHtml(pageContext);%></td>
    <td align="right"><%

      ButtonJS print = new ButtonJS("window.print();");
      print.label = "";
      print.toolTip = I18n.get("PRINT_PAGE");
      print.iconChar = "p";
      print.toHtmlInTextOnlyModality(pageContext);

    %></td>
  </tr>
</table>

<div id="printFilter" style="visibility:visible;" class="noprint">
  <%
        //filters

        DateField df = new DateField("FROM_DATE");
        df.labelstr =  I18n.get("STATUS_FROM");
        df.setSearchField(true);
        df.size = 10;
        df.separator = "</td><td>";

        DateField df2 = new DateField("TO_DATE");
        df2.labelstr = I18n.get("STATUS_TO");
        df2.setSearchField(true);
        df2.size = 10;
        df2.separator = "</td><td>";

        TextField tf = TextField.getIntegerInstance("DEPTH");
        tf.label =I18n.get("DEPTH");
        tf.fieldSize = 4;
        tf.searchField = true;

        CheckField showTotalDone = new CheckField("SHOW_TOTAL_DONE", "&nbsp;", false);
        showTotalDone.label = I18n.get("WORKLOG_DONE");

        CheckField groupByRoot = new CheckField("GROUP_BY_ROOT", "&nbsp;", false);
        groupByRoot.label = I18n.get("GROUP_BY_ROOT");
        CheckField showAssig = new CheckField("SHOW_ASSIG", "&nbsp;", false);
        showAssig.label = I18n.get("DO_NOT_SHOW_ASSIG");
      %>
        <table width="100%">
          <tr>
            <td><%df.toHtml(pageContext);%></td>
            <td><%df2.toHtml(pageContext);%></td>
            <td><%tf.toHtml(pageContext);%></td>
            <td><%showTotalDone.toHtml(pageContext);%></td>
            <td><%groupByRoot.toHtml(pageContext);%></td>
            <td><%showAssig.toHtml(pageContext);%></td>
            <td><%
              ButtonSubmit sc = new ButtonSubmit(f);
              sc.label = I18n.get("REFRESH");
              sc.additionalCssClass="small";
              sc.toHtml(pageContext);
            %></td>
          </tr>
        </table>

</div>
<table border="0" align="center" cellpadding="5" cellspacing="0"  class="table dataTable edges">
  <tr>
  <th class="tableHead"><%=I18n.get("TASK_CODE")%> <%=I18n.get("NAME_DESCRIPTION")%></th>
  <%if (showTotal){%>
  <th class="tableHead fixedWidthAlignRight"><%=I18n.get("WORKLOG_DONE")%></th>
  <%}%>
  <th class="tableHead fixedWidthAlignRight"><%=I18n.get("WORKLOG_ESTIMATED")%></th>
  <th class="tableHead fixedWidthAlignRight"><%=I18n.get("TOTAL_WORKLOG_ESTIMATED")%></th>
</tr>
  <%
    int rowId = 0;
    for (Object[] t : tasks) {
      Task ntask = (Task) PersistenceHome.findByPrimaryKey(Task.class, ((Task) t[0]).getId());
       if(!root || (root && ntask.getParent() == null)){
        drawTaskRecursively(ntask, ntask.getParent(), rowId, depth, showTotal, from, to, root, assig,pageState, pageContext);
      }
    } %>
</table>
<br>
<p align="right"><i><%=I18n.get("PRINTED_ON", JSP.timeStamp(new Date()))%>
</i></p>
<%
    f.end(pageContext);
  }
%>