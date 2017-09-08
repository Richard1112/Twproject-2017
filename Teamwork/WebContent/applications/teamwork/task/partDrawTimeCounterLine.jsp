<%@ page
    import="com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.TaskBricks, com.twproject.waf.html.TimeCounterDrawer, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date, org.jblooming.waf.html.button.*" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  PageSeed editTask = pageState.pageFromRoot("/task/taskOverview.jsp");
  editTask.setCommand(Commands.EDIT);

  ButtonLink editTaskB = new ButtonLink("", editTask);

  TimeCounterDrawer taskIssueDrawer = (TimeCounterDrawer) JspIncluderSupport.getCurrentInstance(request);
  Assignment assignment = taskIssueDrawer.assig;
  Task task = assignment.getTask();

  PageSeed assList = pageState.pageFromRoot("/task/worklog/worklogAssignmentList.jsp");
  assList.command=Commands.FIND;
  assList.addClientEntry("ASS_ID", assignment.getId());

  editTask.setMainObjectId(task.getId());

%>
<tr class="alternate" assId="<%=assignment.getId()%>">


  <td class="columnTaskName"  style="position: relative;">
    <%if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")){
      %><div class="pathSmall"><%=task.getPath(" / ", false)%></div><%
    }

    editTaskB.label = task.getName();
      editTaskB.toHtmlInTextOnlyModality(pageContext);%>


  </td>

  <td class="textSmall columnTaskCode">
    <span><%=JSP.w(task.getCode())%></span>
  </td>
  <td class="textSmall" width="40"><%= assignment.getRole().getCode() %></td>
  <td align="left" width="30"><%TaskBricks.assignmentPriorityToHtml(15,System.currentTimeMillis(), assignment, pageContext);%></td>

  <td align="right" width="200" nowrap>
    <div class="timeCounter" style="display: none; margin-right: 20px; white-space: nowrap">00<span class="">h</span> 00<span class="counterLabel">m</span><span class="sec"> 00<span class="counterLabel">s</span></span></div>
    <%
      ButtonJS bs=new ButtonJS("pTCStartStop($(this));");
      bs.iconChar = assignment.isCounted() ? "s" : "a";
      bs.additionalCssClass = assignment.isCounted() ? "controls stop":"controls play";
      bs.label = "";
      bs.toHtmlInTextOnlyModality(pageContext);

    %></td>
</tr>
