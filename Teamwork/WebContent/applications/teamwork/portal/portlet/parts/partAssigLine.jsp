<%@ page import="com.twproject.resource.Resource, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.Img, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jbpm.graph.exe.ProcessInstance, org.jbpm.taskmgmt.exe.TaskInstance, java.util.Collection, java.util.List, java.util.Map, com.twproject.task.*, org.jblooming.ontology.Pair, org.jblooming.waf.html.layout.HtmlColors, org.jblooming.waf.html.display.PercentileDisplay" %>
<%
  JspHelper drawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  Assignment ass = (Assignment) drawer.parameters.get("assignment");
  PageState pageState = PageState.getCurrentPageState(request);

  Task task = ass.getTask();

  PageSeed editTask = pageState.pageFromRoot("/task/taskOverview.jsp");
  editTask.setCommand(Commands.EDIT);
  ButtonLink editTaskB = new ButtonLink("", editTask);
  editTask.setMainObjectId(task.getId());
  editTaskB.label+="<span class='taskName' style=\"word-break: break-all;\">"+JSP.w(task.getName())+"</span>";

  if (JSP.ex(ass.getExternalCode()) && task.isProcessDriven()) {
    ProcessInstance instance = task.getProcess();
    if (!instance.hasEnded()) {
      Collection<TaskInstance> instances = instance.getTaskMgmtInstance().getTaskInstances();
      for (TaskInstance ti : instances) {
        if (!ti.hasEnded() && (ti.getTask().getId() + "").equals(ass.getExternalCode())) {
          editTaskB.label+= "&nbsp;&nbsp;<br><span style='color:#df511d; font-size: 80%'>" + I18n.get("STEP_YOUR_ACTION_IS_REQUIRED") + "</span>";
          break;
        }
      }
    }
  }

  editTaskB.toolTip = ass.getDisplayNameWithTask();
  editTaskB.additionalCssClass = "bolder";

  AssignmentPriority assPrio = ass.getAssignmentPriorityAtTime(System.currentTimeMillis());

  PercentileDisplay progressBar = task.bricks.getProgressBar(true);
  progressBar.width="100px";
  progressBar.height="15px";

  String color=task.getTaskColor();
  String styleColor="";
  if (I18n.isActive("CUSTOM_FEATURE_USE_PROJECT_COLOR") && JSP.ex(color)){
    styleColor=" style=\"border-left: 5px solid "+color+";\"";
  }

%>
<tr assId="<%=ass.getId()%>" class="assigRow alternate <%=task.getSchedule()!=null&&task.getSchedule().getEndDate().getTime()<System.currentTimeMillis()?"expired":""%>">
  <td class="columnTaskName" <%=styleColor%>>
    <%if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")){
    %><div class="pathSmall"><%=task.getPath(" / ", false)%></div><%
    }%>

    <%editTaskB.toHtmlInTextOnlyModality(pageContext);%>
  </td>
  <td class="textSmall columnTaskCode" title="<%=JSP.w(task.getCode())%>"><span><%=JSP.w(task.getCode())%></span></td>
  <td title="<%=JSP.w(ass.getRole().getDisplayName())%>"><%=JSP.w(ass.getRole().getCode())%></td>
  <td align="left" class="lreq20"><div class="cvcColorSquare priorityLabel" style="background-color: <%=AssignmentPriority.getPriorityColor(assPrio==null?-1:assPrio.getPriority())%>;" title="<%=AssignmentPriority.getPriorityDescription(assPrio==null?-1:assPrio.getPriority())%>"><%=AssignmentPriority.getPriorityDescription(assPrio==null?-1:assPrio.getPriority())%></div></td>
  <td align="right"><%=task.getTotalIssuesOpen() + "/" + task.getTotalIssues()%>&nbsp;<%

    PageSeed issues = pageState.pageFromRoot("issue/issueList.jsp");
    issues.addClientEntry("FLT_ISSUE_TASK", task.getId());
    IssueBricks.addOpenStatusFilter(issues);
    issues.command = Commands.FIND;
    ButtonLink is = new ButtonLink("", issues);
    is.toHtmlInTextOnlyModality(pageContext);
    %>
</td><td align="right"><span class="wlg"><%=DateUtilities.getMillisInHoursMinutes(ass.getWorklogDone())%></span> <%
  ButtonJS bjs=new ButtonJS("openWorklogEditorPopup($(this),{assId:'"+ass.getId()+"',title:'"+JSP.javascriptEncode(JSP.limWr(task.getDisplayName(),30))+"'});");
  bjs.toolTip=I18n.get("ADD_WORKLOG");
  bjs.iconChar="w";
  bjs.toHtmlInTextOnlyModality(pageContext);

  %></td><td class="lreq20 lreqHide"><%
  ButtonJS bjsc=new ButtonJS("openExpenseEditorPopup($(this),{assId:'"+ass.getId()+"',title:'"+JSP.javascriptEncode(JSP.limWr(task.getDisplayName(),30))+"'});");
  bjsc.toolTip=I18n.get("ADD_EXPENSE");
  bjsc.iconChar="4";
  bjsc.toHtmlInTextOnlyModality(pageContext);

  %> </td>

  <td><div class="facesBox">
    <%
      List<Assignment> assignmentList = task.getAssignementsSortedByRole();
      int count=0;
      boolean listInterruptus=assignmentList.size()>4;

      PageSeed assigEdit = pageState.pageFromRoot("task/taskAssignmentEditor.jsp");
      assigEdit.command =Commands.EDIT;
      for (Assignment assig: assignmentList){
        assigEdit.addClientEntry("TASK_ID",task);
        assigEdit.addClientEntry("OBJID",assig);
        Resource res=assig.getResource();
    %><img src="<%=res.bricks.getAvatarImageUrl()%>" onclick="location.href='<%=assigEdit.toLinkToHref()%>';" title="<%=res.getDisplayName()+" ("+JSP.w(assig.getRole().getCode())+")"%>" class="face small" align="top" style="cursor:pointer;"><%
    count++;
    // add ....
    if (count>2 && listInterruptus){
      PageSeed ps = pageState.pageFromRoot("task/taskAssignmentList.jsp");
      ps.command = "LIST_ASSIGS";
      ps.addClientEntry("TASK_ID", task.getId());
      %><a href="<%=ps.toLinkToHref()%>" class="faceMore" title="<%=I18n.get("MORE")%>"><span class="teamworkIcon" style="line-height:25px; font-size:15px">&hellip;</span></a><%
        break;
      }
    }
  %>
  </div></td>
  <td nowrap class="progressBarTd" ><%progressBar.toHtml(pageContext);%></td>
  </tr>

<%

  //}
%>
