<%@ page import="com.twproject.task.Task,
                 com.twproject.task.TaskBricks,
                 com.twproject.task.TaskStatus,
                 com.twproject.waf.TeamworkPopUpScreen,
                 org.jblooming.agenda.CompanyCalendar,
                 org.jblooming.oql.QueryHelper,
                 org.jblooming.utilities.CollectionUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit,
org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.display.PercentileDisplay, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.SmartCombo,
org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.layout.HtmlColors, org.jblooming.waf.html.state.Form,
org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.*"%><%!


  public class AssignedTaskComparator implements Comparator<Task> {

    public List<Task> activeTasks;

    public int compare(Task t1, Task t2) {

      int result = 0;
      if (t1.getParent() != null && t1.getParent().equals(t2.getParent()))
        result = t1.getDisplayName().compareTo(t2.getDisplayName());
        //are they on the same hierarchy ?
      else if (t1.getAncestorIdsAsList().contains(t2.getId()) && !t2.getAncestorIdsAsList().contains(t1.getId()))
        result = 1;
      else if (t2.getAncestorIdsAsList().contains(t1.getId()) && !t1.getAncestorIdsAsList().contains(t2.getId()))
        result = -1;
      else if (t1.getParent() != null && activeTasks.contains(t1.getParent()) && !t2.getAncestorIdsAsList().contains(t1.getParent().getId()))
        result = compare(t1.getParent(), t2);
      else if (t2.getParent() != null && activeTasks.contains(t2.getParent()) && !t1.getAncestorIdsAsList().contains(t2.getParent().getId()))
        result = compare(t1, t2.getParent());
      else
        result = t1.getDisplayName().compareTo(t2.getDisplayName());

      return result;
    }

  }

%><%

  PageState pageState = PageState.getCurrentPageState(request);

  boolean isPrinting = Commands.PRINT.equals(pageState.command);

    if (isPrinting && !pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

  } else {

  PageSeed self = pageState.pagePart(request);
  Form form = new Form(self);
  pageState.setForm(form);
  form.start(pageContext);


  %><div id="wp_projSumm" style="vertical-align:top;" class="portletBox">
<div style="float:right; padding-top: 5px"><%

  if (!isPrinting) {
    ButtonJS bs = new ButtonJS();
    bs.onClickScript = "$('#configProjSumm').toggle()";
    bs.toolTip=I18n.get("FILTER");
    bs.label="";
    bs.iconChar="g";
    bs.additionalCssClass="ruzzol";
    bs.toHtmlInTextOnlyModality(pageContext);
  }

%>&nbsp;<%

  ButtonSupport pr;
      if (isPrinting){
        ButtonJS bs=new ButtonJS();
        bs.onClickScript="window.print();window.close();";
        pr=bs;
      } else {
        PageSeed printPs = pageState.pagePart(request);
        printPs.command = Commands.PRINT;
        pr=ButtonLink.getBlackInstance("", 600, 1000, printPs);
      }

      pr.iconChar="p";
      pr.toolTip=I18n.get("PRINT_TODO_LIST");
      pr.label="";
      pr.toHtmlInTextOnlyModality(pageContext);



  %></div><h1><%=I18n.get("PROJECTS_SUMMARY")%></h1><%

  CompanyCalendar cc = new CompanyCalendar(SessionState.getLocale());

  ButtonSubmit refresh = ButtonSubmit.getAjaxButton(form, "wp_projSumm");

  boolean showChildren = pageState.getEntryOrDefault("TASK_SHOW_CHILDREN", Fields.FALSE).checkFieldValue();
  pageState.getEntryOrDefault("OVERDUE_WARN","3");
  boolean showCosts = pageState.getEntryOrDefault("TASK_SHOW_COSTS", Fields.FALSE).checkFieldValue();
  int filter = 0;
  int err2 = 2;
  int warn = 3;

  err2 =   pageState.getEntryOrDefault("OVERDUE_ERR","1").intValue();
  warn =   pageState.getEntryOrDefault("OVERDUE_WARN","3").intValue();
  filter = pageState.getEntryOrDefault("TASK_TYPE_SUMM").intValueNoErrorCodeNoExc();

  if (!isPrinting) {
  %>  <div id="configProjSumm" class="portletParams" style="display:none;"> <table class="table"><tr><td><%

  TextField tf = TextField.getIntegerInstance("OVERDUE_WARN");
  tf.separator = "<br>";
  tf.fieldSize = 2;
  tf.script=" onBlur=\""+refresh.generateJs()+"\"";
  tf.toHtmlI18n(pageContext);

  %></td><td><%

  TextField err = TextField.getIntegerInstance("OVERDUE_ERR");
  err.separator = "<br>";
  err.fieldSize = 2;
  err.script=" onBlur=\""+refresh.generateJs()+"\"";
  err.toHtmlI18n(pageContext);

   %></td><td><%

  SmartCombo sc = TaskBricks.getTaskTypeCombo("TASK_TYPE_SUMM", pageState);
  sc.label = I18n.get("TASK_TYPE_SUMM");
  sc.separator = "<br>";
  sc.fieldSize = 15;
  sc.onBlurAdditionalScript=refresh.generateJs()+"";
  sc.toHtmlI18n(pageContext);

  %></td><td><%

  CheckField cf = new CheckField("TASK_SHOW_CHILDREN", "&nbsp;",false);
  cf.additionalOnclickScript=refresh.generateJs()+"";
  cf.toHtmlI18n(pageContext);
%></td><td><%

  CheckField cos = new CheckField("TASK_SHOW_COSTS", "&nbsp;",false);
  cos.additionalOnclickScript=refresh.generateJs()+"";
  cos.toHtmlI18n(pageContext);
  %></td><td><%

  PageSeed pageSeed = pageState.pageFromRoot("task/taskList.jsp");
  pageSeed.command = Commands.FIND;
  pageSeed.addClientEntry("STATUS",TaskStatus.STATUS_ACTIVE);
  if (JSP.ex(filter))
     pageSeed.addClientEntry("TYPE",filter);
  if (!showChildren)
     pageSeed.addClientEntry("ROOT_OR_STANDALONE",Fields.TRUE);

  ButtonLink tl = new ButtonLink(">> "+I18n.get("TO_TASK_FILTER"),pageSeed);
  tl.toHtmlInTextOnlyModality(pageContext);

  %><td></tr></table> </div><%
  }

  String hql = "select task from " + Task.class.getName() + " as task where task.status = :active ";

  QueryHelper qhelp = new QueryHelper(hql);
  qhelp.setParameter("active", TaskStatus.STATUS_ACTIVE);

  if (filter>0) {
    qhelp.addOQLClause("task.type.id=:typex", "typex", filter);
  }

  if (!showChildren)
    qhelp.addOQLClause("task.parent is null");

  //  security
  TaskBricks.addSecurityReadClauses(qhelp, pageState);

  List<Task> tks = qhelp.toHql().list();
  AssignedTaskComparator ctc = new AssignedTaskComparator();
  ctc.activeTasks = tks;
  Set<Task> tasks = new TreeSet<Task>(ctc);
  tasks.addAll(tks);


%><table class="table projectSummary" id="wp_ps_default" cellpadding="2" style="vertical-align:top;">
  <thead class="dataTableHead" >
  <tr>
      <th class="tableHead" id="wpPSputFilerHere"><%=I18n.get("TASK_TASK")%></th>
      <th class="tableHead"><%=I18n.get("TASK_CODE")%></th>
      <th class="tableHead"><%=I18n.get("RELEVANCE")%></th>
      <th class="tableHead"><%=I18n.get("END")%></th>
      <th class="tableHead"><%=I18n.get("TASK_REMAINING_SHORT")%></th>
      <th class="tableHead"><%=I18n.get("PROGRESS")%></th><%
      if (showCosts) {
        %><th class="tableHead"><%=I18n.get("COST_BUDGET")%></th><%
      }
      %>
    </tr></thead><%

    PageSeed editTask = pageState.pageFromRoot("/task/taskOverview.jsp");
    editTask.setCommand(Commands.EDIT);

   boolean swap = true;
   for (Task task : tasks) {

          //days missing
          String backColor = "#38d056";
          String rowClass = "";
          String dateClass = "";
          String daysMissing = I18n.get("UNSPECIFIED");
          if (task.getSchedule()!=null) {
          //long timeLapse = (task.getSchedule().getValidityEndTime() - cc.getTime().getTime());

          int dayLapse = CompanyCalendar.getDistanceInWorkingDays(cc.getTime(),task.getSchedule().getEndDate());
          //if ( timeLapse < CompanyCalendar.MILLIS_IN_DAY*err2) {
          if ( dayLapse<err2) {
            backColor = "#D30202";
            rowClass = "expired";
            dateClass = "warning warningIcon";
          //} else if (timeLapse <CompanyCalendar.MILLIS_IN_DAY*warn){
          } else if (dayLapse<warn){
            backColor = "#FF712E";
          }


          if (task.getSchedule() != null && task.getSchedule().getEndDate()!=null) {

            daysMissing = dayLapse+"";
             /* if (task.getSchedule().getValidityEndTime() > System.currentTimeMillis()) {
                daysMissing = dayLapse+"";
              } else
                daysMissing = I18n.get("OVERDUE");*/
            }
          }
          editTask.setMainObjectId(task.getId());
          ButtonLink editTaskB = new ButtonLink("", editTask);
          editTaskB.additionalCssClass="bolder";
          editTaskB.noPrint=false;
          editTaskB.label=JSP.w(task.getName());
  %>
  <style type="text/css">
    .expired .columnTaskName .button.textual {
      color: #D30202;
    }
  </style>

  <tr class="alternate <%=rowClass%>" taskId="<%=task.getId()%>">
    <td class="columnTaskName"><%editTaskB.toHtmlInTextOnlyModality(pageContext);%></td>
    <td class="textSmall columnTaskCode" title="<%=JSP.w(task.getCode())%>"><span><%=JSP.w(task.getCode())%></span></td>
    <td align="center" class="textSmall"><%=task.getRelevance()%>%</td>
    <td align="right" class="textSmall <%=dateClass%>"><%=task.getSchedule() != null && task.getSchedule().getEndDate()!=null ? JSP.w(task.getSchedule().getEndDate()) : "&nbsp;-&nbsp;"%></td>
    <td align="center" nowrap><span style="color:<%=backColor%>; "><%=daysMissing%></span></div></td>
    <td width="120"><%

      PercentileDisplay pd = task.bricks.getProgressBar();
      pd.width="100px";
      pd.height="15px";
      pd.toHtml(pageContext);

    %></td><%

    if (showCosts) {
  %>

    <td align="left"><%
      if (task.getForecasted()>0) {
        pd = task.bricks.getBudgetBarForTask();
        pd.toHtml(pageContext);
      } else {
    %><small>(<%=I18n.get("COMPUTED_COST")%>:<%=JSP.currency(task.getTotalCostsDone())%>; <%=I18n.get("NO_BUDGET")%>)</small><%
      }

    %></td><%

    }

  %> </tr>
  <%
  }
  %></table>
   </div>

<script>
  //inject the table search
  createTableFilterElement($("#wpPSputFilerHere"),'#wp_ps_default [taskId]','.columnTaskName,.columnTaskCode');

</script>


<%

  form.end(pageContext);
  }
%>
