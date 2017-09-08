<%@ page import="org.jblooming.persistence.PersistenceHome,
                 org.jblooming.scheduler.Job,
                 org.jblooming.scheduler.Scheduler,
                 org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.SmileyUtilities,
                 org.jblooming.waf.ScreenBasic,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.exceptions.ActionException,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.html.table.ListHeader,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 java.io.Serializable,
                 java.text.ParseException,
                 java.util.Date,
                 java.util.Map,
                 java.util.TreeSet"
  %><%

  PageState pageState = PageState.getCurrentPageState(request);
  pageState.getLoggedOperator().testIsAdministrator();

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;

    if (Commands.START.equals(pageState.getCommand())) {

      try {
        long tick = pageState.getEntryAndSetRequired("TICK").intValue();
        Scheduler.instantiate(tick*1000, pageState.getLoggedOperator());
      } catch (ActionException e) {
      } catch (ParseException e) {
      }

    } else if (Commands.STOP.equals(pageState.getCommand())) {
      Scheduler.getInstance().stop();
    } else if ("UPDATE".equals(pageState.getCommand())) {
      Scheduler instance = Scheduler.getInstance();
      if (instance!=null)
        instance.fillFromPersistence();
    }

    //make default 5 sec.
    pageState.addClientEntry("TICK",5);

    ScreenBasic.preparePage(null, pageContext);
    pageState.perform(request, response).toHtml(pageContext);

  } else {
    Form f = new Form(pageState.thisPage(request));
    f.url.addClientEntry(Fields.APPLICATION_NAME, pageState.getEntry(Fields.APPLICATION_NAME).stringValueNullIfEmpty());

  f.start(pageContext);

  %><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>
<%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
%>
<%adminLink.toHtmlInTextOnlyModality(pageContext);%>
<h1><%=I18n.get("SCHEDULER_MANAGER")%></h1>
<div class="box">
<table class="table"><tr><%

  if (Scheduler.getInstance() != null) {

      Date it = Scheduler.getInstance().instantiationTime;
      %>
        <td><h2><%=SmileyUtilities.getTextWithSmileys(I18n.get("SCHEDULER_IS_RUNNING"),pageContext)%></h2></td>
        <td><%=I18n.get("SCHEDULER_INSTANTIATED_BY")%>&nbsp;<%=Scheduler.getInstance().instantiator%><br> <%=I18n.get("SCHEDULER_INSTANTIATED_AT")%>&nbsp;<%=DateUtilities.dateAndHourToString(it)%></td><%
    } else {
      %><td><h2><%=SmileyUtilities.getTextWithSmileys(I18n.get("SCHEDULER_IS_NOT_RUNNING"),pageContext)%></h2></td><%
    }


%><td><%
    TextField tf = new TextField("TICK", "&nbsp;");
    tf.type="hidden";
    tf.label = I18n.get("TICK_IN_SECONDS");
    tf.fieldSize = 4;
    tf.disabled = Scheduler.isRunning();
    tf.toHtml(pageContext);
%></td><td align="center" width="100"><%

if (!Scheduler.isRunning()) {

  ButtonSubmit start = new ButtonSubmit(f);
  start.variationsFromForm.setCommand(Commands.START);
  start.label = "";
  start.toolTip = I18n.get("START");
  start.iconChar = "a";
  start.style = "font-size:180%";
  start.additionalCssClass = "icon controls play";
  start.enabled = !Scheduler.isRunning();
  start.toHtmlInTextOnlyModality(pageContext);

  } else {

  ButtonSubmit stop = new ButtonSubmit(f);
  stop.variationsFromForm.setCommand(Commands.STOP);
  stop.label = "";
  stop.toolTip = I18n.get("STOP");
  stop.iconChar = "s";
  stop.style = "font-size:180%";
  stop.enabled = Scheduler.isRunning();
  stop.additionalCssClass = "icon controls stop";
  stop.toHtmlInTextOnlyModality(pageContext);
  }




%></td><td align="right" width="140"><%

    ButtonSubmit upd = new ButtonSubmit(f);
    upd.variationsFromForm.setCommand("UPDATE");
    upd.label = I18n.get("UPDATE");
    upd.iconChar = "h";
    upd.toHtmlInTextOnlyModality(pageContext);

  %></td></tr></table></div><%


  Scheduler scheduler = Scheduler.getInstance();


  %><h2>jobs recently launched</h2><%

  Map<Serializable, Scheduler.FutureIsPink> jf = null;
  if (scheduler != null)
    jf = scheduler.inExecution;


    %>
    <table class="table"><tr><%
      ListHeader lh = new ListHeader("JOBS", f);
      lh.addHeaderFitAndCentered("id");
      lh.addHeader(I18n.get("NAME"));
      lh.addHeader("last run time");
      lh.toHtml(pageContext);
    %></tr><%

      if (scheduler != null && jf.keySet().size() > 0) {

  for (Serializable jobId : jf.keySet()) {

    Job job = (Job) PersistenceHome.findByPrimaryKey(Job.class, (Integer) jobId);
    %><tr class="alternate" >
      <td><%=job.getId()%></td>
      <td><%=job.getName()%></td>
      <td><%=JSP.timeStamp(new Date(job.getLastExecutionTime()))%></td>
    </tr><%

  }

    } else {
    %><tr><td colspan="3"><div class="paginatorNotFound hint"><%=I18n.get("NO_JOBS_RUNNING")%></div></td></tr><%
      }
    %></table>

<h2><%=I18n.get("JOBS_TO_BE_EXECUTED")%></h2><%




  TreeSet<Scheduler.OrderedJob> ojs = null;
  if (scheduler != null)
    ojs = scheduler.toBeExecuted;


%>
<table class="table"><tr><%
  lh = new ListHeader("JOBS", f);
  lh.addHeader(I18n.get("NAME"));
  lh.addHeader("next run time");
  lh.addHeader("last run time");
  lh.addHeader(I18n.get("RUN_NOW"));
  lh.addHeaderFitAndCentered("<span class=\"lreq30 lreqLabel\">"+I18n.get("DELETE_SHORT")+"</span>");
  lh.toHtml(pageContext);
%></tr><%

  if (scheduler != null && ojs != null && ojs.size() > 0) {


    PageSeed pi = new PageSeed("jobEditor.jsp");
  pi.setCommand(Commands.EDIT);


  PageSeed pdel = new PageSeed("jobEditor.jsp");
  pdel.setCommand(Commands.DELETE_PREVIEW);

  for (Scheduler.OrderedJob oj : ojs) {
    Job job = (Job) PersistenceHome.findByPrimaryKey(Job.class, oj.jobId);
    if (job != null) {
      pi.setMainObjectId(job.getId());
      pdel.setMainObjectId(job.getId());


      ButtonLink bl = new ButtonLink(pi);
      bl.label=job.getName();

      ButtonLink delLink = new ButtonLink(pdel);
      delLink.iconChar="d";
      delLink.label="";
      delLink.additionalCssClass="delete";

      PageSeed edit = pageState.pageInThisFolder("jobEditor.jsp", request);
      edit.mainObjectId = job.getId();
      edit.setCommand("RUN_NOW");
      ButtonLink runNowLink = new ButtonLink(edit);
      runNowLink.label = "";//I18n.get("RUN_NOW");
      runNowLink.iconChar="z";


%> <tr class="alternate" >

  <td><%bl.toHtmlInTextOnlyModality(pageContext);%></td>
  <td><%=DateUtilities.dateAndHourToString(oj.exeTimeDate)%></td>
  <td nowrap><%=job.getLastExecutionTime()>0 ? DateUtilities.dateToRelative(new Date(job.getLastExecutionTime())) : "never run"%></td>
  <td align="center"><%runNowLink.toHtmlInTextOnlyModality(pageContext);%></td>
  <td align="center" class="lreq30"><%delLink.toHtmlInTextOnlyModality(pageContext);%></td>

</tr><%
    }
  }

} else {
%><tr><td colspan="5"><div class="paginatorNotFound hint"><%=I18n.get("NO_JOBS_TO_BE_EXECUTED")%></div></td></tr><%
  }
%></table>
<%



  ButtonBar bb2 = new ButtonBar();

  PageSeed ps = new PageSeed("jobList.jsp");
  ps.addClientEntry(Fields.APPLICATION_NAME, pageState.getEntry(Fields.APPLICATION_NAME).stringValueNullIfEmpty());
  ButtonLink bll = new ButtonLink(I18n.get("JOB_LIST"), ps);

  bb2.addButton(bll);
  bb2.toHtml(pageContext);

  f.end(pageContext);

  }
%>