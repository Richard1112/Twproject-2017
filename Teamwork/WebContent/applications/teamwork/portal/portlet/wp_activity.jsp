<%@ page import="com.twproject.agenda.Event, com.twproject.document.TeamworkDocument, com.twproject.forum.TeamworkForumEntry, com.twproject.meeting.Meeting, com.twproject.messaging.board.Board, com.twproject.operator.TeamworkOperator, com.twproject.rank.EntityGroupRank, com.twproject.rank.RankUtilities, com.twproject.resource.Resource, com.twproject.task.Issue, com.twproject.task.Task, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.DateField, org.jblooming.waf.html.input.RadioButton, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date, java.util.List, java.util.Map" %>


<%

  PageState pageState = PageState.getCurrentPageState(request);

  //Rank.opHits
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


  boolean orderByStat = "HIT_ORDER_BY_STAT".equals(pageState.getEntryOrDefault("HIT_ORDER_BY", "HIT_ORDER_BY_STAT").stringValueNullIfEmpty());

  double relativeMaxW = 100;
  List<EntityGroupRank> groupRanks = null;

  Date date =pageState.getEntry("HIT_FILTER_BEFORE").dateValueNoErrorNoCatchedExc();
  if (date==null)
    date=new Date();

  if (orderByStat) {
    groupRanks = RankUtilities.getEntityRankStatistically(logged.getIntId(),date);
    if (groupRanks.size()>0)
      relativeMaxW = groupRanks.get(0).weight;
  } else {
    groupRanks = RankUtilities.getEntityRankByFreshness(logged.getIntId(),date);
  }

  Map<String, List<EntityGroupRank>> grouped = RankUtilities.splitByClassName(groupRanks);

  boolean something= JSP.ex(grouped.get(Task.class.getName())) || JSP.ex(grouped.get(Resource.class.getName())) || JSP.ex(grouped.get(Issue.class.getName()));

  if (something){
    PageSeed ps = pageState.pagePart(request);
    Form f = new Form(ps);
    f.start(pageContext);

%>
<style type="text/css">
  #configActivity div {
    display: inline-block;
    width:100px;
  }
</style>
<div class="activity portletBox small">

  <div style="float:right; padding-top: 5px"><%
    ButtonJS bs = new ButtonJS();
    bs.onClickScript = "$('#configActivity').toggle()";
    bs.iconChar="g";
    bs.label="";
    bs.additionalCssClass="ruzzol";
    bs.toolTip=I18n.get("FILTER");
    bs.toHtmlInTextOnlyModality(pageContext);
    %></div><%


  String bsa = ButtonSubmit.getAjaxButton(f, "myActExDiv").generateJs().toString();


%><h1><%=I18n.get("GROUP_RANK_POPULAR_ENTITIES")+(JSP.ex(pageState.getEntry("HIT_FILTER_BEFORE"))?"&nbsp;"+I18n.get("GROUP_RANK_AT")+":"+JSP.w(date):"")%></h1>
  <div class="portletParams" id="configActivity" style="display:none;">

    <%
        DateField df = new DateField("HIT_FILTER_BEFORE");
        df.separator="<br>";
        df.onblurOnDateValid=bsa;
        df.toHtmlI18n(pageContext);
      //inhibit submit
  %><input type="text" style="display:none;">
    <p><%

        RadioButton rbDate = new RadioButton("HIT_ORDER_BY_DATE", "HIT_ORDER_BY", "HIT_ORDER_BY_DATE", "", null, false, null);
        rbDate.script=bsa;
        rbDate.toHtmlI18n(pageContext);
      %>&nbsp;&nbsp;&nbsp;<%
        RadioButton rbStats = new RadioButton("HIT_ORDER_BY_STAT", "HIT_ORDER_BY", "HIT_ORDER_BY_STAT", "", null, false, null);
        rbStats.script=bsa;
        rbStats.toHtmlI18n(pageContext);
      %></p>
    <div><%


      // task
      boolean showTasks = pageState.getEntryOrDefault("HIT_SHOW_TASKS", Fields.TRUE).checkFieldValue();
      CheckField cfTask = new CheckField("HIT_SHOW_TASKS","",false);
      cfTask.additionalOnclickScript=bsa;
      cfTask.toHtmlI18n(pageContext);
      %></div><div><%

      //issue
      boolean showIssues = pageState.getEntryOrDefault("HIT_SHOW_ISSUES", Fields.TRUE).checkFieldValue();
      CheckField cfIssues = new CheckField("HIT_SHOW_ISSUES","",false);
      cfIssues.additionalOnclickScript=bsa;
      cfIssues.toHtmlI18n(pageContext);
      %></div><div><%

      //resource
      boolean showResources = pageState.getEntryOrDefault("HIT_SHOW_RESOURCES", Fields.TRUE).checkFieldValue();
      CheckField cfResources = new CheckField("HIT_SHOW_RESOURCES","",false);
      cfResources.additionalOnclickScript=bsa;
      cfResources.toHtmlI18n(pageContext);
  %></div><div><%

      //resource
      boolean showDocuments = pageState.getEntryOrDefault("HIT_SHOW_DOCUMENTS", Fields.TRUE).checkFieldValue();
      CheckField cfDocs = new CheckField("HIT_SHOW_DOCUMENTS","",false);
      cfDocs.additionalOnclickScript=bsa;
      cfDocs.toHtmlI18n(pageContext);
      %></div><div><%

      //TeamworkForumEntry
      boolean showForum = pageState.getEntryOrDefault("HIT_SHOW_FORUM", Fields.TRUE).checkFieldValue();
      CheckField cfForum = new CheckField("HIT_SHOW_FORUM","",false);
      cfForum.additionalOnclickScript=bsa;
      cfForum.toHtmlI18n(pageContext);
  %></div><div><%

      //Event
      boolean showEvent = pageState.getEntryOrDefault("HIT_SHOW_EVENT", Fields.TRUE).checkFieldValue();
      CheckField cfEvent = new CheckField("HIT_SHOW_EVENT","",false);
      cfEvent.additionalOnclickScript=bsa;
      cfEvent.toHtmlI18n(pageContext);
  %></div><div><%

      //Board
      boolean showBoard = pageState.getEntryOrDefault("HIT_SHOW_BOARD", Fields.TRUE).checkFieldValue();
      CheckField cfBoard = new CheckField("HIT_SHOW_BOARD","",false);
      cfBoard.additionalOnclickScript=bsa;
      cfBoard.toHtmlI18n(pageContext);
      %></div><div><%

      //Meeting
      boolean showMeeting = pageState.getEntryOrDefault("HIT_SHOW_MEETING", Fields.TRUE).checkFieldValue();
      CheckField cfMeeting = new CheckField("HIT_SHOW_MEETING","",false);
      cfMeeting.additionalOnclickScript=bsa;
      cfMeeting.toHtmlI18n(pageContext);
      
      %></div></div><%


  JspHelper drawer = new JspHelper("parts/partActivityLine.jsp");
  drawer.parameters.put("relativeMaxW", relativeMaxW);

  boolean shownSomething = false;
  List<EntityGroupRank> egrs = grouped.get(Task.class.getName());
  if (showTasks && JSP.ex(egrs)) {
    shownSomething = true;
    drawer.parameters.put("label", I18n.get("TASKS"));
    drawer.parameters.put("egrs", egrs);
    drawer.toHtml(pageContext);
  }

  egrs = grouped.get(Issue.class.getName());
  if (showIssues && JSP.ex(egrs)) {
    shownSomething = true;
    drawer.parameters.put("label", I18n.get("ISSUES"));
    drawer.parameters.put("egrs", egrs);
    drawer.toHtml(pageContext);
  }

  egrs = grouped.get(Resource.class.getName());
  if (showResources && JSP.ex(egrs)) {
    shownSomething = true;
    drawer.parameters.put("label", I18n.get("RESOURCES"));
    drawer.parameters.put("egrs", egrs);
    drawer.toHtml(pageContext);
  }

  egrs = grouped.get(TeamworkDocument.class.getName());
  if (showDocuments && JSP.ex(egrs)) {
    shownSomething = true;
    drawer.parameters.put("label", I18n.get("DOCUMENTS"));
    drawer.parameters.put("egrs", egrs);
    drawer.toHtml(pageContext);
  }

  egrs = grouped.get(TeamworkForumEntry.class.getName());
  if (showForum && JSP.ex(egrs)) {
    shownSomething = true;
    drawer.parameters.put("label", I18n.get("TASK_DIARY"));
    drawer.parameters.put("egrs", egrs);
    drawer.toHtml(pageContext);
  }

  egrs = grouped.get(Event.class.getName());
  if (showEvent && JSP.ex(egrs)) {
    shownSomething = true;
    drawer.parameters.put("label", I18n.get("AGENDA"));
    drawer.parameters.put("egrs", egrs);
    drawer.toHtml(pageContext);
  }

  egrs = grouped.get(Board.class.getName());
  if (showBoard && JSP.ex(egrs)) {
    shownSomething = true;
    drawer.parameters.put("label", I18n.get("BOARDS"));
    drawer.parameters.put("egrs", egrs);
    drawer.toHtml(pageContext);
  }

  egrs = grouped.get(Meeting.class.getName());
  if (showMeeting && JSP.ex(egrs)) {
    shownSomething = true;
    drawer.parameters.put("label", I18n.get("MEETINGS"));
    drawer.parameters.put("egrs", egrs);
    drawer.toHtml(pageContext);
  }

  if (!shownSomething) {
    %><em><%=I18n.get("NO_ACTIVITY_YET")%></em><%
  }

  %></div><%
    f.end(pageContext);
  }
%>
