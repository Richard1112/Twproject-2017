<%@ page import="com.twproject.resource.Company,com.twproject.resource.Person,com.twproject.resource.Resource,com.twproject.task.Issue,
                 com.twproject.task.TaskStatus,net.sf.json.JSONArray, org.hibernate.Query, org.jblooming.agenda.CompanyCalendar, org.jblooming.oql.OqlQuery,
                 org.jblooming.oql.QueryHelper, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.io.Serializable, java.util.ArrayList, java.util.Date, java.util.List"%>
<%

  PageState pageState = PageState.getCurrentPageState(request);

%><style>
  .myIssue .columnTaskName {
    width: 10%;
  }
</style>
<div class="portletBox myIssue"><%

  PageSeed ps = new PageSeed(request.getContextPath() + "/applications/teamwork/issue/issueList.jsp");
  ps.command= Commands.FIND;
  ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_OPEN_ISSUES");

  ButtonLink sub = new ButtonLink(ps);
  sub.iconChar="i";
  sub.label="";


  Person resource = Person.getLoggedPerson(pageState);

  ButtonJS bs = new ButtonJS();
  bs.onClickScript = "$('#myIssues').toggle()";
  bs.iconChar="g";
  bs.label="";
  bs.additionalCssClass="ruzzol";
  bs.toolTip=I18n.get("FILTER");


  %><div style="float:right;padding-top: 5px">
  <%//sub.toHtmlInTextOnlyModality(pageContext);%>&nbsp;<%bs.toHtmlInTextOnlyModality(pageContext);%>
  </div>
  <%
    sub.label=I18n.get("MYISSUES");
    sub.iconChar="";
  %>
  <h1><%sub.toHtmlInTextOnlyModality(pageContext);%></h1>



  <div id="myIssues" class="portletParams" style="display:none"><%

  int maxIssuesToShow = pageState.getEntryOrDefault("MAX_MY_ISSUES_DISPLAY", "8").intValue();

  TextField tf = new TextField("MAX_MY_ISSUES_DISPLAY","&nbsp;");
  tf.label=I18n.get("MAX_ISSUES_DISPLAY");
  tf.fieldSize=2;
  tf.script=" onBlur=\"refreshPortlet($(this),{'MAX_MY_ISSUES_DISPLAY':$(this).val()});\"";
  tf.toHtmlI18n(pageContext);

%>&nbsp;&nbsp;<%

    CheckField showAlsoDep= new CheckField("SHOW_ALSO_DEP","",false);
    boolean showDep =pageState.getEntryOrDefault("SHOW_ALSO_DEP", "yes").checkFieldValue();
    showAlsoDep.toHtmlI18n(pageContext);


  %></div><%


  List<Resource>myDeps=new ArrayList<Resource>();
  if (showDep){
    Query query = new OqlQuery("select c from " + Company.class.getName() + " as c where c.myManager=:me").getQuery();
    query.setEntity("me",resource);
    myDeps=query.list();

    myDeps.add(resource);
  }


      //first get issues expired
  String hql = "from "+Issue.class.getName()+" as issue";
  QueryHelper qh = new QueryHelper(hql);
  qh.addOQLClause("issue.status.behavesAsOpen = true");


  if (showDep){
    qh.addOQLInClause("issue.assignedTo","depsandme",myDeps);
  } else {
    qh.addOQLClause("issue.assignedTo = :myself","myself",resource);
  }
  qh.addOQLClause("issue.task != null");
  qh.addOQLClause("issue.task.status = :taskOpen","taskOpen", TaskStatus.STATUS_ACTIVE);
  qh.addOQLClause("issue.shouldCloseBy<=:tomorrow","tomorrow", new Date(System.currentTimeMillis()+ CompanyCalendar.MILLIS_IN_DAY*2));
  qh.addToHqlString("order by issue.shouldCloseBy, issue.gravity desc, issue.orderFactorByResource, issue.orderFactor");
  Query query = qh.toHql().getQuery();
  List<Issue> expired = query.list();
  List<Serializable> visitedIds= new ArrayList();
  JSONArray expiredIssues= new JSONArray();
  for (Issue i : expired) {
    expiredIssues.add(i.jsonify());
    visitedIds.add(i.getId());
  }

  //then get standard issues
  hql = "from "+Issue.class.getName()+" as issue";
  qh = new QueryHelper(hql);
  qh.addOQLClause("issue.status.behavesAsOpen = true");
  if (showDep){
    qh.addOQLInClause("issue.assignedTo","depsandme",myDeps);
  } else {
    qh.addOQLClause("issue.assignedTo = :myself","myself",resource);
  }
  qh.addOQLClause("issue.task != null");
  qh.addOQLClause("issue.task.status = :taskOpen","taskOpen", TaskStatus.STATUS_ACTIVE);
  if (visitedIds.size()>0)
    qh.addOQLClause("issue.id not in (:visited)","visited",visitedIds);
  qh.addToHqlString("order by issue.gravity desc, issue.orderFactorByResource, issue.orderFactor");
  query = qh.toHql().getQuery();
  query.setMaxResults(maxIssuesToShow);
  List<Issue> issues = query.list();

  JSONArray jsIssues = new JSONArray();
  for (Issue i : issues) {
    jsIssues.add(i.jsonify());
  }

%>


  <table class="table dataTable" >
    <thead>
    <tr>
      <th class="tableHead" width="1%"><%=I18n.get("ISSUE_GRAVITY_SHORT")%></th>
      <th class="tableHead"><%=I18n.get("ISSUE_STATUS")%></th>
      <th class="tableHead"><%=I18n.get("ISSUE_DESCRIPTION")%></th>
      <th class="tableHead" colspan="2"><%=I18n.get("ISSUE_TASK")%></th>
      <th class="tableHead" nowrap><%=I18n.get("ISSUE_ASSIGNED_TO")%></th>
      <th class="tableHead" nowrap><%=I18n.get("ISSUE_CLOSE_BY_SHORT")%></th>
      <th class="tableHead" nowrap></th>
    </tr>
    </thead>
    <tbody id="wp_myIssuesExpired" style="display:none;"></tbody>
    <tbody id="wp_myIssues"></tbody>
  </table>

  <%
    if (issues.size()==0 && visitedIds.size() ==0) {
  %><div class="hint"><%=I18n.get("NO_ISSUES")%></div><%
    }
  %>


<script type="text/javascript">
  var defs = [initialize(contextPath + "/applications/teamwork/portal/portlet/parts/partIssuePortletSupport.jsp", "html"),
    initialize(contextPath + "/applications/teamwork/issue/partIssueNotes.jsp", "html")];

  $.when.apply(null, defs) // per chiamare la when con un array [a,b,c,d] invece che parametri sep.  (a,b,c,d)
    .done(function () {
      //console.debug("wp_myIssues : all loaded")
      $("#wp_myIssueTemplates").loadTemplates().remove();

      var expired =<%=expiredIssues%>;
      var issues =<%=jsIssues%>;

      var ndo = $("#wp_myIssuesExpired");
      for (var i = 0; i < expired.length; i++) {
        var iss = $.JST.createFromTemplate(expired[i], "MYISSLINE");
        cvc_redraw(iss.find(".cvcComponent"));
        ndo.append(iss);
      }
        if (expired.length > 0)
        ndo.show();

      ndo = $("#wp_myIssues");
      for (var i = 0; i < issues.length; i++) {
        var iss = $.JST.createFromTemplate(issues[i], "MYISSLINE");
        cvc_redraw(iss.find(".cvcComponent"));
        ndo.append(iss);
      }


      //bind  issueEvent
      registerEvent("issueEvent.wpMyIssues", function (e, data) {
        //console.debug("wp_myIssues",data);
        refreshPortlet($("#myIssues"));
      });

    })

</script>
</div>


