<%@ page import="com.twproject.task.Issue,com.twproject.task.TaskStatus,net.sf.json.JSONArray,org.hibernate.Query,
                 org.jblooming.agenda.CompanyCalendar,org.jblooming.oql.QueryHelper, org.jblooming.waf.html.core.OneTimeIncluder, org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageState, java.io.Serializable, java.util.ArrayList, java.util.Date, java.util.List, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink"%>
<%

  PageState pageState = PageState.getCurrentPageState(request);

%><div class="portletBox"><%

  ButtonJS bs = new ButtonJS();
  bs.onClickScript = "$('#myCreatedIssues').toggle()";
  bs.iconChar="g";
  bs.label="";
  bs.additionalCssClass="ruzzol";
  %>
  <div style="float:right;padding-top: 5px"><%bs.toHtmlInTextOnlyModality(pageContext);%></div>

  <h1><%=I18n.get("PF_MY_INSERTED_ISSUES")%></h1>


  <div id="myCreatedIssues" class="portletParams" style="display:none"><%

    int maxIssuesToShow = pageState.getEntryOrDefault("MAX_ISSUES_DISPLAY", "8").intValue();

    TextField tf = new TextField("MAX_ISSUES_DISPLAY","&nbsp;");
    tf.fieldSize=2;
    tf.script=" onBlur=\"refreshPortlet($(this),{'MAX_ISSUES_DISPLAY':$(this).val()});\"";
    tf.toHtmlI18n(pageContext);

  %></div><%


    //first get issues expired
    String hql = "from "+Issue.class.getName()+" as issue";
    QueryHelper qh = new QueryHelper(hql);
    qh.addOQLClause("issue.status.behavesAsOpen = true");
    qh.addOQLClause("issue.owner = :myself","myself",pageState.getLoggedOperator());
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
    qh.addOQLClause("issue.owner = :myself","myself",pageState.getLoggedOperator());
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

   if (issues.size()==0 && visitedIds.size() ==0) {
  %><h2 class="hint" style="margin: 0;padding: 0"><%=I18n.get("NO_ISSUES")%></h2><%
    }

  %>
    <table class="table dataTable" >
      <thead>
      <tr>
        <th class="tableHead" width="1%"><%=I18n.get("ISSUE_GRAVITY_SHORT")%></th>
        <th class="tableHead" width="1%"><%=I18n.get("ISSUE_STATUS")%></th>
        <th class="tableHead"><%=I18n.get("ISSUE_DESCRIPTION")%></th>
        <th class="tableHead"><%=I18n.get("ISSUE_TASK")%></th>
        <th class="tableHead" nowrap><%=I18n.get("ISSUE_ASSIGNED_TO")%></th>
        <th class="tableHead"><%=I18n.get("ISSUE_CLOSE_BY_SHORT")%></th>
      </tr>
      </thead>
      <%--<tr><td colspan="6" ><div class="boxAlert" style="margin: 0"><span class="teamworkIcon warning">!</span> <span class="textSmall" style="padding-left: 5px"><%=I18n.get("ISSUE_EXPIRED")%></span></div></td></tr>--%>
      <tbody id="wp_myInsIssuesExpired" style="display:none;"></tbody>
      <%--<tr class="tableSection"><td colspan="6" ><h2 class="sectionTitle" style="height: 25px"></h2></td></tr>--%>
      <tbody id="wp_myInsIssues"></tbody>
    </table>


<script type="text/javascript">

  var defs=[initialize(contextPath+"/applications/teamwork/portal/portlet/parts/partIssuePortletSupport.jsp","html"),
    initialize(contextPath+"/applications/teamwork/issue/partIssueNotes.jsp","html")];


  $.when.apply(null, defs) // per chiamare la when con un array [a,b,c,d] invece che parametri sep.  (a,b,c,d)
    .done(function () {
      //console.debug("wp_issueCreatedByMe : all loaded")

      $("#wp_myInsIssueTemplates").loadTemplates().remove();

      var expired =<%=expiredIssues%>;
      var issues =<%=jsIssues%>;

      var ndo = $("#wp_myInsIssuesExpired");
      for (var i = 0; i < expired.length; i++) {
        var iss = $.JST.createFromTemplate(expired[i], "MYINSISSLINE");
        cvc_redraw(iss.find(".cvcComponent"));
        ndo.append(iss);
      }
      if (expired.length > 0)
        ndo.show();

      ndo = $("#wp_myInsIssues");
      for (var i = 0; i < issues.length; i++) {
        var iss = $.JST.createFromTemplate(issues[i], "MYINSISSLINE");
        cvc_redraw(iss.find(".cvcComponent"));
        ndo.append(iss);
      }


      //bind  issueEvent
      registerEvent("issueEvent.myCreatedIssues",function(e,data){
        //console.debug("wp_issueCreatedByMe event issueEvent",data);
        //todo in caso di delete potrebbe essere meglio
        refreshPortlet($("#myCreatedIssues"));
      });

    });


</script>

</div>



