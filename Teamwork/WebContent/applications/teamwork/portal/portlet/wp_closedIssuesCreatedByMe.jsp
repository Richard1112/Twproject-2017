<%@ page import="com.twproject.operator.TeamworkOperator,com.twproject.resource.Resource,com.twproject.task.Issue,com.twproject.task.IssueBricks,
                 com.twproject.task.Task,org.jblooming.oql.QueryHelper, org.jblooming.page.ListPage, org.jblooming.page.Page, org.jblooming.utilities.JSP,
                 org.jblooming.waf.html.button.ButtonImg, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.display.Img, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.html.table.ListHeader,
                 org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List"
        %><div id="closedIssuesCreatedByMeExDiv" class="portletBox"><%

  PageState pageState = PageState.getCurrentPageState(request);


  PageSeed self = pageState.pagePart(request);
  Form f = new Form(self);
  f.id="MYCLOSEDISSUES_FORM";
  pageState.setForm(f);
  f.start(pageContext);

  Container cont = new Container("CLOSED_ISSUES_CREATED_BY_ME");
  cont.title = "CLOSED_ISSUES_CREATED_BY_ME" ; //Open issues created by me
  cont.overflow="auto";

  Img img = new Img("issuesBig.png", pageState.getI18n("MYISSUES"));

  PageSeed ps = new PageSeed(request.getContextPath() + "/applications/teamwork/issue/issueList.jsp");
  ButtonLink sub = new ButtonLink(ps);
  ButtonImg bimg = new ButtonImg(sub, img);
  cont.icon = bimg;



  TeamworkOperator logged = (TeamworkOperator)pageState.getLoggedOperator();
  //cont.tableHeight="100%";

  ButtonJS bs = new ButtonJS();
  bs.onClickScript = "$('#closedIssuesCreatedByMe').toggle()";
  bs.iconChar="g";
  bs.outputModality=bs.TEXT_ONLY;
  cont.titleRightElements.add(bs);

  String bsa = ButtonSubmit.getAjaxButton(f, "closedIssuesCreatedByMeExDiv").generateJs().toString();

  cont.start(pageContext);

  %><div id="closedIssuesCreatedByMe" class="portletParams" style="display:none"><%

  int maxIssuesToShow = pageState.getEntryOrDefault("MAX_CLOSED_ISSUES_DISPLAY", "8").intValue();

  TextField tf = new TextField("MAX_CLOSED_ISSUES_DISPLAY","&nbsp;");
  tf.fieldSize=2;
  tf.script=" onBlur=\""+bsa+"\" ";
  tf.toHtmlI18n(pageContext);
  //inhibit submit
  %><input type="text" style="display:none;">&nbsp;<%

%></div><%
  boolean swap = true;

  String hql = "from "+Issue.class.getName()+" as issue";
  QueryHelper qh = new QueryHelper(hql);
  qh.addOQLClause("issue.status.behavesAsClosed = true");
  qh.addOQLClause("issue.owner = :myself","myself",logged);

  ListHeader.orderAction(qh, "ISSMYLH", pageState, "issue.gravity desc");
  qh.addToHqlString(", issue.orderFactor asc");

  Page oldPage = pageState.getPage();

  List<Issue> issues = qh.toHql().list();
  if (issues.size()>0) {
  Page newPage = new ListPage(issues, 1, issues.size());
  if (newPage!=null)
    pageState.setPage(newPage);
  }

  %><table class="table" cellpadding="2" border="0"><%

  ListHeader lh = ListHeader.getAjaxInstance("ISSMYLH", f, "closedIssuesCreatedByMe");
  lh.addHeader(pageState.getI18n("ISSUE_GRAVITY_SHORT"));//,"issue.gravity");
  lh.addHeader(pageState.getI18n("ISSUE_DESCRIPTION"));
  lh.addHeader(pageState.getI18n("TASK"));
  lh.addHeader(pageState.getI18n("ASSIGNEE"));
  lh.addHeader(pageState.getI18n("ISSUE_CLOSE_BY"));//,"issue.shouldCloseBy");
  lh.toHtml(pageContext);


    int displayedIssue = 0;

    for (Issue i : issues) {

      if (!(maxIssuesToShow==0) && displayedIssue>=maxIssuesToShow)
        break;

      if (i.getTask()!=null && !i.getTask().isActive())
        continue;
      ButtonLink editIssue = IssueBricks.getPopoupLinkToEditor(i.getId());
      //editIssue.label = JSP.limWr(i.getDisplayName(),60);

      %> <tr class="alternate" >
        <td width="10" style="border:1px solid #999999; background-color:<%=i.bricks.getGravityColor()%>" title="<%=pageState.getI18n(i.getGravity())%>" align="center">
          <%
            editIssue.label = "&nbsp;&nbsp;&nbsp;";
            editIssue.toHtmlInTextOnlyModality(pageContext);%>
        </td>
        <td title="(id:<%=i.getId()%>)<%=i.getTask()!=null ? "<br>t:"+i.getTask().getDisplayName() : ""%>"><%
          editIssue.label = "<small>"+i.getDisplayName()+"</small>";
          editIssue.toHtmlInTextOnlyModality(pageContext);%></td>
  <td>
    <%
      Task task = i.getTask();
    if(task != null ){
      %><%=task.getDisplayName()%><%       
    }
    %>
  </td>
  <td><%
    Resource assignee = i.getAssignedTo();
    if(assignee != null ){
      %><%=assignee.getDisplayName()%><%       
    }
  %></td>
  <td align="center" ><%if(JSP.ex(i.getShouldCloseBy())){%><%=JSP.w(i.getShouldCloseBy())%><%}else{%>&nbsp;<%}%></td><%

        %></tr> <%
      displayedIssue++;
    }
   %></table><%
  if (issues.size()>20) {
    %><table class="table" cellpadding="2" border="0">
      <tr><td>...</td></tr></table><%
  } else  if (issues.size()==0) {
    %><table class="table" cellpadding="2" border="0">
      <tr><td><%=pageState.getI18n("NO_ISSUES")%></td></tr></table><%
  }
  cont.end(pageContext);

  f.end(pageContext);
  if (oldPage!=null)
    pageState.setPage(oldPage);
%>
</div>
