<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.security.TeamworkArea, com.twproject.waf.TeamworkASPFilter, com.twproject.waf.TeamworkHBFScreen, org.jblooming.agenda.CompanyCalendar, org.jblooming.oql.OqlQuery, org.jblooming.oql.QueryHelper, org.jblooming.page.HibernatePage, org.jblooming.page.Page, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.display.Paginator, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.DateField, org.jblooming.waf.html.input.LoadSaveFilter, org.jblooming.waf.html.state.Form, org.jblooming.waf.html.table.ListHeader, org.jblooming.waf.settings.I18n, org.jblooming.waf.state.PersistentSearch, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Iterator, java.util.List, java.util.Date" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    if (!logged.hasPermissionAsAdmin())
      throw new SecurityException("Not admin");

    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    String hql = "from " + TeamworkArea.class.getName() + " as area";
    QueryHelper qhelp = new QueryHelper(hql);

    boolean somethingSearched = false;
    boolean recoveredFromSavedFilter = PersistentSearch.feedFromSavedSearch(pageState);
    somethingSearched = recoveredFromSavedFilter;

    if (pageState.getEntry("SHOW_UNUSED").checkFieldValue()) {
      somethingSearched = true;
      CompanyCalendar cc = new CompanyCalendar();
      cc.add(CompanyCalendar.DAY_OF_YEAR, -60);
      qhelp.addQBEClause("area.lastLoginOnArea", "lastLoginOnArea", "<" + DateUtilities.dateToString(cc.getTime()), QueryHelper.TYPE_DATE);
      qhelp.addQBEClause("area.freeAccount", "freeAccount", Fields.TRUE, QueryHelper.TYPE_CHAR);
    }

    if (pageState.getEntry("SHOW_CUSTOMERS").checkFieldValue()) {
      somethingSearched = true;
      qhelp.addQBEClause("area.freeAccount", "freeAccount", Fields.FALSE, QueryHelper.TYPE_CHAR);
    }

    if (pageState.getEntry("SHOW_FREE").checkFieldValue()) {
      somethingSearched = true;
      qhelp.addQBEClause("area.freeAccount", "freeAccount", Fields.TRUE, QueryHelper.TYPE_CHAR);
    }

    String ld = pageState.getEntry("LAST_LOGIN").stringValueNullIfEmpty();
    if (ld != null) {
      somethingSearched = true;
      qhelp.addQBEClause("area.lastLoginOnArea", "lastLoginOnArea", ld, QueryHelper.TYPE_DATE);
    }

    ld = pageState.getEntry("CREATION_DATE").stringValueNullIfEmpty();
    if (ld != null) {
      somethingSearched = true;
      qhelp.addQBEClause("area.creationDate", "creationDate", ld, QueryHelper.TYPE_DATE);
    }
    
    //if (somethingSearched) {
      ListHeader.orderAction(qhelp, "ONLLH", pageState, "area.lastLoginOnArea DESC");
      pageState.setPage(HibernatePage.getHibernatePageInstance(qhelp.toHql().getQuery(), Paginator.getWantedPageNumber(pageState), Paginator.getWantedPageSize(pageState)));

    //}

    pageState.toHtml(pageContext);

  } else {

    Container cont = new Container();
    cont.title = "online accounts administration";
    cont.start(pageContext);

    Form f = new Form(pageState.thisPage(request));
    pageState.setForm(f);
    f.start(pageContext);

%>
<style>
  .expired{
    text-decoration: line-through;
  }
</style>

<table class="table"><%

    %><tr><td width="1%"><%
    CheckField cf = new CheckField("SHOW_UNUSED","</td><td>",true);
    cf.toHtml(pageContext);
    %></td><td width="1%"><%

    cf = new CheckField("SHOW_CUSTOMERS","</td><td>",true);
    cf.toHtml(pageContext);
    %></td><td width="1%"><%

    cf = new CheckField("SHOW_FREE","</td><td>",true);
    cf.toHtml(pageContext);
    %></td>

  <td width="1%"><%

    DateField dfL = new DateField("CREATION_DATE");
    dfL.setSearchField(true);
    dfL.toHtml(pageContext);
  %></td>


  <td width="1%"><%

    dfL = new DateField("LAST_LOGIN");
    dfL.setSearchField(true);
    dfL.toHtml(pageContext);
  %></td>



    </tr><%

    %></table><%

  ButtonBar bb = new ButtonBar();

  LoadSaveFilter lsf = new LoadSaveFilter("ONL", f);
  bb.addButton(lsf);

  bb.addSeparator(10);
  ButtonSupport qbe = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("HELP")), 700, 800, pageState.pageFromCommonsRoot("help/qbe.jsp"));
  qbe.toolTip = I18n.get("HELP_QBE");
  bb.addButton(qbe);
  bb.addSeparator(10);


  bb.addButton(ButtonSubmit.getSearchInstance(f,pageState));
  bb.toHtml(pageContext);

  if ("SHOW_DETAIL".equals(pageState.command)) {
    TeamworkArea area = (TeamworkArea) PersistenceHome.findByPrimaryKey(TeamworkArea.class, pageState.mainObjectId);
    String hqlP = "from " + Person.class.getName() + " as p where p.myself!=null and p.area = :area order by p.creationDate";
    OqlQuery pOfArea = new OqlQuery(hqlP);
    pOfArea.getQuery().setEntity("area", area);
    List<Person> list = pOfArea.list();
    %><hr><b>area <%=area.getId()%></b><br>
    resources:<br><%
    for (Person p : list) {
      %><%=p.getDisplayName()%> <%=p.getDefaultEmail()%><br><%
    }
    %>tasks: <%=TeamworkASPFilter.tasksOfThisArea(area)%><%
    %><hr><%
  }

  %><table border="0" cellpadding="0" class="table">
    <tr>
    <td><%new Paginator("ONLPG", f).toHtml(pageContext);%></td>
    </tr></table><%
   %> <table class="table"><%
  
  ListHeader lh = new ListHeader("ONLLH", f);
  lh.addHeader("area id","area.id");
  lh.addHeader("created","area.creationDate");
  lh.addHeader("user");
  lh.addHeader("email");
  lh.addHeader("last login","area.lastLoginOnArea");
  lh.addHeader("is free");
  lh.addHeader("ops.enab.","area.enabledOperators");
  lh.addHeader("expiry");
  lh.addHeader("went beyond");
  lh.addHeader("#ops");
  lh.addHeader("#tasks");

  lh.toHtml(pageContext);





   Page areas = pageState.getPage();
   if (areas!=null) {
  for (Iterator iterator = areas.getThisPageElements().iterator(); iterator.hasNext();) {

    TeamworkArea area = (TeamworkArea) iterator.next();

    //find first op of area
    String hqlP = "from " + Person.class.getName() + " as p where p.myself!=null and p.area = :area order by p.creationDate";
    OqlQuery pOfArea = new OqlQuery(hqlP);
    pOfArea.getQuery().setEntity("area", area);
    pOfArea.getQuery().setMaxResults(1);
    Person p = (Person) pOfArea.uniqueResultNullIfEmpty();
    if (p != null) {
%> <tr class="alternate <%=area.getExpiry()!=null&&area.getExpiry().before(new Date())?"expired":""%>" > <%
        PageSeed pageSeed = pageState.thisPage(request);
        pageSeed.mainObjectId = area.getId();
        pageSeed.command = "SHOW_DETAIL";
        ButtonLink ad = new ButtonLink(area.getId() + "", pageSeed);
        %>
        <td><%ad.toHtmlInTextOnlyModality(pageContext);%></td>
        <td><%=DateUtilities.dateToString(area.getCreationDate(),"dd MMM yyyy")%></td>
        <td><%=p.getDisplayName()%></td>
        <td><%=p.getDefaultEmail()%></td>
        <td><%=DateUtilities.dateToString(area.getLastLoginOnArea(),"dd MMM yyyy HH:mm")%></td>
        <td><%=JSP.w(area.getFreeAccount())%></td>
        <td><%=JSP.w(area.getEnabledOperators())%></td>
        <td><%=DateUtilities.dateToString(area.getExpiry(),"dd MMM yyyy")%></td>
        <td><%=DateUtilities.dateToString(area.getBeyondFreeVersion(),"dd MMM yyyy")%></td>
        <td><%=JSP.w(TeamworkASPFilter.enabledUsers(area))%></td>
        <td><%=JSP.w(TeamworkASPFilter.tasksOfThisArea(area))%></td>
      </tr><%
    }
    }
    }
  
    %></table><%
    f.end(pageContext);

    cont.end(pageContext);    

  }
%>