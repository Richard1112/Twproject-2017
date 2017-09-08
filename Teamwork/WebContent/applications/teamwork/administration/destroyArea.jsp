<%@ page import="com.twproject.agenda.Event, com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.task.Issue, com.twproject.task.Task, com.twproject.waf.TeamworkHBFScreen, org.jblooming.PlatformRuntimeException, org.jblooming.messaging.Message, org.jblooming.ontology.businessLogic.DeleteHelper, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.hibernate.PersistenceContext, org.jblooming.security.Area, org.jblooming.security.Role, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.view.PageState, java.util.List" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    //put controller !
    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

    if (pageState.getLoggedOperator() == null || !pageState.getLoggedOperator().hasPermissionAsAdmin()) {
      throw new SecurityException("Access denied");
    }

  } else {

    Container c = new Container();
    c.title="Danger: definitively destroying areas";
    c.start(pageContext);


    String areaId = pageState.getEntry("AREA_MORITURA").stringValueNullIfEmpty();

    if (JSP.ex(areaId)) {

    Area a = (Area) PersistenceHome.findByPrimaryKey(Area.class, areaId);

    //people
    OqlQuery oql = new OqlQuery("from " + Person.class.getName() + " as i where i.area = :area");
    oql.getQuery().setEntity("area", a);
    List<Person> ps = oql.list();

    oql = new OqlQuery("select i.myself from " + Person.class.getName() + " as i where i.area = :area");
    oql.getQuery().setEntity("area", a);
    List<TeamworkOperator> ops = oql.list();

    //task
    oql = new OqlQuery("from " + Task.class.getName() + " as task where task.area = :area and task.parent is null");
    oql.getQuery().setEntity("area", a);
    List<Task> dels = oql.list();
    pageState.command = Commands.DELETE_DESCENDANTS;
    for (Task del : dels) {
      %><%=del.getId()%> <%=del.getDisplayName()%><%
      DeleteHelper.recursivelyDeleteNode(del,pageState);
      PersistenceContext.getDefaultPersistenceContext().checkPoint();
      %> -> OK<br><%            
    }

    //task groups
    
    //worklogs

    //issues
  if (JSP.ex(ops)) {
    oql = new OqlQuery("from " + Issue.class.getName() + " as i where i.owner in (:ops)");
    oql.getQuery().setParameterList("ops", ops);
    List<Issue> is = oql.list();
    pageState.command = Commands.DELETE_DESCENDANTS;
    for (Issue del : is) {
      %><%=del.getId()%> <%=del.getDisplayName()%><%
      DeleteHelper.cmdDelete(del,pageState);
      %> -> OK<br><%
    }
    PersistenceContext.getDefaultPersistenceContext().checkPoint();
  }
    //boards

  //agenda & meeting
  if (JSP.ex(ps)) {
    oql = new OqlQuery("from " + Event.class.getName() + " as e where e.author in (:person)");
    oql.getQuery().setParameterList("person", ps);
    List<Event> ev = oql.list();
    pageState.command = Commands.DELETE_DESCENDANTS;
    for (Event del : ev) {
      %><%=del.getId()%> <%=del.getSummary()%><%
      DeleteHelper.cmdDelete(del,pageState);
      %> -> OK<br><%
    }
    PersistenceContext.getDefaultPersistenceContext().checkPoint();
  }

  //remove as target
  if (JSP.ex(ps)) {
    oql = new OqlQuery("from " + Event.class.getName() + " as e join e.targets as target where target in (:person)");
    oql.getQuery().setParameterList("person", ps);
    List<Object[]> evTrg = oql.list();
    pageState.command = Commands.DELETE_DESCENDANTS;
    for (Object[] evO : evTrg) {
      Event ev = (Event) evO[0];
      %><%=ev.getId()%> <%=ev.getSummary()%> removing from targets <%
      ev.getTargets().removeAll(ps);
      ev.store();
      %> -> OK<br><%
    }
    PersistenceContext.getDefaultPersistenceContext().checkPoint();
  }

  //destroy messages
  if (JSP.ex(ps)) {
    oql = new OqlQuery("from " + Message.class.getName() + " as e where e.fromOperator in (:ops) or e.toOperator in (:ops)");
    oql.getQuery().setParameterList("ops", ops);
    List<Message> ev = oql.list();
    for (Message del : ev) {
      %><%=del.getId()%> <%=del.getName()%><%
      DeleteHelper.cmdDelete(del,pageState);
      %> -> OK<br><%
    }
    PersistenceContext.getDefaultPersistenceContext().checkPoint();
  }

  //res class

  //destroy resources
  boolean dr = true;
  try {
  oql = new OqlQuery("from " + Resource.class.getName() + " as i where i.area = :area");
  oql.getQuery().setEntity("area", a);
  List<Resource> rs = oql.list();
  pageState.command = Commands.DELETE_DESCENDANTS;
  for (Resource del : rs) {
    %><%=del.getId()%> <%=del.getName()%><%
    DeleteHelper.recursivelyDeleteNode(del,pageState);
    %> -> OK<br><%
  }
  PersistenceContext.getDefaultPersistenceContext().checkPoint();
  } catch (Throwable t) {
    dr = false;
    %><hr>Resource/Operators could not be deleted:<br><%

    for (Person p : ps) {
      %><%=p.getId()%> <%=p.getDisplayName()%> <br><%
    }

    %><pre><%=JSP.htmlEncodeApexes(PlatformRuntimeException.getStackTrace(t))%></pre><hr><%
  }

    // roles
  oql = new OqlQuery("from " + Role.class.getName() + " as i where i.area = :area");
  oql.getQuery().setEntity("area", a);
  List<Role> rl = oql.list();
  pageState.command = Commands.DELETE_DESCENDANTS;
  for (Role del : rl) {
    %><%=del.getId()%> <%=del.getName()%><%
    DeleteHelper.cmdDelete(del,pageState);
    %> -> OK<br><%
  }
  PersistenceContext.getDefaultPersistenceContext().checkPoint();

    //area!
    if (dr)
      DeleteHelper.cmdDelete(a,pageState);

      %>ALL OK<hr><%

     pageState.removeEntry("AREA_MORITURA");
    }

    Form f = new Form(pageState.thisPage(request));
    f.start(pageContext);
    TextField tf = new TextField("area to destroy","AREA_MORITURA","&nbsp;",5,false);
    tf.toHtml(pageContext);
    ButtonSubmit saveInstance = ButtonSubmit.getSaveInstance(f, "DESTROY");
    saveInstance.confirmRequire=true;
    saveInstance.toHtml(pageContext);

    f.end(pageContext);

    c.end(pageContext);

  }
%>