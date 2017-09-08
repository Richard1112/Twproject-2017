<%@ page import="com.twproject.waf.TeamworkPopUpScreen,
                 org.hibernate.SessionFactory,
                 org.jblooming.ontology.Node,
                 org.jblooming.ontology.PerformantNodeSupport,
                 org.jblooming.oql.OqlQuery,
                 org.jblooming.persistence.hibernate.HibernateFactory,
                 org.jblooming.utilities.ReflectionUtilities,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 java.util.*" %>
<%! private boolean checkPath(Node n, List<Node> visited) {
  if (n == null)
    return true;
  else if (visited.contains(n))
    return false;
  else {
    visited.add(n);
    return checkPath(n.getParentNode(), visited);
  }

}%><%
  //if (!PlatformConfiguration.psw.equalsIgnoreCase(request.getParameter("psw")))
  //   response.sendError(HttpServletResponse.SC_FORBIDDEN,"Access reserved to server administrators");

  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    //put controller !
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

  } else {

    PageSeed pageSeed = pageState.thisPage(request);
    Form form = new Form(pageSeed);
    form.start(pageContext);


    SessionFactory sf = HibernateFactory.getSessionFactory();

    Map acm = sf.getAllClassMetadata();
    Set keysAcm = acm.keySet();

%><h1>Check tree integrity: select entity</h1>
<table class="table"><%
  if (keysAcm != null && keysAcm.size() > 0) {



    for (Iterator iterator = keysAcm.iterator(); iterator.hasNext();) {
      String className = (String) iterator.next();
      //it may be an entity name, and not a class
      Class persClass = null;
      try {
        persClass = Class.forName(className);
      } catch (ClassNotFoundException e) { }


      if (ReflectionUtilities.extendsOrImplements(persClass, PerformantNodeSupport.class)) {


        %> <tr class="alternate" ><td><%

  CheckField field = new CheckField(persClass.getName(), "</td><td align=\"center\">", true);
  field.label = persClass.getSimpleName();
  field.toHtml(pageContext); %></td></tr><%
      }
    }
  }
  %></table><%

  ButtonBar bb = new ButtonBar();

  ButtonSubmit sb = new ButtonSubmit(form);
  sb.variationsFromForm.command = "RANCID";
  sb.label = "recompute ancestorIds";
   bb.addButton(sb);

  sb = new ButtonSubmit(form);
  sb.variationsFromForm.command = "DOTEST";
  sb.label = "check";
  bb.addButton(sb);

  bb.toHtml(pageContext);


  form.end(pageContext);


  if ("DOTEST".equals(pageState.command)) {

     for (Iterator iterator = keysAcm.iterator(); iterator.hasNext();) {

       String className = (String) iterator.next();
             //it may be an entity name, and not a class
             Class cln = null;
             try {
               cln = Class.forName(className);
             } catch (ClassNotFoundException e) { }

    if (cln != null && pageState.getEntry(cln.getName()).checkFieldValue()) {

      try {

        Map<String, Node> nodes = new HashMap();
        Set<Node> roots = new HashSet();

        // si carica tutto in memoria, alla facciaccia della scalabilità
        List<Node> nl = new OqlQuery("from " + cln.getName()).list();
        for (Node n : nl) {
          nodes.put(n.getId() + "", n);
        }

        boolean error=false;
        // si va a caccia di loop
        for (Node n : nodes.values()) {

          if (n.getParentNode() == null) {
            roots.add(n);
          }

          List<Node> visited = new ArrayList();
          if (!checkPath(n, visited)) {
            error=true;
            %> Node : <%=n.getId()%> : <%=n.getName()%> circular reference [<%
            for (Node p:visited){
              %><%=p.getId()%>,<%
            }
            %>]<br><%
          }
        }

        if (!error){
          %><br><%=cln%>: no error found.<br><%
        }


      } catch (Exception e) {
        %><hr> <font size="5" color="red">ERROR:</font><br>
        <font size="4" color="orange"><%=e.toString()%></font><br><%
        e.printStackTrace(response.getWriter());
        %><hr> <%
      }
    }
  }
  } else if ("RANCID".equals(pageState.command)) {


        for (Iterator iterator = keysAcm.iterator(); iterator.hasNext();) {

          String className = (String) iterator.next();
          //it may be an entity name, and not a class
          Class cln = null;
          try {
            cln = Class.forName(className);
          } catch (ClassNotFoundException e) { }

          if (cln != null && pageState.getEntry(cln.getName()).checkFieldValue()) {
            try {
              // si carica tutto in memoria, alla facciaccia della scalabilità
              List<PerformantNodeSupport> nl = new OqlQuery("from " + cln.getName() +" as pfn where pfn.parent is not null").list();
              for (PerformantNodeSupport n : nl) {
                %>trying <%=n.getName()%> with parent <%=n.getParentNode().getName()%>: <%
                n.setParentAndStore((PerformantNodeSupport) n.getParentNode());
                %>OK<br> <%
              }
            } catch (Exception e) {
              %>:-( <%=cln.getName()%> <%=e%><br><%
            }
          HibernateFactory.checkPoint();
        }
      }
    }
  }
%>