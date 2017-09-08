<%@ page import="com.twproject.resource.Person, com.twproject.task.Assignment, org.jblooming.agenda.Period, org.jblooming.security.Role,
org.jblooming.utilities.JSP, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.util.Date,
java.util.List, java.util.Set" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  
  %><div class="myResponsabilities portletBox small">
  <h1><%=I18n.get("MY_RESPONSIBILITIES")%></h1><%

  Person resource = Person.getLoggedPerson(pageState);

  Set<Role> glRoles = resource.getMyself().getInheritedRoles();

  List<Assignment> ass = resource.getActiveAssignments(Period.getDayPeriodInstance(new Date()),true);
  for (Assignment as : ass) {
    glRoles.add(as.getRole());
  }

  for (Role role : glRoles) {
    if (JSP.ex(role.getDescription())) {
      %><div class="myResponsability"><b><%=role.getName()%></b><br>
      <%=role.getDescription()%></div><%
    }
  }

%></div>
