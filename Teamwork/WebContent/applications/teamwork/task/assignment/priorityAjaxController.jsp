<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.task.Assignment, com.twproject.task.AssignmentPriority, com.twproject.task.businessLogic.TaskAction, org.jblooming.agenda.Period, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.tracer.Tracer, org.jblooming.utilities.JSP, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.view.PageState, java.util.Date, com.twproject.security.TeamworkPermissions, com.twproject.task.businessLogic.AssignmentAction" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  try {

    if ("CHANGEPRIO".equals(pageState.command)) {
      pageState.initializeEntries("row");
      String assId = pageState.getEntry("ASSID").stringValueNullIfEmpty();
      long time = pageState.getEntry("TIME").longValueNoErrorNoCatchedExc();
      long minMillisInBar = pageState.getEntry("MINMILLISINBAR").longValueNoErrorNoCatchedExc();
      int prio = pageState.getEntry("PRIO").intValueNoErrorCodeNoExc();

      if (JSP.ex(assId) && time > 0 && prio != 0) {
        Assignment ass = (Assignment) PersistenceHome.findByPrimaryKey(Assignment.class, assId);
        if (ass.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.resource_manage)) {
          long stamanPrest = Period.getDayPeriodInstance(new Date(time)).getStartDate().getTime();
          AssignmentAction.updateAssignmentPriority(ass, prio, stamanPrest);
        }

        JspHelper rowDrawer = new JspHelper("partPriorityRow.jsp");
        rowDrawer.parameters.put("person", ass.getResource());
        rowDrawer.parameters.put("minMillisInBar", minMillisInBar);
        rowDrawer.toHtml(pageContext);

      }
    } else if ("CLEAR".equals(pageState.command)) {
      String assId = pageState.getEntry("ASSID").stringValueNullIfEmpty();
      long time = pageState.getEntry("TIME").longValueNoErrorNoCatchedExc();
      long minMillisInBar = pageState.getEntry("MINMILLISINBAR").longValueNoErrorNoCatchedExc();
      if (JSP.ex(assId) && time > 0) {
        Assignment ass = (Assignment) PersistenceHome.findByPrimaryKey(Assignment.class, assId);
        if (ass.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.resource_manage)) {
          // batch delete ap
          String hql = "delete from " + AssignmentPriority.class.getName() + " where assignment=:ass and cutPoint>=:staman";
          OqlQuery oql = new OqlQuery(hql);
          oql.getQuery().setEntity("ass", ass);
          oql.getQuery().setLong("staman", time);
          int quant = oql.getQuery().executeUpdate();
        }
        JspHelper rowDrawer = new JspHelper("partPriorityRow.jsp");
        rowDrawer.parameters.put("person", ass.getResource());
        rowDrawer.parameters.put("minMillisInBar", minMillisInBar);
        rowDrawer.toHtml(pageContext);
      }
    }
  } catch (Throwable e) {
    out.println("error=true;");
    out.println("message=\"" + JSP.javascriptEncode(e.getMessage()) + "\"");
    Tracer.platformLogger.error(e);
  }


%>