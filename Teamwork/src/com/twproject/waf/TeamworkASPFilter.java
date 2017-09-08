package com.twproject.waf;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.security.TeamworkArea;
import com.twproject.task.Task;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.AccessControlFilter;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageState;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Date;

public class TeamworkASPFilter implements Filter {

  public void init(FilterConfig filterConfig) throws ServletException {
    ApplicationState.applicationParameters.put("TEAMWORK_ASP_INSTANCE", Fields.TRUE);
  }

  public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws ServletException, IOException {
    HttpServletRequest request = (HttpServletRequest) servletRequest;
    HttpServletResponse response = (HttpServletResponse) servletResponse;

    boolean redirectToPayPage = false;
    boolean canTest = !Commands.SAVE.equalsIgnoreCase(request.getParameter("CM")) && request.getRequestURI().indexOf("buyTwproject") == -1 && AccessControlFilter.isReservedUrl(request);

    PageState pageState = PageState.getCurrentPageState(request);
    SessionState sessionState = pageState.getSessionState();

    if (canTest) {
      if (Fields.TRUE.equals(sessionState.getAttribute("DEMO_EXPIRED"))) {
        redirectToPayPage = true;
      } else if (Fields.FALSE.equals(sessionState.getAttribute("DEMO_EXPIRED"))) {
        redirectToPayPage = false;
      } else {

        sessionState.setAttribute("DEMO_EXPIRED", "no");

        TeamworkOperator op = (TeamworkOperator) pageState.getLoggedOperator();
        if (op != null && !op.hasPermissionAsAdmin()) {
          Person p = op.getPerson();
          TeamworkArea a = (TeamworkArea) ReflectionUtilities.getUnderlyingObject(p.getArea()); //questo schifo perchè l'area sugli oggetti è Area ma in realtà è istanza di TeamworkArea
          if (a != null) {
            Date expiryDate = a.getExpiry();
            if (expiryDate != null && expiryDate.getTime() < System.currentTimeMillis()) {
              redirectToPayPage = true;
              sessionState.setAttribute("DEMO_EXPIRED", "yes");
            }
          }
        }
      }
    }
    if (redirectToPayPage)
      response.sendRedirect(ApplicationState.serverURL + "/applications/teamwork/buyTwproject.jsp");
    else
      filterChain.doFilter(request, response);
  }

  public static int tasksOfThisArea(TeamworkArea a) throws FindException {
    int totExTasks;
    String totTask = "select count(task.id) from " + Task.class.getName() + " as task where task.area=:area and task.parent is null";
    OqlQuery oql = new OqlQuery(totTask);
    oql.getQuery().setEntity("area", a);
    totExTasks = ((Long) oql.uniqueResult()).intValue();
    return totExTasks;
  }

  public static int enabledUsers(TeamworkArea a) throws FindException {
    String hql = "select count(op) from " + Person.class.getName() + " as person join person.myself as op  where op.enabled = :truth and person.area = :area";
    OqlQuery oqlQuery = new OqlQuery(hql);
    oqlQuery.getQuery().setBoolean("truth", Boolean.TRUE);
    oqlQuery.getQuery().setEntity("area", a);
    int totExOp = ((Long) oqlQuery.uniqueResult()).intValue();
    return totExOp;
  }


  public void destroy() {
  }

}
