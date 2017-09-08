package com.twproject.agenda;

import org.jblooming.waf.view.PageState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.tracer.Tracer;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;

public class AgendaInIcalServlet extends HttpServlet {

  protected void doGet(HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) throws javax.servlet.ServletException, java.io.IOException {
    doMe(httpServletRequest, httpServletResponse);
  }

  protected void doPost(HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) throws javax.servlet.ServletException, java.io.IOException {
    doMe(httpServletRequest, httpServletResponse);
  }

  private void doMe(HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) {

    httpServletResponse.setContentType("charset=\"utf-8\"");
    httpServletResponse.setCharacterEncoding("utf-8");

    //response.addHeader("Expires", "Sat, 20 Sep 2000 01 :01 :01 GMT");
    httpServletResponse.addHeader("Expires", "Sat, 23 Sep 2000 01:01:01 GMT");
    httpServletResponse.addHeader("Cache-Control", "no-cache");


    net.fortuna.ical4j.model.Calendar ical = null;
    try {
      ical = AgendaInIcalServlet.generateIcal(httpServletRequest, httpServletResponse);
      if (ical != null) {
        httpServletResponse.getWriter().print(ical);
      }
    } catch (Throwable e) {
      Tracer.platformLogger.error(e);
    }

  }


  public static net.fortuna.ical4j.model.Calendar generateIcal(HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) throws PersistenceException {
    TeamworkOperator arrived = null;
    String command = httpServletRequest.getParameter(Commands.COMMAND);
    String usr = httpServletRequest.getParameter("USR");
    String ck = httpServletRequest.getParameter("CK");
    if ("EXTERNAL".equals(command) && usr != null) {
      TeamworkOperator logCand = (TeamworkOperator) PersistenceHome.findByPrimaryKey(TeamworkOperator.class, usr);
      if (logCand != null && logCand.getPassword().equals(ck))
        arrived = logCand;
    }
    if (arrived != null) {
      httpServletResponse.setContentType("text/calendar");
      httpServletResponse.setHeader("Content-Disposition", "attachment;filename=\"TeamworkCalendar.ics\";");

      Person loggedPerson = arrived.getPerson();
      return IcalUtilities.getIcalForPerson(loggedPerson);

    } else
      return null;
  }

}