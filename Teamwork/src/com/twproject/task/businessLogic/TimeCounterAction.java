package com.twproject.task.businessLogic;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.task.Assignment;
import com.twproject.task.Task;
import com.twproject.utilities.TeamworkComparators;
import com.twproject.worklog.Worklog;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.Period;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.text.ParseException;
import java.util.Collections;
import java.util.List;
import java.util.Date;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class TimeCounterAction extends ActionSupport {

  public TeamworkOperator logged;

  public TimeCounterAction(RestState pageState) {
    super(pageState);
    this.logged = (TeamworkOperator) pageState.getLoggedOperator();
  }

  public void cmdSaveOptions(PageState pageState) throws PersistenceException, ActionException {
    Person resource = Person.getLoggedPerson(pageState);

    try {
      long value = pageState.getEntry("WORKING_HOURS").timeValueInMillis();
      resource.setWorkDailyCapacity(value);
    } catch (ParseException p) {
    }
  }

  public Worklog cmdStop(PageState pageState) throws PersistenceException {
    restState.initializeEntries("cell");
    //find the one to stop if any
    Person resource = Person.getLoggedPerson(pageState);
    Assignment a = Assignment.load(pageState.mainObjectId);
    Worklog ret=null;
    if (a != null && a.getResource().equals(resource) && a.isCounted()) {
      ret=a.closeCounter(pageState);
      a.store();
    }
    return ret;
  }

  public Worklog cmdStart(PageState pageState) throws PersistenceException {
    restState.initializeEntries("cell");
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Worklog ret=null;

    //cerca quello da chiudere
    Assignment toStop = Assignment.getCountedAssignment(logged);
    if (toStop != null) {
      ret=toStop.closeCounter(pageState);
      toStop.store();
    }

    //find the one to start
    Assignment a = Assignment.load(pageState.mainObjectId);
    if (a != null && a.getResource().equals(logged.getPerson()) && !a.isCounted()) {
      a.setCounted(true);
      a.setCountingStartedAt(new Date());
      a.store();
    }

    return ret;
  }
}
