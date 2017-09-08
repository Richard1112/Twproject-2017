package com.twproject.setup;

import org.jblooming.security.License;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.PlatformConfiguration;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.PluginBricks;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.view.PageState;
import org.jblooming.system.SystemConstants;
import org.jblooming.remoteFile.FileStorage;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.security.Area;
import org.jblooming.scheduler.Scheduler;

import java.util.*;

import com.twproject.resource.Person;
import com.opnlb.website.page.WebSitePage;
import com.opnlb.website.news.News;
import org.jbpm.graph.exe.ProcessInstance;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Sep 9, 2008
 * Time: 4:22:14 PM
 */
public class TeamworkConfiguration {

  public static Map<String, ConfigurationStep> configurationSteps = new LinkedHashMap();

  public static void evaluateLevel(PageState pageState) throws PersistenceException {

    //SMTP configuration
    addStep("SMTP", "SMTP configuration", 10, JSP.ex(ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_SMTP)));

    //Repository configuration
    addStep("REPOSITORY", "Repository configuration", 9, JSP.ex(ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL)));

    //Some file storage configured
    String hql = "from " + FileStorage.class.getName() + " as document";
    OqlQuery qh = new OqlQuery(hql);
    qh.getQuery().setMaxResults(1);
    addStep("FILESTORAGE", "Some file storage configured", 8, qh.list().size() > 0);

    //POP3 configuration
    addStep("POP3", "POP3 configuration", 10, JSP.ex(ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_HOST)));

    //Company holydays configured
    addStep("HOLYDAYS", "Company holidays configured", 4, JSP.ex((Collection) new CompanyCalendar().getHolyDays()));

    //Set the public DNS
    addStep("DNS", "Set the public DNS", 6, JSP.ex(ApplicationState.getApplicationSetting(SystemConstants.PUBLIC_SERVER_NAME)));

    //Somebody else may log in
    hql = "from " + Person.class.getName() + " as person where person.myself is not null and person.myself.enabled = :truth";
    qh = new OqlQuery(hql);
    qh.getQuery().setBoolean("truth", true);
    qh.getQuery().setMaxResults(2);
    addStep("NOTALONE", "Somebody else may log in", 6, qh.list().size() > 1);

    //Created a custom form
    addStep("CUSTOMFORMS", "Created a custom form", 5, PluginBricks.getPageQuarks().size() > 4);

    //Created a custom field
    boolean youDidCustomize = false;
    for (int i = 1; i < 7; i++) {
      if (I18n.isActive("TASK_CUSTOM_FIELD_" + i)) {
        youDidCustomize = true;
        break;
      }
    }
    addStep("CUSTOMFIELD", "Created a custom field", 5, youDidCustomize);

    //Using custom pages
    addStep("CUSTOMPAGES", "Using custom pages", 8, new OqlQuery("from " + WebSitePage.class.getName() + " as page where page.name!='ROOT'").list().size() > 0);


    //A process instantiated
    addStep("PROCESSINSTANCE", "A process driven task created", 8, (Long)new OqlQuery("select count(pi) from " + ProcessInstance.class.getName() + " as pi").uniqueResult() > 0);


    //Default area renamed
    Area a = (Area) PersistenceHome.findUnique(Area.class, "name", "DEFAULT_AREA");
    addStep("DEFAULTAREARENAMED", "Default area renamed", 8, !JSP.ex(a));

    //reset admin.jsp passowrd
    addStep("DEFAULTADMINJSPPSWRENAMED", "Default admin.jsp password changed", 8, !"domagic".equalsIgnoreCase(PlatformConfiguration.psw));


    //Using a non-trial licence
    try {
      long daysRemaining = (License.getLicense().expires.getTime()- System.currentTimeMillis()) / CompanyCalendar.MILLIS_IN_DAY;
      addStep("VALIDLICENCE", "Using a non-trial license", 6, daysRemaining>90);
    } catch (Exception e) {
    }

    //Some company' news created
    addStep("USINGNEWS", "Some company' news created", 5, new OqlQuery("from " + News.class.getName() + " as news").list().size()>1);

    //Using a real DB
    //addStep("REALDBINUSE", "Using a real db", 7, !PersistenceContext.getDefaultPersistenceContext().persistenceConfiguration.driver_class.toLowerCase().contains("hsql"));

    //Some links created
    addStep("LINKCREATED", "Using links", 3, pageState.getLoggedOperator().getFavoriteUrls().size()>0);

    //Scheduler is running
    addStep("SCHEDULERRUNNING","Scheduler is running",3, Scheduler.isRunning());

    //customize print logo
    addStep("PRINTLOGO","Customized print logo",2, !"printLogo.gif".equalsIgnoreCase(ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_HOST)));

    //nobody shynch calendar & loggable user all with e-mail
    hql = "from " + Person.class.getName() + " as person where person.myself is not null and person.myself.enabled = :truth";
    qh = new OqlQuery(hql);
    qh.getQuery().setBoolean("truth", true);
    List<Person> perss=qh.list();
    boolean atLeastOneSynch=false;
    boolean atLeastOneHaveCustomFilter=false;
    boolean allLoggableHaveEmail=true;
    for (Person pers:perss){
      allLoggableHaveEmail=allLoggableHaveEmail && JSP.ex(pers.getDefaultEmail());
      atLeastOneSynch=atLeastOneSynch || Fields.TRUE.equals(pers.getMyself().getOption(OperatorConstants.SEND_EVENT_BY_ICAL));
    }
    addStep("SOMEONESYNCH", "At least one user synch calendar with ICAL", 7, atLeastOneSynch);
    addStep("EVERYLOGGABLEWITHEMAIL", "Every loggable has e-mail", 9, allLoggableHaveEmail);




  }

  public static ConfigurationStep addStep(String name, String description, double points, boolean done) {
    ConfigurationStep cs = new ConfigurationStep();
    cs.name = name;
    cs.description = description;
    cs.points = points;
    cs.done = done;
    configurationSteps.put(name, cs);
    return cs;
  }


  public static class ConfigurationStep {
    public String name;
    public String description;
    public ButtonSupport linkTo;
    public double points;
    public boolean done;
  }

}
