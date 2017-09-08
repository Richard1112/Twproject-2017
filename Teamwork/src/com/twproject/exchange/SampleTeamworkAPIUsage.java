package com.twproject.exchange;

import com.twproject.agenda.Event;
import com.twproject.messaging.stickyNote.StickyNote;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkArea;
import com.twproject.security.TeamworkPermissions;
import com.twproject.setup.WizardSupport;
import com.twproject.task.Assignment;
import com.twproject.task.Issue;
import com.twproject.task.Task;
import com.twproject.task.TaskStatus;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.hibernate.cfg.Environment;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.agenda.ScheduleWeekly;
import org.jblooming.anagraphicalData.AnagraphicalData;
import org.jblooming.messaging.Message;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.tracer.Tracer;
import org.jblooming.waf.constants.Fields;

import java.io.Serializable;
import java.util.Date;
import java.util.Properties;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Mar 28, 2007
 * Time: 2:25:15 PM
 */
public class SampleTeamworkAPIUsage {

  private static final String LOG_ROOT = "./";


  public static void main(String[] args) {
    boolean tCommitted = false;
    //arg[0] mustr contains the teamwork db property file. e.g. "c:\\twdb.properties"
    //arg[1] mustr contains the teamwork global property file. e.g. "c:\\global.properties"
    config(args[0], args[1]);
    PersistenceContext pc = null;
    try {

      //this puts the session on current thread, which makes writing queries simpler
      pc = PersistenceContext.getDefaultPersistenceContext();

      /**
       *  YOUR CODE STARTS HERE    ------------------------------------------------------------------------------------------------  START
       */

      // check if test operator already exists
      TeamworkOperator operator=null;
      Person person = null;
      try {
        operator = (TeamworkOperator) Operator.findByLoginName("TEST_LOGIN_NAME");
      } catch(PersistenceException p){
        Tracer.platformLogger.warn(p);
      }

      if (operator == null) {
        // create the person
        person = new Person();
        person.setIdAsNew();
        person.setPersonName("TEST_NAME");
        person.setPersonSurname("TEST_SURNAME");
        //person.setArea(logged.getDefaultAreaForPermission(TeamworkPermissions.task_canCreate));
        person.store();

        // create the operator
        operator = new TeamworkOperator();
        operator.setIdAsNew();
        operator.setName(person.getPersonName());
        operator.setSurname(person.getPersonSurname());
        operator.setLoginName("TEST_LOGIN_NAME");
        operator.changePassword("PASSWORD");
        operator.store();

        //     PersistenceSession persistenceSession = HibernateFactory.getPersistenceSession();
        //     persistenceSession.operator = operator;

        person.setMyself(operator);
        person.store();
      } else {
        person=operator.getPerson();
      }

      // create the area
      TeamworkArea area;
      area = new TeamworkArea();
      area.setIdAsNew();
      area.setName("TEST_AREA");
      //area.setOwner(operator);
      area.setOwner(operator);
      area.setFreeAccount(Fields.TRUE);
      area.setEnabledOperators(10);

      area.store();
      person.setArea(area);

      // create the AnagraphicalData
      AnagraphicalData data = new AnagraphicalData();
      data.setIdAsNew();
      data.setOrderFactor(1);
      data.setLocationDescription("default");
      data.setEmail("TESTEMAIL@test.com");
      data.store();
      person.getAnagraphicalDatas().add(data);

      // ---------------------------------- Area Manager ----------------------------------
      RoleTeamwork am = WizardSupport.getRole("AM","Area manager", area, operator);
      WizardSupport.setAreaManagerPermissions(am);
      am.store();

      //  ---------------------------------- Project manager ----------------------------------
      RoleTeamwork pm = WizardSupport.getRole("PM","Project manager", area, operator);
      WizardSupport.setProjectManagerPermissions(pm);
      pm.store();

      // ---------------------------------- Customer ----------------------------------
      RoleTeamwork so = WizardSupport.getRole("SH","Customer", area, operator);
      WizardSupport.setStakeholderPermissions(so);
      so.store();

      // ---------------------------------- Worker ----------------------------------
      RoleTeamwork wo = WizardSupport.getRole("WK","Worker", area, operator);
      WizardSupport.setWorkerPermissions(wo);
      wo.store();

      // ---------------------------------- Supervisor ----------------------------------
      RoleTeamwork reader = WizardSupport.getRole("SU","Supervisor", area, operator);
      WizardSupport.setSupervisorPermissions(reader);
      reader.store();

      // ---------------------------------- Operational ----------------------------------
      RoleTeamwork stdOperatorRole = WizardSupport.getRole("OP","Operational", area, operator);
      WizardSupport.setOperatorPermissions(stdOperatorRole);
      stdOperatorRole.store();

      // ---------------------------------- ProjectLauncher----------------------------------
      RoleTeamwork prjLauncher = WizardSupport.getRole("PL","Project launcher", area, operator);
      WizardSupport.setProjectLauncherPermissions(prjLauncher);
      prjLauncher.store();

      // set operator as AreaManager
      operator.addRoleAndPersist(am);

      operator.store();

      /*
      // creates a file storage
      FileStorage fs = new FileStorage();
      fs.setIdAsNew();
      fs.setName("File_Storage_" + emailAndLoginName);
      fs.setArea(area);
      fs.setAuthor(emailAndLoginName);
      fs.setCode("FS_" + emailAndLoginName);
      fs.setConnType(Document.ConnectionType.FS);
      new File("D:\\wwwroot\\onlineFilestorage\\" + emailAndLoginName).mkdirs();
      fs.setContent("D:\\wwwroot\\onlineFilestorage\\" + emailAndLoginName);
      fs.setOwner(administrator);
      fs.store();
      */

      // SEND a Message
      Operator systemOperator = null;
      Message m1 = new Message();
      m1.setIdAsNew();
      m1.setMedia(MessagingSystem.Media.STICKY.toString());
      m1.setToOperator(operator);
      m1.setFromOperator(systemOperator);
      m1.setReceived(new Date());
      m1.setMessageBody("TEST message");
      m1.store();

      // create area specific lookup
      WizardSupport.createFixedTypesForArea(area);

      // create a demo task
      Task task = new Task();
      task.setIdAsNew();
      task.setOwner(operator);
      task.setArea(area);
      task.setCode("TSK_1");
      task.setName("Project demo for " + operator.getName());
      task.setDescription("this is a demo project created for your test");
      CompanyCalendar cc = new CompanyCalendar();
      cc.setTimeInMillis(System.currentTimeMillis());
      cc.addWorkingDays(-5);
      Date start = cc.getTime();
      cc.addWorkingDays(20);
      Date end = cc.getTime();
      Period period = new Period(start, end);
      period.store();
      task.setDuration(25);
      task.setSchedule(period);
      task.setStatus(TaskStatus.STATUS_ACTIVE);
      task.setEndIsMilestone(true);
      task.setRelevance(75);
      task.setProgress(35);
      task.store();

      //child 1
      Task child = new Task();
      child.setIdAsNew();
      child.setOwner(operator);
      child.setArea(area);
      child.setCode("TSK_1.01");
      child.setName("Task first child demo for " + operator.getName());
      child.setDescription("this is a first demo task created for your test");
      period = new Period(start, end);
      period.store();
      child.setSchedule(period);
      child.setDuration(25);
      child.setEndIsMilestone(true);
      child.setStatus(TaskStatus.STATUS_ACTIVE);
      child.setEndIsMilestone(true);
      child.setParentAndStore(task);
      child.store();

      //child 2
      Task child2 = new Task();
      child2.setIdAsNew();
      child2.setOwner(operator);
      child2.setArea(area);
      child2.setCode("TSK_1.02");
      child2.setName("Task second child demo for " + operator.getName());
      child2.setDescription("this is a second demo task created for your test");
      period = new Period(start, end);
      period.store();
      child2.setDuration(25);
      child2.setSchedule(period);      
      child2.setEndIsMilestone(true);
      child2.setStatus(TaskStatus.STATUS_ACTIVE);
      child2.setEndIsMilestone(true);
      child2.setParentAndStore(task);
      child2.store();

      // create an assignment
      Assignment assig = new Assignment();
      assig.setIdAsNew();
      assig.setResource(person);
      assig.setRole(pm);
      assig.setTask(child);
      assig.setDescription("this is your assignment for this task");
      assig.setEstimatedWorklog(20*CompanyCalendar.MILLIS_IN_HOUR);
      assig.setHourlyCost(10);
      assig.setRisk(80);
      assig.store();


      StickyNote sn = new StickyNote();
      sn.setIdAsNew();
      sn.setReceiver(person);
      sn.setColor("#ffffa0");
      sn.setMessage("Sticky note example");
      sn.store();

      //issues on task
      Issue i = new Issue();
      i.setIdAsNew();
      i.setArea(operator);
      i.setDescription("First demo issue");
      i.setTask(child);
      i.setAssignedTo(person);
      i.setAssignedBy(person);
      i.setGravity(Issue.GRAVITY_HIGH);
      i.setEstimatedDuration((long)(CompanyCalendar.MILLIS_IN_HOUR*2.5));
      i.setStatusOpen();
      i.store();

      i = new Issue();
      i.setIdAsNew();
      i.setArea(operator);
      i.setDescription("Second demo issue");
      i.setTask(child);
      i.setAssignedTo(person);
      i.setAssignedBy(person);
      i.setGravity(Issue.GRAVITY_LOW);
      i.setEstimatedDuration((long)(CompanyCalendar.MILLIS_IN_HOUR*1.5));
      i.setStatusOpen();
      i.store();

      Serializable secondIssueId = i.getId();

      //agenda events
      Event e = new Event();
      e.setIdAsNew();
      e.setDescription("Daily meeting");
      e.setLocation("My office");
      e.setAuthor(person);
      e.getTargets().add(person);
      ScheduleWeekly s = new ScheduleWeekly(new int[]{CompanyCalendar.MONDAY, CompanyCalendar.TUESDAY, CompanyCalendar.WEDNESDAY, CompanyCalendar.THURSDAY, CompanyCalendar.FRIDAY},
              new Date(), (int) (CompanyCalendar.MILLIS_IN_HOUR * 9.5), (int) (CompanyCalendar.MILLIS_IN_HOUR), 1, 20);
      e.setSchedule(s);
      s.store();
      e.store();

      //commit the transaction: used only why we want to delete a just created entity
      HibernateFactory.checkPoint();

      //recover issue from db
      Issue issueToBeDeleted = Issue.load(secondIssueId +"");

      //remove the object
      issueToBeDeleted.remove();

      /**
       *  YOUR CODE ENDS HERE    ------------------------------------------------------------------------------------------------  START
       */
      pc.commitAndClose();

    } catch (Throwable e) {
      if (pc!=null)
        pc.rollbackAndClose();
      throw new RuntimeException(e);
    }
  }
  
  public static void config(String dbProperties, String globalProperties) {
    try {
      // logging
      configLog4J();
/*

      Tracer.platformLogger.debug("Loading global properties: "+globalProperties);
      File global = new File(globalProperties);
      if (!global.exists())
        throw new RuntimeException("Global Settings File Name points to a non existing file: "+globalProperties);
      Properties properties = FileUtilities.getProperties(globalProperties);

      ApplicationState.getApplicationSettings().putAll(properties);
      //PlatformConfiguration.applyGlobalSettingDefaults();


      Tracer.platformLogger.debug("Loading db properties: "+dbProperties);
      Properties p = new Properties();
      p.load(new FileInputStream(dbProperties));


      String driver_class = p.getProperty("driver");
      String driver_url = p.getProperty("url");
      Class dialect = Class.forName(p.getProperty("dialect"));
      String db_user_name = p.getProperty("user");
      String db_user_psw = p.getProperty("password");


      // hibernate
      if (!(dialect.newInstance() instanceof Dialect))
        throw new RuntimeException("Must configure dialect with a class extending org.hibernate.dialect.Dialect");

      Tracer.platformLogger.debug("Creating hibernate properties.");
      Properties hibInfo = new Properties();
      hibInfo.setProperty(Environment.QUERY_SUBSTITUTIONS, "true 1, false 0");

      hibInfo.setProperty(Environment.DIALECT, dialect.getName());

      hibInfo.setProperty(Environment.MAX_FETCH_DEPTH, "2");
      hibInfo.setProperty(Environment.USE_STREAMS_FOR_BINARY, "true");
      hibInfo.setProperty(Environment.STATEMENT_BATCH_SIZE, "15");


      hibInfo.setProperty(Environment.CONNECTION_PROVIDER, C3P0ConnectionProvider.class.getName());
      hibInfo.setProperty(Environment.RELEASE_CONNECTIONS, ConnectionReleaseMode.ON_CLOSE.toString());

      hibInfo.setProperty(Environment.CACHE_PROVIDER, HashtableCacheProvider.class.getName());
      hibInfo.setProperty(Environment.USE_REFLECTION_OPTIMIZER, "true");
      hibInfo.setProperty(Environment.USE_QUERY_CACHE, "true");

      //no connection isolation with hsqldb
      if (dialect.getName().indexOf("HSQL") == -1)
        hibInfo.setProperty(Environment.ISOLATION, "2");

      Tracer.platformLogger.debug("Creating pooling.");
      configPool(hibInfo, db_user_name, db_user_psw, driver_class, driver_url);

      PlatformAnnotationConfiguration hibConfiguration = new PlatformAnnotationConfiguration();

      //no connection isolation with hsqldb
      if (dialect.getName().indexOf("HSQL") == -1)
        hibInfo.setProperty(Environment.ISOLATION, "2");

      hibConfiguration.addProperties(hibInfo);
      hibConfiguration.setNamingStrategy(new OLNamingStrategy());

      Tracer.platformLogger.debug("Loading hibernate mapping: common.hbm.xml");
      URL common = HibernateFactory.class.getClassLoader().getResource("common.hbm.xml");
      hibConfiguration.addURL(common);
      hibConfiguration.addAnnotatedClass(PersistentText.class);

      Tracer.platformLogger.debug("Loading hibernate annotated class: ForumEntry");
      hibConfiguration.addAnnotatedClass(ForumEntry.class);

      Tracer.platformLogger.debug("Loading hibernate mapping: designer.hbm.xml");
      URL ce = HibernateFactory.class.getClassLoader().getResource("designer.hbm.xml");
      hibConfiguration.addURL(ce);

      Tracer.platformLogger.debug("Loading hibernate mapping: teamwork.hbm.xml");
      ce = HibernateFactory.class.getClassLoader().getResource("teamwork.hbm.xml");
      hibConfiguration.addURL(ce);


      Tracer.platformLogger.debug("Building hibernate session factory");
      HibernateFactory.setConfig(hibConfiguration);
      HibernateFactory.setSessionFactory(HibernateFactory.getConfig().buildSessionFactory());

      Tracer.platformLogger.info("Hibernate configuration completed.");
*/

    } catch (Exception e) {
      throw new RuntimeException(e);
    }


  }

  private static void configLog4J() {
    String pattern = "%d{yyyy MMM dd HH:mm:ss} %5p %c{1}:%L - %m%n";


    // how to log on file
    String logFilesRoot = LOG_ROOT;
    createLogger(pattern, logFilesRoot, Tracer.hibernateLogger, "hibConsoleAppender", "hibernate.log", "hibFileAppender");
    createLogger(pattern, logFilesRoot, Tracer.platformLogger, "platformConsoleAppender", "platform.log", "plaFileAppender");

    Tracer.hibernateLogger.setLevel(Level.ERROR);
    Tracer.platformLogger.setLevel(Level.DEBUG);

  }

  private static void createLogger(String pattern, String logFilesRoot, Logger logger, String consoleAppenderName, String logFileName, String fileAppenderName) {
    logger.removeAllAppenders();

    PatternLayout pl = new PatternLayout();
    pl.setConversionPattern(pattern);

    // log on console
    ConsoleAppender consoleAppender = new ConsoleAppender(pl);
    consoleAppender.setName(consoleAppenderName);
    logger.addAppender(consoleAppender);

    /*
    // log on file
    File log = new File(logFilesRoot + logFileName);
    if (!log.exists())
      log.getParentFile().mkdirs();
    FileAppender hibFileAppender = null;
    try {
      hibFileAppender = new FileAppender(pl, log.getPath());

    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    hibFileAppender.setName(fileAppenderName);
    logger.addAppender(hibFileAppender);
    */
  }


  public static void configPool(Properties hibInfo, String db_user_name, String db_user_psw, String driver_class, String driver_url) {

    hibInfo.setProperty(Environment.DRIVER, driver_class);
    hibInfo.setProperty(Environment.URL, driver_url);
    hibInfo.setProperty(Environment.USER, db_user_name);
    hibInfo.setProperty(Environment.PASS, db_user_psw);
    hibInfo.setProperty(Environment.POOL_SIZE, "1");
    hibInfo.setProperty(Environment.C3P0_ACQUIRE_INCREMENT, "1");
    hibInfo.setProperty(Environment.C3P0_IDLE_TEST_PERIOD, "300");
    hibInfo.setProperty(Environment.C3P0_MAX_SIZE, "100");
    hibInfo.setProperty(Environment.C3P0_MAX_STATEMENTS, "0");
    hibInfo.setProperty(Environment.C3P0_MIN_SIZE, "10");
    hibInfo.setProperty(Environment.C3P0_TIMEOUT, "300");


  }


}
