package com.twproject.waf.settings;

import java.io.File;
import java.io.FileFilter;
import java.net.URL;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.PageContext;

import org.hibernate.MappingException;
import org.hibernate.mapping.Column;
import org.hibernate.mapping.PersistentClass;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.designer.DesignerData;
import org.jblooming.flowork.FlowHibernateConfiguration;
import org.jblooming.flowork.FloworkApplication;
import org.jblooming.ldap.LdapUtilities;
import org.jblooming.logging.DeleteLog;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.hibernate.HibernateUtilities;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.persistence.hibernate.PlatformAnnotationConfiguration;
import org.jblooming.persistence.hibernate.PlatformSchemaUpdater;
import org.jblooming.persistence.hibernate.Release;
import org.jblooming.remoteFile.Document;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.security.Permissions;
import org.jblooming.security.businessLogic.LoginAction;
import org.jblooming.system.SystemConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.AccessControlFilter;
import org.jblooming.waf.PluginBricks;
import org.jblooming.waf.ScreenBasic;
import org.jblooming.waf.configuration.LoaderSupport;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.PersistenceConfiguration;
import org.jblooming.waf.settings.PlatformConfiguration;
import org.jblooming.waf.view.PageState;
import org.jbpm.graph.def.ProcessDefinition;

import com.opnlb.fulltext.IndexingConstants;
import com.opnlb.fulltext.IndexingHelper;
import com.opnlb.fulltext.IndexingMachine;
import com.opnlb.website.content.Content;
import com.opnlb.website.forum.ForumEntry;
import com.opnlb.website.page.WebSitePage;
import com.opnlb.website.security.WebSitePermissions;
import com.opnlb.website.waf.WebSiteConstants;
import com.teamwork.expand.TaskReport;
import com.twproject.agenda.DiscussionPointStatus;
import com.twproject.agenda.Event;
import com.twproject.document.TeamworkDocument;
import com.twproject.forum.TeamworkForumEntry;
import com.twproject.meeting.DiscussionPoint;
import com.twproject.meeting.DiscussionPointType;
import com.twproject.meeting.Meeting;
import com.twproject.messaging.board.Board;
import com.twproject.messaging.stickyNote.StickyNote;
import com.twproject.operator.TeamworkOperator;
import com.twproject.rank.Hit;
import com.twproject.resource.Company;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.scheduler.TeamworkJobsLauncher;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkPermissions;
import com.twproject.setup.SetupSupport;
import com.twproject.setup.WizardSupport;
import com.twproject.setup.businessLogic.TeamworkClassLoader;
import com.twproject.task.Assignment;
import com.twproject.task.AssignmentDataHistory;
import com.twproject.task.Issue;
import com.twproject.task.IssueHistory;
import com.twproject.task.IssueStatus;
import com.twproject.task.Task;
import com.twproject.task.TaskAudit;
import com.twproject.task.TaskAuditLog;
import com.twproject.task.TaskAuditReview;
import com.twproject.task.TaskAuditStatus;
import com.twproject.task.TaskAuditType;
import com.twproject.task.TaskCustomerField;
import com.twproject.task.TaskCustomerFieldRelation;
import com.twproject.task.TaskDataHistory;
import com.twproject.task.financial.Cost;
import com.twproject.task.process.TaskProcess;
import com.twproject.waf.TeamworkCommandController;
import com.twproject.waf.TeamworkHBFScreen;
import com.twproject.waf.TeamworkLoader;
import com.twproject.waf.TeamworkViewerBricks;
import com.twproject.worklog.Worklog;
import com.twproject.worklog.WorklogPlan;
import com.twproject.worklog.WorklogStatus;
import com.twproject.worklog.WorklogSupport;

import net.fortuna.ical4j.util.CompatibilityHints;
import pt.tumba.ngram.LanguageClass;

//import com.twproject.scheduler.TeamworkJobsLauncher;


public class TeamworkSettings extends FloworkApplication {

  public TeamworkSettings() {
    super(TeamworkOperator.class, new Permissions[]{new TeamworkPermissions(), new WebSitePermissions()});

    // ovverride the default command controller
    ApplicationState.commandController = TeamworkCommandController.class;

    ApplicationState.platformConfiguration.schedulerRunsByDefault = true;


    //override login cookie name
    LoginAction.cookieName="TEAMWORKCOOKIE";

    //override login cookie path
    LoginAction.cookiePath="/applications/teamwork/security";

    // configure views
    TeamworkViewerBricks tvb = new TeamworkViewerBricks();
    ApplicationState.entityViewers.put(Issue.class.getName(), tvb);
    ApplicationState.entityViewers.put(Task.class.getName(), tvb);
    ApplicationState.entityViewers.put(Person.class.getName(), tvb);
    ApplicationState.entityViewers.put(Company.class.getName(), tvb);
    ApplicationState.entityViewers.put(Resource.class.getName(), tvb);
    ApplicationState.entityViewers.put(Board.class.getName(), tvb);
    ApplicationState.entityViewers.put(Event.class.getName(), tvb);
    ApplicationState.entityViewers.put(TeamworkForumEntry.class.getName(), tvb);
    ApplicationState.entityViewers.put(Worklog.class.getName(), tvb);
    ApplicationState.entityViewers.put(WorklogPlan.class.getName(), tvb);
    ApplicationState.entityViewers.put(Meeting.class.getName(), tvb);
    ApplicationState.entityViewers.put(TeamworkDocument.class.getName(), tvb);

    ApplicationState.entityViewers.put(TaskReport.class.getName(), tvb);
  }

  /**
   * used in inerithed class
   */
  protected TeamworkSettings(Class defaultOperatoSubClass, Permissions[] permissionsImpl) {
    super(defaultOperatoSubClass, permissionsImpl);
  }

  @Override
public boolean isLoginCookieEnabled() {
    return !Fields.TRUE.equals(ApplicationState.getApplicationSetting(SystemConstants.DISABLE_COOKIE_LOGIN));
  }

  @Override
public String getName() {
    return "Teamwork";
  }

  @Override
public String getRootFolder() {
    return "applications/teamwork";
  }

  @Override
public void configurePersistence(PlatformConfiguration pc) throws Exception {
    try {
      new TeamworkClassLoader().getResource(Task.class.getName());

      PlatformAnnotationConfiguration hibConfiguration = (PlatformAnnotationConfiguration) HibernateFactory.getConfig();
      //hibConfiguration.setProperty(Environment.CACHE_PROVIDER, HashtableCacheProvider.class.getName());

      IndexingHelper.baseConfiguration(hibConfiguration);
      FlowHibernateConfiguration.configure(hibConfiguration);

      URL ce = HibernateFactory.class.getClassLoader().getResource("designer.hbm.xml");
      hibConfiguration.addURL(ce);

      ce = HibernateFactory.class.getClassLoader().getResource("flowork.hbm.xml");
      hibConfiguration.addURL(ce);

      ce = HibernateFactory.class.getClassLoader().getResource("teamwork.hbm.xml");
      hibConfiguration.addURL(ce);

      ce = HibernateFactory.class.getClassLoader().getResource("website.hbm.xml");
      hibConfiguration.addURL(ce);


      // custom mapping loading
      ce = HibernateFactory.class.getClassLoader().getResource("customMapping.hbm.xml");
      if (ce !=null)
        hibConfiguration.addURL(ce);


      hibConfiguration.addAnnotatedClass(WorklogSupport.class);
      hibConfiguration.addAnnotatedClass(Worklog.class);
      hibConfiguration.addAnnotatedClass(WorklogPlan.class);

      hibConfiguration.addAnnotatedClass(TaskProcess.class);
			hibConfiguration.addAnnotatedClass(TaskCustomerFieldRelation.class);
			hibConfiguration.addAnnotatedClass(TaskCustomerField.class);
			hibConfiguration.addAnnotatedClass(TaskAudit.class);
			hibConfiguration.addAnnotatedClass(TaskAuditLog.class);
			hibConfiguration.addAnnotatedClass(TaskAuditReview.class);
			hibConfiguration.addAnnotatedClass(TaskAuditStatus.class);
			hibConfiguration.addAnnotatedClass(TaskAuditType.class);

      hibConfiguration.addAnnotatedClass(ForumEntry.class);
      hibConfiguration.addAnnotatedClass(Issue.class);

      hibConfiguration.addAnnotatedClass(TeamworkForumEntry.class);
      hibConfiguration.addAnnotatedClass(Meeting.class);
      hibConfiguration.addAnnotatedClass(Event.class);
      hibConfiguration.addAnnotatedClass(DiscussionPoint.class);
      hibConfiguration.addAnnotatedClass(DiscussionPointType.class);
      hibConfiguration.addAnnotatedClass(DiscussionPointStatus.class);
      hibConfiguration.addAnnotatedClass(Hit.class);
      hibConfiguration.addAnnotatedClass(IssueHistory.class);
      hibConfiguration.addAnnotatedClass(IssueStatus.class);

      hibConfiguration.addAnnotatedClass(TaskDataHistory.class);
      hibConfiguration.addAnnotatedClass(AssignmentDataHistory.class);



    } catch (MappingException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
public void configureFreeAccess(PlatformConfiguration pc) {

    AccessControlFilter.freePatterns.add("createUserEnvironment.jsp");
    AccessControlFilter.freePatterns.add("forgotPassword.jsp");
    AccessControlFilter.freePatterns.add("systemCheck.jsp");
    AccessControlFilter.freePatterns.add("manageFavorites.jsp");
    AccessControlFilter.freePatterns.add("agendaInIcal.ics");

    AccessControlFilter.servletPath.add("/project/");
    AccessControlFilter.servletPath.add("/projects/");
    AccessControlFilter.servletPath.add("/task/");
    AccessControlFilter.servletPath.add("/tasks/");
    AccessControlFilter.servletPath.add("/widget/");

    //AccessControlFilter.freeFolders.add("mobile" + File.separator );
    //AccessControlFilter.freeFolders.add("mobile2");

    AccessControlFilter.freeFolders.add("applications" + File.separator + "teamwork" + File.separator + "test" + File.separator );

    //AccessControlFilter.freeFolders.add("applications" + File.separator + "teamwork" + File.separator + "plugins"+ File.separator +"customers"+ File.separator +"openLab"); // for Amazon callback

    //tutto il contenuto del "publicPage" folder è free
    AccessControlFilter.freeFolders.add("applications" + File.separator + "teamwork" + File.separator + "publicPage" );

    AccessControlFilter.freeFiles.add("applications" + File.separator + "teamwork" + File.separator + "task" + File.separator + "gantt" + File.separator + "widget.jsp");
    AccessControlFilter.freeFiles.add("applications" + File.separator + "teamwork" + File.separator + "security" + File.separator + "loginClient.jsp");

  }

  public void configureNotMonitored(PlatformConfiguration pc) {
  }

  public void configureLog(PlatformConfiguration pc) {
  }

  @Override
public void configureNeedingPersistence(PlatformConfiguration pc) {

    // language guess configuration
    try {
      LanguageClass lc = new LanguageClass(ApplicationState.webAppFileSystemRootPath + File.separator +
              "applications" + File.separator + "teamwork" + File.separator + "settings" + File.separator + "i18n" + File.separator + "languageGuess");
      ApplicationState.applicationParameters.put(IndexingConstants.LANGUAGE_GUESS, lc);
    } catch (Throwable e) {
      Tracer.logExceptionOnPlatformOrOther(e);
    }

    if (ApplicationState.applicationSettings.get(WebSiteConstants.HOME_PAGE) == null)
      ApplicationState.applicationSettings.put(WebSiteConstants.HOME_PAGE, "index.jsp");


    // --------------------------------------------------------------------------------------------- Start 4.0 ---------------------------------------------------------------------------------------
    Release rel400 = new Release("4.0.0");
    rel400.addPropertyToCheck(Issue.class, "orderFactorByResource", 0);
    //rel400.addPropertyToCheck(Affiliate.class, "orderFactor", 0);
    //removed as inserted in releas 4.1. rel400.addPropertyToCheck(Worklog.class, "status", 0);

    rel400.beforeHibSql.add(HibernateUtilities.generateDropForeignKey(Meeting.class, "fk_meeting_event"));

    rel400.beforeHibSql.add(HibernateUtilities.generateDropInxed(Meeting.class, "idx_meeting_event"));

    rel400.beforeHibSql.add("alter table twk_meeting drop column event");

    rel400.postHibSql.add("alter table twk_issue drop column estimatedDurationx");

    //this must be done after factory but before sqlSelect queries. so MUST be first executable
    rel400.addExec(new ExecutableSupport() {
      @Override
	public JobLogData run(JobLogData jobLog) throws Exception {
        SetupSupport.tw400UpdateDiscriminatorsAndBooleans();
        return jobLog;
      }
    }
    );


    rel400.addExec(new ExecutableSupport() {
      @Override
	public JobLogData run(JobLogData jobLog) throws Exception {
        SetupSupport.tw400UpdateForumEntry();
        return jobLog;
      }
    }
    );

    rel400.addExec(new ExecutableSupport() {
      @Override
	public JobLogData run(JobLogData jobLog) throws Exception {
        SetupSupport.tw400UpdateIssues();
        return jobLog;
      }
    });

    rel400.addExec(new ExecutableSupport() {
      @Override
	public JobLogData run(JobLogData jobLog) throws Exception {
        SetupSupport.tw400CopyRoleNamesInCodes();
        return jobLog;
      }
    }
    );

    // --------------------------------------------------------------------------------------------- Start 4.1 ---------------------------------------------------------------------------------------
    {
      Release rel410 = new Release("4.1.0");
      rel410.addPropertyToCheck(RoleTeamwork.class, "defaultSubscriptions", null);
      rel410.addPropertyToCheck(Resource.class, "hourlyCost", 0);

      rel410.beforeHibSql.add(HibernateUtilities.generateDropInxed(WorklogSupport.class, "idx_worklog_wklsts"));
      rel410.beforeHibSql.add("alter table twk_worklog drop column status");

      rel410.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw410UpdateMessages();
          return jobLog;
        }
      }
      );
      rel410.postHibSql.add("UPDATE twk_resource SET hourlyCostx = hourlyCost where hourlyCost is not null");
      rel410.postHibSql.add("alter table twk_resource drop column hourlyCost");
      rel410.postHibSql.add("alter table olpl_message drop column messageBody");
    }

    // --------------------------------------------------------------------------------------------- Start 4.2 ---------------------------------------------------------------------------------------
    {
      Release rel420 = new Release("4.2.0");

      //----- ISSUE ----
      rel420.addPropertyToCheck(Issue.class, "tags", null);
      rel420.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw420UpdateIssueArea();
          return jobLog;
        }
      }
      );
      rel420.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw420UpdateIssueTagsFromTypes();
          return jobLog;
        }
      }
      );

      //----- TASK ----
      rel420.addPropertyToCheck(Task.class, "tags", null);
      rel420.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw420UpdateTaskTagsFromClassification();
          return jobLog;
        }
      }
      );
      rel420.postHibSql.add("drop table twk_taskclassass");
      rel420.postHibSql.add("drop table twk_taskclass");


      //----- RESOURCE ----
      rel420.addPropertyToCheck(Resource.class, "tags", null);
      rel420.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw420UpdateResourceTagsFromClassification();
          return jobLog;
        }
      }
      );
      rel420.postHibSql.add("drop table twk_resourceclassass");
      rel420.postHibSql.add("drop table twk_resclass");


      //----- DOCUMENT ----
      rel420.addPropertyToCheck(TeamworkDocument.class, "tags", null);
      rel420.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw420UpdateDocumentAreaAndContent();
          return jobLog;
        }
      }
      );
      rel420.postHibSql.add("drop table twk_docclassass");
      rel420.postHibSql.add("drop table twk_docclass");
      rel420.postHibSql.add("alter table twk_document drop column content");


      // ------------ MEETING -----------------
      rel420.addPropertyToCheck(DiscussionPoint.class, "timeScheduled", "0");
      rel420.addPropertyToCheck(DiscussionPoint.class, "status", null);
      //rel420.addPropertyToCheck(DiscussionPoint.class, "documents", null);

      rel420.beforeHibSql.add(HibernateUtilities.generateDropForeignKey(Meeting.class, "fk_meeting_description"));
      rel420.beforeHibSql.add(HibernateUtilities.generateDropForeignKey(Meeting.class, "fk_meeting_minute"));

      rel420.beforeHibSql.add(HibernateUtilities.generateDropInxed(Meeting.class, "idx_meeting_description"));
      rel420.beforeHibSql.add(HibernateUtilities.generateDropInxed(Meeting.class, "idx_meeting_minute"));

      rel420.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw420UpdateMeetingEvents();
          return jobLog;
        }
      }
      );
      rel420.postHibSql.add("alter table twk_meeting drop column description");
      rel420.postHibSql.add("alter table twk_meeting drop column minutex");

    }
    {
      Release rel4210080 = new Release("4.2.10080");
      rel4210080.addPropertyToCheck(DiscussionPoint.class, "owner", null);
    }

    // --------------------------------------------------------------------------------------------- Start 4.3 ---------------------------------------------------------------------------------------
    {
      Release rel4314232 = new Release("4.3.14232");
      rel4314232.addPropertyToCheck(Assignment.class, "owner", null);
    }

    // --------------------------------------------------------------------------------------------- Start 4.4 ---------------------------------------------------------------------------------------
    {
      Release rel440 = new Release("4.4.0");
      rel440.addPropertyToCheck(Issue.class, "customField1", null);
    }

    // --------------------------------------------------------------------------------------------- Start 4.5 ---------------------------------------------------------------------------------------
    {
      Release rel450 = new Release("4.5.0");
      rel450.addPropertyToCheck(Resource.class, "workDailyCapacity", 0l);
      rel450.postHibSql.add("alter table twk_disc_point drop column documents");


      PersistenceContext pcpi = null;
      try {
        pcpi = new PersistenceContext();
        int length = getColumnLenght("twk_issue", "id", pcpi.session.connection());
        if (length < 255) {
          PersistentClass pclIssueHistory = HibernateUtilities.getClassMapping(IssueHistory.class);
          ((Column) pclIssueHistory.getProperty("issue").getColumnIterator().next()).setLength(length);
          PersistentClass pclIssue = HibernateUtilities.getClassMapping(Issue.class);
          ((Column) pclIssue.getProperty("id").getColumnIterator().next()).setLength(length);
        }
        pcpi.rollbackAndClose();
      } catch (Throwable t) {
        if (pcpi != null)
          pcpi.rollbackAndClose();
        Tracer.platformLogger.error(t);
      }


      rel450.addPropertyToCheck(IssueStatus.class, "orderBy", 0);
      rel450.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw450CreateDefaultIssueStatuses();
          SetupSupport.tw450ImportIssueStatuses();
          return jobLog;
        }
      }
      );


      rel450.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw450CopyWorkDailyCapacityFromOperatorOptions();
          //SetupSupport.tw450RemoveHTMLFromIssueNotes();
          return jobLog;
        }
      }
      );


    }

    {
      Release rel4517900 = new Release("4.5.17900");
      rel4517900.addPropertyToCheck(IssueHistory.class, "comment", null);

      rel4517900.postHibSql.add("UPDATE twk_issue_history SET commentx = comment where comment is not null");

      rel4517900.postHibSql.add("alter table twk_issue_history drop column comment");
    }


    // --------------------------------------------------------------------------------------------- Start 5.0 ---------------------------------------------------------------------------------------
    {
      Release rel500 = new Release("5.0.0");

      rel500.addPropertyToCheck(WorklogStatus.class, "color", "#a0a0a0");
      rel500.addPropertyToCheck(Assignment.class, "worklogDone", 0);

      rel500.addPropertyToCheck(Task.class, "totalWorklogDone", 0);
      rel500.addPropertyToCheck(Task.class, "totalWorklogEstimated", 0);
      rel500.addPropertyToCheck(Task.class, "totalEstimatedFromIssues", 0);
      rel500.addPropertyToCheck(Task.class, "totalCostsDone", 0.0);
      rel500.addPropertyToCheck(Task.class, "totalCostsEstimated", 0.0);

      rel500.addPropertyToCheck(Task.class, "totalIssues", 0);
      rel500.addPropertyToCheck(Task.class, "totalIssuesOpen", 0);
      rel500.addPropertyToCheck(Task.class, "totalIssuesScoreOpen", 0);
      rel500.addPropertyToCheck(Task.class, "totalIssuesScoreClosed", 0);

      rel500.addPropertyToCheck(Issue.class, "worklogDone", 0);


      rel500.beforeHibSql.add(HibernateUtilities.generateDropInxed(WebSitePage.class, "idx_wspage_wspage"));
      rel500.beforeHibSql.add(HibernateUtilities.generateDropInxed(WebSitePage.class, "idx_wspage_ancids"));
      rel500.beforeHibSql.add(HibernateUtilities.generateDropForeignKey(WebSitePage.class, "fk_wspage_wspage"));
      rel500.beforeHibSql.add("alter table olpl_ws_page drop column parent");

      rel500.beforeHibSql.add("alter table olpl_ws_page drop column ordinal");
      rel500.beforeHibSql.add("alter table olpl_ws_page drop column home");
      rel500.beforeHibSql.add("alter table olpl_ws_page drop column ancestorIds");


      rel500.beforeHibSql.add("alter table olpl_ws_news drop column discriminator");
      rel500.beforeHibSql.add("alter table olpl_ws_news drop column topNews");

      rel500.beforeHibSql.add("alter table olpl_ws_template drop column isDefault");
      rel500.beforeHibSql.add("alter table olpl_ws_template drop column installed");

      rel500.beforeHibSql.add("alter table twk_disc_point drop column timeEffective");

      rel500.beforeHibSql.add("alter table twk_assignment drop column loadFromPlan");
      rel500.beforeHibSql.add("alter table twk_assignment drop column loadFromEstimation");
      rel500.beforeHibSql.add("alter table twk_assignment drop column loadFromIssues");



      // updates denorm fields on Task, Issue, Assignment
      /*rel500.addExec(new ExecutableSupport() {
        public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw500UpdateTaskAssigIssueDenormFields();
          return jobLog;
        }
      }
      );*/
      rel500.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw500RecreatePages();
          return jobLog;
        }
      }
      );

    }


    // --------------------------------------------------------------------------------------------- Start 5.2 ---------------------------------------------------------------------------------------
    {
      Release rel520 = new Release("5.2.0");

      rel520.beforeHibSql.add("update twk_task set totalWorklogDone=0, totalWorklogEstimated=0, totalEstimatedFromIssues=0, totalCostsDone=0, totalCostsEstimated=0");

      rel520.addPropertyToCheck(WorklogSupport.class, "customField1", null);
      rel520.addPropertyToCheck(WorklogSupport.class, "customField2", null);
      rel520.addPropertyToCheck(WorklogSupport.class, "customField3", null);
      rel520.addPropertyToCheck(WorklogSupport.class, "customField4", null);

      rel520.addPropertyToCheck(Cost.class, "customField1", null);
      rel520.addPropertyToCheck(Cost.class, "customField2", null);
      rel520.addPropertyToCheck(Cost.class, "customField3", null);
      rel520.addPropertyToCheck(Cost.class, "customField4", null);

      rel520.addPropertyToCheck(Assignment.class, "budget", 0.0);

      // updates denorm fields on Task, Issue, Assignment
      rel520.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw500UpdateTaskAssigIssueDenormFields(null);
          return jobLog;
        }
      });


    }

    // --------------------------------------------------------------------------------------------- Start 5.3.0 ---------------------------------------------------------------------------------------
    {
      Release rel530 = new Release("5.3.0");

      rel530.addPropertyToCheck(IssueHistory.class, "owner", null);

      // updates owner from creator in issueHistory
      rel530.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw530UpdateIssueHistoryOwner();
          return jobLog;
        }
      });


    }

    // --------------------------------------------------------------------------------------------- Start 5.4.0 ---------------------------------------------------------------------------------------
    {
      Release rel540 = new Release("5.4.0");

      rel540.addPropertyToCheck(Task.class, "orderFactor", null);
      rel540.addPropertyToCheck(Resource.class, "customField1", null);
      rel540.beforeHibSql.add("alter table twk_task drop column commentOnClose");

    }

    // --------------------------------------------------------------------------------------------- Start 6.0.60000 ---------------------------------------------------------------------------------------
    {
      Release rel600 = new Release("6.0.0");

      rel600.addPropertyToCheck(StickyNote.class, "readOn", null);

      rel600.addPropertyToCheck(Issue.class, "jsonData", null);
      rel600.addPropertyToCheck(Event.class, "exceptions", null);

      rel600.beforeHibSql.add(HibernateUtilities.generateDropInxed(Content.class, "idx_content_template"));
      rel600.beforeHibSql.add(HibernateUtilities.generateDropForeignKey(Content.class, "fk_content_template"));
      rel600.beforeHibSql.add("alter table olpl_ws_content drop column template_id");

      //remove areas on template
      rel600.beforeHibSql.add(HibernateUtilities.generateDropForeignKey(Content.class, "ws_templare_aree_id"));
      rel600.beforeHibSql.add("drop table olpl_ws_template_aree");

      // indici sulle note
      rel600.beforeHibSql.add(HibernateUtilities.generateDropInxed(Issue.class, "idx_issue_notes"));
      rel600.beforeHibSql.add(HibernateUtilities.generateDropForeignKey(Issue.class, "fk_issue_notes"));

      //reset 'name' field of person as surname+name
      rel600.beforeHibSql.add("update twk_resource set name= personSurname +' '+personName where discriminator='PERSON'");

      //add LOG subscription
      rel600.beforeHibSql.add("update olpl_listener set media= media+',LOG' where media is not null and media<>'' and not media like '%LOG%' ");
      rel600.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw600AddDefaultSubscriptionsForRoles();
          return jobLog;
        }
      });

      //rimuove le url sulle issues
      rel600.beforeHibSql.add("alter table twk_issue drop column url1");
      rel600.beforeHibSql.add("alter table twk_issue drop column url2");

      //si copiano i type degli sticky su title
      rel600.beforeHibSql.add("update twk_stickynote set title= typex");
      rel600.beforeHibSql.add("update twk_stickynote set typex=null");


      // elimina tutte le forum entry create a vuoto, decidere se farla girare o no
      //rel600.beforeHibSql.add("delete from dbo.olpl_ws_forum where id not in (select parent from dbo.olpl_ws_forum where parent is not null) and parent is null");
      //rel600.beforeHibSql.add("update twk_task set forumEntry=null where forumEntry not in (select parent from dbo.olpl_ws_forum where parent is not null)");


      // copia le note e le mette nei commenti
      rel600.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw600IssueMoveNotesToComments();
          return jobLog;
        }
      });

      // mette screenShot e screenShot2 nel calderon
      rel600.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw600UpdateIssueScreenShots();
          return jobLog;
        }
      });

      //rimuovere screenshot
      rel600.postHibSql.add("alter table twk_issue drop column screenshot");
      rel600.postHibSql.add("alter table twk_issue drop column screenshot2");

      //rimuove colonna notes
      //rel600.postHibSql.add("alter table twk_issue drop column notes");

      //task spostano i dati da options a jsonData
      rel600.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw600UpdateTaskPublicPageOptions();
          return jobLog;
        }
      });
      rel600.postHibSql.add("alter table twk_task drop column options");


      rel600.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw600UpdatePersonNames();
          return jobLog;
        }
      });

      // sistema la wp_headline e la wp_summary
      rel600.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw600FixLayout();
          return jobLog;
        }
      });



    }

    // --------------------------------------------------------------------------------------------- Start 6.0.60011 ---------------------------------------------------------------------------------------
    {
      Release rel606011 = new Release("6.0.60011");
      rel606011.addPropertyToCheck(Cost.class, "status", null);
      rel606011.addPropertyToCheck(Cost.class, "attachment", null);

    }

    // --------------------------------------------------------------------------------------------- Start 6.0.60015 ---------------------------------------------------------------------------------------
    {
      Release rel606015 = new Release("6.0.60015");
      rel606015.addPropertyToCheck(Task.class, "budgetCustomField1", null);

      //aggiornamento permessi sui ruoli
      rel606015.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw6060015RolesUnifyPermissions();
          return jobLog;
        }
      });
    }

    // --------------------------------------------------------------------------------------------- Start 6.0.60018 ---------------------------------------------------------------------------------------
    {
      Release rel606018 = new Release("6.0.60018");
      rel606018.setUseSchemaBuildNumer();

      //aggiornamento permessi assegnazioni sui ruoli
      rel606018.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw6060018RolesUpdateAssigPermissions();
          return jobLog;
        }
      });
    }

    // --------------------------------------------------------------------------------------------- Start 6.1.61000 ---------------------------------------------------------------------------------------
    {
      Release rel616100 = new Release("6.1.61000");
      rel616100.addPropertyToCheck(Issue.class, "extRequesterEmail", null);
      rel616100.addPropertyToCheck(IssueHistory.class, "extRequesterEmail", null);
    }

    // --------------------------------------------------------------------------------------------- Start 6.1.61006 ---------------------------------------------------------------------------------------
    {
      Release rel616106 = new Release("6.1.61006");
      rel616106.addPropertyToCheck(DesignerData.class, "lastModified", null);
    }

    // --------------------------------------------------------------------------------------------- Start 6.2.62003 ---------------------------------------------------------------------------------------
    {
      Release rel6262003 = new Release("6.2.62003");
      rel6262003.addPropertyToCheck(Task.class, "color", null);
      rel6262003.addPropertyToCheck(DeleteLog.class, "deletedOn", null);

      //aggiornamento permessi assegnazioni sui ruoli
      rel6262003.addExec(new ExecutableSupport() {
        @Override
		public JobLogData run(JobLogData jobLog) throws Exception {
          SetupSupport.tw6262003RolesUpdate();
          return jobLog;
        }
      });


    }

      // --------------------------------------------------------------------------------------------- End of releases ----------------------------------------------------------------------------------

    try {
      PersistenceHome.isUpgradingSchema=true;
      PlatformSchemaUpdater.updateToLatestVersion();
    } catch (Throwable t){
      Tracer.logExceptionOnPlatformOrOther(t);
    } finally {
      PersistenceHome.isUpgradingSchema=false;  // per veritare che si rimanga in stato   "isUpgradingSchema" che inibisce il salvataggio del "lastModified"
    }


    if (PlatformSchemaUpdater.isSomeUpdateNeeded() || !Fields.TRUE.equals(ApplicationState.applicationSettings.get("SETUP_DB_UPDATE_DONE"))) {
      try {
        //beatiful hack for the fuc... PostgreSQL
        try {
          String dialectClassName = PersistenceConfiguration.getDefaultPersistenceConfiguration().dialect.getName();
          if (dialectClassName.indexOf("PostgreSQL") >= 0) {
            String fixCast = "CREATE OR REPLACE FUNCTION int_to_text(INT4) RETURNS TEXT AS '    SELECT textin(int4out($1));' LANGUAGE SQL STRICT IMMUTABLE;CREATE CAST (INT4 AS TEXT)    WITH FUNCTION int_to_text(INT4)    AS IMPLICIT;";
            Connection connection = PersistenceContext.getDefaultPersistenceContext().session.connection();
            PreparedStatement ps = connection.prepareStatement(fixCast);
            ps.execute();
            ps.close();
          }
        } catch (Throwable e) {
          Tracer.desperatelyLog(e.getMessage(), false, e);
        }
        PersistenceContext.getDefaultPersistenceContext().checkPoint();
        ((TeamworkLoader) LoaderSupport.myself).createTeamworkDefaultOperator(null);
        //((TeamworkLoader) LoaderSupport.myself).createTeamworkSystemOperator(null);
        PersistenceContext.getDefaultPersistenceContext().checkPoint();
        ApplicationState.applicationSettings.put("SETUP_DB_UPDATE_DONE", Fields.TRUE);

      } catch (Throwable e) {
        Tracer.logExceptionOnPlatformOrOther(e);
        throw new PlatformRuntimeException(e);
      }
    }

    //active media configuration (keep ABOVE below hence before launching jobs)
    MessagingSystem.activeMedia.add(MessagingSystem.Media.EMAIL);
    MessagingSystem.activeMedia.add(MessagingSystem.Media.DIGEST);
    MessagingSystem.activeMedia.add(MessagingSystem.Media.STICKY);
    MessagingSystem.activeMedia.add(MessagingSystem.Media.LOG);
    //MessagingSystem.activeMedia.add(MessagingSystem.Media.NEWS);
    //MessagingSystem.activeMedia.add(MessagingSystem.Media.RSS);


    if (!Fields.TRUE.equals(ApplicationState.applicationSettings.get(SystemConstants.SETUP_NOTIFIED_ADMIN_WIZARDS))) {
      ApplicationState.applicationSettings.put("JUST_SETUPPED", Fields.TRUE);
    }

    CompatibilityHints.setHintEnabled(CompatibilityHints.KEY_OUTLOOK_COMPATIBILITY, true);
    CompatibilityHints.setHintEnabled(CompatibilityHints.KEY_RELAXED_PARSING, true);
    CompatibilityHints.setHintEnabled(CompatibilityHints.KEY_RELAXED_UNFOLDING, true);
    CompatibilityHints.setHintEnabled(CompatibilityHints.KEY_RELAXED_VALIDATION, true);

    try {
      TeamworkJobsLauncher.launch("system");
    } catch (Exception e) {
      Tracer.logExceptionOnPlatformOrOther(e);
    }

    try {
      IndexingMachine.start();
    } catch (Throwable e) {
      Tracer.logExceptionOnPlatformOrOther(e);
    }

    try {
      LdapUtilities.loadLdapMappingFromFile();
    } catch (Throwable e) {
      Tracer.logExceptionOnPlatformOrOther(e);
    }

    Document.enabledConnectionTypes.add(Document.ConnectionType.FS);
    // Document.enabledConnectionTypes.add(Document.ConnectionType.DROPBOX);

    //conditional SVN activation
    try {
      Class.forName("org.tmatesoft.svn.core.io.diff.SVNDeltaGenerator").newInstance();
      Document.enabledConnectionTypes.add(Document.ConnectionType.SVN);
      Document.enabledConnectionTypes.add(Document.ConnectionType.SVN_Https);
      Document.enabledConnectionTypes.add(Document.ConnectionType.SVN_Http);

    } catch (Throwable e) {  }

      //conditional Amazon S3 activation
    try {
      Class.forName("com.amazonaws.services.s3.AmazonS3Client").newInstance();
      Document.enabledConnectionTypes.add(Document.ConnectionType.S3);

    } catch (Throwable e) {  }

      //conditional FTP activation
    try {
      Class.forName("org.apache.commons.net.ftp.FTPClient").newInstance();
      Document.enabledConnectionTypes.add(Document.ConnectionType.FTP);
    } catch (Throwable e) {}


      //conditional SFTP activation
    try {
      Class.forName("com.jcraft.jsch.JSch").newInstance();
      Document.enabledConnectionTypes.add(Document.ConnectionType.SFTP);
    } catch (Throwable e) {}


      // portlets loader



    ApplicationState.dumpApplicationSettings();

  }

  @Override
public ScreenBasic getDefaultScreenInstance() {
    return new TeamworkHBFScreen();
  }


  @Override
public void configureNeedingPageContext(PageContext pageContext) {

    //notify the admin of the existence of wizard
    boolean pagesJustCreated = false;
    if (!Fields.TRUE.equals(ApplicationState.applicationSettings.get(SystemConstants.SETUP_NOTIFIED_ADMIN_WIZARDS))) {
      try {
        WizardSupport.createSampleData();

        //load sample definitions

        //create sample news

        HttpServletRequest servletRequest = (HttpServletRequest) pageContext.getRequest();

        WizardSupport.createTemplatesAndWebSitePages();
        pagesJustCreated = true;

        WizardSupport.createSampleFluxDefinitions(PageState.getCurrentPageState(pageContext));

        ApplicationState.applicationSettings.put(SystemConstants.SETUP_NOTIFIED_ADMIN_WIZARDS, Fields.TRUE);

        //set intro as home page
        //ApplicationState.applicationSettings.put(WebSiteConstants.HOME_PAGE, "firstStart.page");
        ApplicationState.dumpApplicationSettings();


      } catch (Throwable e) {
        Tracer.logExceptionOnPlatformOrOther(e);
      }
    }

    /*/ --------------------- updater with page context
    for (Release r : PlatformSchemaUpdater.releases) {
      if ("5.0.0".equals(r.releaseLabel) && r.needsToBeLaunched) {
        try {
        } catch (Throwable t) {
          throw new PlatformRuntimeException(t);
        }
      }
    }*/



    //scan plugin folder for each customer
    String pathname = ApplicationState.webAppFileSystemRootPath + File.separator + this.getRootFolder() + File.separator + "customers";
    File customers = new File(pathname);
    customers.mkdirs();
    for (File pq : customers.listFiles()) {
      if (pq.isDirectory()) {
        File plugins = new File(pq, "plugins");
        if (plugins.exists() && plugins.isDirectory()) {
          PluginBricks.scanFolderAndInitializeQuarks("customers/" + pq.getName() + "/plugins", this, pageContext);
        }

      }
    }

    PluginBricks.scanFolderAndInitializeQuarks("plugins", this, pageContext);
    ReportBricks.loadAndCompileReports();

    //check if there is css custom
    File customersFolder = new File(ApplicationState.webAppFileSystemRootPath+"/applications/teamwork/customers");
    customersFolder.mkdirs();
    FileFilter filter = new FileFilter() {
      @Override
	public boolean accept(File pathname) {
        return pathname.isDirectory() || pathname.getName().endsWith(".css");
      }
    };
    Set<File> cssFilesSources = FileUtilities.getFilesRecursively(customersFolder, filter);

    for (File cssFile:cssFilesSources){
      try{
        String absolutePath = cssFile.getAbsolutePath();
        String path = absolutePath.substring(absolutePath.indexOf("customers"), absolutePath.length());
        ApplicationState.applicationSettings.put("customCSSFile",path );
        break;
      } catch (Throwable t){
        Tracer.platformLogger.error("Unable to add custom css to Application: "+cssFile.getAbsolutePath(),t);
      }
    }
  }

  @Override
protected void removeLocalDependencies(ProcessDefinition def) throws PersistenceException {
    /*String sqlSelect = "select pf from " + ProductFlux.class.getName() + " as pf where pf.fluxInstance.processDefinition=:pd";
    OqlQuery oql = new OqlQuery(sqlSelect);
    oql.getQuery().setEntity("pd", def);
    List<ProductFlux> pfl = oql.list();
    for (ProductFlux productFlux : pfl) {
      productFlux.remove();
    } */
  }

  public int getColumnLenght(String tablename, String columnName, Connection conn) throws SQLException {

    String sql = "select " + columnName + " from " + tablename;
    PreparedStatement select = conn.prepareStatement(sql); //is null or result = '' or result='KO'
    ResultSet result = select.executeQuery();
    ResultSetMetaData meta = result.getMetaData();
    int displaySize = meta.getColumnDisplaySize(1);
    result.close();
    select.close();
    return displaySize;

  }


}
