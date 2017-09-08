package com.twproject.setup;

import com.opnlb.website.content.Content;
import com.opnlb.website.forum.ForumEntry;
import com.opnlb.website.page.WebSitePage;
import com.opnlb.website.portlet.Portlet;
import com.twproject.agenda.Event;
import com.twproject.meeting.DiscussionPoint;
import com.twproject.meeting.DiscussionPointType;
import com.twproject.meeting.Meeting;
import com.twproject.resource.Resource;
import com.twproject.resource.Person;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.*;
import com.twproject.operator.TeamworkOperator;
import com.twproject.document.TeamworkDocument;
import com.twproject.worklog.Worklog;
import net.sf.json.JSONObject;
import org.hibernate.SessionFactory;
import org.hibernate.Query;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.*;
import org.jblooming.operator.Operator;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.remoteFile.Document;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.messaging.Listener;
import org.jblooming.messaging.Message;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.persistence.hibernate.Release;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.Job;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.security.Role;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.*;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.settings.PersistenceConfiguration;
import org.jblooming.waf.view.RestState;

import javax.servlet.jsp.JspWriter;
import java.sql.*;
import java.util.Date;
import java.util.*;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: May 11, 2007
 * Time: 4:26:26 PM
 */
public class SetupSupport {

  public static void tw307and310Update() {

    try {

      //3.0.7
      Release rel307 = new Release("3.0.7");
      rel307.addPropertyToCheck(Issue.class, "orderFactor", new Integer(0));
      rel307.addPropertyToCheck(Job.class, "lastExecutionTime", new Integer(0));
      rel307.addPropertyToCheck(Portlet.class, "params", "");

      //3.1.0
      Release rel310 = new Release("3.1.0");
      rel310.addPropertyToCheck(Task.class, "duration", 0);
      rel310.beforeHibSql.add("alter table twk_task drop column autoSynchPeriods");
      rel310.beforeHibSql.add("drop table olpl_joblog");

      //reset all parents due to _ bug
      rel310.getExecs().add(new ExecutableSupport() {

                              public JobLogData run(JobLogData jobLog) throws Exception {

                                PersistenceContext pc = null;
                                try {
                                  pc = PersistenceContext.getDefaultPersistenceContext();
                                  SessionFactory sf = HibernateFactory.getSessionFactory();
                                  Map acm = sf.getAllClassMetadata();
                                  Set keysAcm = acm.keySet();
                                  for (Iterator iterator = keysAcm.iterator(); iterator.hasNext(); ) {
                                    String className = (String) iterator.next();
                                    Class cln = null;
                                    try {
                                      cln = Class.forName(className);
                                      try {
                                        List<PerformantNodeSupport> nl = new OqlQuery("from " + cln.getName() + " as pfn where pfn.parent is not null", pc).list();
                                        for (PerformantNodeSupport n : nl) {
                                          n.setAncestorIds(StringUtilities.replaceAllNoRegex(n.getAncestorIds(), "_", "^"));
                                          n.store(pc);
                                        }
                                        pc.checkPoint();
                                      } catch (Throwable f) {
                                        jobLog.notes = f.getMessage();
                                      }
                                    } catch (ClassNotFoundException e) {
                                      jobLog.notes = e.getMessage();
                                    }
                                  }
                                } catch (Throwable t) {
                                  Tracer.desperatelyLog("", true, t);
                                } finally {
                                  if (pc != null)
                                    pc.commitAndClose();
                                }
                                return jobLog;
                              }
                            }

      );

      rel310.getExecs().add(new ExecutableSupport() {

                              public JobLogData run(JobLogData jobLog) throws Exception {
                                try {
                                  List<Resource> nl = new OqlQuery("from " + Resource.class.getName() + " as res where res.myManager is not null").list();
                                  for (Resource n : nl) {
                                    n.setMyManagerIds(StringUtilities.replaceAllNoRegex(n.getMyManagerIds(), "_", "^"));
                                    n.store();
                                  }
                                  HibernateFactory.checkPoint();
                                } catch (Exception e) {
                                  jobLog.notes = e.getMessage();
                                }
                                return jobLog;
                              }
                            }

      );

      //add duration
      rel310.getExecs().add(new ExecutableSupport() {

                              public JobLogData run(JobLogData jobLog) throws Exception {

                                String hql = "select task from " + Task.class.getName() + " as task where task.schedule is not null";
                                OqlQuery oql = new OqlQuery(hql);
                                List<Task> all = oql.list();
                                for (Task task : all) {
                                  int durInDays = CompanyCalendar.getWorkingDaysCountInPeriod(task.getSchedule());
                                  if (durInDays < Integer.MAX_VALUE)
                                    task.setDuration(durInDays);
                                  task.store();
                                }
                                HibernateFactory.checkPoint();
                                return jobLog;
                              }
                            }

      );

      //remove SchedulerLogKiller
      rel310.getExecs().add(new ExecutableSupport() {

                              public JobLogData run(JobLogData jobLog) throws Exception {
                                String hql = "select job from " + Job.class.getName() + " as job where job.name = :namex";
                                OqlQuery oql = new OqlQuery(hql);
                                oql.getQuery().setString("namex", "SchedulerLogKiller");
                                List<Job> all = oql.list();
                                for (Job job : all) {
                                  job.remove();
                                }
                                HibernateFactory.checkPoint();
                                return jobLog;
                              }
                            }

      );
      //end 3.1.0
    } catch (Exception e) {
      Tracer.platformLogger.fatal(e);
      e.printStackTrace();
      throw new PlatformRuntimeException(e);
    }
  }


  public static void tw320UpdateForum() throws Exception {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      List<ForumEntry> all = new OqlQuery("from " + ForumEntry.class.getName() + " as fe order by fe.id", pc).list();

      Connection conn = pc.session.connection();
      Statement statement = conn.createStatement();
      ResultSet rs = statement.executeQuery("select id, content from olpl_ws_forum order by id");
      for (ForumEntry fe : all) {
        rs.next();
        int id = rs.getInt("id");
        if (!fe.getId().equals(id))
          throw new PlatformRuntimeException("There are some evil presences here.");
        fe.setContent(rs.getString("content"));
        fe.store(pc);
      }
      rs.close();
      statement.close();

    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    } finally {
      if (pc != null)
        pc.commitAndClose();
    }
  }

  public static void tw320UpdateSchedules() throws Exception {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      Date now = new Date();
      Date endOfTimes = CompanyCalendar.MAX_DATE;
      List<Period> all = new OqlQuery("from " + Period.class.getName() + " as sc where sc.start is null or sc.end is null", pc).list();
      for (Period scs : all) {
        if (scs.getStartDate() == null && scs.getEndDate() == null) {
          scs.setStartDate(now);
          scs.setEndDate(endOfTimes);
        } else if (scs.getStartDate() != null && scs.getEndDate() == null) {
          scs.setEndDate(endOfTimes);
        } else if (scs.getStartDate() == null && scs.getEndDate() != null) {
          scs.setStartDate(scs.getEndDate());
        }
        scs.recomputeDuration();
        scs.store(pc);
      }

      CompanyCalendar cc = new CompanyCalendar();
      cc.setTime(now);
      int workingDays = 1;
      cc.addWorkingDays(workingDays);
      Date end = cc.getTime();
      List<Task> allT = new OqlQuery("from " + Task.class.getName() + " as t where t.schedule is null", pc).list();
      for (Task scs : allT) {
        Period p = new Period(now, end);
        p.store(pc);
        scs.setSchedule(p);
        scs.store(pc);
      }

    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    } finally {
      if (pc != null)
        pc.commitAndClose();
    }
  }

  public static void tw320UpdateListeners() throws Exception {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      OqlQuery query = new OqlQuery("from " + Listener.class.getName() + " as ls where ls.eventType = :iac", pc);
      query.getQuery().setString("iac", "TASK_ISSUE_ADDED_CLOSED");

      List<Listener> ls = query.list();
      for (Listener lo : ls) {
        lo.setEventType("TASK_ISSUE_ADDED");
        lo.store(pc);
        Listener l = new Listener(lo.getOwner());
        l.setIdAsNew();
        l.setTheClass(lo.getClass().getName());
        l.setIdentifiableId(lo.getIdentifiableId());
        l.setMedia(lo.getMedia());
        l.setEventType("TASK_ISSUE_CLOSED");
        l.setListenDescendants(lo.isListenDescendants());
        l.store(pc);
      }
    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    } finally {
      if (pc != null)
        pc.commitAndClose();
    }
  }


  public static void tw320UpdateRoles() throws Exception {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      List<Role> all = new OqlQuery("from " + Role.class.getName() + " as role", pc).list();
      for (Role role : all) {
        if (role.hasPermissionFor(TeamworkPermissions.fileStorage_canCreate))
          role.addPermission(TeamworkPermissions.fileStorage_explorer_canCreate);
        if (role.hasPermissionFor(TeamworkPermissions.fileStorage_canRead))
          role.addPermission(TeamworkPermissions.fileStorage_explorer_canRead);
        if (role.hasPermissionFor(TeamworkPermissions.fileStorage_canWrite)) {
          role.addPermission(TeamworkPermissions.fileStorage_explorer_canRead);
          role.addPermission(TeamworkPermissions.fileStorage_explorer_canCreate);
          role.addPermission(TeamworkPermissions.fileStorage_explorer_canWrite);
        }
        role.store(pc);
      }

    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    } finally {
      if (pc != null)
        pc.commitAndClose();
    }
  }

  public static void tw320UpdateIssues() throws Exception {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      List<Issue> all = new OqlQuery("from " + Issue.class.getName() + " as issue", pc).list();
      for (Issue i : all) {
        i.setDateSignalled(i.getCreationDate());
        i.store(pc);
      }
    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    } finally {
      if (pc != null)
        pc.commitAndClose();
    }
  }

  public static void tw320UpdateScheduleMinute() {
    Connection conn = null;
    try {
      conn = PersistenceContext.getNewConnection();
      conn.setAutoCommit(false);
      String sql = "UPDATE olpl_schedule SET onlyWorkingDays = ?  WHERE discriminator = 'U' AND onlyWorkingDays IS NULL";
      PreparedStatement ps = conn.prepareStatement(sql);
      ps.setObject(1, false);
      ps.execute();
      ps.close();
      conn.commit();
      conn.close();

    } catch (Exception e) {

      try {
        if (conn != null && !conn.isClosed())
          conn.close();
      } catch (SQLException e1) {
      }
      Tracer.platformLogger.error(e);
    }
  }

  public static void tw320ICalId() throws Exception {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      List<Event> all = new OqlQuery("from " + Event.class.getName(), pc).list();
      for (Event ev : all) {
        if (!JSP.ex(ev.getIcalId())) {
          ev.setIcalId("TW_" + ev.getId());
          ev.store(pc);
        }
      }

    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    } finally {
      if (pc != null)
        pc.commitAndClose();
    }
  }

  // 3.2.1

  public static void tw321CreateDiscussionPointsDefaultTypes() throws Exception {

    PersistenceContext pc = null;
    try {
      pc = new PersistenceContext();

      OqlQuery exs = new OqlQuery("select count(dp.id) from " + DiscussionPointType.class.getName() + " as dp", pc);
      long l = (Long) exs.uniqueResult();
      if (l == 0) {
        int i = 1;
        DiscussionPointType dpt;
        dpt = new DiscussionPointType();
        dpt.setCode("1");
        dpt.setDescription("Start");
        dpt.store(pc);

        dpt = new DiscussionPointType();
        dpt.setCode("2");
        dpt.setDescription("During");
        dpt.store(pc);

        dpt = new DiscussionPointType();
        dpt.setCode("3");
        dpt.setDescription("End");
        dpt.store(pc);

        dpt = new DiscussionPointType();
        dpt.setCode("4");
        dpt.setDescription("Final");
        dpt.store(pc);
      }
    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    } finally {
      if (pc != null)
        pc.commitAndClose();
    }
  }

  public static void tw321UpdateIssues() throws Exception {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      List<Issue> all = new OqlQuery("from " + Issue.class.getName() + " as fe order by fe.id", pc).list();

      Connection conn = pc.session.connection();
      Statement statement = conn.createStatement();
      ResultSet rs = statement.executeQuery("select id, description from twk_issue order by id");
      for (Issue fe : all) {
        rs.next();
        int id = rs.getInt("id");
        if (!(fe.getId() + "").equals(id + ""))
          throw new PlatformRuntimeException("There are some evil presences here.");
        fe.setDescription(rs.getString("description"));
        fe.store(pc);
      }
      rs.close();
      statement.close();

    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    } finally {
      if (pc != null)
        pc.commitAndClose();
    }
  }


  public static void tw400UpdateForumEntry() {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      String sql = "UPDATE olpl_ws_forum SET task = ? WHERE ancestorIds like ? or id = ?";
      PreparedStatement ps = pc.session.connection().prepareStatement(sql);
      int i = 0;
      List<Object[]> feIdtaskId = new OqlQuery("select task.forumEntry.id, task.id from " + Task.class.getName() + " as task", pc).list();
      for (Object[] feIdtaskIdOne : feIdtaskId) {
        ps.setString(1, (String) feIdtaskIdOne[1]);
        ps.setString(2, feIdtaskIdOne[0] + PerformantNode.SEPARATOR + "%");
        ps.setInt(3, (Integer) feIdtaskIdOne[0]);

        ps.execute();
        i++;
        if (i == 100) {
          pc.checkPoint();
          i = 0;
        }
      }

      ps.close();
      pc.commitAndClose();

    } catch (Throwable t) {
      if (pc != null)
        pc.rollbackAndClose();
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    }
  }

  public static void tw400CopyRoleNamesInCodes() {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      List<RoleTeamwork> all = new OqlQuery("from " + RoleTeamwork.class.getName() + " as r where r.code is null", pc).list();
      for (RoleTeamwork r : all) {
        String name = r.getName();
        if (name.equalsIgnoreCase("WORKER"))
          r.setCode("WK");
        else if (name.equalsIgnoreCase("PROJECT MANAGER"))
          r.setCode("PM");
        else if (name.toUpperCase().indexOf("STAKEHOLDER") > -1)
          r.setCode("SH");
        else if (name.equalsIgnoreCase("AREA MANAGER"))
          r.setCode("AM");
        else if (name.equalsIgnoreCase("Operational"))
          r.setCode("OP");
        else if (name.equalsIgnoreCase("Supervisor"))
          r.setCode("SU");
        else if (name.equalsIgnoreCase("Resource reader"))
          r.setCode("RR");
        else if (name.equalsIgnoreCase("Scrum Master"))
          r.setCode("SM");
        else if (name.equalsIgnoreCase("Product owner"))
          r.setCode("OW");
        else if (name.equalsIgnoreCase("Scrum Team"))
          r.setCode("TM");
        else
          r.setCode(r.getName());
      }

      pc.commitAndClose();

    } catch (Throwable t) {
      if (pc != null)
        pc.rollbackAndClose();
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    }
  }


  public static void tw400UpdateIssues() {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      String sql = "UPDATE twk_issue SET estimatedDuration = 0 where estimatedDuration is null";
      PreparedStatement ps = pc.session.connection().prepareStatement(sql);
      ps.execute();
      ps.close();

      sql = "UPDATE twk_issue SET orderfactorbyres = 0  where orderfactorbyres is null";
      ps = pc.session.connection().prepareStatement(sql);
      ps.execute();
      ps.close();

      pc.commitAndClose();

      pc = PersistenceContext.getDefaultPersistenceContext();

      List<Issue> all = new OqlQuery("select fe from " + Issue.class.getName() + " as fe order by fe.id", pc).list();

      Connection conn = pc.session.connection();
      Statement statement = conn.createStatement();
      ResultSet rs = statement.executeQuery("select id, estimatedDurationx from twk_issue order by id");
      int i = 0;
      for (Issue fe : all) {
        i++;
        rs.next();
        int id = rs.getInt("id");
        if (!(fe.getId() + "").equals(id + ""))
          throw new PlatformRuntimeException("There are some evil presences here.");
        String estDurX = rs.getString("estimatedDurationx");
        long estDur = 0;
        if (JSP.ex(estDurX)) {
          try {
            estDur = Long.parseLong(estDurX);
          } catch (NumberFormatException e) {
          }
        }
        fe.setEstimatedDuration(estDur);
        fe.store(pc);
        if (i == 100) {
          pc.checkPoint();
          i = 0;
        }
      }
      rs.close();
      statement.close();

      pc.commitAndClose();

    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
    }
  }


  public static void tw400UpdateDiscriminatorsAndBooleans() {

    PersistenceContext pc = null;

    try {

      pc = PersistenceContext.getDefaultPersistenceContext();
      Connection conn = pc.session.connection();

      String sql = "UPDATE olpl_ws_forum SET discriminator = 'TFE'";
      PreparedStatement ps = conn.prepareStatement(sql);
      ps.execute();
      ps.close();
      pc.commitAndClose();

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null) {
        pc.rollbackAndClose();
      }
    }

    /* try {

          pc= PersistenceContext.getDefaultPersistenceContext();
          Connection conn = pc.session.connection();
          String sql = "UPDATE olpl_ws_news SET discriminator = 'R'";
          PreparedStatement ps = conn.prepareStatement(sql);
          ps.execute();
          ps.close();
          pc.commitAndClose();

        } catch (Throwable t) {
          Tracer.desperatelyLog("", true, t);
          if (pc != null) {
            pc.rollbackAndClose();
          }
        }
    */
    try {

      pc = PersistenceContext.getDefaultPersistenceContext();
      Connection conn = pc.session.connection();
      String sql = "UPDATE twk_worklog SET discriminator = 'W' where discriminator is null";
      PreparedStatement ps = conn.prepareStatement(sql);
      ps.execute();
      ps.close();
      pc.commitAndClose();

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null) {
        pc.rollbackAndClose();
      }
    }

    try {

      pc = PersistenceContext.getDefaultPersistenceContext();
      Connection conn = pc.session.connection();
      String sql = "UPDATE twk_worklog SET status = 0 where status is null";
      PreparedStatement ps = conn.prepareStatement(sql);
      ps.execute();
      ps.close();
      pc.commitAndClose();

    } catch (Throwable t) {
      //Tracer.desperatelyLog("", true, t);
      if (pc != null) {
        pc.rollbackAndClose();
      }
    }

    try {

      pc = PersistenceContext.getDefaultPersistenceContext();
      Connection conn = pc.session.connection();
      String sql = "UPDATE twk_worklog SET hidden = ?";
      PreparedStatement ps = conn.prepareStatement(sql);
      ps.setBoolean(1, false);
      ps.execute();
      ps.close();
      pc.commitAndClose();

    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
      if (pc != null) {
        pc.rollbackAndClose();
      }
    }

  }


  public static void tw410UpdateMessages() throws Exception {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      List<Message> all = new OqlQuery("from " + Message.class.getName() + " as fe order by fe.id", pc).list();

      Connection conn = pc.session.connection();
      Statement statement = conn.createStatement();
      ResultSet rs = statement.executeQuery("select id, messageBody from olpl_message order by id");
      for (Message fe : all) {
        rs.next();
        int id = rs.getInt("id");
        if (!(fe.getId() + "").equals(id + ""))
          throw new PlatformRuntimeException("There are some evil presences here.");
        fe.setMessageBody(rs.getString("messageBody"));
        fe.store(pc);
      }
      rs.close();
      statement.close();

    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    } finally {
      if (pc != null)
        pc.commitAndClose();
    }
  }

  public static void tw420UpdateIssueArea() throws Exception {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      List<Issue> iss = new OqlQuery("select iss from " + Issue.class.getName() + " as iss", pc).getQuery().list();

      for (Issue issue : iss) {
        TeamworkOperator operator = (TeamworkOperator) ReflectionUtilities.getUnderlyingObject(issue.getOwner());
        issue.setArea(operator);
        issue.store(pc);
      }

    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    } finally {
      if (pc != null)
        pc.commitAndClose();
    }
  }

  public static void tw420UpdateIssueTagsFromTypes() throws Exception {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      Connection conn = pc.session.connection();
      Statement statement = conn.createStatement();
      ResultSet rs = statement.executeQuery("select i.id,t.description tag from twk_issue  i,olpl_lookup t where i.type=t.id");
      while (rs.next()) {
        String id = rs.getString("id");
        String tag = rs.getString("tag");
        Issue iss = (Issue) PersistenceHome.findByPrimaryKey(Issue.class, id, pc);
        iss.setTags(tag);
        iss.store(pc);
      }

      rs.close();
      statement.close();

    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    } finally {
      if (pc != null)
        pc.commitAndClose();
    }
  }

  public static void tw420UpdateTaskTagsFromClassification() throws Exception {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      Connection conn = pc.session.connection();
      Statement statement = conn.createStatement();
      ResultSet rs = statement.executeQuery("select task.id, class.code tag1, class.description tag2  from twk_task  task,twk_taskclass class, twk_taskclassass ass where task.id=ass.task and ass.classificationx=class.id order by task.id");
      String lastId = "";
      String tag = "";
      //prepare for key break
      if (rs.next()) {
        lastId = rs.getString("id");
        tag = (JSP.w(rs.getString("tag1")) + " " + JSP.w(rs.getString("tag2"))).trim();
      }
      while (rs.next()) {
        String id = rs.getString("id");
        //key broken
        if (!lastId.equals(id)) {
          //save the previous task
          Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, lastId, pc);
          task.setTags(tag);
          task.store(pc);
          lastId = id;
          tag = (JSP.w(rs.getString("tag1")) + " " + JSP.w(rs.getString("tag2"))).trim();
        } else {
          //accumulate tags
          tag = tag + (JSP.ex(tag) ? ", " : "") + (JSP.w(rs.getString("tag1")) + " " + JSP.w(rs.getString("tag2"))).trim();
        }
      }

      //save the last task
      Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, lastId, pc);
      if (task != null) {
        task.setTags(tag);
        task.store(pc);
      }

      rs.close();
      statement.close();

    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    } finally {
      if (pc != null)
        pc.commitAndClose();
    }
  }

  public static void tw420UpdateResourceTagsFromClassification() throws Exception {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      Connection conn = pc.session.connection();
      Statement statement = conn.createStatement();
      ResultSet rs = statement.executeQuery("select resource.id, class.code tag1, class.description tag2  from twk_resource  resource,twk_resclass class, twk_resourceclassass ass where resource.id=ass.resourcex and ass.classificationx=class.id order by resource.id");
      String lastId = "";
      String tag = "";
      //prepare for key break
      if (rs.next()) {
        lastId = rs.getString("id");
        tag = (JSP.w(rs.getString("tag1")) + " " + JSP.w(rs.getString("tag2"))).trim();
      }
      while (rs.next()) {
        String id = rs.getString("id");
        //key broken
        if (!lastId.equals(id)) {
          //save the previous resource
          Resource resource = (Resource) PersistenceHome.findByPrimaryKey(Resource.class, lastId, pc);
          resource.setTags(tag);
          resource.store(pc);
          lastId = id;
          tag = (JSP.w(rs.getString("tag1")) + " " + JSP.w(rs.getString("tag2"))).trim();
        } else {
          //accumulate tags
          tag = tag + (JSP.ex(tag) ? ", " : "") + (JSP.w(rs.getString("tag1")) + " " + JSP.w(rs.getString("tag2"))).trim();
        }
      }

      //save the last resource
      Resource resource = (Resource) PersistenceHome.findByPrimaryKey(Resource.class, lastId, pc);
      if (resource != null) {
        resource.setTags(tag);
        resource.store(pc);
      }

      rs.close();
      statement.close();

    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    } finally {
      if (pc != null)
        pc.commitAndClose();
    }
  }


  public static void tw420UpdateDocumentAreaAndContent() throws Exception {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      Connection conn = pc.session.connection();
      Statement statement = conn.createStatement();
      ResultSet rs = statement.executeQuery("select id, discriminator, content from twk_document");
      while (rs.next()) {
        String id = rs.getString("id");
        String discr = rs.getString("discriminator");

        if ("TWDOC".equalsIgnoreCase(discr)) {
          TeamworkDocument td = (TeamworkDocument) PersistenceHome.findByPrimaryKey(TeamworkDocument.class, id);
          td.setContent(rs.getString("content"));

          if (td.getArea() == null) {
            if (td.getTask() != null && td.getTask().getArea() != null) {
              td.setArea(td.getTask().getArea());
              td.store(pc);
            } else if (td.getResource() != null && td.getResource().getArea() != null) {
              td.setArea(td.getResource().getArea());
              td.store(pc);
            }
          }
        } else {
          Document td = (Document) PersistenceHome.findByPrimaryKey(Document.class, id);
          td.setContent(rs.getString("content"));
          td.store();
        }
      }
      rs.close();
      statement.close();
    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    } finally {
      if (pc != null)
        pc.commitAndClose();
    }
  }

  public static void tw420UpdateMeetingEvents() {

    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      List<Event> all = new OqlQuery("from " + Event.class.getName() + " as e order by e.id", pc).list();

      Connection conn = pc.session.connection();
      Statement statement = conn.createStatement();
      Statement statement2 = conn.createStatement();

      /* copy the meeting description into descriptionx in event */
      ResultSet rs = statement.executeQuery("select m.id, t.text from twk_meeting m join olpl_persistentText t on m.description = t.id  order by m.id");
      while (rs.next()) {
        for (Event fe : all) {
          int id = rs.getInt("id");
          if (fe.getMeeting() != null && fe.getMeeting().getId().equals(id)) {

            if (rs.getClob(2) != null) {
              String str = FileUtilities.readReader(rs.getClob(2).getCharacterStream());
              //FileUtilities.readInputStream(rs.getClob(2).getAsciiStream(), sb);
              fe.setDescription(JSP.limWr(JSP.w(fe.getDescription()) + (JSP.ex(str) ? "\n " + str : ""), 2000));
            }

          }
        }
      }

      for (Event fe : all) {
        fe.store(pc);
      }
      rs.close();
      //rs2.close();
      statement2.close();
      statement.close();
      pc.commitAndClose();

    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
    }


    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      Connection conn = pc.session.connection();
      Statement statement = conn.createStatement();

      /* copy the meeting minute into its first discussion point  */
      List<Meeting> meet = new OqlQuery("from " + Meeting.class.getName() + " as m order by m.id", pc).list();
      ResultSet rs1 = statement.executeQuery("select m.id , " +
        "p.text as text from olpl_persistentText p join twk_meeting m on m.minutex = p.id order by p.id");
      while (rs1.next()) {
        for (Meeting m : meet) {
          int id = rs1.getInt("id");
          if (m.getId().equals(id)) {
            if (m.getDiscussionPoints().size() > 0) {
              m.getDiscussionPoints().iterator().next().setMinute(rs1.getString("text"));
            } else {
              DiscussionPoint d = new DiscussionPoint();
              d.setTitle("-");
              if (rs1.getClob(2) != null) {

                //StringBuffer sb = new StringBuffer();
                //FileUtilities.readInputStream(rs1.getClob(2).getAsciiStream(), sb);

                String str = FileUtilities.readReader(rs1.getClob(2).getCharacterStream());

                if (JSP.ex(str)) {
                  d.setMinute(str);
                }
              }
              d.setMeeting(m);
              d.store(pc);
              m.getDiscussionPoints().add(d);
            }
            m.store(pc);
          }
        }
      }
      rs1.close();
      statement.close();
      pc.commitAndClose();

    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
    }
  }

  public static void tw450CreateDefaultIssueStatuses() {

    Tracer.platformLogger.debug("tw450CreateDefaultIssueStatuses started");
    PersistenceContext pc = null;
    try {
      pc = new PersistenceContext();

      List<IssueStatus> iss = new OqlQuery("from " + IssueStatus.class.getName(), pc).list();
      if (iss.size() == 0) {

        IssueStatus open = new IssueStatus();
        open.setDescription("open");
        open.setColor("#3BBF67");
        open.setOrderBy(1);
        open.setBehavesAsOpen(true);
        open.setBehavesAsClosed(false);
        open.setAskForWorklog(false);
        open.setAskForComment(false);
        open.store(pc);

        IssueStatus paused = new IssueStatus();
        paused.setDescription("paused");
        paused.setColor("#F9C154");
        paused.setOrderBy(2);
        paused.setBehavesAsOpen(false);
        paused.setBehavesAsClosed(false);
        paused.setAskForWorklog(false);
        paused.setAskForComment(false);
        paused.store(pc);

        IssueStatus intest = new IssueStatus();
        intest.setDescription("in test");
        intest.setColor("#FF9900");
        intest.setOrderBy(3);
        intest.setBehavesAsOpen(true);
        intest.setBehavesAsClosed(false);
        intest.setAskForWorklog(true);
        intest.setAskForComment(false);
        intest.store(pc);

        IssueStatus closed = new IssueStatus();
        closed.setDescription("closed");
        closed.setColor("#6EBEF4");
        closed.setOrderBy(4);
        closed.setBehavesAsOpen(false);
        closed.setBehavesAsClosed(true);
        closed.setAskForWorklog(true);
        closed.setAskForComment(false);
        closed.store(pc);

        IssueStatus aborted = new IssueStatus();
        aborted.setDescription("aborted");
        aborted.setColor("#763A96");
        aborted.setOrderBy(5);
        aborted.setBehavesAsOpen(false);
        aborted.setBehavesAsClosed(true);
        aborted.setAskForWorklog(true);
        aborted.setAskForComment(true);
        aborted.store(pc);

      }

      pc.commitAndClose();
      Tracer.platformLogger.debug("tw450CreateDefaultIssueStatuses ended");

    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
    }
  }


  public static void tw450ImportIssueStatuses() {
    Tracer.platformLogger.debug("tw450ImportIssueStatuses started");
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      Connection conn = pc.session.connection();
      Statement statement = conn.createStatement();


      /*"01_STATUS_OPEN";
      "02_STATUS_PAUSED"
      "03_STATUS_ABORTED
      "04_STATUS_CLOSED"
      "05_STATUS_INTEST"*/

      IssueStatus open = (IssueStatus) PersistenceHome.findUnique(IssueStatus.class, "description", "open", pc);
      IssueStatus paused = (IssueStatus) PersistenceHome.findUnique(IssueStatus.class, "description", "paused", pc);
      IssueStatus inTest = (IssueStatus) PersistenceHome.findUnique(IssueStatus.class, "description", "in test", pc);
      IssueStatus closed = (IssueStatus) PersistenceHome.findUnique(IssueStatus.class, "description", "closed", pc);
      IssueStatus aborted = (IssueStatus) PersistenceHome.findUnique(IssueStatus.class, "description", "aborted", pc);

      if (open != null)
        statement.execute("update twk_issue set statusx=" + open.getId() + " where status='01_STATUS_OPEN'");

      if (paused != null)
        statement.execute("update twk_issue set statusx=" + paused.getId() + " where status='02_STATUS_PAUSED'");

      if (aborted != null) {
        statement.execute("update twk_issue set statusx=" + aborted.getId() + " where status='02_STATUS_ABORTED'");
        statement.execute("update twk_issue set statusx=" + aborted.getId() + " where status='03_STATUS_ABORTED'");
      }

      if (closed != null)
        statement.execute("update twk_issue set statusx=" + closed.getId() + " where status='04_STATUS_CLOSED'");

      if (inTest != null)
        statement.execute("update twk_issue set statusx=" + inTest.getId() + " where status='05_STATUS_INTEST'");


      conn.commit();

      //drop column old status
      statement.execute("alter table twk_issue drop column status");
      statement.execute("alter table twk_issue drop column votes");

      statement.close();


      /* create a minimal history  HERE JUST IN CASE
      String sqlSelect= "select iss from "+ Issue.class.getName()+ " as iss where iss.status.behavesAsClosed=true";
      List<Issue> list = new OqlQuery(sqlSelect,pc).getQuery().list();

      int i=0;
      for (Issue iss:list){
        if (iss.getIssueHistories().size()==0){
          IssueHistory history = new IssueHistory(iss);
          history.setOldStatus(open);
          history.setStatus(iss.getStatus());
          history.setCreationDate(iss.getLastStatusChangeDate());
          history.store();
          i++;

          if (i>200){
            i=0;
            pc.checkPoint();
          }
        }
      }
      */

      pc.commitAndClose();

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
    }

    Tracer.platformLogger.debug("tw450ImportIssueStatuses ended");


  }

  public static void tw450CopyWorkDailyCapacityFromOperatorOptions() {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      {
        Tracer.platformLogger.debug("tw450CopyWorkDailyCapacityFromOperatorOptions started");
        long defaultWorkingHourPerDay = CompanyCalendar.MILLIS_IN_WORKING_DAY;
        String hql = "update " + Resource.class.getName() + " res set res.workDailyCapacity=:wdc";
        Query query = new OqlQuery(hql).getQuery();
        query.setLong("wdc", defaultWorkingHourPerDay);
        query.executeUpdate();
        pc.checkPoint();
      }


      //set workDailyCapacity to Person with operator
      String hql = "select p from " + Person.class.getName() + " p where p.myself is not null";
      Query query = new OqlQuery(hql).getQuery();
      List<Person> list = query.list();

      int i = 0;
      for (Person p : list) {
        Tracer.platformLogger.debug("tw450CopyWorkDailyCapacityFromOperatorOptions id:" + p.getId());
        String wot = p.getMyself().getOption(OperatorConstants.FLD_WORKING_HOUR_TOTAL);
        try {
          long capacity = DateUtilities.millisFromHourMinuteSmart(wot);
          Tracer.platformLogger.debug("tw450CopyWorkDailyCapacityFromOperatorOptions capacity:" + capacity);
          p.setWorkDailyCapacity(capacity);
          p.store(pc);

          i++;
          if (i % 50 == 0)
            pc.checkPoint();
        } catch (NumberFormatException e) {
        }
      }

      pc.commitAndClose();
      Tracer.platformLogger.debug("tw450CopyWorkDailyCapacityFromOperatorOptions ended");

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
    }

  }


  /*
  public static void tw450RemoveHTMLFromIssueNotes() {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      String hql = "select iss from " + Issue.class.getName() + " iss where iss.notes is not null";
      Query query = new OqlQuery(hql, pc).getQuery();
      List<Issue> list = query.list();

      String[] searches = {"<br>", "<BR>"};
      String[] replaces = {"\\n", "\\n"};

      Pattern allowedTags = Pattern.compile("^(br)$");
      for (Issue i : list) {
        PersistentText persistentText = i.getNotes();
        if (JSP.ex(persistentText) && JSP.ex(persistentText.getText())) {
          String sanitized = HtmlSanitizer.sanitizer(persistentText.getText(), allowedTags, HtmlSanitizer.forbiddenTags).html;
          sanitized = StringUtilities.replaceAllNoRegex(sanitized, searches, replaces);
          persistentText.setText(sanitized);
          persistentText.store(pc);
        }

      }

      pc.commitAndClose();

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
    }

  }*/


  public static void tw4517900UpdateIssueHistoryComment() throws PersistenceException {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      Connection conn = pc.session.connection();
      Statement statement = conn.createStatement();
      ResultSet rs = statement.executeQuery("select id, comment from twk_issue_history where comment is not null");
      while (rs.next()) {
        String id = rs.getString("id");
        IssueHistory iH = (IssueHistory) PersistenceHome.findByPrimaryKey(IssueHistory.class, id);
        iH.setComment(rs.getString("comment"));
        iH.store(pc);
      }
      rs.close();
      statement.close();
    } catch (Throwable t) {
      if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
        Tracer.desperatelyLog("", true, t);
    } finally {
      if (pc != null)
        pc.commitAndClose();
    }
  }


  public static void tw500RecreatePages() {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      WizardSupport.createTemplatesAndWebSitePages();

      pc.commitAndClose();

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
      PersistenceContext.getDefaultPersistenceContext();
    }
  }



  public static void tw500UpdateTaskAssigIssueDenormFields() {
    tw500UpdateTaskAssigIssueDenormFields(null);
  }
  public static void tw500UpdateTaskAssigIssueDenormFields(JspWriter out) {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      long now = System.currentTimeMillis();

      //update all worklog done on assignments
      int counter = 1;
      String hql = "select w.assig.id, sum(w.duration) from " + Worklog.class.getName() + " as w where w.assig.worklogDone=0 group by w.assig.id";
      List<Object[]> assIdWl = new OqlQuery(hql).list();
      pc.commitAndClose();
      pc = PersistenceContext.getDefaultPersistenceContext();
      counter = assIdWl.size();
      String message = "Updating assignment.workLogDone: " + counter + " ms:" + (System.currentTimeMillis() - now);
      Tracer.platformLogger.debug(message);
      if (out!=null) out.println(message+"<br>");
      for (Object[] o : assIdWl) {
        String id = (String) o[0];
        //Assignment ass = (Assignment) PersistenceHome.findByPrimaryKey(Assignment.class,id, pc);
        Assignment ass = Assignment.load(id);
        ass.setWorklogDone((Long) o[1]);
        ass.store();
        if (counter % 200 == 0) {
          pc.commitAndClose();
          pc = PersistenceContext.getDefaultPersistenceContext();
          message = "Updating assignment.workLogDone: " + counter + " ms:" + (System.currentTimeMillis() - now);
          Tracer.platformLogger.debug(message);
          if (out!=null) out.println(message+"<br>");
          now = System.currentTimeMillis();
        }
        counter--;
      }
      pc.commitAndClose();
      message = "Updating assignment.workLogDone: COMPLETED - " + (System.currentTimeMillis() - now);
      Tracer.platformLogger.debug(message);
      if (out!=null) out.println(message+"<br>");

      //update all on issues not already touched
      pc = PersistenceContext.getDefaultPersistenceContext();
      hql = "select i.id from " + Issue.class.getName() + " as i where i.task is not null and i.task.totalIssues=0 order by i.task.id";
      List<String> isss = new OqlQuery(hql).list();
      pc.commitAndClose();
      pc = PersistenceContext.getDefaultPersistenceContext();
      counter = isss.size();
      message = "Updating issue.workLogDone: " + counter + " ms:" + (System.currentTimeMillis() - now);
      Tracer.platformLogger.debug(message);
      if (out!=null) out.println(message+"<br>");

      for (String issueId : isss) {
        Issue issue = Issue.load(issueId);
        issue.updateWorklogDone();
        issue.store();   // qui vengono aggiornati anche i contatori degli score issues sul task
        if (counter % 200 == 0) {
          pc.commitAndClose();
          pc = PersistenceContext.getDefaultPersistenceContext();
          message = "Updating issue.workLogDone: " + counter + " ms:" + (System.currentTimeMillis() - now);
          Tracer.platformLogger.debug(message);
          if (out!=null) out.println(message+"<br>");
          now = System.currentTimeMillis();
        }
        counter--;
      }
      pc.commitAndClose();
      message = "Updating issue.workLogDone: COMPLETED - " + (System.currentTimeMillis() - now);
      Tracer.platformLogger.debug(message);
      if (out!=null) out.println(message+"<br>");


      //update all task leaves; propagation to top is done internally
      pc = PersistenceContext.getDefaultPersistenceContext();
      hql = "select t.id from " + Task.class.getName() + " as t where t.totalCostsEstimated=0 and t.totalCostsDone=0 and t.totalWorklogEstimated=0 and t.totalWorklogDone=0 and t.parent is not null and size(t.children)=0 order by t.id";
      List<String> ts = new OqlQuery(hql).list();
      pc.commitAndClose();
      pc = PersistenceContext.getDefaultPersistenceContext();
      counter = ts.size();
      message = "Updating task denormalized fields: " + counter + " ms:" + (System.currentTimeMillis() - now);
      Tracer.platformLogger.debug(message);
      if (out!=null) out.println(message+"<br>");
      for (String tid : ts) {
        Task t = Task.load(tid);
        t.store();
        if (counter % 200 == 0) {
          pc.commitAndClose();
          pc = PersistenceContext.getDefaultPersistenceContext();
          message = "Updating task denormalized fields: " + counter + " ms:" + (System.currentTimeMillis() - now);
          Tracer.platformLogger.debug(message);
          if (out!=null) out.println(message+"<br>");
          now = System.currentTimeMillis();
        }
        counter--;
      }
      pc.commitAndClose();

      pc = PersistenceContext.getDefaultPersistenceContext();
      message = "Updating task denormalized fields: COMPLETED - " + (System.currentTimeMillis() - now);
      Tracer.platformLogger.debug(message);
      if (out!=null) out.println(message+"<br>");


    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
      PersistenceContext.getDefaultPersistenceContext();
    }
  }

  public static void tw530UpdateIssueHistoryOwner() {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      //select distinct issueHistory creators in one query
      String hql = "select distinct h.creator from " + IssueHistory.class.getName() + " as h";
      List<String> creators = new OqlQuery(hql).list();


      for (String creator : creators) {
        // dal nome trovare il vero operatore
        Operator o = Operator.findByLoggableName(creator);

        if (o != null) {
          //si fa una sola query di update
          hql = "update " + IssueHistory.class.getName() + " set owner=:own where creator=:creat";
          Query query = new OqlQuery(hql).getQuery();
          query.setEntity("own", o);
          query.setString("creat", creator);
          int i = query.executeUpdate();
          Tracer.platformLogger.info("Updating issueHistory: updating user '" + creator + "' to " + o.getDisplayName() + " " + i + " row affected");

        } else {
          Tracer.platformLogger.warn("Updating issueHistory: user '" + creator + "' not found");

        }
      }


      pc.commitAndClose();


      pc = PersistenceContext.getDefaultPersistenceContext();


    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
      PersistenceContext.getDefaultPersistenceContext();
    }
  }

  public static void tw600UpdateIssueScreenShots() {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      Connection conn = pc.session.connection();
      Statement statement = conn.createStatement();
      ResultSet rs = statement.executeQuery("select id, screenshot,screenshot2 from twk_issue where  screenshot is not null or screenshot2 is not null");
      while (rs.next()) {
        String id = rs.getString("id");
        Issue issue = (Issue) PersistenceHome.findByPrimaryKey(Issue.class, id, pc);
        String ss1 = rs.getString("screenshot");
        if (JSP.ex(ss1)) {
          issue.addFile(PersistentFile.deserialize(ss1));
        }

        String ss2 = rs.getString("screenshot2");
        if (JSP.ex(ss2)) {
          issue.addFile(PersistentFile.deserialize(ss2));
        }

        issue.store(pc);
      }
      rs.close();
      statement.close();

      pc.commitAndClose();

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
      PersistenceContext.getDefaultPersistenceContext();
    }
  }


  public static void tw600IssueMoveNotesToComments() {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      Connection conn = pc.session.connection();
      Statement statement = conn.createStatement();

      PersistenceConfiguration configuration = PersistenceConfiguration.getDefaultPersistenceConfiguration();
    //  String dialectClassName = configuration.dialect.getName();

//
//      // si creano le issue_history
//      Statement statement = conn.createStatement();
//      String sql = "insert into twk_issue_history (";
//
//      sql += (isPostgresDiMerda?"id, ":"") +"creationDate, creator, lastModified, lastModifier, commentx, issue, ownerx";
//      sql += ") ";
//      sql += "select ";
//      sql +=  (isPostgresDiMerda?"nextval('hibernate_sequence'), ":"")+"twk_issue.creationDate,twk_issue.creator, twk_issue.lastModified, twk_issue.lastModifier, olpl_persistentText.text, twk_issue.id, twk_issue.ownerx  ";
//      sql += "from twk_issue, olpl_persistentText ";
//      sql += "where twk_issue.notes=olpl_persistentText.id and olpl_persistentText.text is not null";
//
//
//      statement.execute(sql);
//      statement.close();


      String sql = "select twk_issue.id, olpl_persistentText.text from twk_issue, olpl_persistentText ";
      sql += " where twk_issue.notes=olpl_persistentText.id and olpl_persistentText.text is not null";

      ResultSet rs = statement.executeQuery(sql);

      while (rs.next()) {

        String id = rs.getString("id");
        Issue i = (Issue) PersistenceHome.findByPrimaryKey(Issue.class, id, pc);

        IssueHistory h = new IssueHistory();
        h.setCreationDate(i.getCreationDate());
        h.setCreator(i.getCreator());
        h.setLastModified(i.getLastModified());
        h.setLastModifier(i.getLastModifier());
        h.setOwner(i.getOwner());
        h.setIssue(i);
        h.setComment(FileUtilities.readReader(rs.getClob("text").getCharacterStream()));

        h.store();

      }
      rs.close();
      statement.close();


      /*// si eliminano i persistent text  //todo fare nel prossimo update
      statement = conn.createStatement();
      statement.execute("delete from olpl_persistentText where id in (select notes from twk_issue)\n");
      statement.close();*/

      pc.commitAndClose();

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
      PersistenceContext.getDefaultPersistenceContext();
    }
  }


  public static void tw600AddDefaultSubscriptionsForRoles() {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      List<RoleTeamwork> roles = new OqlQuery("select r from " + RoleTeamwork.class.getName() + " as r").list();

      for (RoleTeamwork r : roles) {
        SerializedMap<String, String> subm = r.getDefaultSubscriptions();
        if (!JSP.ex((Map) subm))
          continue;
        for (String key : new HashSet<String>(subm.keySet())) {
          if ("yes".equals(subm.get(key)))
            subm.put(key.substring(0, key.lastIndexOf("_")) + "_LOG", "yes");
        }
        r.store(pc);
      }

      pc.commitAndClose();

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
      PersistenceContext.getDefaultPersistenceContext();
    }
  }


  public static void tw600UpdateTaskPublicPageOptions() {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      Connection conn = pc.session.connection();
      Statement statement = conn.createStatement();
      ResultSet rs = statement.executeQuery("select id,options from twk_task where options is not null and options<>'' ");
      while (rs.next()) {
        String id = rs.getString("id");
        String opt = rs.getString("options");
        if (JSP.ex(opt)) {
          Task task = Task.load(id);
          SerializedMap<String, String> options = SerializedMap.deserialize(opt);
          JSONObject publicOptions = new JSONObject();
          for (String key : options.keySet())
            publicOptions.element(key, options.get(key));
          task.getJsonData().element("publicPage", publicOptions);
          task.store();
        }
      }
      rs.close();
      statement.close();

      pc.commitAndClose();

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
      PersistenceContext.getDefaultPersistenceContext();
    }
  }

  public static void tw600UpdatePersonNames() {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      String hql = "update " + Person.class.getName() + " set name= trim(coalesce(personSurname,'') || ' ' || coalesce(personName,''))";
      new OqlQuery(hql, pc).getQuery().executeUpdate();

      pc.commitAndClose();

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
      PersistenceContext.getDefaultPersistenceContext();
    }
  }

  public static void tw600FixLayout() {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      RestState restState = new RestState();

      StringBuffer feedback = new StringBuffer();

      //si crea la portlet Headbar
      Portlet headline = WizardSupport.getPortlet("Headline", "applications/teamwork/portal/portlet/wp_headline.jsp", feedback, restState);


      // si crea/recupera la summaryBar
      Portlet summaryBar = WizardSupport.getPortlet("Summary Bar", "applications/teamwork/portal/portlet/wp_summaryBar.jsp", feedback, restState);

      //si eliminano tutte gli usi della summaryBar
      String hql = "delete from " + Content.class.getName() + " as c where c.portlet=:sbar";
      Query delQuery = new OqlQuery(hql).getQuery();
      delQuery.setEntity("sbar", summaryBar);
      delQuery.executeUpdate();

      //si mette in tutte le pagine
      hql = "select p from " + WebSitePage.class.getName() + " as p";
      List<WebSitePage> pages = new OqlQuery(hql).list();
      for (WebSitePage page : pages) {
        WizardSupport.getContent("HEADER", page, 0, headline, feedback, null);
        WizardSupport.getContent("BOTTOM", page, 0, summaryBar, feedback, null);
      }


      pc.commitAndClose();

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
      PersistenceContext.getDefaultPersistenceContext();
    }
  }

  public static void tw6060015RolesUnifyPermissions() {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      String hql = "select role from " + Role.class.getName() + " as role";
      List<Role> roles= new OqlQuery(hql, pc).list();
      for (Role r:roles){
        if (r.getPermissions().contains(TeamworkPermissions.task_assignment_manage) || r.getPermissions().contains(TeamworkPermissions.resource_assignment_manage)) { //added bicch 30/6/16
          r.removePermission(TeamworkPermissions.task_assignment_manage);
          r.removePermission(TeamworkPermissions.resource_assignment_manage);
          r.addPermission(TeamworkPermissions.resource_manage);
          r.store();
        }
      }

      pc.commitAndClose();

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
      PersistenceContext.getDefaultPersistenceContext();
    }
  }

  public static void tw6060018RolesUpdateAssigPermissions() {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      String hql = "select role from " + Role.class.getName() + " as role";
      List<Role> roles= new OqlQuery(hql, pc).list();
      for (Role r:roles){
        if (r.getPermissions().contains(TeamworkPermissions.resource_manage)) {
          r.addPermission(TeamworkPermissions.assignment_canCRW);
          r.addPermission(TeamworkPermissions.expense_manage);
          r.store();
        }
        if (r.getPermissions().contains(TeamworkPermissions.task_canWrite)) {
          r.addPermission(TeamworkPermissions.expense_manage);
          r.store();
        }
      }

      pc.commitAndClose();

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
      PersistenceContext.getDefaultPersistenceContext();
    }
  }

  public static void tw6262003RolesUpdate() {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      String hql = "select role from " + RoleTeamwork.class.getName() + " as role";
      List<RoleTeamwork> roles= new OqlQuery(hql, pc).list();
      for (RoleTeamwork r:roles){

        //ISSUE_CANCHANGESTATUS a chi aveva issue_canWrite
        if (r.getPermissions().contains(TeamworkPermissions.issue_canWrite))
          r.addPermission(TeamworkPermissions.issue_canChangeStatus);

        //ISSUE_CANDELETE a chi aveva issue_canCreate
        if (r.getPermissions().contains(TeamworkPermissions.issue_canCreate))
          r.addPermission(TeamworkPermissions.issue_canDelete);

        //RESOURCE_CANDELETE a chi aveva resource_canCreate
        if (r.getPermissions().contains(TeamworkPermissions.resource_canCreate))
          r.addPermission(TeamworkPermissions.resource_canDelete);

        //DOCUMENT_CANDELETE a chi aveva document_canCreate
        if (r.getPermissions().contains(TeamworkPermissions.document_canCreate))
          r.addPermission(TeamworkPermissions.document_canDelete);


        //TASK_CANDELETE a chi aveva task_canCreate
        if (r.getPermissions().contains(TeamworkPermissions.task_canCreate))
          r.addPermission(TeamworkPermissions.task_canDelete);

        //TASK_CANCHANGESTATUS a chi aveva task_canWrite
        if (r.getPermissions().contains(TeamworkPermissions.task_canWrite))
          r.addPermission(TeamworkPermissions.task_canChangeStatus);

        //PROJECT_CANCREATE sui ruoli globali di chi aveva task_canCreate
        if (!r.isLocalToAssignment() && r.getPermissions().contains(TeamworkPermissions.task_canCreate))
          r.addPermission(TeamworkPermissions.project_canCreate);

        //PROJECT LAUNCHER tolgo task_canCreate e task_canDelete
        if (!r.isLocalToAssignment() && r.getName().toLowerCase().indexOf("launcher")>=0 && r.getPermissions().contains(TeamworkPermissions.task_canCreate)) {
          r.removePermission(TeamworkPermissions.task_canCreate);
          r.removePermission(TeamworkPermissions.task_canDelete);
        }

        r.store();
      }

      pc.commitAndClose();

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
      if (pc != null)
        pc.rollbackAndClose();
      PersistenceContext.getDefaultPersistenceContext();
    }
  }


}
