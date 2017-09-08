package com.twproject.setup;

import com.opnlb.website.content.Content;
import com.opnlb.website.news.News;
import com.opnlb.website.page.WebSitePage;
import com.opnlb.website.page.businessLogic.WebSitePageAction;
import com.opnlb.website.portlet.Portlet;
import com.opnlb.website.portlet.businessLogic.PortletAction;
import com.opnlb.website.template.Template;
import com.opnlb.website.template.businessLogic.TemplateAction;
import com.opnlb.website.waf.WebSiteConstants;
import com.twproject.messaging.board.Board;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import org.jblooming.security.License;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkArea;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.IssueImpact;
import com.twproject.task.Task;
import com.twproject.task.financial.CostAggregator;
import com.twproject.task.financial.CostClassification;
import com.twproject.worklog.WorklogStatus;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.EventType;
import org.jblooming.company.DepartmentType;
import org.jblooming.flowork.FlowUploader;
import org.jblooming.flowork.PlatformJbpmSessionFactory;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.security.Area;
import org.jblooming.security.PlatformPermissions;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;
import org.jbpm.JbpmContext;
import org.jbpm.graph.def.ProcessDefinition;

import java.io.File;
import java.io.FileReader;
import java.util.Date;
import java.util.List;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class WizardSupport {

  public static void addFeedback(RoleTeamwork pm, StringBuffer feedback) {
    if (pm.isNew())
      feedback.append("Security - created role: " + pm.getName() + "<br>");
    else
      feedback.append("Security - updated role: " + pm.getName() + "<br>");
  }

  public static void getContent(String areaName, WebSitePage page, int order, Portlet aportlet, StringBuffer feedback, Operator operator) throws PersistenceException {

    // 24/11 do not use operator null for identify default bute use defaultConfiguration
    //String sqlSelect = "from " + Content.class.getName() + " as content where content.page=:page and  content.area = :areax and content.operator is null";

    String hql = "from " + Content.class.getName() + " as content where content.page=:page and content.defaultConfiguration =:tr";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setBoolean("tr", true);
    //oql.getQuery().setString("areax", areaName);
    oql.getQuery().setEntity("page", page);

    List<Content> contents = oql.list();
    for (Content c : contents) {
      if (c.getPortlet().getName().equalsIgnoreCase(aportlet.getName())) {
        feedback.append("Portal - removing content " + c.getArea() + "<br>");
        c.remove();
      }
    }

    Content content = new Content();
    content.setIdAsNew();
    content.setDefaultConfiguration(true);
    content.setGlobalAssociation(false);
    content.setPage(page);
    content.setArea(areaName);
    content.setOrder(order);
    content.setPortlet(aportlet);
    content.setOperator(operator);
    content.store();
    feedback.append("Portal - created content " + content.getArea() + "<br>");

  }

  public static RoleTeamwork getRole(String shortCode, String roleName, Area area, TeamworkOperator logged){

    RoleTeamwork role = getRoleByNameAndArea(roleName, area);

    if (role == null) {
      role = new RoleTeamwork();
      role.setIdAsNew();
      role.setLocalToAssignment(true);
      role.setCode(shortCode);
      role.setName(roleName);
      role.setArea(area);
      role.setOwner(logged);
    }

    // resets permissions
    role.setPermissionIds(null);

    return role;
  }

  public static RoleTeamwork getRoleByNameAndArea(String roleName, Area area) {
    String hql = "from " + RoleTeamwork.class.getName() + " as role where lower(role.name)=:namex and  role.area = :areax";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("areax", area);
    oql.getQuery().setString("namex", JSP.w(roleName).toLowerCase());

    RoleTeamwork role = (RoleTeamwork) oql.uniqueResultNullIfEmpty();
    return role;
  }

  public static Template getTemplate(String templateName, String templateFileName, StringBuffer feedback, RestState pageState)   throws PersistenceException, ApplicationException, ActionException {

    String hql = "from " + Template.class.getName() + " as templ where templ.name=:namex";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("namex", templateName);

    Template template = (Template) oql.uniqueResultNullIfEmpty();

    if (template == null) {

      feedback.append("Portal - created " + templateName + " template.<br>");

      //emulate pageState state for action

      pageState.mainObjectId = PersistenceHome.NEW_EMPTY_ID;
      pageState.addClientEntry("NAME", templateName);
      pageState.addClientEntry("DESCRIPTION", templateName);
      pageState.addClientEntry("TEMPLATE_FILE", templateFileName);
      pageState.addClientEntry("TEMPLATE_AREAS", "DUMMY");
      new TemplateAction().cmdSave(pageState);
      template = (Template) pageState.getMainObject();

    } else
      feedback.append("Portal - found default template.<br>");


    return template;
  }


  public static WebSitePage getPage(String frontOfficeTitle, String pageName, Template template, StringBuffer feedback, RestState pageState)  throws PersistenceException, ApplicationException, ActionException {

    String hql = "from " + WebSitePage.class.getName() + " as prt where upper(prt.name)=:namex";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("namex", pageName.toUpperCase());

    WebSitePage page = (WebSitePage) oql.uniqueResultNullIfEmpty();

    if (page == null) {

      feedback.append("Portal - created page:" + pageName + "<br>");
      //emulate pageState state for action

      pageState.mainObjectId = PersistenceHome.NEW_EMPTY_ID;

      pageState.addClientEntry("FOTITLE", frontOfficeTitle);
      pageState.addClientEntry("NAME", pageName);
      pageState.addClientEntry("DEF_TEMPLATE", template.getId());
      pageState.addClientEntry("DESCRIPTION", "DESC_PAGE_" + pageName.toUpperCase());
      pageState.addClientEntry("CUSTOM", Fields.TRUE);
      pageState.addClientEntry("ACTIVE", Fields.TRUE);

      new WebSitePageAction().cmdSave(pageState, WebSitePage.class);
      page = (WebSitePage) pageState.getMainObject();

    } else
      feedback.append("Portal - found page:" + pageName + "<br>");

    return page;
  }


  public static Portlet getPortlet(String portletName, String portletFileName, StringBuffer feedback, RestState pageState)  throws PersistenceException, ApplicationException, ActionException {

    String hql = "from " + Portlet.class.getName() + " as prt where prt.name=:namex";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("namex", portletName);

    Portlet portlet = (Portlet) oql.uniqueResultNullIfEmpty();

    if (portlet == null) {

      feedback.append("Portal - installed portlet:" + portletName + "<br>");
      //emulate pageState state for action

      pageState.mainObjectId = PersistenceHome.NEW_EMPTY_ID;

    } else {
      pageState.mainObjectId = portlet.getId();
      feedback.append("Portal - found portlet:" + portletName + "<br>");
    }

    pageState.addClientEntry("NAME", portletName);
    pageState.addClientEntry("DESCRIPTION", portletName);
    pageState.addClientEntry("INSTALLED", Fields.TRUE);
    pageState.addClientEntry("PORTLET_FILE", portletFileName);
    new PortletAction().cmdSave(pageState);
    portlet = (Portlet) pageState.getMainObject();
    return portlet;
  }


  public static void setAreaManagerPermissions(RoleTeamwork am) {
    am.setDescription("Has complete control on the area.");
    am.setLocalToAssignment(false);

    // Platform
    am.addPermission(PlatformPermissions.area_canManage);

    am.addPermission(PlatformPermissions.role_canRead);
    am.addPermission(PlatformPermissions.role_canWrite);
    am.addPermission(PlatformPermissions.role_canCreate);

    am.addPermission(TeamworkPermissions.resource_canRead);
    am.addPermission(TeamworkPermissions.resource_canWrite);
    am.addPermission(TeamworkPermissions.resource_canCreate);
    am.addPermission(TeamworkPermissions.resource_canDelete);
    am.addPermission(TeamworkPermissions.resource_cost_canRead);
    am.addPermission(TeamworkPermissions.resource_manage);

    am.addPermission(TeamworkPermissions.assignment_canCRW);

    am.addPermission(TeamworkPermissions.project_canCreate);

    am.addPermission(TeamworkPermissions.task_canRead);
    am.addPermission(TeamworkPermissions.task_canCreate);
    am.addPermission(TeamworkPermissions.task_canDelete);
    am.addPermission(TeamworkPermissions.task_canWrite);
    am.addPermission(TeamworkPermissions.task_canChangeStatus);
    am.addPermission(TeamworkPermissions.task_cost_canRead);
    am.addPermission(TeamworkPermissions.task_cost_canCreate);
    am.addPermission(TeamworkPermissions.task_cost_canWrite);

    am.addPermission(TeamworkPermissions.worklog_manage);
    am.addPermission(TeamworkPermissions.expense_manage);

    am.addPermission(TeamworkPermissions.issue_canRead);
    am.addPermission(TeamworkPermissions.issue_canWrite);
    am.addPermission(TeamworkPermissions.issue_canCreate);
    am.addPermission(TeamworkPermissions.issue_canDelete);
    am.addPermission(TeamworkPermissions.issue_canChangeStatus);

    am.addPermission(TeamworkPermissions.document_canRead);
    am.addPermission(TeamworkPermissions.document_canWrite);
    am.addPermission(TeamworkPermissions.document_canCreate);
    am.addPermission(TeamworkPermissions.document_canDelete);

    am.addPermission(TeamworkPermissions.classificationTree_canManage);
    am.addPermission(TeamworkPermissions.board_canRead);
    am.addPermission(TeamworkPermissions.board_canWrite);
    am.addPermission(TeamworkPermissions.board_canCreate);
    am.addPermission(TeamworkPermissions.fileStorage_canRead);
    //am.addPermission(TeamworkPermissions.fileStorage_canWrite);
    //am.addPermission(TeamworkPermissions.fileStorage_canCreate);
    am.addPermission(TeamworkPermissions.fileStorage_explorer_canRead);
    am.addPermission(TeamworkPermissions.fileStorage_explorer_canWrite);
    am.addPermission(TeamworkPermissions.fileStorage_explorer_canCreate);

  }

  public static void setProjectManagerPermissions(RoleTeamwork role) {
    role.setLocalToAssignment(true);
    role.addPermission(TeamworkPermissions.resource_canRead);
    role.addPermission(TeamworkPermissions.resource_canWrite);
    role.addPermission(TeamworkPermissions.resource_canCreate);
    role.addPermission(TeamworkPermissions.task_canRead);
    role.addPermission(TeamworkPermissions.task_canCreate);
    role.addPermission(TeamworkPermissions.task_canDelete);
    role.addPermission(TeamworkPermissions.task_canWrite);
    role.addPermission(TeamworkPermissions.task_canChangeStatus);
    role.addPermission(TeamworkPermissions.resource_manage);
    role.addPermission(TeamworkPermissions.assignment_canCRW);
    role.addPermission(TeamworkPermissions.task_cost_canRead);
    role.addPermission(TeamworkPermissions.task_cost_canCreate);
    role.addPermission(TeamworkPermissions.task_cost_canWrite);
    role.addPermission(TeamworkPermissions.worklog_manage);
    role.addPermission(TeamworkPermissions.expense_manage);
    role.addPermission(TeamworkPermissions.issue_canRead);
    role.addPermission(TeamworkPermissions.issue_canWrite);
    role.addPermission(TeamworkPermissions.issue_canCreate);
    role.addPermission(TeamworkPermissions.issue_canDelete);
    role.addPermission(TeamworkPermissions.issue_canChangeStatus);
    role.addPermission(TeamworkPermissions.document_canRead);
    role.addPermission(TeamworkPermissions.document_canWrite);
    role.addPermission(TeamworkPermissions.document_canCreate);
    role.addPermission(TeamworkPermissions.document_canDelete);
    role.addPermission(TeamworkPermissions.fileStorage_explorer_canRead);
    role.addPermission(TeamworkPermissions.fileStorage_explorer_canWrite);
    role.addPermission(TeamworkPermissions.fileStorage_explorer_canCreate);

    addSubscriptionToRole(role, Task.Event.TASK_STATUS_CHANGE, MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_DATE_CHANGE,  MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_MILESTONE_CLOSER, MessagingSystem.Media.EMAIL,  MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_EXPIRED, MessagingSystem.Media.EMAIL,  MessagingSystem.Media.STICKY,MessagingSystem.Media.DIGEST, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_ISSUE_ADDED, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_ISSUE_CLOSED, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_UPDATED_ISSUE, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_WORKLOG_OVERFLOW, MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_BUDGET_OVERFLOW, MessagingSystem.Media.EMAIL, MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_WORKLOG_MISPLACED, MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_WORKLOG_OVERTIME, MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_DIARY_CHANGE, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_CHILD_ADDED, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_DOCUMENT_ADDED, MessagingSystem.Media.LOG);


    // add descend
    role.getDefaultSubscriptions().put("TASK_NOTIFY_DESC", Fields.TRUE);

    //set description
    role.setDescription("Can manage the project and its descendants on which she/he has been assigned.");
  }

  public static void setStakeholderPermissions(RoleTeamwork role) {
    role.setLocalToAssignment(true);
    role.addPermission(TeamworkPermissions.task_canRead);
    role.addPermission(TeamworkPermissions.issue_canRead);
    role.addPermission(TeamworkPermissions.issue_canWrite);
    role.addPermission(TeamworkPermissions.issue_canCreate);
    role.addPermission(TeamworkPermissions.issue_canDelete);
    role.addPermission(TeamworkPermissions.issue_canChangeStatus);
    role.addPermission(TeamworkPermissions.fileStorage_explorer_canRead);

    addSubscriptionToRole(role, Task.Event.TASK_STATUS_CHANGE,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_MILESTONE_CLOSER, MessagingSystem.Media.EMAIL,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_EXPIRED, MessagingSystem.Media.EMAIL,MessagingSystem.Media.LOG);

    //set description
    role.setDescription("Can read and signal issues on the project and its descendants on which she/he has been assigned.");
  }


  public static void setOperatorPermissions(RoleTeamwork so) {
    so.setLocalToAssignment(false);
    so.addPermission(TeamworkPermissions.resource_canRead);
    so.addPermission(TeamworkPermissions.board_canRead);
    so.addPermission(TeamworkPermissions.board_canWrite);
    //set description
    so.setDescription("Minimal standard permission for an operator.");
  }

  public static void setProjectLauncherPermissions(RoleTeamwork so) {
    so.setLocalToAssignment(false);
    setOperatorPermissions(so);
    so.addPermission(TeamworkPermissions.project_canCreate);
    //set description
    so.setDescription("Can create a new project and assign a PM.");
  }


  public static void setWorkerPermissions(RoleTeamwork role) {
    role.setLocalToAssignment(true);
    role.addPermission(TeamworkPermissions.resource_canRead);
    role.addPermission(TeamworkPermissions.task_canRead);
    role.addPermission(TeamworkPermissions.issue_canRead);
    role.addPermission(TeamworkPermissions.issue_canWrite);
    role.addPermission(TeamworkPermissions.issue_canCreate);
    role.addPermission(TeamworkPermissions.issue_canDelete);
    role.addPermission(TeamworkPermissions.issue_canChangeStatus);
    role.addPermission(TeamworkPermissions.document_canRead);
    role.addPermission(TeamworkPermissions.document_canWrite);
    role.addPermission(TeamworkPermissions.document_canCreate);
    role.addPermission(TeamworkPermissions.document_canDelete);
    role.addPermission(TeamworkPermissions.fileStorage_explorer_canRead);

    addSubscriptionToRole(role, Task.Event.TASK_STATUS_CHANGE, MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_DATE_CHANGE,  MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_DIARY_CHANGE,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_ISSUE_ADDED,  MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_ISSUE_CLOSED,  MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_UPDATED_ISSUE, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_MILESTONE_CLOSER, MessagingSystem.Media.EMAIL,  MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_EXPIRED, MessagingSystem.Media.EMAIL,  MessagingSystem.Media.STICKY, MessagingSystem.Media.LOG);

    // add descend
    role.getDefaultSubscriptions().put("TASK_NOTIFY_DESC", Fields.TRUE);

    //set description
    role.setDescription("Can work (e.g. insert worklog, manage issues and documents) on the project and its descendants on which she/he has been assigned.");
  }

  public static void setSupervisorPermissions(RoleTeamwork reader) {
    reader.setLocalToAssignment(false);
    reader.addPermission(TeamworkPermissions.resource_canRead);
    reader.addPermission(TeamworkPermissions.task_canRead);
    reader.addPermission(TeamworkPermissions.task_cost_canRead);
    reader.addPermission(TeamworkPermissions.issue_canRead);
    reader.addPermission(TeamworkPermissions.document_canRead);
    reader.addPermission(TeamworkPermissions.board_canRead);
    reader.addPermission(TeamworkPermissions.board_canWrite);
    reader.addPermission(TeamworkPermissions.board_canCreate);
    reader.addPermission(TeamworkPermissions.fileStorage_canRead);
    //set description
    reader.setDescription("Can read everything and comment on the project and its descendants on which she/he has been assigned.");
  }

  public static void setReaderPermissions(RoleTeamwork resReader) {
    resReader.setLocalToAssignment(false);
    resReader.addPermission(TeamworkPermissions.resource_canRead);
    //set description
    resReader.setDescription("Can read resources. Should be set on everyone ");
  }

  public static void setScrumProductOwnerPermissions(RoleTeamwork role) {
    role.setLocalToAssignment(true);
    role.addPermission(TeamworkPermissions.resource_canRead);
    role.addPermission(TeamworkPermissions.task_canRead);
    role.addPermission(TeamworkPermissions.task_cost_canRead);
    role.addPermission(TeamworkPermissions.task_cost_canCreate);
    role.addPermission(TeamworkPermissions.task_cost_canWrite);
    role.addPermission(TeamworkPermissions.issue_canRead);
    role.addPermission(TeamworkPermissions.issue_canWrite);
    role.addPermission(TeamworkPermissions.issue_canCreate);
    role.addPermission(TeamworkPermissions.issue_canDelete);
    role.addPermission(TeamworkPermissions.issue_canChangeStatus);
    role.addPermission(TeamworkPermissions.document_canRead);
    role.addPermission(TeamworkPermissions.document_canWrite);
    role.addPermission(TeamworkPermissions.document_canCreate);
    role.addPermission(TeamworkPermissions.document_canDelete);

    addSubscriptionToRole(role, Task.Event.TASK_STATUS_CHANGE, MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_DATE_CHANGE,  MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_DIARY_CHANGE, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_ISSUE_ADDED, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_ISSUE_CLOSED, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_UPDATED_ISSUE, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_MILESTONE_CLOSER, MessagingSystem.Media.EMAIL,  MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_EXPIRED, MessagingSystem.Media.EMAIL,  MessagingSystem.Media.STICKY, MessagingSystem.Media.LOG);

    // add descend
    role.getDefaultSubscriptions().put("TASK_NOTIFY_DESC", Fields.TRUE);

    //set description
    role.setDescription("It's the 'pig'. Is the Product's owner. Can manage the product backlog.");
  }

  public static void setScrumStakeholderPermissions(RoleTeamwork role) {
    role.setLocalToAssignment(true);
    role.addPermission(TeamworkPermissions.task_canRead);
    role.addPermission(TeamworkPermissions.issue_canRead);
    role.addPermission(TeamworkPermissions.issue_canWrite);
    role.addPermission(TeamworkPermissions.issue_canCreate);
    role.addPermission(TeamworkPermissions.issue_canDelete);
    role.addPermission(TeamworkPermissions.issue_canChangeStatus);
    role.addPermission(TeamworkPermissions.document_canRead);

    addSubscriptionToRole(role, Task.Event.TASK_STATUS_CHANGE, MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_DATE_CHANGE,  MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_DIARY_CHANGE, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_ISSUE_ADDED, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_ISSUE_CLOSED, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_UPDATED_ISSUE, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_MILESTONE_CLOSER, MessagingSystem.Media.EMAIL,  MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_EXPIRED, MessagingSystem.Media.EMAIL,  MessagingSystem.Media.STICKY, MessagingSystem.Media.LOG);

    // add descend
    role.getDefaultSubscriptions().put("TASK_NOTIFY_DESC", Fields.TRUE);

    //set description
    role.setDescription("It's a 'chicken'. Can read the project and its descendants on which she/he has been assigned.");
  }


  public static void setScrumMasterPermissions(RoleTeamwork role) {
    role.setLocalToAssignment(true);
    role.addPermission(TeamworkPermissions.resource_canRead);
    role.addPermission(TeamworkPermissions.task_canRead);
    role.addPermission(TeamworkPermissions.task_canCreate);
    role.addPermission(TeamworkPermissions.task_canDelete);
    role.addPermission(TeamworkPermissions.task_canWrite);
    role.addPermission(TeamworkPermissions.task_canChangeStatus);
    role.addPermission(TeamworkPermissions.worklog_manage);
    role.addPermission(TeamworkPermissions.expense_manage);
    role.addPermission(TeamworkPermissions.resource_manage);
    role.addPermission(TeamworkPermissions.assignment_canCRW);
    role.addPermission(TeamworkPermissions.issue_canRead);
    role.addPermission(TeamworkPermissions.issue_canWrite);
    role.addPermission(TeamworkPermissions.issue_canCreate);
    role.addPermission(TeamworkPermissions.issue_canDelete);
    role.addPermission(TeamworkPermissions.issue_canChangeStatus);
    role.addPermission(TeamworkPermissions.document_canRead);
    role.addPermission(TeamworkPermissions.document_canWrite);
    role.addPermission(TeamworkPermissions.document_canCreate);
    role.addPermission(TeamworkPermissions.document_canDelete);
    role.addPermission(TeamworkPermissions.fileStorage_explorer_canRead);
    role.addPermission(TeamworkPermissions.fileStorage_explorer_canWrite);
    role.addPermission(TeamworkPermissions.fileStorage_explorer_canCreate);

    addSubscriptionToRole(role, Task.Event.TASK_STATUS_CHANGE, MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_DATE_CHANGE,  MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_MILESTONE_CLOSER, MessagingSystem.Media.EMAIL,  MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_EXPIRED, MessagingSystem.Media.EMAIL,  MessagingSystem.Media.STICKY,MessagingSystem.Media.DIGEST, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_ISSUE_ADDED, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_ISSUE_CLOSED, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_UPDATED_ISSUE, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_WORKLOG_OVERFLOW, MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_BUDGET_OVERFLOW, MessagingSystem.Media.EMAIL, MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_WORKLOG_MISPLACED, MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_WORKLOG_OVERTIME, MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_DIARY_CHANGE, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_CHILD_ADDED, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_DOCUMENT_ADDED, MessagingSystem.Media.LOG);


    // add descend
    role.getDefaultSubscriptions().put("TASK_NOTIFY_DESC", Fields.TRUE);

    //set description
    role.setDescription("The ScrumMaster is a facilitator for the team and product owner");
  }


  public static void setScrumTeamPermissions(RoleTeamwork role) {
    role.setLocalToAssignment(true);
    role.addPermission(TeamworkPermissions.resource_canRead);
    role.addPermission(TeamworkPermissions.task_canRead);
    role.addPermission(TeamworkPermissions.issue_canRead);
    role.addPermission(TeamworkPermissions.issue_canWrite);
    role.addPermission(TeamworkPermissions.issue_canCreate);
    role.addPermission(TeamworkPermissions.issue_canDelete);
    role.addPermission(TeamworkPermissions.issue_canChangeStatus);
    role.addPermission(TeamworkPermissions.document_canRead);
    role.addPermission(TeamworkPermissions.document_canWrite);
    role.addPermission(TeamworkPermissions.document_canCreate);
    role.addPermission(TeamworkPermissions.document_canDelete);
    role.addPermission(TeamworkPermissions.fileStorage_explorer_canRead);

    addSubscriptionToRole(role, Task.Event.TASK_STATUS_CHANGE, MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_DATE_CHANGE,  MessagingSystem.Media.STICKY, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_DIARY_CHANGE, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_ISSUE_ADDED, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_ISSUE_CLOSED, MessagingSystem.Media.DIGEST,MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_UPDATED_ISSUE, MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_MILESTONE_CLOSER, MessagingSystem.Media.EMAIL,  MessagingSystem.Media.LOG);
    addSubscriptionToRole(role, Task.Event.TASK_EXPIRED, MessagingSystem.Media.EMAIL,  MessagingSystem.Media.STICKY, MessagingSystem.Media.LOG);


    // add descend
    role.getDefaultSubscriptions().put("TASK_NOTIFY_DESC", Fields.TRUE);

    //set description
    role.setDescription("Member of development team.");
  }


  public static TeamworkArea createAreaAndStandardRoles(String areaName, TeamworkOperator owner, StringBuffer feedback) throws PersistenceException {

    // check if area already exists
    TeamworkArea area = (TeamworkArea) PersistenceHome.findUnique(TeamworkArea.class, "name", areaName);

    // create the new area
    if (area == null) {
      area = new TeamworkArea();
      area.setIdAsNew();
      area.setName(areaName);
      area.setOwner(owner);
      area.store();
      feedback.append("Created area: " + areaName + "<br>");
    }

    // ---------------------------------- Area Manager ----------------------------------
    RoleTeamwork am = getRole("AM", "Area manager", area, owner);
    setAreaManagerPermissions(am);
    addFeedback(am, feedback);
    am.store();

    // ---------------------------------- Supervisor ----------------------------------
    RoleTeamwork reader = getRole("SU", "Supervisor", area, owner);
    setSupervisorPermissions(reader);
    addFeedback(reader, feedback);
    reader.store();

    // ---------------------------------- Project launcher ----------------------------------
    RoleTeamwork projectLaucher = getRole("PL", "Project launcher", area, owner);
    projectLaucher.setLocalToAssignment(false);
    setProjectLauncherPermissions(projectLaucher);
    addFeedback(projectLaucher, feedback);
    projectLaucher.store();

    // ---------------------------------- Operational ----------------------------------
    RoleTeamwork stdOperatorRole = WizardSupport.getRole("OP", "Operational", area, owner);
    WizardSupport.setOperatorPermissions(stdOperatorRole);
    stdOperatorRole.setLocalToAssignment(false);
    stdOperatorRole.store();


    //  ---------------------------------- Project manager ----------------------------------
    RoleTeamwork pm = getRole("PM", ApplicationState.getApplicationSetting("DEFAULT_PROJECT_MANAGER_ROLE_NAME", "Project manager"), area, owner);
    setProjectManagerPermissions(pm);
    addFeedback(pm, feedback);
    pm.store();

    // ---------------------------------- Stakeholder ----------------------------------
    RoleTeamwork so = getRole("SH", "Stakeholder", area, owner);
    setStakeholderPermissions(so);
    addFeedback(so, feedback);
    so.store();

    // ---------------------------------- Customer ----------------------------------
    RoleTeamwork cu = getRole("CU",  ApplicationState.getApplicationSetting("DEFAULT_CUSTOMER_ROLE_NAME", "Customer"), area, owner);
    setStakeholderPermissions(cu);
    addFeedback(cu, feedback);
    cu.store();

    // ---------------------------------- Worker ----------------------------------
    RoleTeamwork wo = getRole("WK", ApplicationState.getApplicationSetting("DEFAULT_WORKER_ROLE_NAME", "Worker"), area, owner);
    setWorkerPermissions(wo);
    addFeedback(wo, feedback);
    wo.store();

    return area;
  }


  public static TeamworkArea createAreaAndScrumRoles(String areaName, TeamworkOperator owner, StringBuffer feedback) throws PersistenceException {
    // check if area already exists
    TeamworkArea area = (TeamworkArea) PersistenceHome.findUnique(TeamworkArea.class, "name", areaName);

    // create the new area
    if (area == null) {
      area = new TeamworkArea();
      area.setIdAsNew();
      area.setName(areaName);
      area.setOwner(owner);
      area.store();
      feedback.append("Created area: " + areaName + "<br>");
    }


    // ---------------------------------- Area manager ----------------------------------
    RoleTeamwork am = getRole("AM", "Area manager", area, owner);
    setAreaManagerPermissions(am);
    addFeedback(am, feedback);
    am.store();

    addScrumRolesOnArea(area, owner, feedback);

    return area;
  }

  public static void addScrumRolesOnArea(TeamworkArea area, TeamworkOperator owner, StringBuffer feedback) throws StoreException {

    // ---------------------------------- Product owner ----------------------------------
    RoleTeamwork po = getRole("OW", "Product owner", area, owner);
    setScrumProductOwnerPermissions(po);
    addFeedback(po, feedback);
    po.store();

    //  ---------------------------------- Scrum Master ----------------------------------
    RoleTeamwork pm = getRole("SM", "Scrum Master", area, owner);
    setScrumMasterPermissions(pm);
    addFeedback(pm, feedback);
    pm.store();

    // ---------------------------------- Stakeholder ----------------------------------
    RoleTeamwork so = getRole("SH", "Scrum Stakeholder", area, owner);
    setScrumStakeholderPermissions(so);
    addFeedback(so, feedback);
    so.store();

    // ---------------------------------- Scrum Team ----------------------------------
    RoleTeamwork wo = getRole("TM", "Scrum Team", area, owner);
    setScrumTeamPermissions(wo);
    addFeedback(wo, feedback);
    wo.store();
  }


  public static void createTemplatesAndWebSitePages()
          throws PersistenceException, ApplicationException, ActionException {

    //remove all contents
    String hql = "delete from " + Content.class.getName();
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().executeUpdate();
    PersistenceContext.getDefaultPersistenceContext().checkPoint();

    //remove all pages
    hql = "delete from " + WebSitePage.class.getName();
    oql = new OqlQuery(hql);
    oql.getQuery().executeUpdate();
    PersistenceContext.getDefaultPersistenceContext().checkPoint();

    //remove all templates
    //    sqlSelect = "delete from " + Template.class.getName();
    //    oql = new OqlQuery(sqlSelect);
    //    oql.getQuery().executeUpdate();
    
    hql = "select  c from " + Template.class.getName()+" as c";
    oql = new OqlQuery(hql);

    List<Template> ts= oql.getQuery().list();
    for (Template t:ts)
      t.remove();

    //remove all portlets
    hql = "delete from " + Portlet.class.getName();
    oql = new OqlQuery(hql);
    oql.getQuery().executeUpdate();
    
    PersistenceContext.getDefaultPersistenceContext().checkPoint();

    StringBuffer feedback = new StringBuffer();
    RestState pageState = new RestState();


    Template defaultTemplate = WizardSupport.getTemplate("default", "teamworkTemplate.html", feedback, pageState);
    Template oneColumn = WizardSupport.getTemplate("one column", "teamworkTemplateOneColumn.html", feedback, pageState);

    //Portlet firstStartPortlet = WizardSupport.getPortlet("First Start", "applications/teamwork/portal/portlet/wp_firstStart.jsp", feedback, pageState, request);
    //Portlet introductionPortlet = WizardSupport.getPortlet("Introduction", "applications/teamwork/portal/portlet/wp_introduction.jsp", feedback, pageState, request);
    Portlet createIssuePortlet = WizardSupport.getPortlet("Create an issue", "applications/teamwork/portal/portlet/wp_create_issues.jsp", feedback, pageState);
    Portlet issueCreatedByMePortlet = WizardSupport.getPortlet("Issues created by me", "applications/teamwork/portal/portlet/wp_issuesCreatedByMe.jsp", feedback, pageState);
    Portlet activity = WizardSupport.getPortlet("Activity", "applications/teamwork/portal/portlet/wp_activity.jsp", feedback, pageState);
    Portlet headline = WizardSupport.getPortlet("Headline", "applications/teamwork/portal/portlet/wp_headline.jsp", feedback, pageState);
    Portlet reports = WizardSupport.getPortlet("Report list", "applications/teamwork/portal/portlet/wp_genericReports.jsp", feedback, pageState);
    Portlet timeCounters = WizardSupport.getPortlet("Time Counters", "applications/teamwork/portal/portlet/wp_timeCounter.jsp", feedback, pageState);
    Portlet timeCounterSlim = WizardSupport.getPortlet("Time Counter Slim", "applications/teamwork/portal/portlet/wp_timeCounterSlim.jsp", feedback, pageState);
    Portlet loggedOperators = WizardSupport.getPortlet("Logged Operators", "applications/teamwork/portal/portlet/wp_loggedOperators.jsp", feedback, pageState);
    Portlet myAppointments = WizardSupport.getPortlet("My agenda", "applications/teamwork/portal/portlet/wp_myAppointments.jsp", feedback, pageState);
    Portlet myMeetings = WizardSupport.getPortlet("My meetings", "applications/teamwork/portal/portlet/wp_myMeetings.jsp", feedback, pageState);
    Portlet companyNews = WizardSupport.getPortlet("Company News", "applications/teamwork/portal/portlet/wp_companyNews.jsp", feedback, pageState);
    Portlet myAssignments = WizardSupport.getPortlet("My Assignments", "applications/teamwork/portal/portlet/wp_myAssignments.jsp", feedback, pageState);
    Portlet myIssues = WizardSupport.getPortlet("My Issues", "applications/teamwork/portal/portlet/wp_myIssues.jsp", feedback, pageState);
    Portlet projectsSummary = WizardSupport.getPortlet("Projects Summary", "applications/teamwork/portal/portlet/wp_projectsSummary.jsp", feedback, pageState);
    projectsSummary.addPermission(TeamworkPermissions.task_canRead);
    Portlet summaryBar = WizardSupport.getPortlet("Summary Bar", "applications/teamwork/portal/portlet/wp_summaryBar.jsp", feedback, pageState);
    Portlet teamworkActivity = WizardSupport.getPortlet("Twproject Activity", "applications/teamwork/portal/portlet/wp_teamworkActivity.jsp", feedback, pageState);
    Portlet twLinks = WizardSupport.getPortlet("links", "applications/teamwork/portal/portlet/wp_links.jsp", feedback, pageState);
    Portlet WorklogDay = WizardSupport.getPortlet("WorklogDay", "applications/teamwork/portal/portlet/wp_worklogDay.jsp", feedback, pageState);
    Portlet myResponsabilities = WizardSupport.getPortlet("My responsibilities", "applications/teamwork/portal/portlet/wp_myResponsabilities.jsp", feedback, pageState);
    Portlet iframe = WizardSupport.getPortlet("Iframe", "applications/teamwork/portal/portlet/wp_iframe.jsp", feedback, pageState);
    Portlet nativeSql = WizardSupport.getPortlet("Example of portlet with native sql", "applications/teamwork/portal/portlet/wp_nativeSqlQuery.jsp", feedback, pageState);
    Portlet panicBoard = WizardSupport.getPortlet("Panic board", "applications/teamwork/portal/portlet/wp_panicBoard.jsp", feedback, pageState);
    Portlet burnDown = WizardSupport.getPortlet("Burndown graph", "applications/teamwork/portal/portlet/wp_scrumBurnDown.jsp", feedback, pageState);
    Portlet todo = WizardSupport.getPortlet("Todo", "applications/teamwork/portal/portlet/wp_todo.jsp", feedback, pageState);
    Portlet murphy = WizardSupport.getPortlet("Murphy's laws", "applications/teamwork/portal/portlet/wp_murphy.jsp", feedback, pageState);
    Portlet myDocuments = WizardSupport.getPortlet("My documents", "applications/teamwork/portal/portlet/wp_myDocuments.jsp", feedback, pageState);

    Portlet myPlan = WizardSupport.getPortlet("My planned work", "applications/teamwork/portal/portlet/wp_myPlan.jsp", feedback, pageState);

    {
      WebSitePage twHelpDesk = WizardSupport.getPage("Help Desk", "HelpDesk", defaultTemplate, feedback, pageState);
      // construct content of introduction
      int order = 0;
      WizardSupport.getContent("HEADER", twHelpDesk, order++, headline, feedback, null);
      WizardSupport.getContent("LEFT", twHelpDesk, order++, createIssuePortlet, feedback, null);
      WizardSupport.getContent("LEFT", twHelpDesk, order++, issueCreatedByMePortlet, feedback, null);
      WizardSupport.getContent("RIGHT", twHelpDesk, order++, myAppointments, feedback, null);
      WizardSupport.getContent("RIGHT", twHelpDesk, order++, todo, feedback, null);
      WizardSupport.getContent("RIGHT", twHelpDesk, order++, activity, feedback, null);
      WizardSupport.getContent("RIGHT_BOTTOM", twHelpDesk, order++, companyNews, feedback, null);
      WizardSupport.getContent("BOTTOM", twHelpDesk, order++, summaryBar, feedback, null);
    }

    {
      WebSitePage teamworkHome = WizardSupport.getPage("Get things done", "getsThingsDone", defaultTemplate, feedback, pageState);
      // construct content of Get things done
      int order = 0;
      WizardSupport.getContent("HEADER", teamworkHome, order++, headline, feedback, null);
      WizardSupport.getContent("LEFT", teamworkHome, order++, myAssignments, feedback, null);
      WizardSupport.getContent("LEFT", teamworkHome, order++, myIssues, feedback, null);
      WizardSupport.getContent("RIGHT", teamworkHome, order++, myAppointments, feedback, null);
      WizardSupport.getContent("RIGHT", teamworkHome, order++, myPlan, feedback, null);
      WizardSupport.getContent("RIGHT", teamworkHome, order++, myDocuments, feedback, null);
      WizardSupport.getContent("RIGHT", teamworkHome, order++, todo, feedback, null);
      WizardSupport.getContent("RIGHT", teamworkHome, order++, activity, feedback, null);
      WizardSupport.getContent("RIGHT_BOTTOM", teamworkHome, order++, companyNews, feedback, null);
      WizardSupport.getContent("BOTTOM", teamworkHome, order++, summaryBar, feedback, null);
    }

    {
      WebSitePage pmHome = WizardSupport.getPage("Project Manager", "pm", defaultTemplate, feedback, pageState);
      // construct content of Project Manager home
      int order = 0;
      WizardSupport.getContent("HEADER", pmHome, order++, headline, feedback, null);
      WizardSupport.getContent("LEFT", pmHome, order++, myAssignments, feedback, null);
      WizardSupport.getContent("LEFT", pmHome, order++, myIssues, feedback, null);
      WizardSupport.getContent("RIGHT", pmHome, order++, myAppointments, feedback, null);
      WizardSupport.getContent("RIGHT", pmHome, order++, myPlan, feedback, null);
      WizardSupport.getContent("RIGHT", pmHome, order++, todo, feedback, null);
      WizardSupport.getContent("RIGHT", pmHome, order++, myDocuments, feedback, null);
      WizardSupport.getContent("RIGHT", pmHome, order++, activity, feedback, null);
      WizardSupport.getContent("RIGHT", pmHome, order++, teamworkActivity, feedback, null);
      WizardSupport.getContent("RIGHT", pmHome, order++, myResponsabilities, feedback, null);
      WizardSupport.getContent("RIGHT_BOTTOM", pmHome, order++, companyNews, feedback, null);
      WizardSupport.getContent("BOTTOM", pmHome, order++, summaryBar, feedback, null);
    }

    {
      WebSitePage supervisorHome = WizardSupport.getPage("Supervisor", "supervisor", oneColumn, feedback, pageState);
      // construct content of Supervisor home
      int order = 0;
      WizardSupport.getContent("HEADER", supervisorHome, order++, headline, feedback, null);
      WizardSupport.getContent("MAIN", supervisorHome, order++, panicBoard, feedback, null);
      WizardSupport.getContent("MAIN", supervisorHome, order++, projectsSummary, feedback, null);
      WizardSupport.getContent("MAIN_BOTTOM", supervisorHome, order++, summaryBar, feedback, null);
    }


  }


  public static void createSampleData() throws PersistenceException {

    OqlQuery oql = new OqlQuery("from " + TeamworkOperator.class.getName() + " as op " +
            "where lower(op.loginName) = lower(:name)");
    oql.getQuery().setString("name", "administrator");
    TeamworkOperator administrator = (TeamworkOperator) oql.uniqueResult();

    Person adminPerson = administrator.getPerson();
    if (adminPerson == null)
      throw new PlatformRuntimeException("adminPerson is null");

    //create default area

    //get it from license
    String areaName = "DEFAULT_AREA";

    try {
      String customer = License.getLicense().customerCode;
      if (customer.indexOf("@") > -1)
        customer = customer.substring(customer.indexOf("@") + 1);
      areaName = customer;
    } catch (Throwable e) {
      Tracer.platformLogger.error(e);
    }
    TeamworkArea area = WizardSupport.createAreaAndStandardRoles(areaName, administrator, new StringBuffer());

    //si caricano/creano i ruoli di risorsa
    RoleTeamwork.getManagerRole();
    RoleTeamwork.getMyselfRole();

    //set it on admin
    adminPerson.setArea(area);
    //set working hours per day
    adminPerson.setWorkDailyCapacity(CompanyCalendar.MILLIS_IN_WORKING_DAY);

    createFixedTypesForArea(area);

    DepartmentType dt = new DepartmentType();
    dt.setDescription("Company");
    dt.setStringValue("COMPANY");
    dt.store();

    dt = new DepartmentType();
    dt.setDescription("Department");
    dt.setStringValue("DEPARTMENT");
    dt.store();

    dt = new DepartmentType();
    dt.setDescription("Branch");
    dt.setStringValue("BRANCH");
    dt.store();

    dt = new DepartmentType();
    dt.setDescription("Office");
    dt.setStringValue("OFFICE");
    dt.store();

    WorklogStatus wls = new WorklogStatus();
    wls.setIntValue(1);
    wls.setDescription("Approved");
    wls.setColor("#43CF43");
    wls.store();

    wls = new WorklogStatus();
    wls.setIntValue(2);
    wls.setDescription("Billed");
    wls.setColor("#DA1D33");
    wls.store();

    News anews = new News();
    anews.setTitle("Twproject running on your server");
    anews.setText("Now you can manage work with a powerful tool.");
    anews.setStartingDate(new Date());
    CompanyCalendar cc = new CompanyCalendar();
    cc.add(CompanyCalendar.WEEK_OF_YEAR, 7);
    anews.setEndingDate(cc.getTime());
    anews.setVisible(true);
    anews.store();

    Board board = new Board();
    board.setIdAsNew();
    board.setArea(area);
    board.setOwner(administrator);
    board.setName("Office space quality");
    board.setDescription("Office space quality discussion and proposals");
    board.setActive(true);
    board.store();

    try {
      SetupSupport.tw321CreateDiscussionPointsDefaultTypes();
    } catch (Throwable e) {
      throw new PlatformRuntimeException(e);
    }

    try {
      SetupSupport.tw450CreateDefaultIssueStatuses();
    } catch (Throwable e) {
      throw new PlatformRuntimeException(e);
    }


  }

  public static void createFixedTypesForArea(TeamworkArea area) throws StoreException {

    IssueImpact ii = new IssueImpact();
    ii.setDescription("Light");
    ii.setIntValue(30);
    ii.setArea(area);
    ii.store();

    ii = new IssueImpact();
    ii.setDescription("Medium");
    ii.setIntValue(60);
    ii.setArea(area);
    ii.store();

    ii = new IssueImpact();
    ii.setDescription("Severe");
    ii.setIntValue(90);
    ii.setArea(area);
    ii.store();

    EventType et = new EventType();
    et.setDescription("First sample event type");
    et.setArea(area);
    et.store();

    et = new EventType();
    et.setDescription("Second sample event type");
    et.setArea(area);
    et.store();

    CostClassification cc = new CostClassification();
    cc.setDescription("Travel");
    cc.setStringValue("TRL");
    cc.setArea(area);
    cc.store();

    cc = new CostClassification();
    cc.setDescription("Stay");
    cc.setStringValue("STY");
    cc.setArea(area);
    cc.store();

    CostAggregator ca = new CostAggregator();
    ca.setCode("1");
    ca.setDescription("Master sample");
    ca.setArea(area);
    ca.store();

  }

  public static void createSampleFluxDefinitions(PageState pageState) {

    try {
      JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);

      FlowUploader jpdlReader = new FlowUploader(new FileReader(ApplicationState.webAppFileSystemRootPath +
              File.separator + "applications" + File.separator + "teamwork" + File.separator + "processes" + File.separator + "sampleProjectProcess.xml"));
      ProcessDefinition processDefinition = jpdlReader.readProcessDefinition();
      jbpmSession.deployProcessDefinition(processDefinition);
      jpdlReader = new FlowUploader(new FileReader(ApplicationState.webAppFileSystemRootPath +
              File.separator + "applications" + File.separator + "teamwork" + File.separator + "processes" + File.separator + "sampleSimpleProjectProcess.xml"));
      processDefinition = jpdlReader.readProcessDefinition();
      jbpmSession.deployProcessDefinition(processDefinition);


    } catch (Throwable e) {
      Tracer.platformLogger.error(e);
    }
  }


  public static void addSubscriptionToRole(RoleTeamwork role, Task.Event event, MessagingSystem.Media... medias) {
    if (role.getDefaultSubscriptions() == null)
      role.setDefaultSubscriptions(new SerializedMap());
    for (MessagingSystem.Media media : medias)
      role.getDefaultSubscriptions().put(event + "_" + media.toString().toUpperCase(), Fields.TRUE);
  }

  public static String getHomePage(PageState pageState) throws org.jblooming.persistence.exceptions.PersistenceException {
    String homePage = Operator.getOperatorOption(pageState.getLoggedOperator(), WebSiteConstants.HOME_PAGE);
    boolean hasExistingCustomHome = true;
    if (homePage!=null && homePage.indexOf(".page")>-1 && !"getsThingsDone.page".equalsIgnoreCase(homePage)) {
      hasExistingCustomHome = false;
      //does page exist?
      String hql = "select page.id from "+ WebSitePage.class.getName()+" as page where upper(page.name)=:mypage";
      OqlQuery oql = new OqlQuery(hql);
      String root = homePage.substring(0, homePage.indexOf(".page"));
      oql.getQuery().setString("mypage",root.toUpperCase());
      if (oql.list().size()>0)
        hasExistingCustomHome = true;
    } else if("createCompany.jsp".equalsIgnoreCase(homePage)  || "personFirstStart.jsp".equalsIgnoreCase(homePage) ){

    }else
      homePage = "getsThingsDone.page";

    //where should I go?
    if (!hasExistingCustomHome)
       homePage = "getsThingsDone.page";
    return org.jblooming.waf.settings.ApplicationState.contextPath+"/applications/teamwork/"+homePage;

  }

}

