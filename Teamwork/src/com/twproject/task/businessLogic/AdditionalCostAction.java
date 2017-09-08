package com.twproject.task.businessLogic;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Resource;
import com.twproject.security.TeamworkArea;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Assignment;
import com.twproject.task.Task;
import com.twproject.task.TaskBricks;
import com.twproject.task.financial.Cost;
import com.twproject.worklog.WorklogStatus;
import net.sf.json.JSONObject;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.designer.DesignerField;
import org.jblooming.ontology.PerformantNode;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.Area;
import org.jblooming.security.Permission;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.html.table.ListHeader;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.RestState;

import java.text.ParseException;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


public class AdditionalCostAction extends ActionSupport {

  public TeamworkOperator logged;
  public String url;

  public AdditionalCostAction(RestState restState) {
    super(restState);
    this.logged = (TeamworkOperator) restState.getLoggedOperator();
  }


  //save task additional cost
  public JSONObject cmdSave() throws PersistenceException, ActionException, ParseException {
    restState.initializeEntries("row");
    JSONObject jCost=null;

    String taskId = restState.getEntry("taskId").stringValueNullIfEmpty();

    boolean disabledInThePast=false;
    ClientEntry dateEntry = restState.getEntry("creationDate");
    if (taskId != null && !disabledInThePast) {
      Task task = Task.load(taskId);
      Cost cost = Cost.load(restState.getEntry("costId").intValueNoErrorCodeNoExc() + "");
      if (cost==null && task.hasPermissionFor(logged, TeamworkPermissions.task_cost_canCreate)
        || cost!=null && task.hasPermissionFor(logged, TeamworkPermissions.task_canWrite)) {

        // add
        if (cost == null) {
          cost = new Cost();
          task.getCosts().add(cost);

          // ------------------ FILE IN CASE OF NEW cost -------------------------------
          //in case of new cost it could exists some pending files
          String s = restState.getEntry("PENDING_PF").stringValueNullIfEmpty();
          if (JSP.ex(s)) {
            PersistentFile pf = PersistentFile.deserialize(s);
            if (pf != null) {
              cost.setAttachment(pf);
            }
          }
        }

        ActionUtilities.setString(restState.getEntry("description"), cost, "description");
        ActionUtilities.setCurrency(restState.getEntry("estimatedCost"), cost, "estimatedCost");
        ActionUtilities.setCurrency(restState.getEntry("realCost"), cost, "realCost");
        ActionUtilities.setDate(dateEntry, cost, "creationDate");
        ActionUtilities.setIdentifiable(restState.getEntry("classification"), cost, "classification");
        //Custom fields
        DesignerField.saveCustomFields("COST_ADD_CUSTOM_FIELD_", 4, cost, restState);

        cost.store();
        jCost = cost.jsonify();
        jCost.element("taskId",task.getId());

        task.store();
      }

    }
    return jCost;
  }


  public void cmdPrepareDefaultFind() throws PersistenceException, SecurityException {
    TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();

    //se non hai i permessi globali si esce e via!
    //logged.testPermission(TeamworkPermissions.task_cost_canRead);

    boolean canManageCostsGlobally = logged.hasPermissionFor(TeamworkPermissions.task_cost_canRead);

    //search for default filter
    if (restState.getCommand() == null)
      if (!PersistentSearch.feedFromDefaultSearch("ADDCOSTFILTER", restState)) {
        // when not set use my open task
        restState.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MONTH_ADD_COSTS");
      }

    /*if (!canManageCostsGlobally) {
      //check permission on task
      if (JSP.ex(restState.getEntry("TASK"))) {
        Task task = Task.load(restState.getEntry("TASK").stringValueNullIfEmpty() + "");
        if (task == null || !task.hasPermissionFor(logged, TeamworkPermissions.worklog_manage)) {
          restState.addClientEntry("RES_ID", logged.getPerson().getId());
        }
      } else {
        if (!JSP.ex(restState.getEntry("RES_ID"))) {
          restState.addClientEntry("RES_ID", logged.getPerson().getId());
        } else {
          //non c'è bisogno di controllare i permessi sulla risorsa perchè c'è comunque il controllo di sicurazza sui task
          Resource res = Resource.load(restState.getEntry("RES_ID").intValueNoErrorCodeNoExc() + "");
          if (res == null) {
            restState.addClientEntry("RES_ID", logged.getPerson().getId());
            restState.addClientEntry("RES_ID_txt", logged.getPerson().getDisplayName());
          }
        }
      }
    }*/

    //search for default filter
    if (restState.getCommand() == null)
      PersistentSearch.feedFromDefaultSearch("ADDCOSTFILTER", restState);


    if (!PersistentSearch.feedFromSavedSearch(restState)) {
      // uso di un filtro presettato
      String cmd = restState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
      if (JSP.ex(cmd)) {
        restState.getClientEntries().getClientEntries().clear();

        // add the filter name in order to display boldify
        restState.addClientEntry(Fields.FLD_FILTER_NAME, cmd);

        if ("PF_MONTH_ADD_COSTS".equals(cmd)) {
          restState.addClientEntry("COST_AT_DAY", "TM");

        } else if ("PF_WEEK_ADD_COSTS".equals(cmd)) {
          restState.addClientEntry("COST_AT_DAY", "TW");
        }
      }
    }
  }


  public void cmdFind() throws PersistenceException, SecurityException {
    TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();
    cmdPrepareDefaultFind();

    boolean somethingSearched = false;
    String filter = null;
    String hql = "select distinct task,cost from " + Task.class.getName() + " as task join task.costs as cost";
    //hql = "select task,cost from " + Task.class.getName() + " as task join task.costs as cost";
    QueryHelper qhelp = new QueryHelper(hql);

    //areas
    String areaId=restState.getEntry("AREA").stringValueNullIfEmpty();
    if (JSP.ex(areaId)){
      TeamworkArea ta= TeamworkArea.load(areaId);
      qhelp.addOrQueryClause("task.area = :area");
      qhelp.addParameter("area", ta);
      somethingSearched=true;
    }

    // si aggiungo le aree su cui si è abilitati anche se c'è quella sopra in modo da essere sicuri che non si salti la sicurezza
    if (!logged.hasPermissionAsAdmin()) {
      Set<Area> areas = new HashSet();
      areas.addAll(logged.getAreasForPermission(TeamworkPermissions.task_cost_canRead));
      areas.addAll(logged.getAreasForPermission(TeamworkPermissions.task_cost_canWrite));
      if (JSP.ex(areas)) {
        qhelp.addOrQueryClause("task.area in (:areas)");
        qhelp.addParameter("areas", areas);
      }
    }


    // search by task id
    filter = restState.getEntry("TASK").stringValueNullIfEmpty();
    if (filter != null) {
      somethingSearched = true;
      if (!restState.getEntry("TASK_COST_SHOW_CHILDREN").checkFieldValue()) {
        qhelp.addOQLClause(" task.id = :taskId", "taskId", filter);
      } else {
        Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, filter);
        qhelp.addOQLClause(" task.id in (select p.id from " + Task.class.getName() + " as p where p.id=:tid or p.ancestorIds like :ancid )", "tid", task.getId());
        qhelp.addParameter("ancid", task.getChildAncentorIds());
      }

      // search by name/code
    } else {
      filter = restState.getEntry("TASK" + SmartCombo.TEXT_FIELD_POSTFIX).stringValueNullIfEmpty();
      if (filter != null) {
        somethingSearched = true;

        if (!restState.getEntry("TASK_WORKLOG_SHOW_CHILDREN").checkFieldValue()) {   // caso semplice solo per nome o codice
          qhelp.addQBEORClauses(filter, qhelp.getOrElement("task.name", "taskName", QueryHelper.TYPE_CHAR), qhelp.getOrElement("task.code", "taskCode", QueryHelper.TYPE_CHAR));

        } else {// caso complesso, si deve prima estrarre i task e poi preparare la query
          QueryHelper tqh = new QueryHelper("select t.id,t.ancestorIds from " + Task.class.getName() + " as t");
          tqh.addQBEORClauses(filter, qhelp.getOrElement("t.name", "taskName", QueryHelper.TYPE_CHAR), qhelp.getOrElement("t.code", "taskCode", QueryHelper.TYPE_CHAR));
          List<Object[]> tsIdAncid = tqh.toHql().list();
          if (tsIdAncid.size()>0){
            int c=1;
            String taskQuery = " task.id in (select p.id from " + Task.class.getName() + " as p where  ";
            for (Object[] obs:tsIdAncid){
              taskQuery+=((c > 1 ? " or " : "") + "p.id=:tid" + c + " or p.ancestorIds like :ancid" + c) ;
              qhelp.addParameter("tid" + c, obs[0]);
              qhelp.addParameter("ancid"+c, "%"+(obs[1] == null? obs[0] + PerformantNode.SEPARATOR : ""+obs[1] + obs[0] + PerformantNode.SEPARATOR));
              c++;
            }
            taskQuery+=" )";
            qhelp.addOQLClause(taskQuery);

          }
        }
      }
    }

    // search by customer id
    filter = restState.getEntry("CUST_ID").stringValueNullIfEmpty();
    if (filter != null) {
      somethingSearched = true;
      if (!restState.getEntry("TASK_COST_SHOW_CHILDREN").checkFieldValue()) {   // caso semplice solo per il singolo task

        qhelp.addOQLClause(" task.id in (select distinct assc.task.id from " + Assignment.class.getName() + " as assc where assc.resource.id=:custId and assc.role.name like :roleCust)", "custId",filter);
        qhelp.addParameter("roleCust", ApplicationState.getApplicationSetting("DEFAULT_CUSTOMER_ROLE_NAME", "Customer")+"%");

      } else {// caso complesso, con i parent. si deve prima estrarre i task e poi preparare la query
        QueryHelper tqh = new QueryHelper("select distinct assc.task.id,assc.task.ancestorIds from " + Assignment.class.getName() + " as assc where assc.resource.id=:custId and assc.role.name like :roleCust");
        tqh.addParameter("custId",filter);
        tqh.addParameter("roleCust", ApplicationState.getApplicationSetting("DEFAULT_CUSTOMER_ROLE_NAME", "Customer")+"%");
        List<Object[]> tsIdAncid = tqh.toHql().list();
        if (tsIdAncid.size()>0){
          int c=1;
          String taskQuery = " task.id in (select p.id from " + Task.class.getName() + " as p where  ";
          for (Object[] obs:tsIdAncid){
            taskQuery+=((c > 1 ? " or " : "") + "p.id=:tid" + c + " or p.ancestorIds like :ancid" + c) ;
            qhelp.addParameter("tid" + c, obs[0]);
            qhelp.addParameter("ancid"+c, "%"+(obs[1] == null? obs[0] + PerformantNode.SEPARATOR : ""+obs[1] + obs[0] + PerformantNode.SEPARATOR));
            c++;
          }
          taskQuery+=" )";
          qhelp.addOQLClause(taskQuery);

        }

      }
    }

    filter = restState.getEntry("TASK_TYPE").stringValueNullIfEmpty();
    if (filter != null) {
      somethingSearched = true;
      qhelp.addQBEClause("task.type.id", "typeDescription", filter, QueryHelper.TYPE_INT);
    }

    somethingSearched = somethingSearched | ActionUtilities.addQBEClause("TASK_TAGS", "task.tags", "tags", qhelp, QueryHelper.TYPE_CHAR, restState);
    somethingSearched = somethingSearched | DesignerField.queryCustomFields("ADD_COST_CUSTOM_FIELD_", 4, "cost", qhelp, restState);

    String assId = restState.getEntry("RES_ID").stringValueNullIfEmpty();
    if (JSP.ex(assId)) {
      qhelp.addOQLClause("assignment.resource.id = :assignee", "assignee", assId);
      somethingSearched = true;
    } else {
      String assigText = restState.getEntry("RES_ID" + SmartCombo.TEXT_FIELD_POSTFIX).stringValueNullIfEmpty();
      if (JSP.ex(assigText)) {
        qhelp.addQBEORClauses(
          assigText,
          qhelp.getOrElement("assignment.resource.name", "name", QueryHelper.TYPE_CHAR),
          qhelp.getOrElement("assignment.resource.personSurname || ' ' || assignment.resource.personName", "surnameName", QueryHelper.TYPE_CHAR),
          qhelp.getOrElement("assignment.resource.personName || ' ' || assignment.resource.personSurname", "nameSurname", QueryHelper.TYPE_CHAR)
        );
        somethingSearched = true;
      }
    }


    filter = restState.getEntry("DESCRIPTION").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause("cost.description", "description", filter, QueryHelper.TYPE_CHAR);
      somethingSearched = true;
    }

    filter = restState.getEntry("COST_AT_DAY").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause("cost.creationDate", "creationDate", filter, QueryHelper.TYPE_DATE);
      somethingSearched = true;
    }


    filter = restState.getEntry("REAL_COST").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause("cost.realCost", "realCost", filter, QueryHelper.TYPE_DOUBLE);
      somethingSearched = true;
    }

    filter = restState.getEntry("COST_ESTIMATED").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause("cost.estimatedCost", "estCst", filter, QueryHelper.TYPE_DOUBLE);
      somethingSearched = true;
    }

    if (JSP.ex(restState.getEntry("ONLY_COST_WITH_ATTACHMENT"))) {
      if (restState.getEntry("ONLY_COST_WITH_ATTACHMENT").checkFieldValue()) {
        qhelp.addOQLClause("cost.attachment is not null");
        somethingSearched = true;
      } else {
        qhelp.addOQLClause("cost.attachment is null");
        somethingSearched = true;
      }
    }


    //add security on task
    Set<Permission> perms = new HashSet();
    perms.add(TeamworkPermissions.task_cost_canRead);
    perms.add(TeamworkPermissions.task_cost_canWrite);
    TaskBricks.addSecurityClauses(qhelp,perms, restState);

    // compute sum: si cannibalizza la query mettendo il sum ed in group (obbligatorio perchè la query ha il distinct)
    String sumS = "select sum(cost.estimatedCost), sum(cost.realCost) from "+Cost.class.getName()+" as cost where cost.id in (select cost.id ";
    String sumG = ")";
    qhelp.setHqlString(StringUtilities.replaceAllNoRegex(qhelp.getHqlString(), "select distinct task,cost", sumS));
    qhelp.setHqlString(qhelp.getHqlString()+" "+sumG);
    OqlQuery oqlQuerySum = qhelp.toHql();

    Object[] totCosts = (Object[]) oqlQuerySum.uniqueResultNullIfEmpty();
    if (totCosts != null) {
      restState.addClientEntry("COST_EST_TOTAL", totCosts[0]!=null?(new Double(totCosts[0]+"")):0.0d);
      restState.addClientEntry("COST_TOTAL", totCosts[1]!=null?(new Double(totCosts[1]+"")):0.0d);
    }

    // restore query sanity before apply order
    qhelp.setHqlString(StringUtilities.replaceAllNoRegex(qhelp.getHqlString(), sumS, "select distinct task,cost"));
    qhelp.setHqlString(qhelp.getHqlString().substring(0,qhelp.getHqlString().length()-2));


    ListHeader.orderAction(qhelp, "COSTLISTDTBL", restState, "cost.creationDate");
    OqlQuery oqlQuery = qhelp.toHql();



    //if (somethingSearched)
    restState.setPage(HibernatePage.getHibernatePageInstance(oqlQuery.getQuery(), Paginator.getWantedPageNumber(restState), Paginator.getWantedPageSize("COSTLISTDTBL", restState)));

  }




}
