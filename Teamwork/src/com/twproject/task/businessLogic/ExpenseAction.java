package com.twproject.task.businessLogic;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Resource;
import com.twproject.security.TeamworkArea;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Assignment;
import com.twproject.task.Task;
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
import java.util.*;


public class ExpenseAction extends ActionSupport {

  public TeamworkOperator logged;
  public String url;

  public ExpenseAction(RestState restState) {
    super(restState);
    this.logged = (TeamworkOperator) restState.getLoggedOperator();
  }


  // save assignment cost
  public JSONObject cmdSave() throws PersistenceException, ActionException, ParseException {
    restState.initializeEntries("row");

    String assId = restState.getEntry("assId").stringValueNullIfEmpty();

    JSONObject jCost=null;

    ClientEntry dateEntry = restState.getEntry("creationDate");

    if (assId != null ) {
      Assignment assig = Assignment.load(assId);
      Task task = assig.getTask();
      boolean isYours = logged.getPerson().equals(assig.getResource());
      boolean canManage = assig.hasPermissionFor(logged, TeamworkPermissions.expense_manage);
      if (isYours || canManage) {
        Cost cost=Cost.load(restState.getEntry("costId").intValueNoErrorCodeNoExc() + "");

        if (cost == null) {
          // add
          cost = new Cost();
          assig.getCosts().add(cost);

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
        ActionUtilities.setCurrency(restState.getEntry("realCost"), cost, "realCost");
        ActionUtilities.setDate(dateEntry, cost, "creationDate");
        ActionUtilities.setIdentifiable(restState.getEntry("classification"), cost, "classification");
        //Custom fields
        DesignerField.saveCustomFields("COST_CUSTOM_FIELD_", 4, cost, restState);


        if (isYours && cost.isLockedByDateOrStatus()){
          dateEntry.errorCode = "CANNOT_SAVE_COST_IN_THE_PAST";
          return jCost;
        }

        cost.store();
        jCost=cost.jsonify();
        jCost.element("assId",assig.getId());
        jCost.element("taskId",assig.getTask().getId());
        jCost.element("taskName", assig.getTask().getName());
        jCost.element("taskCode", assig.getTask().getCode());

        assig.store();
      }
    }
    return jCost;
  }


  public void cmdPrepareDefaultFind() throws PersistenceException {
    TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();

    boolean canReadCostsGlobally = logged.hasPermissionFor(TeamworkPermissions.task_cost_canRead) || logged.hasPermissionFor(TeamworkPermissions.expense_manage);

    //search for default filter
    if (restState.getCommand() == null)
      if (!PersistentSearch.feedFromDefaultSearch("COSTSFILTER", restState)) {
        // when not set use my open task
        restState.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_MONTH_COSTS");
      }

    if (!canReadCostsGlobally) {
      //check permission on task
      if (JSP.ex(restState.getEntry("TASK"))) {
        Task task = Task.load(restState.getEntry("TASK").stringValueNullIfEmpty() + "");
        if (task == null || !(task.hasPermissionFor(logged, TeamworkPermissions.task_cost_canRead) || logged.hasPermissionFor(TeamworkPermissions.expense_manage))) {
          restState.addClientEntry("RES_ID", logged.getPerson().getId());
        }
      } else {
        if (!JSP.ex(restState.getEntry("RES_ID"))) {
          restState.addClientEntry("RES_ID", logged.getPerson().getId());
        } else {
          Resource res = Resource.load(restState.getEntry("RES_ID").intValueNoErrorCodeNoExc() + "");
          if (res == null || (!res.hasPermissionFor(logged, TeamworkPermissions.expense_manage) )) {
            restState.addClientEntry("RES_ID", logged.getPerson().getId());
            restState.addClientEntry("RES_ID_txt", logged.getPerson().getDisplayName());
          }
        }
      }
    }

    //search for default filter
    if (restState.getCommand() == null)
      PersistentSearch.feedFromDefaultSearch("COSTFILTER", restState);


    if (!PersistentSearch.feedFromSavedSearch(restState)) {
      // uso di un filtro presettato
      String cmd = restState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
      if (JSP.ex(cmd)) {
        restState.getClientEntries().getClientEntries().clear();

        // add the filter name in order to display boldify
        restState.addClientEntry(Fields.FLD_FILTER_NAME, cmd);

        if ("PF_MY_MONTH_COSTS".equals(cmd)) {
          restState.addClientEntry("RES_ID", logged.getPerson().getId());
          restState.addClientEntry("COST_AT_DAY", "TM");

        } else if ("PF_MY_WEEK_COST".equals(cmd)) {
          restState.addClientEntry("RES_ID", logged.getPerson().getId());
          restState.addClientEntry("COST_AT_DAY", "TW");
        }
      }
    }
  }


  public void cmdFind() throws PersistenceException {
    TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();
    cmdPrepareDefaultFind();

    boolean somethingSearched = false;
    String filter = null;
    String hql = "select assig,cost from " + Assignment.class.getName() + " as assig join assig.costs as cost";
    QueryHelper qhelp = new QueryHelper(hql);

    //areas
    String areaId=restState.getEntry("AREA").stringValueNullIfEmpty();
    if (JSP.ex(areaId)){
      TeamworkArea ta= TeamworkArea.load(areaId);
      qhelp.addOrQueryClause("assig.task.area = :area");
      qhelp.addParameter("area", ta);
      somethingSearched=true;
    }

    // si aggiungo le aree su cui si è abilitati anche se c'è quella sopra in modo da essere sicuri che non si salti la sicurezza
    if (!logged.hasPermissionAsAdmin()) {
      Set<Area> areas = new HashSet();
      areas.addAll(logged.getAreasForPermission(TeamworkPermissions.task_cost_canRead));
      areas.addAll(logged.getAreasForPermission(TeamworkPermissions.expense_manage));
      if (JSP.ex(areas)) {
        qhelp.addOrQueryClause("assig.task.area in (:areas)");
        qhelp.addParameter("areas", areas);
      }
    }


    // search by task id
    filter = restState.getEntry("TASK").stringValueNullIfEmpty();
    if (filter != null) {
      somethingSearched = true;
      if (!restState.getEntry("TASK_COST_SHOW_CHILDREN").checkFieldValue()) {
        qhelp.addOQLClause(" assig.task.id = :taskId", "taskId", filter);
      } else {
        Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, filter);
        qhelp.addOQLClause(" assig.task.id in (select p.id from " + Task.class.getName() + " as p where p.id=:tid or p.ancestorIds like :ancid )", "tid", task.getId());
        qhelp.addParameter("ancid", task.getChildAncentorIds());
      }

      // search by name/code
    } else {
      filter = restState.getEntry("TASK" + SmartCombo.TEXT_FIELD_POSTFIX).stringValueNullIfEmpty();
      if (filter != null) {
        somethingSearched = true;

        if (!restState.getEntry("TASK_WORKLOG_SHOW_CHILDREN").checkFieldValue()) {   // caso semplice solo per nome o codice

          qhelp.addQBEORClauses(filter, qhelp.getOrElement("assig.task.name", "taskName", QueryHelper.TYPE_CHAR), qhelp.getOrElement("assig.task.code", "taskCode", QueryHelper.TYPE_CHAR));

        } else {// caso complesso, si deve prima estrarre i task e poi preparare la query
          QueryHelper tqh = new QueryHelper("select t.id,t.ancestorIds from " + Task.class.getName() + " as t");
          tqh.addQBEORClauses(filter, qhelp.getOrElement("t.name", "taskName", QueryHelper.TYPE_CHAR), qhelp.getOrElement("t.code", "taskCode", QueryHelper.TYPE_CHAR));
          List<Object[]> tsIdAncid = tqh.toHql().list();
          if (tsIdAncid.size()>0){
            int c=1;
            String taskQuery = " assig.task.id in (select p.id from " + Task.class.getName() + " as p where  ";
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

        qhelp.addOQLClause(" assig.task.id in (select distinct assc.task.id from " + Assignment.class.getName() + " as assc where assc.resource.id=:custId and assc.role.name like :roleCust)", "custId",filter);
        qhelp.addParameter("roleCust", ApplicationState.getApplicationSetting("DEFAULT_CUSTOMER_ROLE_NAME", "Customer")+"%");

      } else {// caso complesso, con i parent. si deve prima estrarre i task e poi preparare la query
        QueryHelper tqh = new QueryHelper("select distinct assc.task.id,assc.task.ancestorIds from " + Assignment.class.getName() + " as assc where assc.resource.id=:custId and assc.role.name like :roleCust");
        tqh.addParameter("custId",filter);
        tqh.addParameter("roleCust", ApplicationState.getApplicationSetting("DEFAULT_CUSTOMER_ROLE_NAME", "Customer")+"%");
        List<Object[]> tsIdAncid = tqh.toHql().list();
        if (tsIdAncid.size()>0){
          int c=1;
          String taskQuery = " assig.task.id in (select p.id from " + Task.class.getName() + " as p where  ";
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
      qhelp.addQBEClause("assig.task.type.id", "typeDescription", filter, QueryHelper.TYPE_INT);
    }

    somethingSearched = somethingSearched | ActionUtilities.addQBEClause("TASK_TAGS", "assig.task.tags", "tags", qhelp, QueryHelper.TYPE_CHAR, restState);
    somethingSearched = somethingSearched | DesignerField.queryCustomFields("COST_CUSTOM_FIELD_", 4, "cost", qhelp, restState);

    String assId = restState.getEntry("RES_ID").stringValueNullIfEmpty();
    if (JSP.ex(assId)) {
      qhelp.addOQLClause("assig.resource.id = :assignee", "assignee", assId);
      somethingSearched = true;
    } else {
      String assigText = restState.getEntry("RES_ID" + SmartCombo.TEXT_FIELD_POSTFIX).stringValueNullIfEmpty();
      if (JSP.ex(assigText)) {
        qhelp.addQBEORClauses(
          assigText,
          qhelp.getOrElement("assig.resource.name", "name", QueryHelper.TYPE_CHAR),
          qhelp.getOrElement("assig.resource.personSurname || ' ' || assig.resource.personName", "surnameName", QueryHelper.TYPE_CHAR),
          qhelp.getOrElement("assig.resource.personName || ' ' || assig.resource.personSurname", "nameSurname", QueryHelper.TYPE_CHAR)
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

    if (JSP.ex(restState.getEntry("ONLY_COST_WITH_ATTACHMENT"))) {
      if (restState.getEntry("ONLY_COST_WITH_ATTACHMENT").checkFieldValue()) {
        qhelp.addOQLClause("cost.attachment is not null");
        somethingSearched = true;
      } else {
        qhelp.addOQLClause("cost.attachment is null");
        somethingSearched = true;
      }
    }


    int wlStat = restState.getEntry("COST_STATUS").intValueNoErrorCodeNoExc();
    if (wlStat == 0 && restState.getEntry("COST_STATUS").stringValueNullIfEmpty() != null) {
      qhelp.addOQLClause("cost.status is null");
    } else if (wlStat > 0) {
      qhelp.addOQLClause("cost.status.id =:wlstat", "wlstat", wlStat);
    }

    /* //todo sarebbe bello ma nel caso di "to be approved" il valore è 0 mentre sul record c'è un null
    String wlSts = restState.getEntry("WORKLOG_STATUS").stringValueNullIfEmpty();
    if (JSP.ex(wlSts)) {
      somethingSearched = true;
      String[] idss = wlSts.split(",");
      List<Integer> idsi = new ArrayList();
      for (String id : idss) idsi.add(Integer.valueOf(id)<0?null:Integer.valueOf(id)); //sono interi e vanno convertiti
      qhelp.addOQLInClause("worklog.status.id", "wlstat", idsi);
    }*/


    //todo qui si faceva la somma ma con la nuova query non so come si fa

    // compute sum
    String sumS = "select sum(cost.realCost)";
    qhelp.setHqlString(StringUtilities.replaceAllNoRegex(qhelp.getHqlString(), "select assig,cost", sumS));
    OqlQuery oqlQuerySum = qhelp.toHql();

    // restore query sanity before apply order
    qhelp.setHqlString(StringUtilities.replaceAllNoRegex(qhelp.getHqlString(), sumS, "select assig,cost"));


    ListHeader.orderAction(qhelp, "COSTLISTDTBL", restState, "cost.creationDate");
    OqlQuery oqlQuery = qhelp.toHql();


    Double totCost = (Double) oqlQuerySum.uniqueResultNullIfEmpty();
    if (totCost != null ) {
      restState.addClientEntry("COST_TOTAL", (Double) totCost);
    }

    if (somethingSearched)
      restState.setPage(HibernatePage.getHibernatePageInstance(oqlQuery.getQuery(), Paginator.getWantedPageNumber(restState), Paginator.getWantedPageSize("COSTLISTDTBL", restState)));

  }


  public void bulkSetStatus() throws PersistenceException, SecurityException {
    restState.initializeEntries("table");
    String stid = restState.getEntryAndSetRequired("WL_STATUS").stringValueNullIfEmpty();
    WorklogStatus wls = null;
    if (stid != null)
      wls = (WorklogStatus) PersistenceHome.findByPrimaryKey(WorklogStatus.class, stid);
    Set<String> assCostIds = StringUtilities.splitToSet(JSP.w(restState.getEntry("ASSID_COSTID").stringValueNullIfEmpty()),",");
    for (String assCostId : assCostIds) {
      String[]ids=assCostId.split("_");
      Assignment ass = Assignment.load(ids[0]);
      Cost cost = Cost.load(ids[1]);
      if (ass!=null && cost!=null) {
        ass.testPermission(logged, TeamworkPermissions.expense_manage);
        cost.setStatus(wls);
        cost.store();
      }
    }
  }


}
