package org.jblooming.logging.businessLogic;

import com.twproject.operator.TeamworkOperator;
import com.twproject.security.TeamworkPermissions;
import org.jblooming.ApplicationException;
import org.jblooming.logging.DeleteLog;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.table.ListHeader;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Created by rbicchierai on 08/05/2017.
 */
public class DeleteLogAction extends ActionSupport {


  public TeamworkOperator logged;
  public String url;

  public DeleteLogAction(RestState restState) {
    super(restState);
    this.logged = (TeamworkOperator) restState.getLoggedOperator();
  }


  public void cmdPrepareDefaultFind() throws PersistenceException, SecurityException {
    TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();


    //search for default filter
    if (restState.getCommand() == null)
      if (!PersistentSearch.feedFromDefaultSearch("DELLOGFILTER", restState)) {
        // when not set use my open task
        restState.addClientEntry(Fields.FLD_FILTER_NAME, "PF_RECENTLY_DELETED");
      }

    //search for default filter
    if (restState.getCommand() == null)
      PersistentSearch.feedFromDefaultSearch("DELLOGFILTER", restState);


    if (!PersistentSearch.feedFromSavedSearch(restState)) {
      // uso di un filtro presettato
      String cmd = restState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
      if (JSP.ex(cmd)) {
        restState.getClientEntries().getClientEntries().clear();

        // add the filter name in order to display boldify
        restState.addClientEntry(Fields.FLD_FILTER_NAME, cmd);

        if ("PF_RECENTLY_DELETED".equals(cmd)) {
          restState.addClientEntry("deletedOn", ">-7d");

        }
      }
    }
  }


  public void cmdFind() throws PersistenceException, SecurityException {
    logged.testIsAdministrator();
    cmdPrepareDefaultFind();

    String hql = "from " + DeleteLog.class.getName() + " as dl order by dl.deletedOn desc";

    QueryHelper qhelp = new QueryHelper(hql);

    boolean recoveredFromSavedFilter = PersistentSearch.feedFromSavedSearch(restState);

    ActionUtilities.addQBEClause("entityClass","dl.entityClass","ecl",qhelp,QueryHelper.TYPE_CHAR,restState);
    ActionUtilities.addQBEClause("entityId","dl.entityId","eid",qhelp,QueryHelper.TYPE_CHAR,restState);
    ActionUtilities.addQBEClause("entityName","dl.entityName","enm",qhelp,QueryHelper.TYPE_CHAR,restState);
    ActionUtilities.addQBEClause("deletedOn","dl.deletedOn","edo",qhelp,QueryHelper.TYPE_DATE,restState);
    ActionUtilities.addQBEClause("deletedBy","dl.deletedBy","edb",qhelp,QueryHelper.TYPE_CHAR,restState);
    ActionUtilities.addQBEClause("jsonData","dl.jsonData","ejd",qhelp,QueryHelper.TYPE_CLOB,restState);

    DataTable.orderAction(qhelp, "DELLOGDT", restState, "dl.deletedOn");
    restState.setPage(HibernatePage.getHibernatePageInstance(qhelp.toHql().getQuery(), Paginator.getWantedPageNumber(restState), Paginator.getWantedPageSize(restState)));


  }



}
