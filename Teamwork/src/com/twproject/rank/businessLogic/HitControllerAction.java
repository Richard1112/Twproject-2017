package com.twproject.rank.businessLogic;

import com.twproject.rank.Hit;
import org.jblooming.ApplicationException;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Created by rbicchierai on 11/11/2014.
 */
public class HitControllerAction implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException {
    PageState pageState = PageState.getCurrentPageState(request);

    if (Commands.FIND.equals(pageState.command)) {
      String hql = "from " + Hit.class.getName() + " as hit";
      QueryHelper qhelp = new QueryHelper(hql);

      PersistentSearch.feedFromSavedSearch(pageState);

      String ec = pageState.getEntry("ENTITY_CLASS").stringValueNullIfEmpty();
      if (ec != null) {
        qhelp.addQBEClause("hit.entityClass", "entityClass", ec, QueryHelper.TYPE_CHAR);
      }

      ec = pageState.getEntry("ENTITY_ID").stringValueNullIfEmpty();
      if (ec != null) {
        qhelp.addQBEClause("hit.entityId", "entityId", ec, QueryHelper.TYPE_CHAR);
      }


      DataTable.orderAction(qhelp, "HITLLH", pageState, "hit.when DESC");
      pageState.setPage(HibernatePage.getHibernatePageInstance(qhelp.toHql().getQuery(), Paginator.getWantedPageNumber(pageState), Paginator.getWantedPageSize(pageState)));

    } else if ("REPAIR".equals(pageState.command)) {
      Hit.removeDeleted(null);
    }


    return pageState;
  }
}
