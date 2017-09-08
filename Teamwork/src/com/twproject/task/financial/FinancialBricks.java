package com.twproject.task.financial;

import com.twproject.resource.Person;
import com.twproject.security.TeamworkPermissions;
import org.jblooming.oql.QueryHelper;
import org.jblooming.security.Area;
import org.jblooming.waf.Bricks;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import java.util.Set;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class FinancialBricks extends Bricks {

  public static SmartCombo getCostAggregatorCombo(String fieldName, Set<Area> areas, Person manager, String additionalHql, PageState pageState) {

    String hql = "select ca.id, ca.code || ' ' || ca.description from " + CostAggregator.class.getName() + " as ca ";
    QueryHelper queryHelperForFiltering = new QueryHelper(hql);
    String baseFilter = " (ca.code || ' ' || ca.description like :" + SmartCombo.FILTER_PARAM_NAME +
        " or ca.description || ' ' || ca.code like :" + SmartCombo.FILTER_PARAM_NAME + ")";

    if (manager!=null) {
      baseFilter = baseFilter + " and (ca.manager :=manager)";
    }

    if (areas!=null && areas.size() > 0) {
       queryHelperForFiltering.addOQLClause("ca.area in (:areas) or ca.area is null");
       queryHelperForFiltering.addParameter("areas", areas);
     }
    
    if (additionalHql!=null)
      baseFilter = baseFilter + additionalHql;

    queryHelperForFiltering.addOQLClause(baseFilter);
    queryHelperForFiltering.addToHqlString(" order by ca.code");
  

    String whereForId = "where ca.id = :" + SmartCombo.FILTER_PARAM_NAME;

    SmartCombo sc = new SmartCombo(fieldName, hql, null, whereForId);
    sc.queryHelperForFiltering = queryHelperForFiltering;

    if (manager!=null) {
      sc.fixedParams.put("manager", manager);
    }

    sc.separator = "</td><td>";
    sc.fieldSize = 40;

    if (pageState.getLoggedOperator().hasPermissionFor(TeamworkPermissions.classificationTree_canManage)) {
      PageSeed cagr = pageState.pageFromRoot("task/financial/costAggregatorManager.jsp");
      cagr.command = Commands.FIND;
      sc.addEntityButton = ButtonLink.getBlackInstance(I18n.get("ADD"),600,800, cagr);
    }

      return sc;
  }

  public static SmartCombo getCostClassificationCombo(String fieldName,  Area area, PageState pageState) {

     String hql = "select ca.id, ca.stringValue || ' ' || ca.description from " + CostClassification.class.getName() + " as ca ";
     QueryHelper queryHelperForFiltering = new QueryHelper(hql);
     String baseFilter = " (ca.stringValue || ' ' || ca.description like :" + SmartCombo.FILTER_PARAM_NAME +
         " or ca.description || ' ' || ca.stringValue like :" + SmartCombo.FILTER_PARAM_NAME + ")";

    if (area!=null ) {
       queryHelperForFiltering.addOQLClause("ca.area=:area or ca.area is null");
        queryHelperForFiltering.addParameter("area", area);
    }

     queryHelperForFiltering.addOQLClause(baseFilter);
     queryHelperForFiltering.addToHqlString(" order by ca.stringValue");

     String whereForId = "where ca.id = :" + SmartCombo.FILTER_PARAM_NAME;

    SmartCombo sc = new SmartCombo(fieldName, hql, null, whereForId);
    sc.queryHelperForFiltering = queryHelperForFiltering;

    sc.separator = "</td><td>";
    sc.fieldSize = 25;

    if (pageState.getLoggedOperator().hasPermissionFor(TeamworkPermissions.classificationTree_canManage)) {
      PageSeed cagr = pageState.pageFromRoot("task/costClassification.jsp");
      cagr.command = Commands.FIND;
      sc.addEntityButton = ButtonLink.getBlackInstance(I18n.get("ADD"),600,800, cagr);
    }


    return sc;
  }

}
