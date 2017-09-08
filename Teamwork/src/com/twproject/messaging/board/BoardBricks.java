package com.twproject.messaging.board;

import com.twproject.operator.TeamworkOperator;
import com.twproject.security.TeamworkPermissions;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.Area;
import org.jblooming.waf.Bricks;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.oql.QueryHelper;
import org.jblooming.waf.view.RestState;

import java.util.Set;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class BoardBricks extends Bricks {

  public Board mainObject;


  public BoardBricks(Board document) {
    this.mainObject = document;
  }

  public static SmartCombo getVisibleBoards(String fieldName,RestState pageState) throws PersistenceException {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Set<Area> areaLst = logged.getAreasForPermission(TeamworkPermissions.board_canRead);
    String hqlBoard = "select board.id, board.name from " + Board.class.getName() + " as board";
    String whereForId = "where board.id = :" + SmartCombo.FILTER_PARAM_NAME;

    String whereForfiltering = "board.description like :" + SmartCombo.FILTER_PARAM_NAME + " and board.active = :truth";
    QueryHelper queryHelperForFiltering = new QueryHelper(hqlBoard);
    queryHelperForFiltering.addQueryClause(whereForfiltering);
    queryHelperForFiltering.addParameter("truth", Boolean.TRUE);

    whereForfiltering = "board.area in (:myarea)";
    queryHelperForFiltering.addQueryClause(whereForfiltering);
    queryHelperForFiltering.addParameter("myarea", areaLst);

    queryHelperForFiltering.addToHqlString("order by board.description");

    SmartCombo smc = new SmartCombo(fieldName, hqlBoard, null, whereForId);
    smc.queryHelperForFiltering = queryHelperForFiltering;

    return smc;
  }

}
