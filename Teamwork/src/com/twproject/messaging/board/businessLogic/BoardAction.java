package com.twproject.messaging.board.businessLogic;

import com.twproject.messaging.board.Board;
import com.twproject.messaging.stickyNote.StickyNote;
import com.twproject.operator.TeamworkOperator;
import com.twproject.security.TeamworkPermissions;
import com.twproject.rank.Hit;
import org.hibernate.Query;
import org.jblooming.messaging.Listener;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.security.Area;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.view.RestState;

import java.util.List;
import java.util.Set;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class BoardAction extends ActionSupport {

  public TeamworkOperator logged;
  public Board board;

  public BoardAction(RestState pageState) {
    super(pageState);
    this.logged = (TeamworkOperator) pageState.getLoggedOperator();

  }
  public void cmdAdd() throws PersistenceException, org.jblooming.security.SecurityException {

    board = new Board();
    board.setIdAsNew();
    board.setArea(logged.getDefaultAreaForPermission(TeamworkPermissions.board_canCreate));

    board.testPermission(logged, TeamworkPermissions.board_canCreate);

    board.setOwner(logged);

    restState.setMainObject(board);
    restState.addClientEntry("BOARD_ACTIVE", Fields.TRUE);

  }

  public void cmdGuess() throws PersistenceException, SecurityException, ActionException {
    board = null;
    if (board == null)
      board = Board.load(restState.getMainObjectId());

    if (board != null) {
      restState.mainObjectId = board.getId();
      cmdEdit();
      if (!board.hasPermissionFor(logged, TeamworkPermissions.board_canRead))
        throw new ActionException("REF_PERMISSION_LACKING");
      make(board);
    } else {
      throw new ActionException("REF_NOT_FOUND");
    }

  }

  public void cmdEdit() throws PersistenceException, SecurityException {
    Board board = (Board) PersistenceHome.findByPrimaryKey(Board.class, restState.getMainObjectId());

    board.testPermission(logged, TeamworkPermissions.board_canRead);

    restState.setMainObject(board);
    make(board);

    Hit.getInstanceAndStore(board, logged, .1);
  }

  public void cmdSave() throws PersistenceException, ActionException, SecurityException {

    boolean invalidClientEntries = false;

    Board board = null;
    boolean isNew = PersistenceHome.NEW_EMPTY_ID.equals(restState.mainObjectId);
    if (isNew) {
      board = new Board();
      board.setIdAsNew();
      ActionUtilities.setIdentifiable(restState.getEntryAndSetRequired("AREA"), board, "area");
    } else
      board = (Board) PersistenceHome.findByPrimaryKey(Board.class, restState.getMainObjectId());

    board.testPermission(logged, TeamworkPermissions.board_canWrite);
    
    if (isNew)
      board.setOwner(restState.getLoggedOperator());

    restState.setMainObject(board);

    try {
      board.setName(restState.getEntryAndSetRequired("NAME").stringValue());
    } catch (ActionException a) {
      invalidClientEntries = true;
    }

    board.setDescription(JSP.limWr(restState.getEntry("DESCRIPTION").stringValue(),4000));

    ActionUtilities.setIdentifiable(restState.getEntryAndSetRequired("AREA"), board, "area");

    ActionUtilities.setBoolean(restState.getEntry("BOARD_ACTIVE"), board, "active");

    if (!invalidClientEntries) {
      board.store();
      restState.mainObjectId=board.getId();
      Hit.getInstanceAndStore(board, logged, .3);
      cmdSaveSubs();
    }
  }

  public void cmdDelete() throws PersistenceException, SecurityException {
    Board delenda = (Board) PersistenceHome.findByPrimaryKey(Board.class, restState.getMainObjectId());
    delenda.testPermission(logged, TeamworkPermissions.board_canCreate);
    DeleteHelper.cmdDelete(delenda, restState);
  }

  private void make(Board board) throws PersistenceException {
    TeamworkOperator teamworkOperator = (TeamworkOperator) restState.getLoggedOperator();

    restState.addClientEntry("NAME", board.getName());
    restState.addClientEntry("DESCRIPTION", board.getDescription());
    restState.addClientEntry("BOARD_ACTIVE", board.isActive() ? Fields.TRUE : Fields.FALSE);
    if (board.getArea() != null)
      restState.addClientEntry("AREA", board.getArea().getId());

    String hql = "select listener from " + Listener.class.getName() + " as listener where listener.owner=:own and listener.identifiableId=:iid and listener.theClass=:tcl";
    QueryHelper listenerQH = new QueryHelper(hql);
    listenerQH.addParameter("own",teamworkOperator);
    listenerQH.addParameter( "iid",board.getId().toString());
    listenerQH.addParameter("tcl",Board.class.getName());


    Listener l = (Listener) listenerQH.toHql().uniqueResultNullIfEmpty();

    if (l != null) {

      List<String> medias = StringUtilities.splitToList(l.getMedia(), ",");
      for (String media : medias) {
        restState.addClientEntry("BOARD_SUBSCRIBE_" + media, Fields.TRUE);
      }
    }


  }

  public void cmdSaveSubs() throws PersistenceException, SecurityException {

    TeamworkOperator teamworkOperator = (TeamworkOperator) restState.getLoggedOperator();

    Board board = (Board) PersistenceHome.findByPrimaryKey(Board.class, restState.getMainObjectId());
    board.testPermission(logged, TeamworkPermissions.board_canRead);
    restState.setMainObject(board);

    //recreate listener
    String hql = "from " + Listener.class.getName() + " as listen where listen.owner = :owner and listen.theClass = :theClass and listen.identifiableId = :identifiableId";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("owner", teamworkOperator);
    oql.getQuery().setString("theClass", Board.class.getName());
    oql.getQuery().setString("identifiableId", board.getId().toString());
    List<Listener> delendi = oql.list();
    for (Listener l : delendi) {
      l.remove();
    }

    String prefix = "BOARD_SUBSCRIBE_";
    String mediaSubscribed = MessagingSystem.mediaSubscribed(prefix, restState);
    if (mediaSubscribed.length() > 0) {
      Listener l = new Listener(teamworkOperator);
      l.setIdAsNew();
      l.setIdentifiable(board);
      l.setMedia(mediaSubscribed);
      l.setEventType("BOARD_POST_MODIFIED");
      l.setOneShot(true);
      l.store();
    }
  }

  public void cmdFind() throws PersistenceException {

    String hql = "select distinct b from " +StickyNote.class.getName() + " as s right outer join s.board as b where "+
            "(s.message like :src or b.name like :src or b.description like :src) ";

    if (!restState.getEntry("SHOW_NOTACTIVE_BOARDS").checkFieldValue())
      hql+="and (b.active=true)";

    Set<Area> areasForPermission = logged.getAreasForPermission(TeamworkPermissions.board_canRead);
    if (!logged.hasPermissionAsAdmin()) {
      if (JSP.ex(areasForPermission))
        hql += "and (b.area in (:areas)) ";
      else
        hql += "and (b.area is null) ";
    }

    hql+="order by b.active desc, b.lastPostedOn desc";

    Query query = new OqlQuery(hql).getQuery();
    query.setString("src", "%" + JSP.w(restState.getEntry("SEARCH").stringValueNullIfEmpty()) + "%");

    if (!logged.hasPermissionAsAdmin() && JSP.ex(areasForPermission)) {
      query.setParameterList("areas", areasForPermission);
    }

    HibernatePage page = HibernatePage.getHibernatePageInstance(query, Paginator.getWantedPageNumber(restState), Paginator.getWantedPageSize("ISSUEFILTER", restState));
    restState.setPage(page);
  }

  public static List<Board> findBoards(boolean showAlsoNotActive) throws FindException {

    String hql = "from "+Board.class.getName()+" as board ";
    if (!showAlsoNotActive)
      hql = hql + "where board.active = :truth";

    OqlQuery oql = new OqlQuery(hql);
    if (!showAlsoNotActive)
      oql.getQuery().setBoolean("truth",Boolean.TRUE);

    return oql.list();

  }

}