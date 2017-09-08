package com.twproject.fulltext.waf;

import org.apache.lucene.queryparser.classic.QueryParser;
import org.hibernate.search.engine.ProjectionConstants;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageState;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.ApplicationException;
import org.jblooming.utilities.JSP;
import org.jblooming.oql.OqlQuery;
import org.jblooming.page.HibernatePage;
import org.jblooming.page.Page;
import org.jblooming.page.ListPage;
import org.jblooming.security.Area;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.BooleanClause;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.core.StopAnalyzer;

import org.apache.lucene.index.Term;
import org.apache.lucene.document.Document;
import org.apache.lucene.util.Version;
import org.hibernate.search.FullTextSession;
import org.hibernate.search.FullTextQuery;
import org.hibernate.search.Search;

import com.twproject.operator.TeamworkOperator;
import com.twproject.security.TeamworkPermissions;
import com.twproject.rank.Hit;
import com.twproject.rank.RankUtilities;
import com.twproject.document.TeamworkDocument;
import com.opnlb.fulltext.SnowballHackedAnalyzer;
import org.jblooming.waf.view.RestState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.*;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Jan 29, 2008
 * Time: 11:14:45 AM
 */
public class FullTextSearchControllerAction extends ActionSupport implements ActionController {

  public TeamworkOperator logged;

  public FullTextSearchControllerAction(RestState pageState)  {
    super(pageState);
    this.logged = (TeamworkOperator) pageState.getLoggedOperator();
  }

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException, IOException {
    PageState currentPageState = PageState.getCurrentPageState(request);
    String command = restState.getCommand();
    if (Commands.FIND.equals(command)) {
    //if ("FIND_TEXT".equals(command)) {
      // gets Id Event object
      cmdFindRanked();
    }
    return currentPageState;
  }

  public void cmdFind() throws PersistenceException {

    boolean somethingSearched = false;
    FullTextQuery fullTextQuery = null;

    String filter = restState.getEntry("TEXT").stringValueNullIfEmpty();
    String className = restState.getEntry("CLASSNAME").stringValueNullIfEmpty();

    if (JSP.ex(filter)) {

      try {

        BooleanQuery areasInOR = new BooleanQuery();
        Set<Area> someWayReadable = new HashSet();
        someWayReadable.addAll(logged.getAreasForPermission(TeamworkPermissions.resource_canRead));
        someWayReadable.add(logged.getPerson().getArea());
        String areasTerm = "";
        for (Area area : someWayReadable) {
          if (area != null) {
            Term t = new Term("area.id", area.getId() + "");
            Query areaFilter = new TermQuery(t);
            areasInOR.add(areaFilter, BooleanClause.Occur.SHOULD);
          }
        }

        BooleanQuery bqContentSnow = new BooleanQuery();
        bqContentSnow.add(areasInOR, BooleanClause.Occur.MUST);

        Analyzer snow = new SnowballHackedAnalyzer();
        QueryParser snowParser = new QueryParser(Version.LUCENE_30,"content", snow);
        Query searchQuerySnow = snowParser.parse(filter);
        bqContentSnow.add(searchQuerySnow, BooleanClause.Occur.MUST);


        BooleanQuery bqContentStop = new BooleanQuery();
        bqContentStop.add(areasInOR, BooleanClause.Occur.MUST);

        Analyzer stop = new StopAnalyzer(Version.LUCENE_30);
        QueryParser stopParser = new QueryParser(Version.LUCENE_30,"fullcontent", stop);
        Query searchQueryStop = stopParser.parse(filter);
        bqContentStop.add(searchQueryStop, BooleanClause.Occur.MUST);

        BooleanQuery bqt = new BooleanQuery();
        bqt.add(bqContentSnow, BooleanClause.Occur.SHOULD);
        bqt.add(bqContentStop, BooleanClause.Occur.SHOULD);
        

        BooleanQuery bq = new BooleanQuery();
        bq.add(bqt, BooleanClause.Occur.MUST);

        //filter by type
        if (JSP.ex(className)){
          BooleanQuery bqType = new BooleanQuery();
          Query typeQuery = new TermQuery(new Term(ProjectionConstants.OBJECT_CLASS, TeamworkDocument.class.getName()));
          bqType.add(typeQuery,BooleanClause.Occur.MUST);
          bq.add(bqType, BooleanClause.Occur.MUST);
        }


        FullTextSession fullTextSession = Search.getFullTextSession(PersistenceContext.getDefaultPersistenceContext().session);
        fullTextQuery = fullTextSession.createFullTextQuery(bq);
        fullTextQuery.setProjection(
                //Pietro 29Jan2008
                //removed THIS as if the entity is deleted -> BANG
                //FullTextQuery.THIS,
                //DocumentBuilder.CLASS_FIELDNAME,
                FullTextQuery.DOCUMENT,
                //FullTextQuery.BOOST,  // hib 3.6: BOOST no more available is always 1
                FullTextQuery.SCORE
        );
        somethingSearched = true;


      } catch (Throwable e) {
        restState.getEntry(Fields.FORM_PREFIX + "search").errorCode = "Query text invalid";
      }
    }

    if (somethingSearched) {
      int pageNumber = Paginator.getWantedPageNumber(restState);
      int pageSize = Paginator.getWantedPageSize(restState);
      HibernatePage hibernatePageInstance = HibernatePage.getHibernatePageInstance(fullTextQuery, pageNumber, pageSize);
      if (hibernatePageInstance.getTotalNumberOfElements() == 0 && !filter.endsWith("*") && !filter.contains("\"")) {
        restState.addClientEntry("TEXT", filter + "*");
        cmdFind();
        if (restState.getPage()!=null && restState.getPage().getTotalNumberOfElements() == 0)
          restState.addClientEntry("TEXT", filter.substring(0, filter.length()));
      } else
        restState.setPage(hibernatePageInstance);
    }


  }

  private void cmdFindRanked() throws PersistenceException {
    cmdFind();
    Page page = restState.getPage();
    if (page != null) {
      String hql = "from " + Hit.class.getName() + " as hit where hit.areaId = :area order by hit.when desc";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setInteger("area", (Integer) logged.getPerson().getArea().getId());
      oql.getQuery().setMaxResults(10);
      List<Hit> groupHits = oql.list();
      Map<String, Double> groupEntityHits = RankUtilities.computeWeightForEntities(groupHits);
      double max = 0;
      for (Double d : groupEntityHits.values()) {
        max = Math.max(max, d);
      }
      if (max > 0)
        for (Map.Entry<String, Double> k : groupEntityHits.entrySet()) {
          k.setValue(k.getValue() / max);
        }

    hql = "from " + Hit.class.getName() + " as hit where hit.operatorId = :operator order by hit.id desc";
      oql = new OqlQuery(hql);
      oql.getQuery().setInteger("operator", (Integer) logged.getId());
      oql.getQuery().setMaxResults(1000);

      List<Hit> personalHits = oql.list();
      Map<String, Double> personalEntityHits = RankUtilities.computeWeightForEntities(personalHits);
      max = 0;
      for (Double d : personalEntityHits.values()) {
        max = Math.max(max, d);
      }
      if (max > 0)
        for (Map.Entry<String, Double> k : personalEntityHits.entrySet()) {
          k.setValue(k.getValue() / max);
        }


      List<Object[]> poj = page.getAllElements();
      for (Object[] p : poj) {

        Document lucDoc = (Document) p[0];
        String id = lucDoc.get("id");
        String clazz = lucDoc.get(ProjectionConstants.OBJECT_CLASS);

        Double val = new Double((Float) p[1]);

        if (groupEntityHits.containsKey(clazz + "^" + id))
          val = val + groupEntityHits.get(clazz + "^" + id);


        if (personalEntityHits.containsKey(clazz + "^" + id))
          val = val + personalEntityHits.get(clazz + "^" + id);

        p[1] = val / 3;
      }

      Collections.sort(poj, new Comparator<Object[]>() {
        public int compare(Object[] p1, Object[] p2) {
          return ((Double) p2[1]).compareTo((Double) p1[1]);
        }
      });

      if (poj.size() > 0) {
        ListPage lp = new ListPage(poj, 1, poj.size());
        restState.setPage(lp);
      }
    }

  }


}
