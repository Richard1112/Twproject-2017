package org.jblooming.page;

import org.apache.log4j.Logger;
import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.hibernate.search.FullTextQuery;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.tracer.Tracer;
import org.jblooming.waf.settings.PersistenceConfiguration;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class HibernatePage implements Page {

  protected Query query;

  protected List elements;
  protected int pageSize;
  protected int pageNumber;
  protected int totalElements = -1;

//  public static Set jdbcClassesSupportingScrollCursors = new HashSet();

  private static Boolean jdbcSupportsScrollable=null;

  private ScrollableResults scrollableResults;
  public static final int DEFAULT_PAGE_SIZE = 10 ;

  private HibernatePage(int pageNumber, int pageSize) {
    this.pageNumber = pageNumber;
    this.pageSize = pageSize;

    if (this.pageSize<=0) {
      this.pageSize= HibernatePage.DEFAULT_PAGE_SIZE;
    }


  }


  public boolean isFirstPage() {
    return getPageNumber() == 0;
  }

  public boolean isLastPage() {
    return getPageNumber() >= getLastPageNumber();
  }

  public boolean hasNextPage() {
    return !isLastPage();
  }

  public boolean hasPreviousPage() {
    return getPageNumber() > 0;
  }

  public int getLastPageNumber() {

    double totalResults = new Integer(getTotalNumberOfElements()-1).doubleValue();
    return new Double(Math.floor((totalResults) / getPageSize())).intValue();
  }

  public List getThisPageElements() {
    return elements;
  }

  public Logger getLogger() {
    return Tracer.hibernateLogger;
  }

 public int getTotalNumberOfElements() {
    return totalElements;
  }

  public int getThisPageFirstElementNumber() {
    return getPageNumber() * getPageSize() + 1;
  }

  public int getThisPageLastElementNumber() {
    int fullPage = getThisPageFirstElementNumber() + getPageSize() - 1;
    return getTotalNumberOfElements() < fullPage ?
            getTotalNumberOfElements() :
            fullPage;
  }

  public int getNextPageNumber() {
    return getPageNumber() + 1;
  }

  public int getPreviousPageNumber() {
    return getPageNumber() - 1;
  }

  public int getPageSize() {
    return pageSize;
  }

  public int getPageNumber() {
    return pageNumber;
  }

  public List getAllElements() {
    HibernatePage pageTmp = getHibernatePageInstance(query,0,getTotalNumberOfElements());
    return pageTmp.getThisPageElements();
  }


  public static HibernatePage getHibernatePageInstance(Query query, int pageNumber, int pageSize) {

    return getHibernatePageInstance(query, pageNumber, pageSize, PersistenceConfiguration.getDefaultPersistenceConfiguration().driver_class);
  }


  public static boolean isJdbcScrollable(){

    if(jdbcSupportsScrollable==null){
      try {
        Connection connection = PersistenceContext.getDefaultPersistenceContext().session.connection();
        DatabaseMetaData metadata = connection.getMetaData();
        jdbcSupportsScrollable=metadata.supportsResultSetType(ResultSet.TYPE_SCROLL_SENSITIVE) || metadata.supportsResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);
      } catch (SQLException e) {
        Tracer.platformLogger.error(e);
        throw new PlatformRuntimeException(e);
      }
    }
    return jdbcSupportsScrollable;
  }

  public static HibernatePage getHibernatePageInstance(Query query, int pageNumber, int pageSize, String driverClass) {
    if (query.getQueryString().toLowerCase().indexOf("order by")==-1 && !(query instanceof FullTextQuery)) {
      Tracer.platformLogger.warn("Using pagination without order by can lead to inconsistent results, for example on certain Oracle instances: "+query.getQueryString());
      Tracer.platformLogger.warn("Query used in: "+Tracer.getCallTrace(false));
    }
    query.setReadOnly(true);

    if (HibernatePage.isJdbcScrollable())
      return HibernatePage.getPageSupportingScroll(query, pageNumber, pageSize);
    else
      return HibernatePage.getPageNonSupportingScroll(query, pageNumber, pageSize);
  }

  /**
   * Construct a new HibernatePage. HibernatePage numbers are zero-based so the
   * first page is page 0.
   *
   * @param query      the Hibernate Query
   * @param pageNumber the page number (zero-based);
   *                   if Integer.MAX_VALUE will return the last page for the query
   * @param pageSize   the number of results to display on the page
   */
  protected static HibernatePage getPageSupportingScroll(Query query, int pageNumber, int pageSize) {

    HibernatePage sp = new HibernatePage(pageNumber, pageSize);
    sp.query = query;
    try {
      sp.scrollableResults = query.scroll(ScrollMode.SCROLL_SENSITIVE);
      sp.scrollableResults.last();
      sp.totalElements = sp.scrollableResults.getRowNumber()+1;  //as rownumber is zero based

      if (Integer.MAX_VALUE == sp.pageNumber)
        sp.pageNumber = (sp.totalElements / sp.pageSize);
      else if (sp.pageNumber> Math.ceil(sp.totalElements/sp.pageSize))
        sp.pageNumber = (int)Math.ceil(sp.totalElements/sp.pageSize);

      sp.scrollableResults.first();
      sp.scrollableResults.scroll(sp.pageNumber * sp.pageSize-1);
      sp.elements= new ArrayList(sp.pageSize);
      int stop=sp.pageSize;
      while (sp.scrollableResults.next() && stop>0){
        Object[] objects = sp.scrollableResults.get();
        if (objects.length>1)
          sp.elements.add(objects);
        else
          sp.elements.add(objects[0]);
        stop--;
      }
      sp.scrollableResults.close();
    } catch (HibernateException e) {
      sp.getLogger().error("Failed to create ScrollPage by getScrollPageInstanceWithTotalByScroll: " + e.getMessage());
      throw new PlatformRuntimeException(e);
    }

    return sp;
  }


  /**
   * Construct a new HibernatePage. HibernatePage numbers are zero-based so the
   * first page is page 0.
   *
   * @param query      the Hibernate Query
   * @param pageNumber the page number (zero-based);
   *                   if Integer.MAX_VALUE will return the last page for the query
   * @param pageSize   the number of results to display on the page
   */
  protected static HibernatePage getPageNonSupportingScroll(Query query, int pageNumber, int pageSize) {

    HibernatePage sp = new HibernatePage(pageNumber, pageSize);
    sp.query = query;
    try {
      List list = query.list();
      sp.totalElements = list.size();

      if (Integer.MAX_VALUE == sp.pageNumber)
        sp.pageNumber = (sp.totalElements / sp.pageSize);
      else if (sp.pageNumber> Math.ceil(sp.totalElements/sp.pageSize))
        sp.pageNumber = (int)Math.ceil(sp.totalElements/sp.pageSize);



      int from=sp.pageNumber * sp.pageSize;
      int to=sp.pageNumber * sp.pageSize+sp.pageSize;
      to=to>sp.totalElements?sp.totalElements:to;

      sp.elements=list.subList(from,to);

    } catch (HibernateException e) {
      sp.getLogger().error("Failed to create ScrollPage by getScrollPageInstanceWithTotalByList: " + e.getMessage());
      throw new PlatformRuntimeException(e);
    }

    return sp;
  }


}
