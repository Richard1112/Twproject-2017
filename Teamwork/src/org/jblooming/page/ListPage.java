package org.jblooming.page;

import org.apache.log4j.Logger;
import org.jblooming.tracer.Tracer;

import java.util.List;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class ListPage implements Page {

  private List elements;
  private int pageSize;
  private int pageNumber;

  /**
   * Construct a new ListPage. ListPage numbers are zero-based, so the
   * first page is page 0.
   *
   * @param pageNumber the page number (zero-based);
   *                   if Integer.MAX_VALUE will return the last page for the query
   * @param pageSize   the number of results to display on the page
   */
  public ListPage(List elements, int pageNumber, int pageSize) {
    this.elements = elements;
    this.pageSize = pageSize;
    this.pageNumber = pageNumber;
    if (Integer.MAX_VALUE == this.pageNumber)
      this.pageNumber = ((getTotalNumberOfElements()-1) / this.pageSize);
    else if (this.pageNumber > ( (getTotalNumberOfElements()-1) / this.pageSize))
      this.pageNumber = (elements.size()-1) / this.pageSize;

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
/*
* We use the Math.floor() method because page numbers are zero-based
* (i.e. the first page is page 0).
*/
    double totalResults = new Integer( (getTotalNumberOfElements()-1)).doubleValue();
    return new Double(Math.floor(totalResults / getPageSize())).intValue();
  }

  public List getThisPageElements() {

/*
* Since we retrieved one more than the specified pageSize when the
* class was constructed, we now trim it down to the pageSize if a next
* page exists.
*/
    final int start = getPageNumber() * getPageSize();
    if(start != -1)
    return elements.subList(Math.min(start, getTotalNumberOfElements()),
            Math.min(start + getPageSize(), getTotalNumberOfElements()));
    return elements;
  }

  public Logger getLogger() {
    return Tracer.hibernateLogger;
  }

  /**
   * this is 0-based, differently from list.size();
   */
  public int getTotalNumberOfElements() {
    return elements.size();
  }

  public int getThisPageFirstElementNumber() {
    return getPageNumber() * getPageSize() + 1;
  }

  public int getThisPageLastElementNumber() {
    int fullPage = getThisPageFirstElementNumber() + getPageSize() - 1;
    return (getTotalNumberOfElements()-1) < fullPage ?
            (getTotalNumberOfElements()-1) :
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
    return elements;
  }

}

