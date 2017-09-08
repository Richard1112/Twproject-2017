package org.jblooming.page;

import org.apache.log4j.Logger;

import java.util.List;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public interface Page {

  boolean isFirstPage();

  boolean isLastPage();

  boolean hasNextPage();

  boolean hasPreviousPage();

  int getLastPageNumber();

  List getThisPageElements();

  Logger getLogger();

  int getTotalNumberOfElements();

  int getThisPageFirstElementNumber();

  int getThisPageLastElementNumber();

  int getNextPageNumber();

  int getPreviousPageNumber();

  int getPageSize();

  int getPageNumber();

  List getAllElements();  

}
