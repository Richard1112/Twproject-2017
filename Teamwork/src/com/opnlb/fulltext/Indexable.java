package com.opnlb.fulltext;

import org.hibernate.search.annotations.Indexed;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Oct 18, 2007
 * Time: 2:01:20 PM
 */

public interface Indexable {

  String getAbstractForIndexing();
  //String getDocumentLanguage();

}
