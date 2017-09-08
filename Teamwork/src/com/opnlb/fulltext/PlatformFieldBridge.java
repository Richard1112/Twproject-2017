package com.opnlb.fulltext;

import com.opnlb.fulltext.waf.IndexingBricks;
import org.apache.lucene.document.Document;
import org.hibernate.search.bridge.FieldBridge;
import org.hibernate.search.bridge.LuceneOptions;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Nov 9, 2007
 * Time: 1:11:27 PM
 */
public class PlatformFieldBridge implements FieldBridge {

  public void set(String name, Object value, Document document, LuceneOptions luceneOptions) {

    if (value != null) {

      String content = value+"";
      String guessedLanguage = IndexingBricks.guess(content);

      org.apache.lucene.document.Field contentField = new org.apache.lucene.document.Field(name, content, org.apache.lucene.document.Field.Store.NO, org.apache.lucene.document.Field.Index.ANALYZED);
      document.add(contentField);

      org.apache.lucene.document.Field language = new org.apache.lucene.document.Field("language", guessedLanguage, org.apache.lucene.document.Field.Store.YES, org.apache.lucene.document.Field.Index.NOT_ANALYZED);
      document.add(language);

    }
  }
}