package com.opnlb.fulltext;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.snowball.SnowballAnalyzer;
import org.apache.lucene.util.Version;

import java.io.Reader;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Oct 18, 2007
 * Time: 4:19:40 PM
 */
public class SnowballHackedAnalyzer extends org.apache.lucene.analysis.Analyzer {

  public static String language="English";
   SnowballAnalyzer sa;

   public SnowballHackedAnalyzer() {
    sa = new SnowballAnalyzer(Version.LUCENE_30,language);
  }

  @Override
  protected TokenStreamComponents createComponents(String s, Reader reader) {
    return sa.createComponents(s, reader);
  }

}
