package com.opnlb.fulltext;

import org.hibernate.search.cfg.Environment;
import org.jblooming.persistence.hibernate.PlatformAnnotationConfiguration;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.utilities.JSP;
import org.jblooming.tracer.Tracer;
import org.hibernate.search.store.impl.FSDirectoryProvider;
import org.hibernate.search.FullTextSession;
import org.hibernate.search.Search;
import org.hibernate.search.jpa.FullTextEntityManager;

import javax.persistence.EntityManager;
import java.io.File;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Jun 13, 2008
 * Time: 4:27:36 PM
 */
public class IndexingHelper {

  public static void baseConfiguration(PlatformAnnotationConfiguration hibConfiguration) {
    String indexPath = ApplicationState.getApplicationSetting(IndexingConstants.INDEX_PATH);
    if (!JSP.ex(indexPath)) {
      indexPath = ApplicationState.webAppFileSystemRootPath + File.separator + "WEB-INF" + File.separator + "index";
      ApplicationState.getApplicationSettings().put(IndexingConstants.INDEX_PATH, indexPath);
    }

    if (!new File(indexPath).exists())
      new File(indexPath).mkdirs();


    //if (JSP.ex(indexPath) && new File(indexPath).exists()) {
      try {
        String analyzerLanguage = ApplicationState.getApplicationSetting(IndexingConstants.ANALYZER_LANGUAGE);
        if (JSP.ex(analyzerLanguage))
          SnowballHackedAnalyzer.language = analyzerLanguage;

        hibConfiguration.getProperties().put(Environment.ANALYZER_CLASS, SnowballHackedAnalyzer.class.getName());

        hibConfiguration.getProperties().put("hibernate.search.default.​exclusive_index_use","false");
        hibConfiguration.getProperties().put("hibernate.search.default.indexBase", indexPath);

      } catch (Throwable t) {
        Tracer.logExceptionOnPlatformOrOther(t);
      }
    //}
  }


  public static void refreshIndexSettings(String analyzLanguage, PageState pageState) {
    
    String indexPath = ApplicationState.getApplicationSetting(IndexingConstants.INDEX_PATH);
    if (JSP.ex(indexPath))
      HibernateFactory.getConfig().getProperties().put("hibernate.search.default.indexBase", indexPath);
    if (JSP.ex(analyzLanguage))
      SnowballHackedAnalyzer.language = analyzLanguage;
  }
}
