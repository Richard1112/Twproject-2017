package com.opnlb.fulltext.waf;

import com.opnlb.fulltext.IndexingConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.CollectionUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.settings.ApplicationState;
import pt.tumba.ngram.LanguageClass;

import java.util.Collections;
import java.util.List;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Oct 18, 2007
 * Time: 3:20:40 PM
 */
public class IndexingBricks {

  public static String analyzerLanguage() {
    String result = "English";
    String analyzerLanguage = ApplicationState.getApplicationSetting(IndexingConstants.ANALYZER_LANGUAGE);
    if (JSP.ex(analyzerLanguage))
      result = analyzerLanguage;
    return result;
  }

  public static String guess(String text) {

    String result = "";

    if (JSP.ex(text) && text.length() > 100) {
      try {
        LanguageClass lc = (LanguageClass) ApplicationState.applicationParameters.get(IndexingConstants.LANGUAGE_GUESS);
        result = lc.classify(text);
      } catch (Exception e) {
        Tracer.platformLogger.error(e);
      }
    }

    if (!JSP.ex(result))
      result = analyzerLanguage().toLowerCase();

    return result;
  }

   public static String stemmerFromLanguage(String language) {
     return language.substring(0,1).toUpperCase()+language.substring(1);
   }


  public static List<String> getStemmers() {

    List<String> stemmers = CollectionUtilities.toList(
            "English", "Spanish", "French", "German", "Portuguese", "Italian",
            "Swedish", "Danish", "Dutch", "Norwegian", "Russian", "Finnish"
    );
    Collections.sort(stemmers);
    return stemmers;
  }

}
