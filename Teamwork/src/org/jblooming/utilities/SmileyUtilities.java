package org.jblooming.utilities;

import org.jblooming.waf.html.display.Img;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageState;

import javax.servlet.jsp.PageContext;
import java.util.HashSet;
import java.util.Set;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class SmileyUtilities {

  public static Set<Smiley> smileys = new HashSet<Smiley>();

  private static void feedSmileys() {
    smileys.add(new Smiley(":)", "smile"));
    smileys.add(new Smiley(":-)", "smile"));
    smileys.add(new Smiley(":-]", "polite_smile"));
    smileys.add(new Smiley(":-(", "frown"));
    smileys.add(new Smiley(":(", "frown"));
    smileys.add(new Smiley(":-/", "skepticism"));
    smileys.add(new Smiley(":-\\", "skepticism"));
    smileys.add(new Smiley(":-|", "sarcasm"));
    smileys.add(new Smiley(";-)", "wink"));
    smileys.add(new Smiley(":-D", "grin"));
    smileys.add(new Smiley(":-P", "tongue"));
    smileys.add(new Smiley(":-p", "tongue"));
    smileys.add(new Smiley(":-o", "surprise"));
    smileys.add(new Smiley(":-O", "surprise"));
    smileys.add(new Smiley(":-0", "surprise"));
    smileys.add(new Smiley(":'-(", "tear"));
    smileys.add(new Smiley("(@)", "angry"));
  }


  public static String getTextWithSmileys(String text, PageContext pageContext) {

    synchronized (smileys) {
      if (smileys.size() == 0)
        feedSmileys();
    }
    PageState pageState = PageState.getCurrentPageState(pageContext);


    for (Smiley smiley : smileys) {

      Img img = new Img(ApplicationState.contextPath+"/img/smiley/" + smiley.img + ".png", pageState.getI18n(smiley.code));
      img.align = "absmiddle";
      text = StringUtilities.replaceAllNoRegex(text, smiley.code, img.toHtmlStringBuffer().toString());
    }

    return ""+text+"";
  }

  public static class Smiley {

    String code;
    String img;

    public Smiley(String code, String img) {
      this.code = code;
      this.img = img;

    }

  }

}
