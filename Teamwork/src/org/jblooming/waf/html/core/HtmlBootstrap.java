package org.jblooming.waf.html.core;

import org.jblooming.waf.view.PageState;

import javax.servlet.ServletException;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.HashSet;

/**
 * @author Pietro Polsinelli : ppolsinelli@open-lab.com
 */
public interface HtmlBootstrap {
  /**
   * permits to identify the kind of bootstapper involved
   * e.g. avoid to saveACopy includes of js;
   * the standard value is Class.getName()
   */
  String getDiscriminator();

  boolean validate(PageState pageState) throws IOException, ServletException;

  public class HtmlBootstrappers {
    private Hashtable<String,HtmlBootstrap> bsprs = new Hashtable<String,HtmlBootstrap>();

    public void add(HtmlBootstrap hb) {
      bsprs.put(hb.getDiscriminator(), hb);
    }

    public Iterator<HtmlBootstrap> iterator() {
      return new HashSet<HtmlBootstrap>(bsprs.values()).iterator();
    }

    public HtmlBootstrap get(String discriminator) {
      return bsprs.get(discriminator);
    }

  }

}
