package org.jblooming.waf;

import org.jblooming.tracer.Tracer;
import org.jblooming.waf.settings.PersistenceConfiguration;
import org.jblooming.waf.view.PageState;

public class ScreenRoot extends ScreenArea {
  protected ScreenArea body;

  public void register(PageState pageState) {

   //in order to debug main page
   if (PersistenceConfiguration.getDefaultPersistenceConfiguration().useHibStats)
     Tracer.traceHibernateStart();

    //register as root screen
    pageState.rootScreen = this;

    // register controller
    if (getBody() != null)
      getBody().register(pageState);

    //for future implementations, register also the controller of all parts that may have a controller

    pageState.registerPart(this);


  }

  public ScreenArea getBody() {
    return body;
  }

  public void setBody(ScreenArea body) {
    this.body = body;
    if (body != null) {
      body.parent = this;
    }
  }
}
