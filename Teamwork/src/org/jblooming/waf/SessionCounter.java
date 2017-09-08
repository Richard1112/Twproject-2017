package org.jblooming.waf;

import javax.servlet.http.HttpSessionBindingEvent;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Sep 18, 2007
 * Time: 12:33:35 PM
 */
public class SessionCounter implements javax.servlet.http.HttpSessionBindingListener {

  public void valueBound(HttpSessionBindingEvent event) {
    SessionState.totalSessionStates++;
  }

  public void valueUnbound(HttpSessionBindingEvent event) {
    SessionState.totalSessionStates--;
  }


}
