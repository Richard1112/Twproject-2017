package org.jblooming.waf;

import org.jblooming.waf.view.RestState;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class ActionSupport {

  protected RestState restState;

  public ActionSupport(RestState restState) {
    this.restState = restState;
  }

}
