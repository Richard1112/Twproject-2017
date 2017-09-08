package org.jblooming.flowork.security;

import org.jblooming.operator.Operator;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.flowork.*;
import java.util.Set;
import java.util.HashSet;

public class FloworkOperator extends Operator implements FluxusOperator {
  /**
   * Tags all the swimlanes interpreted in the workflow engine
   */
  private String swimmingIn;

  public Set<String> getSwimlaneNames() {
    return getSwimminLanes();
  }

  protected String getSwimmingIn() {
    return swimmingIn;
  }

  public void setSwimmingIn(String swimmingIn) {
    this.swimmingIn = swimmingIn;
  }

  public Set<String> getSwimminLanes() {
    if (swimmingIn != null && swimmingIn.length() > 2)
      return StringUtilities.splitToSet(swimmingIn.substring(1, swimmingIn.length() - 1), "*");
    else
      return new HashSet();
  }
}
