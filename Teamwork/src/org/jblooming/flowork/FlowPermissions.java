package org.jblooming.flowork;

import org.jblooming.security.Permission;
import org.jblooming.security.Permissions;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 8-apr-2005 : 16.05.34
 */
public class FlowPermissions extends Permissions {

  public static final String FLOW_BASE = "FLOW_";

  public final static Permission canManageFlows = new Permission(FLOW_BASE +  "canManageFlows");
  public final static Permission canForceFlowFormData = new Permission(FLOW_BASE +  "canForceFlowFormData");

}
