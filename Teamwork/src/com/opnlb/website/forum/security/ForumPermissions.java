package com.opnlb.website.forum.security;

import org.jblooming.security.Permissions;
import org.jblooming.security.Permission;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 8-apr-2005 : 15.46.02
 */
public class ForumPermissions extends Permissions {

  protected static final String FORUM_BASE = "FORUM_";

  public static final Permission forum_canRead = new Permission(FORUM_BASE + "for_r");
  public static final Permission forum_canPost = new Permission(FORUM_BASE + "for_p");
  public static final Permission forum_canEdit = new Permission(FORUM_BASE + "for_w");

}
