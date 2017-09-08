package com.opnlb.website.security;

import org.jblooming.security.Permission;
import org.jblooming.security.Permissions;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 8-apr-2005 : 15.57.23
 */
public class WebSitePermissions extends Permissions {

  public static final String WEBSITE_BASE = "WS_";

  public static final Permission page_canManage = new Permission(WEBSITE_BASE + "page_canManage");
  public static final Permission template_canManage = new Permission(WEBSITE_BASE + "template_canManage");
  public static final Permission portlet_canManage = new Permission(WEBSITE_BASE + "portlet_canManage");

  public static final Permission news_canManage = new Permission(WEBSITE_BASE + "news_canManage");
  public static final Permission news_canWrite = new Permission(WEBSITE_BASE + "news_canWrite");

}
