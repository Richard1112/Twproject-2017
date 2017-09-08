package org.jblooming.security;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class PlatformPermissions extends Permissions {

  public static final String BASE = "PL_";

  public static final Permission role_canRead = new Permission(BASE + "role_canRead");
  public static final Permission role_canWrite = new Permission(BASE + "role_canWrite");
  public static final Permission role_canCreate = new Permission(BASE + "role_canCreate");

  public static final Permission area_canManage = new Permission(BASE + "area_canManage");

  public static final Permission i18n_manage = new Permission(BASE + "i18n_manage");

  public final static Permission schedule_manage = new Permission(BASE + "schedule_manage");

}
