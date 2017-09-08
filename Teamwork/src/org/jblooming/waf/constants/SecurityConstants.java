/*
 * Created by Roberto Bicchierai and Pietro Polsinelli.
 * User: Pietro Polsinelli
 * Date: Jul 8, 2002
 * Time: 10:18:53 AM
 */
package org.jblooming.waf.constants;


public interface SecurityConstants {

// form fields
// commands
// session scoped

  // i18n
  String I18N_PERMISSION_LACKING = "PERMISSION_LACKING";

// configured urls

// errors
// other constants
  String package_prefix = "com.twproject.teamwork.security";
  String MAX_USERS = package_prefix + ".MAX_USERS";
  String MAX_SIMULT_USERS = package_prefix + ".MAX_SIMULT_USERS";
  String MAC = package_prefix + ".MAC";
  String EXPIRY = package_prefix + ".EXPIRY";
  String SECURITY_MODULE = package_prefix + ".SECURITY_MODULE";
  String REQUIRED_PERMISSION = "RP";
}
