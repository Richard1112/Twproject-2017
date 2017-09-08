package org.jblooming.security;

import org.jblooming.ontology.Identifiable;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.constants.SecurityConstants;


public class SecurityException extends Exception {

  public Permission p;

  public SecurityException() {
    super();
  }

  public SecurityException(String s) {
    super(s);
  }

  public SecurityException(String s, Permission p, Identifiable identifiable) {
    this(s+(identifiable==null?"":": (id "+identifiable.getId()+") - "+identifiable.getName()),p);
  }

  public SecurityException(String s, Permission p) {
    super(s+" "+p.name);
    this.p = p;
  }


  public static void riseExceptionIfNoPermission(boolean havePermission, Permission p, Identifiable identifiable) throws SecurityException {
    if (!havePermission)
      throw new SecurityException(SecurityConstants.I18N_PERMISSION_LACKING,p,identifiable);
  }

}
