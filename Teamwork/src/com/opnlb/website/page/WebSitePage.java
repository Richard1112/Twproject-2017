package com.opnlb.website.page;


import com.opnlb.website.content.Content;
import com.opnlb.website.portlet.Portlet;
import com.twproject.operator.TeamworkOperator;
import com.twproject.security.SecurityBricks;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.Permission;
import org.jblooming.tracer.Tracer;

import java.util.*;

/**
 * WebSitePage (c) 2005 - Open Lab - www.open-lab.com
 */
public class WebSitePage extends WebSitePagePersistent {

  /**
   * CONSTRUCTORS
   */
  public WebSitePage() {
  }

  public boolean hasPermissionToSee(Operator logged) {
    boolean hasPerm = false;
    if (this.getArea() == null) {
      if (getPermissions().size() == 0) {
        hasPerm = true;
      } else {
        for (Object p : getPermissions()) {
          hasPerm = logged.hasPermissionFor((Permission) p);
          if (!hasPerm)
            break;
        }
      }
    } else {

      if (getPermissions().size() == 0) {

        try {
          hasPerm = ((TeamworkOperator) logged).getAreasForPermission(null).contains(this.getArea());
        } catch (Throwable e) {
          Tracer.platformLogger.error("", e);
        }

      } else {

        for (Object p : getPermissions()) {
          hasPerm = this.hasPermissionFor(logged, (Permission) p);
          if (!hasPerm)
            break;
        }
      }
    }

    return hasPerm;
  }


  public static WebSitePage load(String id) throws FindByPrimaryKeyException {
    return (WebSitePage) PersistenceHome.findByPrimaryKey(WebSitePage.class, id);
  }
}
