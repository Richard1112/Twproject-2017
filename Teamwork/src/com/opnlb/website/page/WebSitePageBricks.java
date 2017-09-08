package com.opnlb.website.page;

import com.opnlb.website.portlet.Portlet;
import com.opnlb.website.portlet.businessLogic.PortletAction;
import com.opnlb.website.waf.WebSiteConstants;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.security.Area;
import org.jblooming.waf.Bricks;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;

/**
 * WebSitePageBricks (c) 2005 - Open Lab - www.open-lab.com
 */
public class WebSitePageBricks  extends Bricks {

  public WebSitePage mainObject;

  public WebSitePageBricks (WebSitePage wsPage) {
    this.mainObject = wsPage;
  }

  public static void makePortletParams(String portletId, PageState pageState) throws FindByPrimaryKeyException {

      Portlet wp = (Portlet) PersistenceHome.findByPrimaryKey(Portlet.class, portletId);
      PortletAction.makeParams(wp, pageState);

  }

  public static String preparePortletParam(PageState pageState) throws FindByPrimaryKeyException {
    String portletId = pageState.getMainObjectId()+"";

    // preview page only
    makePortletParams(portletId,  pageState);
    return portletId;
  }

  /*------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------*/
}