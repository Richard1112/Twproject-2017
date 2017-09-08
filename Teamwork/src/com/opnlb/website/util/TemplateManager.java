package com.opnlb.website.util;

import com.opnlb.website.content.Content;
import com.opnlb.website.page.WebSitePage;
import com.opnlb.website.portlet.Portlet;
import com.opnlb.website.template.Template;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.Permission;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.core.JspIncluder;
import org.jblooming.waf.html.display.MultimediaFile;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.JspWriter;
import javax.servlet.jsp.PageContext;
import java.io.IOException;
import java.util.*;

/**
 * TemplateManager (c) 2005 - Open Lab - www.open-lab.com
 */
public class TemplateManager {

  /*
   * Called in template jsp file to load user contents with this single query
   *
   * @param logged
   * @param pageId
   * @return
   * @throws PersistenceException
   */
  public static List choosenContents(Operator logged, String pageId) throws PersistenceException {

    List choices = new ArrayList();
    if (logged != null) {
      String hql = " from " + Content.class.getName() + " as content where " +
              "content.operator=:oid and " +
              (pageId != null ? "content.page.id=:pid and " : "") +
              "content.defaultConfiguration=:falsity order by orderx ";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setEntity("oid", logged);
      oql.getQuery().setBoolean("falsity", Boolean.FALSE);
      if (pageId != null)
        oql.getQuery().setString("pid", pageId);
      choices = oql.list();
    }

    if (choices == null || choices.size() == 0) {
      String hql = " from " + Content.class.getName() + " as content where " +
              (pageId != null ? "content.page.id=:pid and " : "") +
              "content.defaultConfiguration=:truth order by orderx ";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setBoolean("truth", Boolean.TRUE);
      if (pageId != null)
        oql.getQuery().setString("pid", pageId);
      choices = oql.list();
    }
    return choices;
  }

  /*
   * called by displayAdmin to load into an uncostumizable area admin contents
   *
   * @param pageId
   * @param area
   * @return
   * @throws PersistenceException
   */
  public static List<Content> choosenContentsAdmin(String pageId, String area) throws PersistenceException {
//    Template defTemplate = null;
//    WebSitePage thisPage = (WebSitePage) PersistenceHome.findByPrimaryKey(WebSitePage.class, pageId);
//    defTemplate = thisPage.getDefaultTemplate();
//    boolean considerTemplate = defTemplate!=null;
//    List choices = new ArrayList();
    String hql = " from " + Content.class.getName() + " as content where " +
            "content.area=:aid and " +
            (pageId != null ? "content.page.id=:pid and " : "") +
            //(considerTemplate ? " content.template.id=:tid and " : "") +
            "content.defaultConfiguration=:truth order by orderx ";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("aid", area);
    oql.getQuery().setBoolean("truth", Boolean.TRUE);
    if (pageId != null)
      oql.getQuery().setString("pid", pageId);
//      if (considerTemplate)
//        oql.getQuery().setString("tid", defTemplate.getId().toString());

    // november06: added for tw3 wizard creates contents in area admin with operator null thus ==>
    //return cleanChoices(oql.list());
    return oql.list();
  }

  public static void displayAdmin(String pageId, String area, PageContext pageContext, PageState pageState) throws PersistenceException {
    List<Content> portlets = choosenContentsAdmin(pageId, area);
    portletLoop(portlets, area, pageContext, pageState);
  }

  /*
   * it's method inserted into an active area tag in the jsp template file
   * custom area
   *
   * @param choices
   * @param area
   * @param pageContext
   * @param out
   */
  @Deprecated
  public static void display(List choices, String area, PageContext pageContext, JspWriter out, PageState pageState) throws IOException{
    display(choices, area, pageContext, pageState);
  }
  public static void display(List choices, String area, PageContext pageContext, PageState pageState) {
    portletLoop(choices, area, pageContext, pageState);
  }

  private static void portletLoop(List choices, String area, PageContext pageContext, PageState pageState) {
    Operator logged = null;
    if (pageState != null) {
      logged = pageState.getLoggedOperator();
    }

    HttpServletRequest req = (HttpServletRequest) pageContext.getRequest();

    String defFilterName = null;
    for (Object choice : choices) {
      Content content = (Content) choice;
      String contentArea = content.getArea();
      if (contentArea.equalsIgnoreCase(area)) {
        Portlet portlet = content.getPortlet();


        /**
         * PERMISSIONS SECURITY CHECK
         */
        boolean show = verifyPermissions(pageState, portlet, logged);
        if (show) {
          PersistentFile pf = portlet.getFile();
          boolean installed = portlet.getInstalled();
          if (pf != null && installed) {
            pageState.setAttribute("WW_PORTLET", portlet);
            pageState.setAttribute("WW_PORTLET_PARAMS", portlet.getParams());
            MultimediaFile mf = new MultimediaFile(pf, req);
            mf.areaName = area;
            mf.width = portlet.getPixelWidth() > 0 ? portlet.getPixelWidth() + "" : "200";
            mf.height = portlet.getPixelHeight() > 0 ? portlet.getPixelHeight() + "" : "200";
            mf.toHtml(pageContext);
          }
        }


      }
    }
  }

  /*--------------------------------------------------------------------------------------------------------------------------------------------------------------

    DYNAMIC PREVIEW

  --------------------------------------------------------------------------------------------------------------------------------------------------------------*/
  public static void displayPreview(String area, PageContext pageContext, PageState pageState) throws FindByPrimaryKeyException {
    Map cesMap = pageState.getClientEntries().getEntriesStartingWithStripped("input_" + area);
    for (Iterator iterator = cesMap.keySet().iterator(); iterator.hasNext();) {
      Object o = iterator.next();
      ClientEntry ce = (ClientEntry) cesMap.get(o);
      String value = ce.stringValueNullIfEmpty();
      if (("input_" + area).equals(ce.name)) {
        if (value != null && value.trim().length() > 0) {
          String[] portletList = StringUtilities.splitToArray(value, ",");
          String portletId = null;
          for (int i = 0; i < portletList.length; i++) {
            portletId = portletList[i];
            boolean goOn = false;
            if (portletId.startsWith("wp_")) {
              portletId = portletId.substring(3, portletId.length());
              goOn = true;
            } else if (portletId.trim().length() > 0) {
              try {
                int id = new Integer(portletId);
                portletId = id + "";
                goOn = true;
              } catch (NumberFormatException d) {
                Tracer.platformLogger.error("templateManager displayPreview ", d);

              }
            }

            if (goOn) {
              Portlet portlet = (Portlet) PersistenceHome.findByPrimaryKey(Portlet.class, portletId);
              PersistentFile pf = portlet.getFile();
              boolean installed = portlet.getInstalled();
              if (pf != null && installed) {
                HttpServletRequest req = (HttpServletRequest) pageContext.getRequest();
                MultimediaFile mf = new MultimediaFile(pf, req);
                mf.areaName = area;
                mf.width = portlet.getPixelWidth() > 0 ? portlet.getPixelWidth() + "" : "200";
                mf.height = portlet.getPixelHeight() > 0 ? portlet.getPixelHeight() + "" : "200";
                mf.toHtml(pageContext);
              }
            }
          }
        }
      }
    }
  }

  private static boolean verifyPermissions(PageState pageState, Portlet portlet, Operator logged) {
    boolean show = true;
    // if called by displayAdmin pagestate is null:: no permissions verify because admin chose such a portlet to be shown
    if (pageState != null) {
      Set<Permission> portletPermissions = portlet.getPermissions();
      if (portletPermissions != null && portletPermissions.size() > 0) {
        show = false;
        for (Permission permission : portlet.getPermissions()) {
          if (logged != null && logged.hasPermissionFor(permission)) {
            show = true;
            break;
          }
        }
      }
    }
    return show;
  }

  public static String getIdFromPageName(String pageName) throws FindException {
    String hql = " select page.id from " + WebSitePage.class.getName() + " as page where upper(page.name)=:name ";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("name", pageName.toUpperCase());
    String id = null;
    try {
      id = (String) oql.uniqueResult();
    } catch (FindException a) {
    }
    // redirect to home
    if (id == null) {
      id = TemplateManager.getRoot();
    }
    return id;
  }

  public static String getRoot() throws FindException {
    String hql = " select page.id from " + WebSitePage.class.getName() + " as page";
    OqlQuery oql = new OqlQuery(hql);
    List<String> ids = oql.list();
    String rootId = null;
    if (JSP.ex(ids))
      rootId = ids.get(0);
    return rootId;
  }



  public static JSONObject getUserContents(WebSitePage wsp,Operator logged) throws PersistenceException {
    List<Content> cChoices = new ArrayList();
    if (logged != null) {
      String hql = " from " + Content.class.getName() + " as content where content.operator=:oid and content.page=:page and content.defaultConfiguration=false order by orderx ";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setEntity("oid", logged);
      oql.getQuery().setEntity("page", wsp);
      cChoices = oql.list();
    }
    return jsonifyContents(cChoices);
  }

  public static JSONObject getDeafultContents(WebSitePage wsp) throws PersistenceException {
    String hql = " from " + Content.class.getName() + " as content where content.page=:page and content.defaultConfiguration=true order by orderx ";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("page", wsp);
    return jsonifyContents(oql.list());
  }



  public static JSONObject jsonifyContents(List<Content>contents){
    JSONObject jContents= new JSONObject();
    for (Content c:contents) {

      //si crea un array per ogni area
      JSONArray jsa;
      if (!jContents.has(c.getArea()))
        jsa = new JSONArray();
      else
        jsa = jContents.getJSONArray(c.getArea());

      //si crea un oggetto per ogni portlet con i suoi parametri
      jsa.add(c.getPortlet().jsonify());

      jContents.element(c.getArea(), jsa);
    }

    return jContents;
  }


}
