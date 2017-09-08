package com.opnlb.website.page.businessLogic;

import com.opnlb.website.content.Content;
import com.opnlb.website.page.WebSitePage;
import com.opnlb.website.template.Template;
import com.opnlb.website.util.WebsiteUtilities;
import com.opnlb.website.waf.WebSiteConstants;
import net.wimpi.pim.util.StringUtil;
import org.jblooming.ApplicationException;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.*;
import org.jblooming.security.Permission;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.FieldErrorConstants;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.input.Collector;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.html.table.ListHeader;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import javax.servlet.http.HttpServletRequest;
import java.util.*;

/**
 * WebSitePageAction (c) 2005 - Open Lab - www.open-lab.com
 */
public class WebSitePageAction {

  public void cmdAdd(PageState pageState, Class pageClass) throws PersistenceException {
    pageState.initializeEntries("row");
    WebSitePage mainObject;
    try {
      mainObject = (WebSitePage) pageClass.newInstance();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }

    mainObject.setIdAsNew();
    pageState.setMainObject(mainObject);
    makePermissions(mainObject, pageState);
  }

  public void cmdEdit(PageState pageState, Class pageClass) throws PersistenceException {
    pageState.initializeEntries("row");

    WebSitePage wsPage = (WebSitePage) PersistenceHome.findByPrimaryKey(pageClass, pageState.getMainObjectId());
    pageState.setMainObject(wsPage);
    make(wsPage, pageState);
  }

  public void cmdSave(RestState pageState, Class pageClass) throws PersistenceException, ActionException, ApplicationException {

    WebSitePage wsPage = null;
    boolean invalidClientEntries = false;

    if (PersistenceHome.NEW_EMPTY_ID.equals(pageState.getMainObjectId())) {
      try {
        wsPage = (WebSitePage) pageClass.newInstance();
        wsPage.setIdAsNew();
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
    } else {
      wsPage = (WebSitePage) PersistenceHome.findByPrimaryKey(pageClass, pageState.getMainObjectId());
    }
    pageState.setMainObject(wsPage);

    String foTitle = pageState.getEntryAndSetRequired("FOTITLE").stringValueNullIfEmpty();
    if (foTitle != null) {
      wsPage.setFrontOfficeTitle(foTitle.trim());
    } else {
      invalidClientEntries = true;
      ClientEntry ceNull = new ClientEntry("FOTITLE", foTitle);
      ceNull.errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
      pageState.addClientEntry(ceNull);
    }

    // il campo name, se vuoto, deriva dal frontofficetitle rielaborato
    String name = pageState.getEntry("NAME").stringValueNullIfEmpty();
    if (JSP.ex(name)) {
      name = WebsiteUtilities.normalizePageName(name);
    } else if (foTitle!=null) {
      name = WebsiteUtilities.normalizePageName(foTitle);
    }
    // if name==null it means that it's not a website interface but a ww4 one, thus it will be setted by WebworkPageAction from foTitle_lang
    try {
      name = StringUtilities.replaceAllNoRegex(name, "+", "_");
    } catch(NullPointerException a) {}


    boolean nameUsed = checkPageName(pageState.getMainObjectId().toString(), name);
    // name can be setted only if obj is new
    if (!nameUsed) {
      wsPage.setName(name);
      pageState.addClientEntry("NAME", name);
    } else {
      invalidClientEntries = true;
      ClientEntry ceNull = new ClientEntry("NAME", name);
      ceNull.errorCode = FieldErrorConstants.ERR_NAME_USED;
      pageState.addClientEntry(ceNull);
    }

    String templateId = pageState.getEntry("DEF_TEMPLATE").stringValue();
    if (JSP.ex(templateId)) {
      Template templ = (Template) PersistenceHome.findByPrimaryKey(Template.class, templateId);
      wsPage.setDefaultTemplate(templ);
    } else {
      invalidClientEntries = true;
      ClientEntry ceNull = new ClientEntry("DEF_TEMPLATE", null);
      ceNull.errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
      pageState.addClientEntry(ceNull);
    }

    ActionUtilities.setIdentifiable(pageState.getEntry("OWNER"), wsPage, "owner");
    ActionUtilities.setIdentifiable(pageState.getEntry("AREA"), wsPage, "area");
    ActionUtilities.setString(pageState.getEntry("DESCRIPTION"), wsPage, "description");
    ActionUtilities.setString(pageState.getEntry("URL"), wsPage, "relativeUrl");
    if (pageState.getEntry("METATAG_KEYWORDS").stringValueNullIfEmpty()!=null)
      ActionUtilities.setString(pageState.getEntry("METATAG_KEYWORDS"), wsPage, "metaKeywords");

    wsPage.setCustomizable(pageState.getEntry("CUSTOM").checkFieldValue());
    wsPage.setActive(pageState.getEntry("ACTIVE").checkFieldValue());

    //save security
    TreeMap<String, String> chosen = Collector.chosen("pagePermColl", pageState);
    String permsIds = StringUtilities.setToString(chosen.keySet(), "|");
    wsPage.setPermissionIds(permsIds);


    //remove contents of unused areas
    String areass = pageState.getEntry("TEMPLATE_AREAS").stringValueNullIfEmpty();
    if (JSP.ex(areass) && !wsPage.isNew()){
      Set<String> areas = StringUtilities.splitToSet(areass, ",");
      String hql="delete from "+Content.class.getName()+" as c where c.page.id=:pid and c.area not in (:areas)";
      OqlQuery query=new OqlQuery(hql);
      query.setParameter("pid",wsPage.getId());
      query.getQuery().setParameterList("areas",areas);
      query.getQuery().executeUpdate();
    }


    if (!invalidClientEntries) {
      wsPage.store();
      pageState.setMainObject(wsPage);
    }
  }

  public static boolean checkPageName(String id, String name) throws PersistenceException {
    return checkPageName(id, name, false);
  }

  public static boolean checkPageName(String id, String name, boolean cloning) throws PersistenceException {
    boolean namePresent = false;
    String hql = " select page.id from " + WebSitePage.class.getName() + " as page where page.name=:name ";
    if(!cloning)
      hql += " and page.id!=:pid ";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("name", name);
    if(!cloning)
      oql.getQuery().setString("pid", id);
    List results = oql.list();
    if (results != null && results.size() > 0)
      namePresent = true;
    return namePresent;
  }

  public void cmdDelete(PageState pageState) throws PersistenceException {
    WebSitePage delenda = (WebSitePage) PersistenceHome.findByPrimaryKey(WebSitePage.class, pageState.getMainObjectId());
    pageState.setMainObject(delenda);
    DeleteHelper.cmdDelete(delenda, pageState);
  }

  public void make(WebSitePage wsPage, PageState pageState) {
    pageState.addClientEntry("NAME", wsPage.getName());
    pageState.addClientEntry("FOTITLE", wsPage.getFrontOfficeTitle());
    pageState.addClientEntry("DESCRIPTION", wsPage.getDescription());
    pageState.addClientEntry("METATAG_KEYWORDS", wsPage.getMetaKeywords());
    pageState.addClientEntry("AREA", wsPage.getArea() != null ? wsPage.getArea().getId() + "" : null);
    pageState.addClientEntry("URL", wsPage.getRelativeUrl());
    pageState.addClientEntry("OWNER", wsPage.getOwner()!=null?wsPage.getOwner().getId():"" );
    pageState.addClientEntry("DEF_TEMPLATE",wsPage.getDefaultTemplate()!=null?wsPage.getDefaultTemplate().getId():"" );
    pageState.addClientEntry("CUSTOM", wsPage.isCustomizable() ? Fields.TRUE : Fields.FALSE);
    pageState.addClientEntry("ACTIVE", wsPage.isActive() ? Fields.TRUE : Fields.FALSE);

    //making permissions collector
    makePermissions(wsPage, pageState);
  }

  public void cmdFind(PageState pageState) throws  PersistenceException {
    Operator logged = pageState.getLoggedOperator();
    //defaults
    boolean somethingSearched = false;
    String filter = null;
    String hql = "select wspage from " + WebSitePage.class.getName() + " as wspage where wspage.name!=:root";
    if (pageState.getEntry(Form.FLD_FORM_ORDER_BY + "WSPGMGR").stringValueNullIfEmpty() == null) {
      //sqlSelect = sqlSelect + " order by ancestorIds ";
      hql = hql + " order by wspage.name";
    }

    QueryHelper qhelp = new QueryHelper(hql);
    boolean recoveredFromSavedFilter = PersistentSearch.feedFromSavedSearch(pageState);

    qhelp.setParameter("root", "ROOT");
    //qhelp.addQBEClause("nc.description", "description", filter, QueryHelper.TYPE_CHAR);

    filter = pageState.getEntry("NAME_DESCRIPTION").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQueryClause(qhelp.getQbeClause("wspage.name", "name", filter, QueryHelper.TYPE_CHAR)
          + " OR " +
          qhelp.getQbeClause("wspage.description", "description", filter, QueryHelper.TYPE_CHAR));
    }

    filter = pageState.getEntry("CUSTOM").stringValueNullIfEmpty();
    if (filter != null) {
      if (Fields.TRUE.equals(filter))
        qhelp.addOQLClause("wspage.customizable=:customizable", "customizable", Boolean.TRUE);
      else
        qhelp.addOQLClause("wspage.custom=:customizable", "customizable", Boolean.FALSE);
    }

    filter = pageState.getEntry("ACTIVE").stringValueNullIfEmpty();
    if (filter != null && !"ALL".equals(filter)) {
      if (Fields.TRUE.equals(filter))
        qhelp.addOQLClause("wspage.active=:active", "active", Boolean.TRUE);
      else
        qhelp.addOQLClause("wspage.active=:active", "active", Boolean.FALSE);
    }

    if (!somethingSearched && Commands.FIND.equals(pageState.getCommand())) {
      qhelp.addQBEClause("wspage.name", "wspage", "*", QueryHelper.TYPE_CHAR);
    }

    DataTable.orderAction(qhelp, "WSPGMGR", pageState);
    pageState.setPage(HibernatePage.getHibernatePageInstance(qhelp.toHql().getQuery(), Paginator.getWantedPageNumber(pageState), Paginator.getWantedPageSize(pageState)));
  }

  /*############################################################################################################################################################

     GESTIONE PERMESSI

  ############################################################################################################################################################*/
  private void makePermissions(WebSitePage page, PageState pageState) {
    Set<Permission> chosen = page.getPermissions();
    TreeMap<String, String> ctm = new TreeMap<String, String>();
    if (chosen != null && chosen.size() > 0) {
      for (Permission p : chosen) {
        if (p != null)
          ctm.put(p.getName(), I18n.get(p.getName()));
      }
    }
    TreeMap<String, String> candTm = new TreeMap<String, String>();
    /*Collection<String> ca nd = ApplicationState.getPermissions().keySet();
    if (cand != null && cand.size() > 0) {
      for (Object aCand : cand) {
        String s = (String) aCand;
        if (chosen == null || !ctm.keySet().contains(s))
          candTm.put(s, I18n.get(s));
      }
    }*/

    for (Permission perm:ApplicationState.getPermissions())
      if (chosen == null || !ctm.keySet().contains(perm.getName()))
        candTm.put(perm.getName(), I18n.get(perm.getName()));


    Collector.make("pagePermColl", candTm, ctm, pageState);
    for (Permission p : ApplicationState.getPermissions()) {
      if (page.hasPermissionFor(p)) {
        pageState.addClientEntry(new ClientEntry(Fields.FORM_PREFIX + p.getName(), "on"));
      }
    }
  }

  public void cmdMove(PageState pageState) throws FindByPrimaryKeyException {
    Collector.move("pagePermColl", pageState);
    WebSitePage page;
    if (PersistenceHome.NEW_EMPTY_ID.equals(pageState.getMainObjectId())) {
      page = new WebSitePage();
      //page.setIdAsNewSer();
      page.setIdAsNew();
    } else {
      page = (WebSitePage) PersistenceHome.findByPrimaryKey(WebSitePage.class, pageState.getMainObjectId());
      if (page == null)
        throw new FindByPrimaryKeyException();
    }
    pageState.setMainObject(page);
  }

  public void cmdResetDefault (PageState pageState) throws PersistenceException {
    WebSitePage wsPage = (WebSitePage) PersistenceHome.findByPrimaryKey(WebSitePage.class, pageState.getMainObjectId());
    pageState.setMainObject(wsPage);
    boolean doubleChoiceActive = Fields.TRUE.equals(ApplicationState.getApplicationSetting(WebSiteConstants.PERSONAL_PAGE_ACTIVE));
    if(doubleChoiceActive && JSP.ex(wsPage)) {
      Operator logged = pageState.getLoggedOperator();
      deletePreviousData(wsPage.getId().toString(), logged, false, true);
    }
  }

  protected void deletePreviousData(String pageId, Operator logged, boolean defConfiguration, boolean considerOperator) throws PersistenceException {
    String sql = " from " + Content.class.getName() + " as cont where cont.page.id =:pageId and cont.defaultConfiguration=:defConfig";
    // in caso di admin multipli vanno eliminati tutti i content indipendentemente da utente loggato, ma solo se considerOperator
    if (considerOperator)
      sql = sql + " and cont.operator.id = :operatorId";

    OqlQuery oql = new OqlQuery(sql);
    oql.getQuery().setString("pageId", pageId);
    oql.getQuery().setBoolean("defConfig", defConfiguration);
    if (considerOperator)
      oql.getQuery().setString("operatorId", logged.getId().toString());

    List<Content> delenda = oql.list();
    if (JSP.ex(delenda)) {
      for (Content content: delenda)
        content.remove();
      }
    }

}
