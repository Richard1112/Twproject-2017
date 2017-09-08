package com.opnlb.website.template.businessLogic;


import com.opnlb.website.htmlParser.transformer.HTMLTemplateTransformer;
import com.opnlb.website.htmlParser.transformer.JSPTemplateTransformer;
import com.opnlb.website.security.WebSitePermissions;
import com.opnlb.website.template.Template;
import com.opnlb.website.waf.WebSiteConstants;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.operator.Operator;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.html.table.ListHeader;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import javax.servlet.http.HttpServletRequest;
import java.io.*;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * TemplateAction (c) 2005 - Open Lab - www.open-lab.com
 */
public class TemplateAction {

  public void cmdAdd(PageState pageState) {
    pageState.initializeEntries("row");
    Template mainObject = new Template();
    mainObject.setIdAsNew();
    pageState.setMainObject(mainObject);
  }

  public void cmdEdit(PageState pageState, HttpServletRequest request) throws PersistenceException {
    pageState.initializeEntries("row");
    Template template = (Template) PersistenceHome.findByPrimaryKey(Template.class, pageState.getMainObjectId());
    pageState.setMainObject(template);
    make(template, pageState, request);
  }

  private void make(Template template, PageState pageState, HttpServletRequest request) {
    pageState.addClientEntry("NAME", template.getName());
    pageState.addClientEntry("DESCRIPTION", template.getDescription());
    if (template.getTemplateFile() != null) {
      File tmp = new File(HttpUtilities.getFileSystemRootPathForRequest(request)+File.separator+template.getTemplateFile().getFileLocation());
      if (tmp.exists())
        pageState.addClientEntry("TEMPLATE_FILE", tmp.getName());
    }
  }

  public void cmdDelete(PageState pageState, HttpServletRequest request) throws org.jblooming.security.SecurityException, PersistenceException {
    pageState.initializeEntries("row");
    Operator user = pageState.getLoggedOperator();
    user.testPermission(WebSitePermissions.template_canManage);
    Template delenda = (Template) PersistenceHome.findByPrimaryKey(Template.class, pageState.getMainObjectId());
    DeleteHelper.cmdDelete(delenda, pageState);
  }

  public void cmdFind(PageState pageState, org.jblooming.operator.User loggedOp) throws  PersistenceException {
    Operator w = pageState.getLoggedOperator();
    //defaults
    boolean somethingSearched = false;
    String filter = null;
    String hql = "from " + Template.class.getName() + " as template ";
    if (pageState.getEntry(Form.FLD_FORM_ORDER_BY + "TEMPLMGR").stringValueNullIfEmpty() == null) {
      hql = hql + " order by template.name asc ";
    }
    QueryHelper qhelp = new QueryHelper(hql);
    boolean recoveredFromSavedFilter = PersistentSearch.feedFromSavedSearch(pageState);

    filter = pageState.getEntry("NAME_DESCRIPTION").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQueryClause(qhelp.getQbeClause("template.name", "name", filter, QueryHelper.TYPE_CHAR)
          + " OR " +
          qhelp.getQbeClause("template.description", "description", filter, QueryHelper.TYPE_CHAR));
      somethingSearched = true;
    }

    filter = pageState.getEntry("ISDEFAULT").stringValueNullIfEmpty();
    if (filter != null && !"ALL".equals(filter)) {
      if (Fields.TRUE.equals(filter))
        qhelp.addOQLClause(" (template.isDefault = :isDefault)", "isDefault", Boolean.TRUE);
      else
        qhelp.addOQLClause(" (template.isDefault = :isDefault)", "isDefault", Boolean.FALSE);
      somethingSearched = true;
    }

    if (!somethingSearched && Commands.FIND.equals(pageState.getCommand())) {
      qhelp.addQBEClause("template.name", "tname", "*", QueryHelper.TYPE_CHAR);
      somethingSearched = true;
    }
    DataTable.orderAction(qhelp, "TEMPLMGR", pageState);
    pageState.setPage(HibernatePage.getHibernatePageInstance(qhelp.toHql().getQuery(), Paginator.getWantedPageNumber(pageState), Paginator.getWantedPageSize(pageState)));
  }

  public void cmdSave(RestState pageState) throws PersistenceException, ActionException, ApplicationException {

    Template template = null;
    if (PersistenceHome.NEW_EMPTY_ID.equals(pageState.getMainObjectId())) {
      template = new Template();
      template.setIdAsNew();
    } else
      template = (Template) PersistenceHome.findByPrimaryKey(Template.class, pageState.getMainObjectId());

    pageState.setMainObject(template);

    ActionUtilities.setString(pageState.getEntryAndSetRequired("NAME"), template, "name");
    ActionUtilities.setString(pageState.getEntry("DESCRIPTION"), template, "description");

    String file = pageState.getEntry("TEMPLATE_FILE").stringValue();
    // file folder creation
    String templateLocation = ApplicationState.getApplicationSetting(WebSiteConstants.TEMPLATE_LOCATION);
    String folderPath = ApplicationState.webAppFileSystemRootPath + templateLocation;
    File folder = new File(folderPath);
    if (!folder.exists()){
      folder.mkdir();
    }
    PersistentFile pf = new PersistentFile(0, templateLocation + "/" + file, PersistentFile.TYPE_WEBAPP_FILESTORAGE);
    pf.setFileLocation(templateLocation + "/" + file);

    //usato per validare il template
    pageState.getEntryAndSetRequired("TEMPLATE_AREAS").stringValue();

    if (pageState.validEntries() ) {
      template.setTemplateFile(pf);
      if (PersistenceHome.NEW_EMPTY_ID.equals(pageState.mainObjectId)) {
        Operator owner = pageState.getLoggedOperator();
        if (owner!=null)
          template.setOwner((Operator) PersistenceHome.findByPrimaryKey(Operator.class, owner.getId()));
      }
      template.store();
    }

  }



}
