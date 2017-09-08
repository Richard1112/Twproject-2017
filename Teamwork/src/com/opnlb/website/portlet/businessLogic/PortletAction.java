package com.opnlb.website.portlet.businessLogic;

import com.opnlb.website.content.Content;
import com.opnlb.website.portlet.Portlet;
import com.opnlb.website.security.WebSitePermissions;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.operator.Operator;
import org.jblooming.oql.QueryHelper;
import org.jblooming.oql.OqlQuery;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.Permission;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.input.Collector;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.ClientEntries;
import org.jblooming.waf.view.RestState;

import javax.servlet.http.HttpServletRequest;
import java.io.*;
import java.util.*;
import java.text.ParseException;
import java.nio.charset.Charset;

/**
 * PortletAction (c) 2005 - Open Lab - www.open-lab.com
 */
public class PortletAction {

  public void cmdAdd(PageState pageState) {
    pageState.initializeEntries("row");
    Portlet wp = new Portlet();
    //wp.setIdAsNewSer();
    wp.setIdAsNew();
    pageState.setMainObject(wp);
    makePermissions(wp, pageState);
  }

  public void cmdEdit(PageState pageState, HttpServletRequest request) throws PersistenceException {
    pageState.initializeEntries("row");
    Portlet wp = (Portlet) PersistenceHome.findByPrimaryKey(Portlet.class, pageState.getMainObjectId());
    pageState.setMainObject(wp);
    make(wp, pageState, request);
  }

  private void make(Portlet wp, PageState pageState, HttpServletRequest request) {
    pageState.addClientEntry("NAME", wp.getName());
    pageState.addClientEntry("DESCRIPTION", wp.getDescription());
    pageState.addClientEntry("PX_WIDTH", wp.getPixelWidth());
    pageState.addClientEntry("PX_HEIGHT", wp.getPixelHeight());
    pageState.addClientEntry("INSTALLED", wp.getInstalled() ? Fields.TRUE : Fields.FALSE);

    if (wp.getFile() != null) {
      File tmp = new File(HttpUtilities.getFileSystemRootPathForRequest(request) + File.separator + wp.getFile().getFileLocation());
      if (tmp.exists())
        pageState.addClientEntry("PORTLET_FILE", wp.getFile().getFileLocation());
    }

    makeParams(wp, pageState);

    //making permissions collector
    makePermissions(wp, pageState);
    // making textarea
    makePortletTextToClientEntry(wp, pageState);
  }

  public static void makeParams(Portlet wp, RestState pageState) {
    SerializedMap<String, String> params = wp.getParams();
    if (params != null) {
      for (String key : params.keySet()) {
        String value = params.get(key);
        String ceKey = Portlet.FLD_PT_PARAM_KEY_ + wp.getId() + (key.startsWith("_") ? key : "_" + key);
        pageState.addClientEntry(ceKey, value);
      }
    }
  }

  public void makePermissions(Portlet wp, RestState pageState) {
    Set<Permission> chosen = wp.getPermissions();
    TreeMap<String, String> ctm = new TreeMap<String, String>();
    if (chosen != null && chosen.size() > 0) {
      for (Permission p : chosen) {
        if (p != null)
          ctm.put(p.getName(), I18n.get(p.getName()));
      }
    }
    TreeMap<String, String> candTm = new TreeMap<String, String>();
    /*Collection<String> c and = ApplicationState.getPermissions().keySet();
    if (cand != null && cand.size() > 0) {
      for (Object aCand : cand) {
        String s = (String) aCand;
        if (chosen == null || !ctm.keySet().contains(s))
          candTm.put(s, I18n.get(s));
      }
    }*/

    for (Permission perm : ApplicationState.getPermissions())
      if (chosen == null || !ctm.keySet().contains(perm.getName()))
        candTm.put(perm.getName(), I18n.get(perm.getName()));


    Collector.make("wpPermColl", candTm, ctm, pageState);
    for (Permission p : ApplicationState.getPermissions()) {
      if (wp.containsPermission(p)) {
        pageState.addClientEntry(new ClientEntry(Fields.FORM_PREFIX + p.getName(), "on"));
      }
    }
  }

  public void cmdMove(String s, PageState pageState) throws FindByPrimaryKeyException {
    Collector.move("wpPermColl", pageState);
    Portlet wp;
    if (PersistenceHome.NEW_EMPTY_ID.equals(pageState.getMainObjectId())) {
      wp = new Portlet();
      //wp.setIdAsNewSer();
      wp.setIdAsNew();
    } else {
      wp = (Portlet) PersistenceHome.findByPrimaryKey(Portlet.class, pageState.getMainObjectId());
      if (wp == null)
        throw new FindByPrimaryKeyException();
    }
    pageState.setMainObject(wp);
  }

  public void cmdDelete(PageState pageState) throws org.jblooming.security.SecurityException, PersistenceException {
    pageState.initializeEntries("row");
    Operator user = pageState.getLoggedOperator();
    user.testPermission(WebSitePermissions.portlet_canManage);
    Portlet delendum = (Portlet) PersistenceHome.findByPrimaryKey(Portlet.class, pageState.getMainObjectId());

    //rimuovere le referenze dalle pagine che la usano
    String hql = "delete from " + Content.class.getName() + " where portlet=:p";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("p", delendum);
    int quant = oql.getQuery().executeUpdate();


    DeleteHelper.cmdDelete(delendum, pageState);
  }

  public void cmdFind(PageState pageState) throws PersistenceException {

    Operator logged = pageState.getLoggedOperator();
    boolean somethingSearched = false;
    String filter = null;
    String hql = "select wp from " + Portlet.class.getName() + " as wp ";
    if (pageState.getEntry(Form.FLD_FORM_ORDER_BY + "WPMGR").stringValueNullIfEmpty() == null)
      hql = hql + " order by installed desc, name asc ";

    QueryHelper qhelp = new QueryHelper(hql);
    boolean recoveredFromSavedFilter = PersistentSearch.feedFromSavedSearch(pageState);

    filter = pageState.getEntry("NAME_DESCRIPTION").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addOQLClause(qhelp.getQbeClause("wp.name", "name", filter, QueryHelper.TYPE_CHAR)
        + " OR " +
        qhelp.getQbeClause("wp.description", "description", filter, QueryHelper.TYPE_CHAR));
      somethingSearched = true;
    }

    filter = pageState.getEntry("INSTALLED").stringValueNullIfEmpty();
    if (filter != null && !"ALL".equals(filter)) {
      if (Fields.TRUE.equals(filter))
        qhelp.addOQLClause("wp.installed=:installed", "installed", Boolean.TRUE);
      else
        qhelp.addOQLClause("wp.installed=:installed", "installed", Boolean.FALSE);

      somethingSearched = true;
    }

    if (!somethingSearched && Commands.FIND.equals(pageState.getCommand())) {
      qhelp.addQBEClause("wp.name", "wp", "*", QueryHelper.TYPE_CHAR);
      somethingSearched = true;
    }

    DataTable.orderAction(qhelp, "WPMGR", pageState);
    pageState.setPage(HibernatePage.getHibernatePageInstance(qhelp.toHql().getQuery(), Paginator.getWantedPageNumber(pageState), Paginator.getWantedPageSize(pageState)));
  }

  /*
   * portlets are viewable to users first, this case, according to their "install" status (from BitNet)
   * (and secondly according to permissions policy)
   * once installed the portlet is set in ApplicationState
   *
   * @param pageState
   * @throws FindByPrimaryKeyException
   * @throws StoreException
   */
  public void cmdInstall(PageState pageState) throws PersistenceException {
    Portlet wp = (Portlet) PersistenceHome.findByPrimaryKey(Portlet.class, pageState.getMainObjectId());
    wp.setInstalled(true);
    wp.store();
  }

  public void cmdUnInstall(PageState pageState) throws PersistenceException {
    Portlet wp = (Portlet) PersistenceHome.findByPrimaryKey(Portlet.class, pageState.getMainObjectId());
    wp.setInstalled(false);
    wp.store();
  }

  public void cmdSave(RestState pageState) throws PersistenceException, ActionException, ApplicationException {

    Portlet wp = null;
    boolean invalidClientEntries = false;
    boolean isNew = false;

    if (PersistenceHome.NEW_EMPTY_ID.equals(pageState.getMainObjectId())) {
      wp = new Portlet();
      wp.setIdAsNew();
      isNew = true;
    } else {
      wp = (Portlet) PersistenceHome.findByPrimaryKey(Portlet.class, pageState.getMainObjectId());
    }
    pageState.setMainObject(wp);

    invalidClientEntries = !ActionUtilities.setString(pageState.getEntryAndSetRequired("NAME"), wp, "name");
    ActionUtilities.setString(pageState.getEntry("DESCRIPTION"), wp, "description");

    int width = 0;
    if (pageState.getEntry("PX_WIDTH").isFilled()) {
      try {
        width = pageState.getEntry("PX_WIDTH").intValue();
        if (width >= 0) {
          wp.setPixelWidth(width);
        }
      } catch (ParseException e) {
        invalidClientEntries = true;
      }
    }

    int height = 0;
    if (pageState.getEntry("PX_HEIGHT").isFilled()) {
      try {
        height = pageState.getEntry("PX_HEIGHT").intValue();
        if (height >= 0) {
          wp.setPixelHeight(height);
        }
      } catch (ParseException e) {
        invalidClientEntries = true;
      }
    }

    String installed = pageState.getEntry("INSTALLED").stringValue();
    if (Fields.TRUE.equals(installed)) {
      wp.setInstalled(true);
    } else {
      wp.setInstalled(false);
    }

    String portletPath = pageState.getEntry("PORTLET_FILE").stringValueNullIfEmpty();

    // PersistentFile instance
    PersistentFile pf = new PersistentFile(0, portletPath, PersistentFile.TYPE_WEBAPP_FILESTORAGE);
    pf.setFileLocation(portletPath);
    wp.setFile(pf);

    String choice = pageState.getEntry("CHOOSER").stringValueNullIfEmpty();
    boolean fromCombo = choice == null || Fields.TRUE.equals(choice);

    if (fromCombo) {
      pf = new PersistentFile(0, portletPath, PersistentFile.TYPE_WEBAPP_FILESTORAGE);
      pf.setFileLocation(portletPath);
      wp.setFile(pf);
      makePortletTextToClientEntry(wp, pageState);
      // choice from textarea: original file text (portlet code) is modified
    } else {
      if (!invalidClientEntries) {
        try {
          persistentFileFromText(wp, pageState);
        } catch (IOException e) {
          throw new PlatformRuntimeException("No file written from portlet editor");
        }
        pageState.addClientEntry("CHOOSER", Fields.FALSE);
      }
    }

    //save permissions
    TreeMap<String, String> chosen = Collector.chosen("wpPermColl", pageState);
    String permsIds = StringUtilities.setToString(chosen.keySet(), "|");
    wp.setPermissionIds(permsIds);
    makePermissions(wp, pageState);


    if (!invalidClientEntries) {
      if (PersistenceHome.NEW_EMPTY_ID.equals(pageState.mainObjectId)) {
        Operator owner = pageState.getLoggedOperator();
        //Pietro 24Apr2008: enable settings creation of portlets
        if (owner == null)
          owner = null;
        else
          owner = (Operator) PersistenceHome.findByPrimaryKey(Operator.class, owner.getId());
        wp.setOwner(owner);
      }
      wp.store();
      //   requested by makeParams
      pageState.setMainObjectId(wp.getId());

      // PARAMS
      savePortletParams(wp, pageState);
      makeParams(wp, pageState);

    }
  }

  /**
   * portlet params storing
   *
   * @param wp
   * @param pageState
   */
  private void savePortletParams(Portlet wp, RestState pageState) {
    ClientEntries ces = pageState.getClientEntries();
    Set<String> cesKeys = ces.getEntryKeys();
    String portletId = wp.getId() + "";
    // first remove all previous params
    SerializedMap<String, String> newParams = new SerializedMap<String, String>();
    SerializedMap<String, String> originalParams = wp.getParams();
    for (String key : originalParams.keySet()) {
      String value = originalParams.get(key);
      newParams.put(key, value);
    }
    for (String origKey : newParams.keySet()) {
      wp.getParams().remove(origKey);
    }

    for (String keyParam : cesKeys) {
      String cleanedKey = "";
      if (keyParam.startsWith(Portlet.FLD_PT_PARAM_KEY_ + portletId)) {
        cleanedKey = keyParam.substring((Portlet.FLD_PT_PARAM_KEY_ + portletId + "_").length(), keyParam.length());
      } else if (keyParam.startsWith(Portlet.FLD_PT_PARAM_KEY_ + "null")) {
        cleanedKey = keyParam.substring((Portlet.FLD_PT_PARAM_KEY_ + "null" + "_").length(), keyParam.length());
      }

      String paramValue = pageState.getEntry(keyParam).stringValueNullIfEmpty();
      if (JSP.ex(cleanedKey) && !cleanedKey.endsWith("_txt") && paramValue != null && !Fields.FALSE.equalsIgnoreCase(paramValue)) {
        wp.getParams().put(cleanedKey, paramValue);
      } else {
        wp.getParams().remove(cleanedKey);
      }

    }
  }


//##############################################################################################################################################################

// JSP CREATING FUNCTIONS

  //##############################################################################################################################################################

  /**
   * this method creates jsp portlet file from user's input text
   *
   * @param pageState
   * @param portlet
   * @throws ActionException
   */
  private void persistentFileFromText(Portlet portlet, RestState pageState) throws ActionException, IOException {
    String templateTextWritten = pageState.getEntry("PORTLET_TEXT").stringValue();
    templateTextWritten = StringUtilities.replaceAllNoRegex(templateTextWritten, "´", "'");
    templateTextWritten = StringUtilities.replaceAllNoRegex(templateTextWritten, "'", "'");
    String pfLocation = portlet.getFile().getFileLocation();
    File fromText = new File(ApplicationState.webAppFileSystemRootPath + File.separator + pfLocation);

    FileOutputStream fos = new FileOutputStream(fromText);
    Charset charset = Charset.forName("UTF-8");
    OutputStreamWriter osw = new OutputStreamWriter(fos, charset);

    PrintWriter pw = new PrintWriter(osw);
    pw.println(templateTextWritten);
    pw.flush();
    pw.close();
    fos.close();
  }

  /**
   * portlet file text transformed in ClientEntry
   *
   * @param portlet
   * @param pageState
   */
  private void makePortletTextToClientEntry(Portlet portlet, RestState pageState) {
    PersistentFile pf = portlet.getFile();
    String basePath = ApplicationState.webAppFileSystemRootPath;
    String serverFileName = basePath + pf.getFileLocation();
    String portletText = "";
    String extension = FileUtilities.getFileExt(pf.getOriginalFileName());
    if (FileUtilities.isDocByFileExt(extension) || FileUtilities.isJspByFileExt(extension)) {
      try {
        portletText = FileUtilities.readTextFile(serverFileName);
        if (JSP.ex(portletText)) {
          pageState.addClientEntry("PORTLET_TEXT", portletText);
        }
      } catch (IOException a) {
        pageState.addClientEntry("PORTLET_TEXT", "Error reading portlet file:: " + a);
      }
    }
  }


  public static List<Portlet> getInstalledPortlets() throws PersistenceException {
    String hql = " from " + Portlet.class.getName() + " as wp where wp.installed=:installed";
    OqlQuery qhelp = new OqlQuery(hql);
    qhelp.getQuery().setBoolean("installed", Boolean.TRUE);

    return qhelp.list();
  }

}