package com.opnlb.website.portlet;

import com.opnlb.website.content.Content;
import net.sf.json.JSONObject;
import org.jblooming.ontology.PlatformComparators;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.CodeValueList;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.PageQuark;
import org.jblooming.waf.PluginBricks;
import org.jblooming.waf.settings.ApplicationState;

import java.io.File;
import java.util.*;

/**
 * Portlet (c) 2005 - Open Lab - www.open-lab.com
 */
public class Portlet extends PageQuark {// BasicPortlet {

  /**
   * BasicPortlet implementation
   */
//  public List<FieldFeature> getParamList(){
//    List<FieldFeature> params = new ArrayList<FieldFeature>();
//    return params;
//  }

  public static List getUsesInPage( Portlet wp) throws PersistenceException {
    if (!wp.isNew()){
      String ql = " select cont.id from "+ Content.class.getName()+" as cont ";
      QueryHelper qh = new QueryHelper(ql);
      qh.addJoinAlias(" join cont.page as page ");
      qh.addOQLClause(" cont.portlet = :wp", "wp", wp);
      List pages = qh.toHql().list();
      return pages;
    } else
      return new ArrayList();
  }


  public static Portlet load (String id) throws FindByPrimaryKeyException {
    return (Portlet) PersistenceHome.findByPrimaryKey(Portlet.class,id);
  }


  public static List<File> getCandidateFiles(){
    String portletLocation = ApplicationState.getApplicationSetting("PORTLET_LOCATION");
    if (!JSP.ex(portletLocation))
      portletLocation="applications/teamwork/portal/portlet";

    File root = new File(ApplicationState.webAppFileSystemRootPath + File.separator + portletLocation);
    root.mkdirs();
    List<File> ret= new ArrayList<File>();
    for (File templFile : root.listFiles()) {
      if (templFile.isFile() && templFile.getName().endsWith(".jsp")&& !templFile.getName().contains("param"))
        ret.add(templFile);
    }

    //scan portlets folder for each customer
    String pathname = ApplicationState.webAppFileSystemRootPath + File.separator + ApplicationState.platformConfiguration.getDefaultApplication().getRootFolder() + File.separator + "customers";
    File customers = new File(pathname);
    customers.mkdirs();
    for (File pq : customers.listFiles()) {
      if (pq.isDirectory()){
        File plugins=new File(pq,"portlets");
        if (plugins.exists() && plugins.isDirectory()){
          for (File templFile : plugins.listFiles()) {
            if (templFile.isFile() && templFile.getName().endsWith(".jsp")&& !templFile.getName().contains("param"))
              ret.add(templFile);
          }
        }

      }
    }

    Collections.sort(ret, new PlatformComparators.FileNameComparator());
    return ret;
  }

  public JSONObject jsonify(){
    JSONObject ret= super.jsonify();
    ret.element("id", getId());
    ret.element("name", getName());
    if (getFile()!=null)
      ret.element("file", getFile().getFileLocation());

    if (JSP.ex((Map)getParams())){
      JSONObject p= new JSONObject();
      for (String key:getParams().keySet()){
        p.element(key,getParams().get(key));
      }
      ret.element("parameters",p);

    }

      return ret;


  }

}
