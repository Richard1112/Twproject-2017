package org.jblooming.waf;

import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.security.Permission;
import org.jblooming.system.SystemConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.core.JspIncluder;
import org.jblooming.waf.settings.Application;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.PageContext;
import java.io.File;
import java.io.IOException;
import java.util.*;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 31-mag-2007 : 10.52.18
 */
public class PluginBricks {


  public static PageQuark getGlobalPageQuarkInstance(Permission permission, String groupOfPartName, HttpServletRequest request) {
    return getPageQuarkInstance(null, null, permission, false, groupOfPartName, request);
  }

  public static PageQuark getEditPageQuarkInstance(Class<? extends Identifiable> partIsReletedToObjectOfClass, Permission permission, String groupOfPartName, HttpServletRequest request) {
    return getPageQuarkInstance(partIsReletedToObjectOfClass, null, permission, true, groupOfPartName, request);
  }

  public static PageQuark getListPageQuarkInstance(Class<? extends Identifiable> partIsReletedToObjectOfClass, Permission permission, String groupOfPartName, HttpServletRequest request) {
    return getPageQuarkInstance(partIsReletedToObjectOfClass, null, permission, false, groupOfPartName, request);
  }

  public static PagePlugin getPagePluginInstance(String groupName, PagePlugin pagePlugin, HttpServletRequest request) {
    return (PagePlugin) getPageQuarkInstance(null, pagePlugin, null, false, groupName, request);
  }

  /**
   * This method must be called in the JspIncluder.INITIALIZE call back. Build the PageQuark for the "current running" page.
   * The pagequark.persistentFile.fileLocation points to the web folder
   *
   * @param partIsRelatedToObjectOfClass is the class of main object. This part will be visible only when the main object class is equal to the specified one
   *                                     when the class is null the security is not delegated
   * @param permission                   permission required to the logged user to make this part visible first and runnable then
   * @param isForSingleObject            TRUE id this part has to be used with editors. FALSE if has to be used in list
   * @param groupOfPartName              It is used to add this part in the appropriate group. It is used as parameter in ApplicationState.getParameparrecover the original file name.
   * @param request                      It is used to recover the original file name.
   * @return
   */
  private static PageQuark getPageQuarkInstance(Class<? extends Identifiable> partIsRelatedToObjectOfClass, PagePlugin pagePlugin, Permission permission, boolean isForSingleObject, String groupOfPartName, HttpServletRequest request) {
    PageQuark pq;
    if (pagePlugin == null) {
      if (partIsRelatedToObjectOfClass != null) {
        pq = new PagePlugin(partIsRelatedToObjectOfClass);
        ((PagePlugin) pq).isForSingleObject = isForSingleObject;
      } else {
        pq = new PageQuark();
      }
    } else {
      pq = pagePlugin;
    }
    String realURI = HttpUtilities.realURI(request);
    String fileName = realURI.substring(realURI.lastIndexOf("/") + 1, realURI.length());
    pq.setName(FileUtilities.getNameWithoutExt(fileName));
    if (permission != null)
      pq.addPermission(permission);
    PersistentFile pf = new PersistentFile(0, fileName, PersistentFile.TYPE_WEBAPP_FILESTORAGE);
    pf.setFileLocation(realURI); // take that!
    pq.setFile(pf);
    getPageQuarkGroup(groupOfPartName).add(pq);
    Tracer.platformLogger.debug("Init ok for: " + pq.getName());
    return pq;
  }


  /**
   * this method must be called in an application setting when the contextPath is available
   *
   * @param folderName  is the name of folder to scan. It is relative to the application root and is http styled (e.g. task/report NOT task\report)
   * @param application is the current application
   * @param pageContext
   */
  public static void scanFolderAndInitializeQuarks(String folderName, Application application, PageContext pageContext) {
    String pathname = ApplicationState.webAppFileSystemRootPath + File.separator + application.getRootFolder() + File.separator + folderName;
    File printPages = new File(pathname);
    if (!printPages.exists())
      printPages.mkdirs();
    for (File pq : printPages.listFiles()) {
      if (pq.getName().toLowerCase().endsWith(".jsp")) {
        PageSeed ps = new PageSeed(application.getRootFolder() + "/" + folderName + "/" + pq.getName());
        ps.setCommand(JspIncluder.INITIALIZE);
        try {
          pageContext.include("/" + ps.toLinkToHref());
          Tracer.platformLogger.debug("Loaded print page: " + pq.getName());
        } catch (ServletException e) {
          Tracer.platformLogger.error("loading " + ps.href, e);
        } catch (IOException e) {
          Tracer.platformLogger.error("loading " + pq.getName(), e);
        } catch (Throwable e) {
          Tracer.platformLogger.error("loading " + pq.getName(), e);
        }
      }
    }
  }

  public static List<ButtonSupport> createPageQuarkItemsButtons(String groupName, PageState pageState) {
    List<ButtonSupport> bss = new ArrayList();
    for (PageQuark pq : getPageQuarkGroup(groupName)) {
      if (pq.isVisibleInThisContext(pageState)) {
        ButtonSupport bs = pq.getPluginLinkButton(pageState);
        bs.additionalCssClass="lreq30 lreqLabel";
        bss.add(bs);
      }
    }
    return bss;
  }

  public static List<ButtonSupport> createPageQuarkItemsButtonsForDataTable(String groupName, PageState pageState) {
    List<ButtonSupport> bss = new ArrayList();
    for (PageQuark pq : getPageQuarkGroup(groupName)) {
      if (pq.isVisibleInThisContext(pageState)) {
        ButtonSupport bs = pq.getPluginJSButton();
        bs.additionalCssClass="lreq30 lreqLabel";
        bss.add(bs);
      }
    }
    return bss;
  }

  // it make possible to get a special plug in to visualize in task's log section
  public static List<JspHelper> addPageQuarkTaskLog(String groupName, PageState pageState) {
    List<JspHelper> lstImport = new ArrayList<JspHelper>();

    for (PageQuark pq : getPageQuarkGroup(groupName)) {
      if (pq.isVisibleInThisContext(pageState)) {
        JspHelper helper = new JspHelper(pq.getHrefForInclude());
        lstImport.add(helper);
      }
    }

    return lstImport;
  }

  public static Set<PageQuark> getPageQuarkGroup(String groupName) {
    Map<String, Set<PageQuark>> sPQ = getPageQuarks();
    Set<PageQuark> pqg = sPQ.get(groupName);
    if (pqg == null) {
      pqg = new HashSet();
      sPQ.put(groupName, pqg);
    }
    return pqg;
  }

  public static Map<String, Set<PageQuark>> getPageQuarks() {
    Map<String, Set<PageQuark>> sPQ = (Map<String, Set<PageQuark>>) ApplicationState.applicationParameters.get(SystemConstants.PAGE_PLUGINS);
    if (sPQ == null) {
      sPQ = new HashTable();
      ApplicationState.applicationParameters.put(SystemConstants.PAGE_PLUGINS, sPQ);
    }
    return sPQ;
  }

}


