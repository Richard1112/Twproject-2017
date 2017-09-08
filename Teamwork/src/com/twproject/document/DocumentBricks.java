package com.twproject.document;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Resource;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Task;
import com.twproject.utilities.TeamworkComparators;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.remoteFile.BasicDocumentBricks;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.Documentable;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.remoteFile.Document;
import org.jblooming.remoteFile.FileStorage;
import org.jblooming.remoteFile.RemoteFile;
import org.jblooming.security.Area;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.Bricks;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.button.AHref;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Explorer;
import org.jblooming.waf.html.display.Img;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import java.util.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class DocumentBricks extends Bricks {

  public TeamworkDocument mainObject;

  public DocumentBricks(TeamworkDocument document) {
    this.mainObject = document;
  }

  public ButtonLink getReferralButton() {

    ButtonLink button = null;
    Identifiable referral = mainObject.getReferral();
    if (referral != null) {

      PageSeed ps = new PageSeed();
      ps.command="LIST_DOCS";

      button = new ButtonLink(ps);

      if (referral instanceof Task) {
        Task t = (Task) referral;
        ps.addClientEntry("TASK_ID",t.getId());
        ps.href = ApplicationState.contextPath + "/applications/teamwork/task/taskDocumentList.jsp";

        //button.label = "<small>" + pageState.getI18n("DOCUMENT_TASK") + ":" + t.getDisplayName() + "</small>";
        button.label = t.getDisplayName();

      } else if (referral instanceof Resource) {
        Resource r = (Resource) referral;
        ps.addClientEntry("RES_ID",r.getId());
        ps.href = ApplicationState.contextPath + "/applications/teamwork/resource/resourceDocumentList.jsp";
        button.label = "<small>" + I18n.get("DOCUMENT_RESOURCE") + ":" + r.getDisplayName() + "</small>";
        button.label = r.getDisplayName();

      }

    }
    return button;
  }

  public ButtonSupport getReferralButtonPointToDoc() {
    Identifiable referral = mainObject.getReferral();

    ButtonSupport button = null;
    if (referral != null) {

      PageSeed ps = new PageSeed();
      ps.setMainObjectId(mainObject.getId());
      ps.setCommand(Commands.EDIT);

      if (referral instanceof Task) {
        ps.addClientEntry("TASK_ID",referral.getId());
        ps.href = ApplicationState.contextPath + "/applications/teamwork/task/taskDocumentEditor.jsp";
        button = ButtonLink.getBlackInstance("",700,1000,ps);
        button.label = "<small>" + I18n.get("DOCUMENT_TASK") + ":" + ((Task) referral).getDisplayName() + "</small>";

      } else if (referral instanceof Resource) {
        ps.addClientEntry("RES_ID",referral.getId());
        ps.href = ApplicationState.contextPath + "/applications/teamwork/resource/resourceDocumentEditor.jsp";
        button = ButtonLink.getBlackInstance("",700,100,ps);
        button.label = "<small>" + I18n.get("DOCUMENT_RESOURCE") + ":" + ((Resource) referral).getDisplayName() + "</small>";

      }

    }
    return button;
  }


  public AHref getContentLink(PageState pageState)  {
   /* AHref aHref = new AHref("<span class='teamworkIcon'>n</span>"+mainObject.getName(),"");*/
    AHref aHref = new AHref(mainObject.getName(),"");
    aHref.id="cl_"+mainObject.getId();
    PageSeed downOrExplore = null;
    String docContent = mainObject.getContent();

    if (Document.IS_FILE_STORAGE == mainObject.getType()) {
      if (JSP.ex(docContent)) {
        downOrExplore = getPageSeedForExplorer(docContent, pageState, false);
        if (downOrExplore != null) {
          // beautiful hack to avoid Firefox download stops when window.open with parameters
          if (downOrExplore.toLinkToHref().contains("/partDownload.jsp")) {
            //dato che si scarica il file vediamo se fosse una immagine par fare la preview
            String fileExtension = mainObject.getRemoteFile().getFileExtension();
            if (FileUtilities.isImageByFileExt(fileExtension) || FileUtilities.isPdfByFileExt(fileExtension)) {
              downOrExplore.setPopup(false);
              aHref.href = "javascript:" + JSP.urlEncode("openPersistentFile({url:'" + downOrExplore.toLinkToHref() + "',mime:'" + HttpUtilities.getContentType(mainObject.getRemoteFile().getName()) + "'})");
            }else {
              aHref.href = downOrExplore.toLinkToHref();
            }
          }else{
            aHref.href="javascript:openBlackPopup('"+downOrExplore.toLinkToHref()+"','80%','80%')";
          }
        }
      }

    } else if (TeamworkDocument.IS_UPLOAD == mainObject.getType()) {
      if (mainObject.getFile() != null) {
        aHref.href=mainObject.getFile().getDownloadOrViewLink().href;
      }

    } else if (TeamworkDocument.IS_URL == mainObject.getType()) {
      aHref.href=docContent;
      aHref.target = "_blank";

    } else if (TeamworkDocument.IS_CONTENT == mainObject.getType()) {
      downOrExplore = new PageSeed(ApplicationState.contextPath + "/applications/teamwork/document/documentContentViewer.jsp");
      downOrExplore.mainObjectId = mainObject.getId();
      downOrExplore.setCommand(Commands.EDIT);
      downOrExplore.setPopup(true);
      aHref.href="javascript:openBlackPopup('"+downOrExplore.toLinkToHref()+"')";
    }

    return aHref;
  }


  /**
   *
   * @param docContent
   * @param pageState
   * @return a page seed pointing to the explorer. It put is sessionState the Explorer.SecurityCarrier filled with permissions and root path.
   *         it will be use by Explorer in order to avoid url hacking
   * @throws PersistenceException
   */
  public PageSeed getPageSeedForExplorer(String docContent, PageState pageState, boolean openMainDir) {

    PageSeed downOrExplore = null;
    if (docContent!=null && docContent.startsWith("RF")) {
      String string = docContent.substring(2);
      String[] valori = string.split(":");
      if (valori != null && valori.length > 0) {
        String id = valori[0];
        FileStorage fileStorage = (FileStorage) PersistenceHome.findByPrimaryKeyNullIfError(FileStorage.class, id);
        if (fileStorage != null) {
          String relativePath = valori[1];
          RemoteFile rf = null;
          try {
            rf = RemoteFile.getInstance(fileStorage);
            rf.setTarget(relativePath);

            // controllo per aprire la directory che contiene il file
            if (!rf.isDirectory() && openMainDir) {
               int lastIndex = relativePath.lastIndexOf("/");
              if (lastIndex != -1) {
                relativePath = relativePath.substring(0, lastIndex + 1);
              }else{
                relativePath ="/";
              }
              rf.setTarget(relativePath);
            }
            if (!rf.exists())
              return null; // add - graziella 04/09/08
          } catch (Exception e) {
            throw new PlatformRuntimeException(e);
          }

          if (rf != null) {
            if (!rf.isDirectory()) {
              downOrExplore = BasicDocumentBricks.getPageSeedForDownload(fileStorage, relativePath);

            } else {
              downOrExplore = new PageSeed(ApplicationState.contextPath + "/applications/teamwork/document/explorer.jsp");
              downOrExplore.mainObjectId = fileStorage.getId();
              downOrExplore.setPopup(true);
              downOrExplore.addClientEntry("PATH", relativePath);

              // set a security provider for
              Explorer.SecurityCarrier esc = new Explorer.SecurityCarrier();
              TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

              esc.canRead = mainObject.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canRead);
              esc.canWrite = mainObject.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canWrite);
              esc.canCreateDirectory = mainObject.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canCreate);
              esc.rootPath = relativePath;
              pageState.sessionState.setAttribute(esc.getKey(fileStorage.getId()), esc);

            }
          }
        }
      }
    }
    return downOrExplore;
  }


  public static List<FileStorage> getFileStoragesOrdered(TeamworkOperator logged, PageState pageState) throws PersistenceException {
    String hql = "from " + FileStorage.class.getName() + " as document";

    QueryHelper qh = new QueryHelper(hql);

    // filter for areas

    Set<Area> al = logged.getAreasForPermission(TeamworkPermissions.fileStorage_canRead);
    if (al.size() > 0)
      qh.addOQLClause("document.area in (:al)", "al", al);

    qh.addOrQueryClause("document.owner = :myself");
    qh.setParameter("myself", logged);

    DataTable.orderAction(qh, "DOCFS", pageState, "document.name");
    return qh.toHql().list();

  }


  public static List<TeamworkDocument> getLastVersionDocuments(Documentable docu){
    List<TeamworkDocument> allDocs= new ArrayList<TeamworkDocument>();

    for (Object o:docu.getDocuments()){
      TeamworkDocument td= (TeamworkDocument) o;
      if (td.getParent()==null){
        allDocs.add(td);
      }
    }

    Collections.sort(allDocs, TeamworkComparators.documentNameVersionComparator);
    return allDocs;
  }


  public Img getMimeImage(){
    Img ret=new Img("mime/unknown.png", "", "30","");

    if (mainObject.getType() == Document.IS_UPLOAD) {
      if (mainObject.getFile() != null) {
        String imgRoot = "mime/" + mainObject.getFile().getMimeImageName();
        ret= new Img(imgRoot, "", "30","");
      }
    } else if (mainObject.getType() == Document.IS_FILE_STORAGE && JSP.ex(mainObject.getContent())) {
      String mime = HttpUtilities.getContentType(mainObject.getContent()).replaceAll("[/\\+]", "_") + ".png";
        ret=new Img("mime/" + mime,"", "30","");
    } else if (mainObject.getType() == Document.IS_URL) {

      if(mainObject.getContent().toLowerCase().contains("github")){
        ret=new Img("mime/github-doc.png", "", "30","");
      } else if(mainObject.getContent().toLowerCase().contains("drive.google") || mainObject.getContent().toLowerCase().contains("docs.google")){
        ret=new Img("mime/drive-doc.png", "", "30","");
      }  else if(mainObject.getContent().toLowerCase().contains("dropbox.com")){
        ret=new Img("mime/dropbox-doc.png", "", "30","");
      } else{
        ret=new Img("mime/url.png", "", "30","");
      }

    } else if (mainObject.getType() == Document.IS_CONTENT) {
      ret=new Img("mime/content.png", "", "30","");
    }else if (mainObject.getType() == Document.IS_FILE_STORAGE){
      ret= new Img("mime/fileStorage.png", "", "30","");
    }

    return ret;
  }

}
