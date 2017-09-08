package com.teamwork.expand;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.Identifiable;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.remoteFile.BasicDocumentBricks;
import org.jblooming.remoteFile.Document;
import org.jblooming.remoteFile.FileStorage;
import org.jblooming.remoteFile.RemoteFile;
import org.jblooming.remoteFile.RemoteFileSystem;
import org.jblooming.system.SystemConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.Bricks;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.button.AHref;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.html.display.Explorer;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import com.twproject.document.TeamworkDocument;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Resource;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Task;

public class TaskReportBricks extends Bricks{

	 private TaskReport mainObject;
	 
	  public TaskReportBricks(TaskReport r) {
		    this.mainObject = r;
		  }
	
	  public static SmartCombo getReportTypeCombo(String fieldName, PageState pageState) {
		    String hql = "select p.id, p.stringValue from " +TaskReportType.class.getName() + " as p ";
		    String whereForFiltering = "where p.stringValue like :" + SmartCombo.FILTER_PARAM_NAME + " order by p.stringValue";
		    String whereForId = "where p.id = :" + SmartCombo.FILTER_PARAM_NAME;
		    SmartCombo serviceType = new SmartCombo(fieldName, hql, whereForFiltering, whereForId);
		    serviceType.label = I18n.get("REPORT_TYPE");
		    serviceType.fieldSize = 25;
		    serviceType.separator = "<br>";

		    if (pageState.getLoggedOperator().hasPermissionAsAdmin()) {
		      ButtonSupport addTT = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("ADD_REPORT_TYPE")), pageState.pageFromRoot("task/report/taskReportType.jsp"));
		      addTT.additionalCssClass = "small";
		      serviceType.addEntityButton = addTT;
		    }

		    return serviceType;
	 }
	  
	  
	  public static SmartCombo getReportYearCombo(String fieldName, PageState pageState) {
		    String hql = "select p.year, p.year from " +TaskReport.class.getName() + " as p ";
		    String whereForFiltering = "where p.year like :" + SmartCombo.FILTER_PARAM_NAME + " group by p.year  order by  p.year";
		    String whereForId = "where p.year = :" + SmartCombo.FILTER_PARAM_NAME;
		    SmartCombo serviceType = new SmartCombo(fieldName, hql, whereForFiltering, whereForId);
		    serviceType.label = I18n.get("REPORT_YEAR");
		    serviceType.fieldSize = 25;
		    serviceType.separator = "<br>";
		    return serviceType;
	 }
	

	  
	  public static SmartCombo getReportDictionNameCombo(String fieldName, PageState pageState) {
		    String hql = "select p.dictionaryName, p.dictionaryName from " +TaskReport.class.getName() + " as p ";
		    String whereForFiltering = "where p.dictionaryName like :" + SmartCombo.FILTER_PARAM_NAME + " group by p.dictionaryName  order by  p.dictionaryName";
		    String whereForId = "where p.dictionaryName = :" + SmartCombo.FILTER_PARAM_NAME;
		    SmartCombo serviceType = new SmartCombo(fieldName, hql, whereForFiltering, whereForId);
		    serviceType.label = I18n.get("REPORT_DiC_NAME");
		    serviceType.fieldSize = 25;
		    serviceType.separator = "<br>";
		    return serviceType;
	 }
	  
	  public AHref getContentLink(PageState pageState)  {
		   /* AHref aHref = new AHref("<span class='teamworkIcon'>n</span>"+mainObject.getName(),"");*/
		    AHref aHref = new AHref(mainObject.getName(),"");
		    aHref.id="cl_"+mainObject.getId();
		    if (mainObject.getFile() != null) {
		        aHref.href=mainObject.getFile().getDownloadOrViewLink().href;
		    }
		    return aHref;
   }
	  
	  public AHref getExportLink(PageState pageState,String key)  {
		   /* AHref aHref = new AHref("<span class='teamworkIcon'>n</span>"+mainObject.getName(),"");*/
		  String fileDir=pageState.getEntry("CURRENT_FILE_DIR").stringValueNullIfEmpty();
		  String relativePath=pageState.getEntry(key).stringValueNullIfEmpty();
		  String folderLocation ="";
		  if(fileDir!=null){
			  folderLocation = ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL) + (fileDir.trim().length() > 0 ? File.separator + fileDir : "");
		  }else{
			  folderLocation = ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL) ;
		  }
		  
		  if(folderLocation==null){
			  folderLocation=ApplicationState.webAppFileSystemRootPath;
		  }
		  FileStorage fileStorage = new FileStorage();
		  fileStorage.setContent(folderLocation);
		  fileStorage.setId(Integer.valueOf("88888"));
		 
		//  fileStorage.setType(type);
		   RemoteFile rf=new RemoteFileSystem(fileStorage);
		   rf.setTarget(relativePath);
		  // PageSeed downOrExplore = null;
		   PageSeed downOrExplore = new PageSeed(ApplicationState.contextPath + "/applications/teamwork/task/report/explorer.jsp");
           downOrExplore.mainObjectId = fileStorage.getId();
           downOrExplore.setPopup(true);
           downOrExplore.addClientEntry("PATH", relativePath);
           downOrExplore.addClientEntry("FILE_REPORT_FOLDER", folderLocation);
          // downOrExplore.addClientEntry("RELATIVE_FILE_DIR", relativePath);
           Explorer.SecurityCarrier esc = new Explorer.SecurityCarrier();
           TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

           //esc.canRead = mainObject.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canRead);
           esc.canRead =true;
           esc.canWrite=false;
           esc.canCreateDirectory=false;
          // esc.canWrite = mainObject.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canWrite);
          // esc.canCreateDirectory = mainObject.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canCreate);
           esc.rootPath = relativePath;
           pageState.sessionState.setAttribute(esc.getKey(fileStorage.getId()), esc);
           AHref aHref = new AHref(mainObject.getName(),"");
           aHref.href="javascript:openBlackPopup('"+downOrExplore.toLinkToHref()+"','80%','80%')";
		   aHref.id="cl_"+mainObject.getId();
		   return aHref;
  } 
	  
	  
	  public static PageSeed getPageSeedForExplorer(HttpServletRequest request, PageState pageState, RemoteFile remoteFile, Document document) {
		    PageSeed downOrExplore = null;

		    if (remoteFile.isDirectory()) {
		      PageSeed ps = pageState.thisPage(request);
		      ps.addClientEntry(pageState.getEntry("ROOTPATH"));
		      ps.mainObjectId = document.getId();
		      ps.setPopup(pageState.isPopup());
		      ps.addClientEntry("PATH", remoteFile.getRelativePath());
		      ps.addClientEntry("TASK_ID", pageState.getEntry("TASK_ID").stringValueNullIfEmpty());
		      downOrExplore = ps;
		    } else {
		      if (Document.ConnectionType.SERVICE.equals(document.getConnType())) {
		        try {
		          String psst = getStringFromRemoteInputStream(remoteFile.getRemoteInputStream());
		          if (psst != null && !psst.trim().equals("")) {
		            downOrExplore = new PageSeed(psst);
		          } else {
		            Tracer.platformLogger.error(" Cannot get URL ");

		          }
		        } catch (IOException e) {
		          Tracer.platformLogger.error(" Cannot get URL ", e);
		        }
		      } else {
		        downOrExplore = getPageSeedForDownload(document,remoteFile.getRelativePath());
		      }
		    }

		    return downOrExplore;

		  }
	  private static String getStringFromRemoteInputStream(InputStream input) throws IOException {
		    StringBuffer str = new StringBuffer();
		    int c;
		    //getClient();
		    //InputStream input = remoteFile.getRemoteInputStream();
		    while (((c = input.read()) != -1)) {
		      str.append((char) c);
		    }
		    input.close();
		    return str.toString();

		  }
	  public static PageSeed getPageSeedForDownload(Document fileStorage, String relativePath){
		    PageSeed ps = new PageSeed(ApplicationState.contextPath + "/applications/teamwork/task/report/partDownload.jsp");
		    ps.mainObjectId = fileStorage.getId();
		    ps.setPopup(true);
		    ps.addClientEntry("PATH", relativePath);
		    ps.addClientEntry("CK", _computeCk(fileStorage.getId(),relativePath,System.currentTimeMillis()));
		    return ps;
		  }
	  private static String _computeCk(Serializable docId, String relativePath,long millis){
		    String ms = Long.toString(millis, Character.MAX_RADIX).toLowerCase();
		    return ms+"."+StringUtilities.md5Encode(docId+relativePath,millis+"s41t3d");
		  }

	  
	 public static String getDocumentUploadUser(String path){
		 try {
			OqlQuery ql=new OqlQuery("select t.creator from twk_task_report t where t.persistentfile like '%"+path+"'",PersistenceContext.getDefaultPersistenceContext(),Boolean.TRUE);
			List<Object> objList=ql.list();
		    if(objList!=null&&objList.size()>0){
		    	Object obj=objList.get(0);
		    	if(obj!=null)return obj.toString();
		    }
		 } catch (PersistenceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
		 return null;
	 }
	 
	 public static String getDocumentUploadHisUser(String path){
		 try {
			OqlQuery ql=new OqlQuery("select t.creator from twk_task_report_history t where t.originalfilename = '"+path+"'",PersistenceContext.getDefaultPersistenceContext(),Boolean.TRUE);
			List<Object> objList=ql.list();
		    if(objList!=null&&objList.size()>0){
		    	Object obj=objList.get(0);
		    	if(obj!=null)return obj.toString();
		    }
		 } catch (PersistenceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
		 return null;
	 }
public ButtonSupport getReferralButtonPointToReport() {
    Identifiable referral = mainObject.getReferral();

    ButtonSupport button = null;
    if (referral != null) {

      PageSeed ps = new PageSeed();
      ps.setMainObjectId(mainObject.getId());
      ps.setCommand(Commands.EDIT);

      if (referral instanceof Task) {
        ps.addClientEntry("TASK_ID",referral.getId());
        ps.href = ApplicationState.contextPath + "/applications/teamwork/task/taskReportEditor.jsp";
        button = ButtonLink.getBlackInstance("",700,1000,ps);
        button.label = "<small>" + I18n.get("REPORT_TASK") + ":" + ((Task) referral).getDisplayName() + "</small>";

      }

    }
    return button;
  }


public ButtonLink getReferralButton() {

    ButtonLink button = null;
    Identifiable referral = mainObject.getReferral();
    if (referral != null) {

      PageSeed ps = new PageSeed();
      ps.command="LIST_REPORTS";

      button = new ButtonLink(ps);

      if (referral instanceof Task) {
        Task t = (Task) referral;
        ps.addClientEntry("TASK_ID",t.getId());
        ps.href = ApplicationState.contextPath + "/applications/teamwork/task/taskReportList.jsp";

        //button.label = "<small>" + pageState.getI18n("DOCUMENT_TASK") + ":" + t.getDisplayName() + "</small>";
        button.label = t.getDisplayName();

      }

    }
    return button;
  }

}
