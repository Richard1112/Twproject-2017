package com.teamwork.expand;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.jblooming.ApplicationException;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.remoteFile.Document;
import org.jblooming.remoteFile.FileStorage;
import org.jblooming.remoteFile.RemoteFile;
import org.jblooming.remoteFile.Document.ConnectionType;
import org.jblooming.system.SystemConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.constants.FieldErrorConstants;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.input.Uploader;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;

import com.twproject.operator.TeamworkOperator;
import com.twproject.rank.Hit;
import com.twproject.task.Task;

public class ReportExplorerAction {
	public void cmdZip(HttpServletRequest request, HttpServletResponse response, PageState pageState) throws PersistenceException, IOException {
		// Document document =null;
//		if(pageState.mainObjectId!=null){
//			  document = (Document) PersistenceHome.findByPrimaryKey(Document.class, pageState.mainObjectId);
//			 
//		}else{
//			 		}
		
		Document document = ReportDocument.getInstance();
		document.setContent(ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL));
		document.setName("Root");
		response.setContentType("application/zip");
		document.setConnType(Document.ConnectionType.FS);
	    RemoteFile rfs = RemoteFile.getInstance(document);
	    String path = JSP.w(pageState.getEntry("PATH").stringValueNullIfEmpty());
	    path+= (path.endsWith(rfs.getSeparator())?"":rfs.getSeparator());


	    //get selected files
	    Set<String> selFiles = pageState.getClientEntries().getEntriesStartingWithStripped("FILE_", Fields.TRUE).keySet();
	    String zipName = document.getName();
	    if (selFiles.size() == 1)
	      zipName = selFiles.iterator().next();
	    else if(selFiles.size() > 1){
	    	 String q=pageState.getEntry("PATH").stringValueNullIfEmpty();
	    	 String[] qa=q.split("/");
	    	 zipName = qa[qa.length-1];
	    }
	    response.setHeader("Content-Disposition", "attachment; filename=\"" + zipName + ".zip\"");
	    ZipOutputStream zipout = new ZipOutputStream(response.getOutputStream());
	    zipout.setComment("File Storage Service");

	    for (String fileName : selFiles) {

	      rfs.setTarget(path + fileName);
	      if (rfs.isDirectory()) {
	        List<RemoteFile> lrf = rfs.expandFileList();
	        for (RemoteFile foundRF : lrf) {
	          if (!foundRF.isDirectory())
	            zipRemoteFile(foundRF, path, zipout);
	        }
	      } else
	        zipRemoteFile(rfs, path, zipout);
	    }

	    try {
	      zipout.finish();
	      //response.getWriter().flush();
	    } catch (java.util.zip.ZipException e) {
	      Tracer.platformLogger.error(e);
	    }

	  }

	  public void cmdDelete(PageState pageState) throws PersistenceException, IOException {
		  FileStorage fileStorage = new FileStorage();
		  fileStorage.setContent(ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL));
		  fileStorage.setName("Root");
		  fileStorage.setConnType(ConnectionType.FS);
	   // Document document = (Document) PersistenceHome.findByPrimaryKey(Document.class, pageState.mainObjectId);

	    RemoteFile rfs = RemoteFile.getInstance(fileStorage);
	    String path = JSP.w(pageState.getEntry("PATH").stringValueNullIfEmpty());
	    path+= (path.endsWith(rfs.getSeparator())?"":rfs.getSeparator());

	    //get selected files
	    Map<String,ClientEntry> entryMap = pageState.getClientEntries().getEntriesStartingWithStripped("FILE_", Fields.TRUE);

	    for (String fileName : entryMap.keySet()) {
	      rfs.setTarget(path + fileName);
	      if (!rfs.delete()){
	        entryMap.get(fileName).errorCode="CANT_DELETE_FILE";         
	      }
	    }

	  }


	  private void zipRemoteFile(RemoteFile foundRF, String currentPath, ZipOutputStream zipout) throws IOException {
	    String fileDir = foundRF.getRelativePath().substring(currentPath.length());
	    ZipEntry zipEntry = new ZipEntry(fileDir);
	    zipout.putNextEntry(zipEntry);
	    InputStream remoteInputStream = foundRF.getRemoteInputStream();
	    BufferedInputStream fr = new BufferedInputStream(remoteInputStream);
	    int b;
	    byte[] buf = new byte[1024];
	    int len;
	    while ((len = fr.read(buf)) > 0) {
	      zipout.write(buf, 0, len);
	    }
	    fr.close();
	    zipout.closeEntry();
	    zipout.flush();
	    remoteInputStream.close();
	  }

	  public void mkdir(PageState pageState) throws FindByPrimaryKeyException {
	    //Document document = (Document) PersistenceHome.findByPrimaryKey(Document.class, pageState.mainObjectId);
		  FileStorage fileStorage = new FileStorage();
		  fileStorage.setContent(ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL));
		  fileStorage.setName("Root");
		  fileStorage.setConnType(ConnectionType.FS);
	    try {
	      String dirName = pageState.getEntryAndSetRequired("DIR_NAME").stringValueNullIfEmpty();

	      if (dirName != null) {
	        RemoteFile rfs = RemoteFile.getInstance(fileStorage);
	        String path = JSP.w(pageState.getEntry("PATH").stringValueNullIfEmpty());
	        path+= (path.endsWith(rfs.getSeparator())?"":rfs.getSeparator());
	        if (dirName.indexOf(rfs.getSeparator()) != -1) {
	          pageState.getEntry("DIR_NAME").errorCode = pageState.getI18n("PATH_NOT_ALLOWED");
	        } else {
	          rfs.setTarget(path +  dirName);
	          rfs.mkdir();
	        }
	      }
	    } catch (Exception e) {
	      pageState.getEntry("DIR_NAME").errorCode = e.getMessage();
	    }
	  }

	  public RemoteFile upload(PageState pageState) throws PersistenceException, IOException, ApplicationException, ActionException {

	    //Document document = (Document) PersistenceHome.findByPrimaryKey(Document.class, pageState.mainObjectId);
	   FileStorage fileStorage = new FileStorage();
	   fileStorage.setContent(ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL));
	   fileStorage.setName("Root");
	   fileStorage.setConnType(ConnectionType.FS);
       Uploader.UploadHelper uh = Uploader.getHelper("UPLOAD_HERE", pageState);
       TeamworkOperator logged=(TeamworkOperator) pageState.getLoggedOperator();
       String fileName = pageState.getEntryAndSetRequired("UPLOAD_HERE").stringValueNullIfEmpty();
       TaskReport reportToBeSaved=null;
	   if (fileName != null) {
		   reportToBeSaved=saveReportHistory(uh,pageState);
	   }
	   String version="1";
	   if(reportToBeSaved==null){
		   reportToBeSaved= new TaskReport();
		   reportToBeSaved.setIdAsNew();
		   Boolean isNew = true;
	   }else{
		   version=String.valueOf(Integer.valueOf(reportToBeSaved.getVersion())+1);
	   }
	   reportToBeSaved.setOwner(logged);
	   reportToBeSaved.setCreator(logged.getDisplayName());
	   reportToBeSaved.setCreationDate(new Date());
	   reportToBeSaved.setOwner(logged);
	   
	   Task task=Task.load(pageState.getEntry("TASK_ID").stringValueNullIfEmpty());
	   RemoteFile rfs = RemoteFile.getInstance(fileStorage);
	   String path = JSP.w(pageState.getEntry("PATH").stringValueNullIfEmpty());
	   path += (path.endsWith(rfs.getSeparator()) ? "" : rfs.getSeparator());
	   String[] pathArr=path.split(rfs.getSeparator());
	   String hql = "from " + TaskReportType.class.getName() + " as tt where tt.stringValue='"+pathArr[1]+"' ";
	   OqlQuery oql = new OqlQuery(hql);
	   List<TaskReportType> tts=oql.list();
	   if(tts!=null&&tts.size()>0)reportToBeSaved.setTaskReportType(tts.get(0));
	   if (fileName != null) {
	    	
	        rfs.setTarget(path + uh.originalFileName);
	        PersistentFile persistentFile = reportToBeSaved.getFile();
		    if (persistentFile == null) {
		        persistentFile = new PersistentFile(0, null,PersistentFile.DEFAULT_STORAGE_TYPE);
		    }
		   
	        persistentFile.fileDir=path;
	    
	        reportToBeSaved.setDictionaryName(path);
	        reportToBeSaved.setYear(DateUtils.dateToString(new Date(), 18));
	    	reportToBeSaved.setStartDate(new Date());
	    	reportToBeSaved.setEndDate(new Date());
	    	reportToBeSaved.setLastModified(new Date());
	    	reportToBeSaved.setLastModifier(logged.getDisplayName());
	        try {
	        reportToBeSaved.setFile(ReportUploader.save(reportToBeSaved, persistentFile, "UPLOAD_HERE", pageState));
	        if (task!=null) {
		    	reportToBeSaved.setTask(task);
		    	reportToBeSaved.setTaskId(task.getId().toString());
		    	reportToBeSaved.setOriginalFileName(persistentFile.getOriginalFileName());
		    	reportToBeSaved.setVersion(version);
		    	reportToBeSaved.store();
		       // ReportAction.generateReportEvent(reportToBeSaved, logged);

		      } else {
		    	  reportToBeSaved.store();
		      }
	        reportToBeSaved.getContentForIndexing();
		      Hit.getInstanceAndStore(reportToBeSaved, logged, .2);
	         // boolean isUpload = rfs.upload(uh.temporaryFile,false);
	         // if (!isUpload)
	         //   pageState.getEntry("UPLOAD_HERE").errorCode = FieldErrorConstants.ERR_NAME_USED;
	        } catch (Throwable e) {
	          pageState.getEntry("UPLOAD_HERE").errorCode = FieldErrorConstants.ERR_NAME_USED;
	          Tracer.platformLogger.error(e);
	        }
	    } else {
	      pageState.getEntry("UPLOAD_HERE").errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
	    }
	    return rfs;

	  }
	  
	  
	  public TaskReport saveReportHistory(Uploader.UploadHelper uh,PageState pageState) throws ApplicationException, ActionException{
		 TaskReport old=null;
		try {
			old = TaskReport.loadInfoByFileName(uh.originalFileName);
            if(old!=null){
            	// TeamworkOperator logged=(TeamworkOperator) pageState.getLoggedOperator();
            	TaskReportHistory taskReportHis=new TaskReportHistory();
            	taskReportHis.setIdAsNew();
            	taskReportHis.setOwner(old.getOwner());
            	taskReportHis.setCreator(old.getCreator());
            	taskReportHis.setCreationDate(old.getCreationDate());
            	taskReportHis.setTaskReportType(old.getTaskReportType());
            	taskReportHis.setDictionaryName(old.getDictionaryName()+"backup/");
            	taskReportHis.setYear(DateUtils.dateToString(new Date(), 18));
            	taskReportHis.setStartDate(new Date());
            	taskReportHis.setEndDate(new Date());
            	taskReportHis.setTask(old.getTask());
            	taskReportHis.setTaskId(old.getTask().getId().toString());
            	String code = FileUtilities.padd(old.getVersion() + "", 6, "0");
            	taskReportHis.setOriginalFileName(code+"."+old.getOriginalFileName());
            	taskReportHis.setVersion(old.getVersion());
            	taskReportHis.setTaskReportType(old.getTaskReportType());
            	taskReportHis.setParentId(old.getId().toString());
            	PersistentFile persistentFile =null;
            	 if (persistentFile == null) {
     		        persistentFile = new PersistentFile(0, null,PersistentFile.DEFAULT_STORAGE_TYPE);
     		    }
    	       persistentFile.fileDir=taskReportHis.getDictionaryName();
    	       
    	        File oldFile=new File(ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL)+"/"+old.getDictionaryName()+old.getOriginalFileName());
    	       

    	        //OutputStream fos = new FileOutputStream(auh.repositoryPath);
    	        OutputStream fos;
				try {
					

					FileInputStream fis = new FileInputStream(oldFile);
					File newFile=new File(ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL)+"/"+taskReportHis.getDictionaryName());
					if(!newFile.exists()){
						newFile.mkdirs();
					}
					fos = new FileOutputStream(ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL)+"/"+taskReportHis.getDictionaryName()+code+"."+old.getOriginalFileName());
					byte[] byteCount = new byte[1024]; // la dimensione puo essere "aggiustata" per esigenze specifiche

	    	        int read;
	    	        for (; ; ) {
	    	          read = fis.read(byteCount);
	    	          if (read < 0)
	    	            break;
	    	          fos.write(byteCount, 0, read);
	    	        }
	    	        fos.flush();
	    	        fis.close();
	    	        fos.close();
	    	        oldFile.delete();
				} catch (FileNotFoundException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} // encrypted files ends with .aes
                catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

    	     // taskReportHis.setFile(Uploader.save(taskReportHis, persistentFile, "DOCUMENT_UPLOAD", pageState));
    	        
    	      taskReportHis.store();
	    	  return old;
	    	}
		} catch (PersistenceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	    	
	    return old;
	  }
}
