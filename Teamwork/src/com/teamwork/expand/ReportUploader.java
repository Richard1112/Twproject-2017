package com.teamwork.expand;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.http.multipartfilter.MultipartFormRequestEncodingFilter;
import org.jblooming.ontology.BinaryLargeObject;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.system.SystemConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.uidgen.CounterHome;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.input.Uploader;
import org.jblooming.waf.html.input.Uploader.UploadHelper;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

public class ReportUploader extends Uploader{

	public ReportUploader(String fieldName, PageState pageState) {
		super(fieldName, pageState);
		// TODO Auto-generated constructor stub
	}
	
	public static PersistentFile save(Identifiable mainObject, PersistentFile persistentFile, String formFieldName, RestState pageState) throws PersistenceException, ApplicationException, ActionException {

	    final ClientEntry entry = pageState.getEntry(formFieldName);
	    String value = entry.stringValue();
	    if (value != null && value.trim().length() <= 0) {
	      value = null;
	      // aprile 2007 teoros:: il nome serializzato del persistent file rischia di superare 255 caratteri ==> data truncation
	      // vedi anche:: UploadHelper.getInstance()
	    } else if (value != null && value.trim().length() > 50) {
	      String extension = FileUtilities.getFileExt(value);
	      value = value.substring(0, 50) + extension;
	    }

	    boolean fileSelected = value != null;
	    boolean uploadEntryMissing = entry.name == null;  //only the first time is shown the page, after we have always the entry.name... thanks to filter
	    boolean alreadyPersisted = (persistentFile != null && persistentFile.getUID() != 0);

	    //creating a file
	    boolean creatingAFile = fileSelected && !alreadyPersisted;
	    //updating a file
	    boolean updatingAFile = fileSelected && alreadyPersisted;
	    //leave it as it is
	    boolean leaveItAlone = uploadEntryMissing && alreadyPersisted;
	    //removing a file
	    boolean removeIt = !uploadEntryMissing && alreadyPersisted && !fileSelected;

	    //DO NOT TOUCH THIS WITHOUT ASKING PIETRO
	    if (!fileSelected && !alreadyPersisted) {
	      persistentFile = null;
	      return null;
	    }

	    //DO NOT TOUCH THIS WITHOUT ASKING PIETRO
	    if (leaveItAlone) {
	      pageState.addClientEntry(formFieldName, persistentFile);
	      return persistentFile;
	    }


	    if (persistentFile != null) {

	      if (creatingAFile || updatingAFile) {

	        if (!pageState.multipart)
	          Tracer.platformLogger.warn("Seems attempted upload on a non-multipart form:" + pageState.href);

	        if (updatingAFile) {
	          persistentFile.delete();
	        }

	        if (PersistentFile.TYPE_DB.equals(persistentFile.getType())) {
	          UploadHelper uh = getHelper(entry.name, pageState);

	          if (uh != null && uh.originalFileName != null && uh.originalFileName.trim().length() > 0) {

	            BinaryLargeObject blo = null;
	            if (updatingAFile) {
	              blo = (BinaryLargeObject) PersistenceHome.findByPrimaryKey(BinaryLargeObject.class, persistentFile.getUID());
	              //we are forced to create always a new one, as at least in Oracle the update of multiple blobs in the same trans seems broken.
	              if (blo != null)
	                blo.remove();
	            }

	            blo = new BinaryLargeObject();

	            try {
	              blo.feed(new FileInputStream(uh.temporaryFile), PersistenceContext.get(BinaryLargeObject.class).session);
	              blo.setReferral(mainObject);
	              blo.setOriginalFileName(uh.originalFileName);
	              blo.store();

	              persistentFile = new PersistentFile(Integer.parseInt(blo.getId().toString()), uh.originalFileName, PersistentFile.TYPE_DB);
	              entry.setValue(persistentFile.serialize());

	            } catch (FileNotFoundException e) {
	              throw new ApplicationException(e);
	            }
	          }

	        } else {
	          try {

	            boolean encrypt = false;

	            persistentFile.setUID(CounterHome.next(PersistentFile.PERSISTENTFILE_ID));
	            String folderLocation = null;

	            //this is the relative path
	            String fileDir = persistentFile.fileDir != null ? persistentFile.fileDir : "";

	            //this is the complete path
	            if (PersistentFile.TYPE_WEBAPP_FILESTORAGE.equals(persistentFile.getType())) {
	              fileDir = StringUtilities.replaceAllNoRegex(StringUtilities.replaceAllNoRegex(fileDir, "/", File.separator), "\\", File.separator);
	              folderLocation = ApplicationState.webAppFileSystemRootPath + File.separator + fileDir;

	            } else if (PersistentFile.TYPE_FILESTORAGE.equals(persistentFile.getType()) || PersistentFile.TYPE_ENCRYPTED_FILESTORAGE.equals(persistentFile.getType())) {
	              folderLocation = ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL) + (fileDir.trim().length() > 0 ? File.separator + fileDir : "");

	              encrypt = PersistentFile.TYPE_ENCRYPTED_FILESTORAGE.equals(persistentFile.getType());

	            } else if (PersistentFile.TYPE_FILESTORAGE_ABSOLUTE.equals(persistentFile.getType())) {
	              folderLocation = fileDir;
	            } else {
	              throw new PlatformRuntimeException("Unsupported persistent file type: " + persistentFile.getType());
	            }
	            UploadHelper uh = saveInFolder(formFieldName, persistentFile.getUID(), folderLocation, encrypt, pageState);
	            if (PersistentFile.TYPE_WEBAPP_FILESTORAGE.equals(persistentFile.getType())) {
	              persistentFile.setFileLocation(fileDir + "/" + uh.repositoryFileName);

	            } else if (PersistentFile.TYPE_FILESTORAGE.equals(persistentFile.getType()) || PersistentFile.TYPE_ENCRYPTED_FILESTORAGE.equals(persistentFile.getType())) {
	              persistentFile.setFileLocation(fileDir + File.separator + uh.repositoryFileName);

	            } else if (PersistentFile.TYPE_FILESTORAGE_ABSOLUTE.equals(persistentFile.getType())) {
	              persistentFile.setFileLocation(uh.repositoryPath);
	            } else {
	              throw new PlatformRuntimeException("Unsupported persistent file type: " + persistentFile.getType());
	            }

	            persistentFile.setOriginalFileName(uh.originalFileName);

	          } catch (IOException e) {
	            throw new ApplicationException(e);
	          }
	        }
	      } else if (removeIt) {
	        persistentFile.delete();
	        persistentFile = null;
	      }
	      pageState.addClientEntry(formFieldName, persistentFile);
	    }
	    return persistentFile;
	  }
	
	
	

	
	
	
	private static UploadHelper saveInFolder(String formFileFieldName, int documentId, String folderLocation, boolean encrypt, RestState pageState) throws IOException {

	    UploadHelper auh = getHelper(formFileFieldName, pageState);

	    if (auh != null && auh.temporaryFileName != null) {
	      auh.temporaryFile = new File(auh.temporaryFileName);

	      if (auh.temporaryFileName != null && auh.temporaryFileName.length() > 0 && auh.temporaryFile.exists()) {

	        String lastPartFileName = null;
	        if (auh.originalFileName.lastIndexOf(".") > -1)
	          lastPartFileName = auh.originalFileName.substring(Math.max(auh.originalFileName.lastIndexOf("\\"), auh.originalFileName.lastIndexOf("/")) + 1, auh.originalFileName.lastIndexOf("."));
	        else
	          lastPartFileName = auh.originalFileName;

	        String code = FileUtilities.padd(documentId + "", 6, "0");
	        ///String code ="";
	        auh.code = code;

	       // auh.repositoryFileName = code + '.' + lastPartFileName + FileUtilities.getFileExt(auh.originalFileName);
	        auh.repositoryFileName =  lastPartFileName + FileUtilities.getFileExt(auh.originalFileName);
	        String folderName = folderLocation + (folderLocation.endsWith("/") ? "/" : "");
	        // robik 17/09/2007 to make more robust when uploading. Remove check from code where possible

	        File folder = new File(folderName);
	        if (!folder.exists())
	          folder.mkdirs();

	        auh.repositoryPath = folderName + '/' + auh.repositoryFileName;


	        FileInputStream fis = new FileInputStream(auh.temporaryFile);

	        //OutputStream fos = new FileOutputStream(auh.repositoryPath);
	        OutputStream fos = new FileOutputStream(auh.repositoryPath + (encrypt ? ".aes" : "")); // encrypted files ends with .aes

	        if (encrypt) {
	          String key = I18n.isActive("CUSTOM_FEATURE_AES_CRYPTO_KEY") ? I18n.get("CUSTOM_FEATURE_AES_CRYPTO_KEY") : StringUtilities.key;
	          try {
	            fos = FileUtilities.getCipherOutputStream(fos, key);
	          } catch (Throwable e) {
	            throw new PlatformRuntimeException(e);
	          }
	        }


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
	      }
	    }
	    return auh;
	  }

}
