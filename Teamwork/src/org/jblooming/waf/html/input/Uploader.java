package org.jblooming.waf.html.input;

import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.http.multipartfilter.MultipartFormRequestEncodingFilter;
import org.jblooming.ontology.BinaryLargeObject;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.system.SystemConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.uidgen.CounterHome;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.input.Uploader.UploadHelper;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import javax.servlet.http.HttpServletResponse;
import javax.servlet.jsp.PageContext;
import java.io.*;
import java.net.URLEncoder;


public class Uploader extends JspHelper {

  public String fieldName;
  public boolean disabled = false;
  public int size;
  public String label;
  public String className = "formElements";
  public String classLabelName;
  public String separator;
  public String jsScript;
  public boolean required = false;
  public String toolTip;
  public boolean doFeedBackError = true;
  public Form form;
  public boolean preserveOldValue = true;


  /**
   * key control
   */
  public String actionListened;
  public int keyToHandle;
  public String launchedJsOnActionListened;
  public boolean translateError = true;
  public boolean readOnly = false;
  public boolean treatAsAttachment = true;


  public Uploader(String fieldName, PageState pageState) {
    this.id = fieldName + "_upl";
    this.urlToInclude = "/commons/layout/partUploader.jsp";
    this.fieldName = fieldName;
    this.required = pageState.getEntry(fieldName).required;
  }

  @Deprecated
  public Uploader(String fieldName, Form form, PageState pageState) {
    this.id = fieldName + "_upl";
    this.urlToInclude = "/commons/layout/partUploader.jsp";
    this.fieldName = fieldName;
    this.form = form;
    this.required = pageState.getEntry(fieldName).required;
  }

  // of great elegance
  public static String getHiddenFieldName(String ceName) {
    return "sp_fi_br_" + ceName + "_upl";
  }


  public String getDiscriminator() {
    return Uploader.class.getName();
  }


  public void toHtml(PageContext pageContext) {
    pageContext.getRequest().setAttribute(ACTION, "");
    super.toHtml(pageContext);
  }

  public static void displayFile(PageState pageState, HttpServletResponse response) throws IOException, FindByPrimaryKeyException {
    displayFile(true, pageState, response);
  }

  public static void displayFile(boolean treatAsAttachment, PageState pageState, HttpServletResponse response) throws IOException, FindByPrimaryKeyException {

    String pfSer = pageState.getEntry(Fields.FILE_TO_UPLOAD).stringValueNullIfEmpty();

    if (pfSer != null) {
      PersistentFile pf = PersistentFile.deserialize(pfSer);

      //checksum
      if (!pf.checkChecksum(pageState.getEntry("CK").stringValueNullIfEmpty())) {
        Tracer.platformLogger.error("Invalid checksum displaying file: " + pf.getOriginalFileName());
        response.getWriter().print("<h1>Invalid checksum</h1>");
        return;
      }

      response.setContentType(HttpUtilities.getContentType(pf.getOriginalFileName()));
      String filename = pf.getOriginalFileName();

      InputStream inputStream = pf.getInputStream();
      String filenameEncoded = URLEncoder.encode(filename, "UTF8");
      filenameEncoded = StringUtilities.replaceAllNoRegex(StringUtilities.replaceAllNoRegex(filenameEncoded, "+", "_"), " ", "_");

      // sets header with original file name
      if (treatAsAttachment) {
        response.setHeader("content-disposition", "attachment; filename=" + filenameEncoded);
      } else
        response.setHeader("content-disposition", "inline; filename=" + filenameEncoded);

      // write data to stream and close it
      FileUtilities.writeStream(inputStream, response.getOutputStream());
      inputStream.close();

    } else {
      throw new PlatformRuntimeException("Unsupported PersistentFile type.");
    }
  }

  public static PersistentFile save(Identifiable mainObject, String formFieldName, PageState pageState) throws PersistenceException, ApplicationException, ActionException {
    PersistentFile persistentFile = new PersistentFile(0, null, PersistentFile.DEFAULT_STORAGE_TYPE);
    persistentFile.fileDir = "";
    return save(mainObject, persistentFile, formFieldName, pageState);
  }

  /**
   * @param mainObject     used only in blob. Can be null
   * @param persistentFile
   * @param formFieldName
   * @param pageState
   * @return
   * @throws PersistenceException
   * @throws ApplicationException
   * @throws ActionException
   */
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
        auh.code = code;

        auh.repositoryFileName = code + '.' + lastPartFileName + FileUtilities.getFileExt(auh.originalFileName);

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


  public static int getMaxUploadSizeInByte() {
    String msS = ApplicationState.getApplicationSetting(SystemConstants.UPLOAD_MAX_SIZE);
    int sizeInMB = 20;
    if (JSP.ex(msS)) {
      try {
        sizeInMB = Integer.parseInt(msS);
      } catch (NumberFormatException e) {
        Tracer.platformLogger.error(e);
      }
    }
    return sizeInMB * 1024 * 1024;
  }


  public static UploadHelper getHelper(String formFileFieldName, RestState pageState) {
    UploadHelper auh = null;
    String originalFileName = pageState.getEntry(formFileFieldName).stringValueNullIfEmpty();
    if (originalFileName != null) {
      // aprile 2007 teoros:: persistentFile serialized name could be longer than 255  ==> data truncation
      // see also:: Uploader.save
      if (originalFileName.trim().length() > 50) {
        String extension = FileUtilities.getFileExt(originalFileName);
        originalFileName = originalFileName.substring(0, 50) + extension;
      }
      auh = new UploadHelper();

      auh.originalFileName = originalFileName;

      auh.mimetype = pageState.getEntry(formFileFieldName + MultipartFormRequestEncodingFilter.CONTENT_TYPE).stringValueNullIfEmpty();

      auh.temporaryFileName = pageState.getEntry(formFileFieldName + MultipartFormRequestEncodingFilter.TEMPORARY_FILENAME).stringValueNullIfEmpty();

      if (auh.temporaryFileName != null)
        auh.temporaryFile = new File(auh.temporaryFileName);

    }
    return auh;
  }


  public static class UploadHelper {
    public String originalFileName;
    public String mimetype;
    public String temporaryFileName;
    public File temporaryFile;

    public String code;
    public String repositoryPath;
    public String repositoryFileName;

    private UploadHelper() {
    }


  }
  
  
	
}

