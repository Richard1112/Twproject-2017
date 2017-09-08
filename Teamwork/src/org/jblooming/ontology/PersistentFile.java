package org.jblooming.ontology;

import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.tracer.Tracer;
import org.jblooming.uidgen.CounterHome;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.html.button.AHref;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.constants.Fields;
import org.jblooming.system.SystemConstants;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.PlatformRuntimeException;

import java.io.*;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;

import net.sf.json.JSONObject;

import javax.crypto.NoSuchPaddingException;
import javax.persistence.Transient;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class PersistentFile implements Serializable {

  public static final String TYPE_DB = "DB";
  public static final String TYPE_FILESTORAGE = "FR";           // this is relative to the REPOSITORY_URL parameter 
  public static final String TYPE_ENCRYPTED_FILESTORAGE = "ER"; // this is relative to the REPOSITORY_URL parameter
  public static final String TYPE_FILESTORAGE_ABSOLUTE = "FS";  // this is absolute e.g. c:\tmp
  public static final String TYPE_WEBAPP_FILESTORAGE = "WF";    // relative to root

  public static String DEFAULT_STORAGE_TYPE = TYPE_FILESTORAGE;

  private int UID;
  private String type = PersistentFile.DEFAULT_STORAGE_TYPE;
  private String originalFileName;

  /**
   * this is the complete path to the file, file inclusive
   */
  private String fileLocation;

  /**
   * this is the complete path to the file, file exclusive; not persisted
   */
  public String fileDir;

  public static final String PERSISTENTFILE_ID = "PERSISTENTFILE_ID";

  // not persisted is used only to get a un-persisted reference
  private BinaryLargeObject blob = null;

  public PersistentFile(int UID, String originalFileName) {
    this(UID, originalFileName, PersistentFile.DEFAULT_STORAGE_TYPE);
  }

  public PersistentFile(int UID, String originalFileName, String type) {
    this.setOriginalFileName(originalFileName);
    this.setUID(UID);
    this.type = type;
  }


  @Transient
  public int getUID() {
    return UID;
  }

  public void setUID(int UID) {
    this.UID = UID;
  }


  public boolean equals(Object o) {
    return this.compareTo(o) == 0;
  }

  public int hashCode() {
    return serialize().hashCode();
  }

  public int compareTo(Object o) {
    if (this == o)
      return 0;
    if (o == null)
      return -1;
    else {
      return serialize().compareTo(((PersistentFile) o).serialize());
    }
  }

  @Transient
  public String getType() {
    return type;
  }

  /**
   * @deprecated use constructor
   */
  public void setType(String type) {
    this.type = type;
  }

  @Transient
  public String getOriginalFileName() {
    return originalFileName;
  }

  public void setOriginalFileName(String originalFileName) {
    this.originalFileName = originalFileName;
  }

  /**
   * @return the file location relative to PersistentFile.type complete with file name
   */
  @Transient
  public String getFileLocation() {
    return fileLocation;
  }

  public void setFileLocation(String fileLocation) {
    this.fileLocation = fileLocation;
  }

  public String serialize() {
    String ser = type + "_" + UID + "_" + originalFileName;
    if (TYPE_FILESTORAGE_ABSOLUTE.equals(type) || TYPE_FILESTORAGE.equals(type) || TYPE_ENCRYPTED_FILESTORAGE.equals(type) || TYPE_WEBAPP_FILESTORAGE.equals(type))
      ser = ser + "+++" + fileLocation;
    return ser;
  }

  @Transient
  public String getName() {
    String name = "";
    if (TYPE_DB.equals(type)) {
      name = originalFileName;
    } else {
      String fileLocNorm = StringUtilities.replaceAllNoRegex(fileLocation, "\\", "/");
      name = fileLocNorm.substring(Math.max(0, fileLocNorm.lastIndexOf("/") + 1));
    }
    return name;
  }

  /*
  public void makedirs(){
   new File( ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL) + File.separator + "photos").mkdirs();
  }
  */

  public static PersistentFile deserialize(String serObj) {
    PersistentFile pf = null;
    if (serObj != null) {
      int first_ = serObj.indexOf("_");
      if (first_ > 0) {
        String type = serObj.substring(0, first_);

        int second_ = serObj.indexOf("_", first_ + 1);
        int third = serObj.length();
        if (TYPE_FILESTORAGE.equals(type) || TYPE_ENCRYPTED_FILESTORAGE.equals(type) || TYPE_WEBAPP_FILESTORAGE.equals(type) || TYPE_FILESTORAGE_ABSOLUTE.equals(type))
          third = serObj.indexOf("+++");
        if (second_ > 0) {
          int UID = Integer.parseInt(serObj.substring(first_ + 1, second_));
          String originalFileName = serObj.substring(second_ + 1, third);
          pf = new PersistentFile(UID, originalFileName, type);
        }
        if (TYPE_FILESTORAGE.equals(type) || TYPE_ENCRYPTED_FILESTORAGE.equals(type) || TYPE_WEBAPP_FILESTORAGE.equals(type) || TYPE_FILESTORAGE_ABSOLUTE.equals(type)) {
          String fileLoc = serObj.substring(serObj.indexOf("+++") + 3);
          pf.fileLocation = fileLoc;
          File file = new File(fileLoc);
          pf.fileDir = file.getParent();
        }
      }
    }
    return pf;
  }


  @Transient
  public PageSeed getPageSeed(boolean treatAsAttach) {
    PageSeed ps = null;
    if (PersistentFile.TYPE_WEBAPP_FILESTORAGE.equals(getType())) {  // WEBAPP_FILESTORAGE is the only case the file is accessible whitout partUploaderView
      //ps = new PageSeed(ApplicationState.contextPath + "/" + StringUtilities.replaceAllNoRegex(getFileLocation(), "\\", "/"));
      ps = new PageSeed( ApplicationState.serverURL+ "/" + StringUtilities.replaceAllNoRegex(getFileLocation(), "\\", "/")); // 7/5/15 meglio usare il serverUrl assoluto, altrimenti per le immagini può fare casino con il fixPath
    } else {
      ps = new PageSeed(ApplicationState.contextPath + "/commons/layout/partUploaderView.jsp");
      String serUid = serialize();
      ps.addClientEntry(Fields.FILE_TO_UPLOAD, serUid);
      if (treatAsAttach)
        ps.addClientEntry("TREATASATTACH", treatAsAttach);
      ps.addClientEntry("CK", _computeCk(System.currentTimeMillis()));
    }
    return ps;
  }


  public boolean exists(){
    boolean ret=false;
    if (TYPE_DB.equals(getType())) {
      try {
        ret= PersistenceHome.findByPrimaryKey(BinaryLargeObject.class, getUID())!=null;
      } catch (FindByPrimaryKeyException e) {
        Tracer.platformLogger.error("Persistent file BLOB type error.",e);
      }

    } else {
      ret=getFile().exists();
    }
    return ret;
  }

  public BinaryLargeObject getBlob(PersistenceContext pc) throws FindByPrimaryKeyException {
    if (TYPE_DB.equals(getType())) {
      if (blob == null) {
        blob = (BinaryLargeObject) PersistenceHome.findByPrimaryKey(BinaryLargeObject.class, getUID(), pc);
      }
      return blob;
    } else {
      throw new PlatformRuntimeException("Blob object can be used in case of TYPE_DB only.");
    }
  }

  /**
   * @param persistentFileType   determine where the final file will be placed. In case of DB type a BinaryLargeObject will be created and stored. Remember to assign referral ID and CLASS and store the blob again.
   * @param inputStream
   * @param originalFileName
   * @param relativePathFileName
   * @param pc                   is used only in case of DB type. Use the PersistenceContext.get(MyObject.Class) where myObject.setPersistentFile(). Can be null  @return a PersistentFile created correctly basing on type, relative path etc..
   */
  public static PersistentFile createPersistentFileFromStream(String persistentFileType, InputStream inputStream, String originalFileName, String relativePathFileName, PersistenceContext pc) throws IOException {

    PersistentFile pf = new PersistentFile(0, originalFileName, persistentFileType);

    boolean encrypt = PersistentFile.TYPE_ENCRYPTED_FILESTORAGE.equals(persistentFileType);

    if (PersistentFile.TYPE_DB.equals(persistentFileType)) {
      if (pc == null)
        pc = PersistenceContext.getDefaultPersistenceContext();
      //throw new PlatformRuntimeException("Invalid use of createPersistentFileFromStream with DB type when pc is null");

      // create BLOB
      BinaryLargeObject blo = new BinaryLargeObject();
      blo.feed(inputStream, pc.session);
      try {
        blo.store(pc);
      } catch (StoreException e) {
        throw new PlatformRuntimeException(e);
      }
      pf.blob = blo;
      pf.setUID(blo.getIntId());

    } else {
      try {
        pf.setUID(CounterHome.next(PersistentFile.PERSISTENTFILE_ID));
      } catch (StoreException e) {
        throw new PlatformRuntimeException(e);
      }

      if (!JSP.ex(relativePathFileName))
        relativePathFileName="/"+FileUtilities.padd(pf.getUID() + "", 6, "0")+"."+originalFileName;

      pf.setFileLocation(relativePathFileName);

      if (PersistentFile.TYPE_FILESTORAGE.equals(persistentFileType) || PersistentFile.TYPE_ENCRYPTED_FILESTORAGE.equals(persistentFileType)) {
        String repositoryUrl = ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL);
        String separator = (repositoryUrl.endsWith("\\") || repositoryUrl.endsWith("/") || relativePathFileName.startsWith("\\") || relativePathFileName.startsWith("/")) ? "" : File.separator;
        String destination = repositoryUrl + separator + relativePathFileName;
        File destFile = new File(destination);
        File parent = destFile.getParentFile();
        parent.mkdirs();
        OutputStream fout = new FileOutputStream(destFile);

        if (encrypt) {
          String key = I18n.isActive("CUSTOM_FEATURE_AES_CRYPTO_KEY") ? I18n.get("CUSTOM_FEATURE_AES_CRYPTO_KEY") : StringUtilities.key;
          try {
            fout = FileUtilities.getCipherOutputStream(fout, key);
          } catch (Throwable e) {
            throw new PlatformRuntimeException(e);
          }
        }

        FileUtilities.copy(inputStream, fout);
        fout.close();


      } else if (PersistentFile.TYPE_FILESTORAGE_ABSOLUTE.equals(persistentFileType)) {
        FileOutputStream fout = new FileOutputStream(relativePathFileName);
        FileUtilities.copy(inputStream, fout);
        fout.close();

      } else if (PersistentFile.TYPE_WEBAPP_FILESTORAGE.equals(persistentFileType)) {
        // be carefull the separator for a webapp is always /
        String repositoryUrl = ApplicationState.webAppFileSystemRootPath;
        String separator = (repositoryUrl.endsWith("\\") || repositoryUrl.endsWith("/") || relativePathFileName.startsWith("\\") || relativePathFileName.startsWith("/")) ? "" : File.separator;
        String destination = repositoryUrl + separator + relativePathFileName;
        File destFile = new File(destination);
        File parent = destFile.getParentFile();
        parent.mkdirs();
        FileOutputStream fout = new FileOutputStream(destFile);
        FileUtilities.copy(inputStream, fout);
        fout.close();

      } else {
        throw new PlatformRuntimeException("Unsupported Persistent File Type: " + persistentFileType);
      }
    }
    return pf;
  }

  @Transient
  public InputStream getInputStream() throws FileNotFoundException, FindByPrimaryKeyException {
    // if you are using multiple connection with BLOB use in both DB you may be in trouble
    return getInputStream(PersistenceContext.getDefaultPersistenceContext());
  }


  public InputStream getInputStream(PersistenceContext pc) throws FileNotFoundException, FindByPrimaryKeyException {

    InputStream inputStream = null;
    if (PersistentFile.TYPE_DB.equals(getType())) {
      if (pc == null)
        throw new PlatformRuntimeException("Invalid use of getInputStream(null) with DB type PersistentFile");
      BinaryLargeObject blo = getBlob(pc);
      try {
        inputStream = blo.getInputStream();
      } catch (Throwable e) {
        throw new PlatformRuntimeException(e);
      }
    } else if (PersistentFile.TYPE_ENCRYPTED_FILESTORAGE.equals(getType())) {
      String key = I18n.isActive("CUSTOM_FEATURE_AES_CRYPTO_KEY") ? I18n.get("CUSTOM_FEATURE_AES_CRYPTO_KEY") : StringUtilities.key;
      try {
        inputStream = FileUtilities.getCipherInputStream(new FileInputStream(getFile()), key);
      } catch (Throwable e) {
        throw new PlatformRuntimeException(e);
      }

    } else if (PersistentFile.TYPE_FILESTORAGE.equals(getType()) ||
      PersistentFile.TYPE_FILESTORAGE_ABSOLUTE.equals(getType()) ||
      PersistentFile.TYPE_WEBAPP_FILESTORAGE.equals(getType())) {
      inputStream = new FileInputStream(getFile());
    } else {
      throw new PlatformRuntimeException("Unsupported Persistent File Type: " + getType());
    }
    return inputStream;
  }

  @Transient
  private File getFile() {
    File file = null;
    if (PersistentFile.TYPE_FILESTORAGE.equals(getType()) || PersistentFile.TYPE_ENCRYPTED_FILESTORAGE.equals(getType())) {
      String repositoryUrl = ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL);
      String fileLocation = getFileLocation();
      String separator = (repositoryUrl.endsWith(File.separator) || repositoryUrl.endsWith("/") ||
        fileLocation.startsWith(File.separator) || fileLocation.startsWith("/"))
        ? "" : File.separator;
      //String url = repositoryUrl + separator + fileLocation;
      // encrypted files ends with .aes
      String url = repositoryUrl + separator + fileLocation+(PersistentFile.TYPE_ENCRYPTED_FILESTORAGE.equals(getType())?".aes":"");
      file = new File(url);

    } else if (PersistentFile.TYPE_FILESTORAGE_ABSOLUTE.equals(getType())) {
      file = new File(getFileLocation());
    } else if (PersistentFile.TYPE_WEBAPP_FILESTORAGE.equals(getType())) {
      file = new File(ApplicationState.webAppFileSystemRootPath + File.separator + getFileLocation());
    } else {
      throw new PlatformRuntimeException("Unsupported Persistent File Type: " + getType());
    }

    return file;
  }

  public JSONObject jsonify() {
    JSONObject ret = new JSONObject();
    ret.element("uid", serialize());
    ret.element("type", getType());
    ret.element("name", getOriginalFileName());
    ret.element("img", getMimeImageName());
    ret.element("mime", HttpUtilities.getContentType(getName()));
    ret.element("url", getPageSeed(false).toLinkToHref());
    ret.element("ext",getFileExtension());
    ret.element("ck",_computeCk(System.currentTimeMillis()));
    return ret;
  }

  public void delete() {

    if (PersistentFile.TYPE_DB.equals(getType()))
      try {
        BinaryLargeObject blob=(BinaryLargeObject) PersistenceHome.findByPrimaryKey(BinaryLargeObject.class, getUID());
        if (blob!=null)
          blob.remove();
      } catch (PersistenceException e) {
        Tracer.platformLogger.info("Strange, you are try removing a BLOB based persistentFile without a BLOB defined :" + getUID());
      }
    else {
      File delendo = getFile();
      if (delendo.exists())
        FileUtilities.tryHardToDeleteFile(delendo);
    }

  }


  @Transient
  public String getFileExtension() {
    return FileUtilities.getFileExt(getOriginalFileName());
  }

  @Transient
  public String getMimeImageName() {
    return HttpUtilities.getContentType(getName()).replaceAll("[/\\+]", "_") + ".png";
  }


  public boolean checkChecksum(String checksum) {
    String ms = checksum.substring(0, checksum.indexOf("."));
    long t = Long.parseLong(ms, Character.MAX_RADIX);
    if (System.currentTimeMillis() - t < CompanyCalendar.MILLIS_IN_DAY) // a link is valid for 24 hours
      return checksum.equals(_computeCk(t));
    else
      return false;
  }

  private String _computeCk(long millis) {
    String ms = Long.toString(millis, Character.MAX_RADIX).toLowerCase();
    return ms + "." + StringUtilities.md5Encode(serialize(), millis + "s4l4t1n0");
  }


  @Transient
  public AHref getDownloadOrViewLink()  {
    AHref aHref= new AHref(getOriginalFileName(),"");
    if (FileUtilities.isImageByFileExt(getFileExtension()) || FileUtilities.isPdfByFileExt(getFileExtension())){
      aHref.href="javascript:"+JSP.urlEncode("openPersistentFile({url:'"+getPageSeed(false).toLinkToHref()+"',mime:'"+ HttpUtilities.getContentType(getName())+"'})");
    } else {
      aHref.href=getPageSeed(true).toLinkToHref();
    }
    return aHref;
  }
}
