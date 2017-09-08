package org.jblooming.remoteFile;

import com.opnlb.fulltext.IndexableDocument;
import org.jblooming.ontology.*;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.JSP;
import org.jblooming.logging.Sniffable;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.PlatformRuntimeException;

import java.util.*;

import net.sf.json.JSONObject;

/**
 * Document (c) 2005 - Open Lab - www.open-lab.com
 */
public abstract class Document extends SecuredNodeWithAreaSupport implements IndexableDocument, Sniffable {

  private String code;
  private String name;
  private String summa;
  private String kind;
  /**
   * from DocumentType
   */
  private int type;
  private String content;
  private String mimeType;
  private String versionLabel;
  private String version;
  private String author;
  private String keywords;
  private Date authored;
  private Date created;
  private long sizeInBytes;
  private boolean index;
  private String connectionHost; // connection host contains eventually the non-standard port
  private String connectionUser;
  private String connectionPwd;
  private String connectionNotes;
  private String connectionType;
  private PersistentFile persistentFile;

  public static final int IS_UPLOAD = 1;
  public static final int IS_CONTENT = 2;
  public static final int IS_URL = 3;
  public static final int IS_FILE_STORAGE = 4;
  public static final String DOCUMENT = "DOC";


  public enum ConnectionType {
    FS, FTP, SFTP, HTTP, SAMBA, NFS, SERVICE, SERVICEGROUP, SVN, SVN_Http, SVN_Https, DROPBOX,S3
  }

  public static Set<ConnectionType> enabledConnectionTypes = new HashSet();


  public Document() {
    super();
  }

  public Document(String dId) {
    super(dId);
  }

  /**
   * Method setVersion
   *
   * @param version a  String
   */
  public void setVersion(String version) {
    this.version = version;
  }

  /**
   * Method getVersion
   *
   * @return a String
   */
  public String getVersion() {
    return version;
  }

  /**
   * Method setKind
   *
   * @param kind a  String
   */
  public void setKind(String kind) {
    this.kind = kind;
  }

  /**
   * Method getKind
   *
   * @return a String
   */
  public String getKind() {
    return kind;
  }

  /**
   * Method setSumma
   *
   * @param summa a  String
   */
  public void setSumma(String summa) {
    this.summa = summa;
  }

  /**
   * Method getSumma
   *
   * @return a String
   */
  public String getSumma() {
    return summa;
  }

  public String getKeywords() {
    return keywords;
  }

  public void setKeywords(String keywords) {
    this.keywords = keywords;
  }

  public String getAuthor() {
    return author;
  }

  public void setAuthor(String author) {
    this.author = author;
  }

  public String getVersionLabel() {
    return versionLabel;
  }

  public void setVersionLabel(String versionlabel) {
    this.versionLabel = versionlabel;
  }

  public String getContent() {
    return content;
  }

  public void setContent(String content) {
    this.content = content;
  }

  public String getMimeType() {
    return mimeType;
  }

  public void setMimeType(String mimeType) {
    this.mimeType = mimeType;
  }

  public String getUrl() {
    return persistentFile.getFileLocation();
  }

  public String getCreator() {
    return creator;
  }

  public void setCreator(String creator) {
    this.creator = creator;
  }

  public Date getAuthored() {
    return authored;
  }

  public void setAuthored(Date authored) {
    this.authored = authored;
  }

  public Date getCreated() {
    return created;
  }

  public void setCreated(Date created) {
    this.created = created;
  }

  public int getType() {
    return type;
  }

  public void setType(int type) {
    this.type = type;
  }

  public long getSizeInBytes() {
    return sizeInBytes;
  }

  public void setSizeInBytes(long sizeInBytes) {
    this.sizeInBytes = sizeInBytes;
  }

  public String nextVersion() {
    if (version == null)
      return VersionHome.VERSION_ROOT;
    return VersionHome.nextVersion(version);
  }


  public String getCode() {
    return code;
  }

  public void setCode(String code) {
    this.code = code;
  }

  public boolean isIndex() {
    return index;
  }

  public void setIndex(boolean index) {
    this.index = index;
  }

  public String getConnectionHost() {
    return connectionHost;
  }

  public void setConnectionHost(String connectionHost) {
    this.connectionHost = connectionHost;
  }

  public String getConnectionUser() {
    return connectionUser;
  }

  public void setConnectionUser(String connectionUser) {
    this.connectionUser = connectionUser;
  }

  public String getConnectionPwd() {
    return connectionPwd;
  }

  public void setConnectionPwd(String connectionPwd) {
    this.connectionPwd = connectionPwd;
  }

  public String getConnectionNotes() {
    return connectionNotes;
  }

  public void setConnectionNotes(String connectionNotes) {
    this.connectionNotes = connectionNotes;
  }

  private String getConnectionType() {
    return connectionType;
  }

  private void setConnectionType(String connectionType) {
    this.connectionType = connectionType;
  }

  public ConnectionType getConnType() {
    if (connectionType != null && !connectionType.equals(""))
      return ConnectionType.valueOf(connectionType);
    else
      return null;
  }

  public void setConnType(ConnectionType connectionType) {
    this.connectionType = connectionType + "";
  }

  public int getAreaId() {
    return getArea().getIntId();
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public PersistentFile getFile() {
    return persistentFile;
  }

  public void setFile(PersistentFile file) {
    this.persistentFile = file;
  }

  private PersistentFile getPersistentFile() {
    return persistentFile;
  }

  private void setPersistentFile(PersistentFile persistentFile) {
    this.persistentFile = persistentFile;
  }

  public static Map<Integer, String> getTypeMap() {

    Map<Integer, String> statuses = new HashTable<Integer, String>();
    statuses.put(IS_CONTENT, "IS_CONTENT");
    statuses.put(IS_FILE_STORAGE, "IS_FILE_STORAGE");
//    statuses.put(IS_STRUCTURAL,"IS_STRUCTURAL");
    statuses.put(IS_UPLOAD, "IS_UPLOAD");
    statuses.put(IS_URL, "IS_URL");
    return statuses;
  }

  public String getTypeName() {
    return getTypeMap().get(getType());
  }


  public boolean existsFile() {
    boolean exists = false;

    if (IS_FILE_STORAGE == getType()) {
      RemoteFile remoteFile = getRemoteFile();
      exists = remoteFile != null && remoteFile.exists();

    } else if (IS_UPLOAD == getType()) {
      if (getFile() != null) {

        try {
          getFile().getInputStream(PersistenceContext.get(Document.class));
          exists = true;
        } catch (Throwable e) {
          Tracer.platformLogger.debug("File does not exist: "+getFile().serialize(),e);
        }
      }

    } else if (IS_CONTENT == getType()) {
      exists = true;

    } else if (IS_URL == getType()) {
      exists = true;
    }


    return exists;
  }


  private RemoteFile remoteFile = null;

  public RemoteFile getRemoteFile() {

    if (remoteFile == null) {
      String docContent = getContent();
      if (JSP.ex(docContent)) {

        if (docContent.startsWith("RF")) {
          String string = docContent.substring(2);
          String[] valori = string.split(":");
          if (valori != null && valori.length > 0) {
            String id = valori[0];
            try {

              FileStorage docFS = (FileStorage) PersistenceHome.findByPrimaryKey(FileStorage.class, id);
              if (docFS != null) {
                String relativePath = valori[1];
                try {
                  remoteFile = RemoteFile.getInstance(docFS);
                  remoteFile.setTarget(relativePath); //docFS.getContent() +
                } catch (Throwable e) {
                  throw new PlatformRuntimeException(e);
                }
              }
            } catch (Throwable e) {
            }

          }
        }
      }
    }
    return remoteFile;
  }


  public JSONObject jsonify() {
    JSONObject ret = super.jsonify();
    ret.element("loadComplete", false);

    ret.element("id", getId());

    ret.element("type", getType());
    ret.element("code", getCode());
    ret.element("name", getName());
    ret.element("summa", getSumma());
    ret.element("kind", getKind());

    ret.element("content", getContent());

    /**
     * from DocumentType
     * /
     private String mimeType;
     private String versionLabel;
     private String version;
     private String author;
     private String keywords;
     private Date authored;
     private Date created;
     private long sizeInBytes;
     private boolean index;
     private String connectionHost; // connection host contains eventually the non-standard port
     private String connectionUser;
     private String connectionPwd;
     private String connectionNotes;
     private String connectionType;
     private PersistentFile persistentFile;
     */

    return ret;

  }


  public void remove(PersistenceContext pc) throws RemoveException {

    //remove files
    if (getFile()!=null)
      getFile().delete();

    super.remove(pc);
  }


}
