package org.jblooming.remoteFile;

import com.dropbox.client2.DropboxAPI;
import com.dropbox.client2.session.AccessTokenPair;
import com.dropbox.client2.session.AppKeyPair;
import com.dropbox.client2.session.Session;
import com.dropbox.client2.session.WebAuthSession;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.settings.ApplicationState;

import java.io.*;
import java.util.*;


/**
 * Created by IntelliJ IDEA.
 * User: schelazzi
 * Date: 8-ott-2010
 * Time: 16.39.20
 * To change this template use File | Settings | File Templates.
 */
public class RemoteFileDropBox extends RemoteFile {


  public static final String CALLBACK_URL = ApplicationState.serverURL + "/applications/teamwork/document/connections/dropBoxUserAuth.jsp";

  private DropboxAPI<?> sourceClient = null;
  private DropboxAPI.Entry DBJsonObjectMap = null;

  private Map configDB = new HashMap();
  private String connectionHost = "dropbox";
  private Boolean isDirectory;
  private Date lastmodified = new Date();
  private String relativePath = "";
  private long length = 0;

  public RemoteFileDropBox(Document document) {
    super(document);
    try {
      setup();
      String tokens = document.getConnectionPwd();
      String[] tokensArray = tokens.split("_");
      AccessTokenPair sourceAccess = new AccessTokenPair(tokensArray[0], tokensArray[1]);
      WebAuthSession sourceSession = new WebAuthSession(new AppKeyPair(configDB.get("consumer_key") + "", configDB.get("consumer_secret") + ""), Session.AccessType.DROPBOX, sourceAccess);

      this.sourceClient = new DropboxAPI<WebAuthSession>(sourceSession);
    } catch (Throwable e) {
    }
  }

  /**
   * this valeus have to be set in the property file of the application each app has its own consumer key and secret.
   */
  public void setup() {
    RemoteFileDropBox.generateConfigMap(this.configDB);
  }

  public static Map generateConfigMap(Map configDB) {
    configDB.put("server", ApplicationState.getApplicationSetting("DROP_BOX_SERVER"));
    configDB.put("content_server", ApplicationState.getApplicationSetting("DROP_BOX_CONTENT_SERVER"));
    configDB.put("port", new Long(ApplicationState.getApplicationSetting("DROP_BOX_PORT")));
    configDB.put("request_token_url", ApplicationState.getApplicationSetting("DROP_BOX_REQUEST_TOKEN_URL"));
    configDB.put("access_token_url", ApplicationState.getApplicationSetting("DROP_BOX_ACCESS_TOKEN_URL"));
    configDB.put("authorization_url", ApplicationState.getApplicationSetting("DROP_BOX_AUTH_URL"));
    configDB.put("consumer_key", ApplicationState.getApplicationSetting("DROP_BOX_CONSUMER_KEY"));
    configDB.put("consumer_secret", ApplicationState.getApplicationSetting("DROP_BOX_CONSUMER_SECRET"));
    configDB.put("verifier", ApplicationState.getApplicationSetting("DROP_BOX_VERIFIER"));
    return configDB;
  }


  private DropboxAPI<?> getClient() {
    if (this.sourceClient == null)
      connect();
    return this.sourceClient;
  }


  public boolean connect() {
    boolean ret = false;
    try {
      setup();
      String path = "";
      if (!JSP.ex(getRelativePath()))
        path = this.document.getContent();
      else
        path = getAbsolutePath();
      if (JSP.ex(path) && !path.startsWith(getSeparator())) {
        path = getSeparator() + path;
      }
      this.DBJsonObjectMap = this.sourceClient.metadata(path, 0, "", true, "");
      this.isDirectory = this.DBJsonObjectMap.isDir;
      if (this.DBJsonObjectMap == null) {
        ret = false;
      } else {
        ret = true;
      }

    } catch (Throwable e) {
      e.printStackTrace();
      ret = false;
    }
    return ret;
  }


  @Override
  public boolean exists() {
    if (this.sourceClient != null) {
      try {
        if (this.sourceClient.metadata(getRelativePath(), 0, "", true, "").isDir)
          return true;
        else
          return false;
      } catch (Throwable e) {
        e.printStackTrace();
        return false;
      }
    } else {
      return false;
    }
  }

  @Override
  public boolean isDirectory() {
    boolean success = true;
    if (!JSP.ex(this.isDirectory))
      success = connect();
    if (success)
      return this.isDirectory;
    else
      return true; }

  @Override
  public long lastModified() {
    return this.lastmodified.getTime();
  }

  @Override
  public long length() {
    return 0;  //To change body of implemented methods use File | Settings | File Templates.
  }

  @Override
  public boolean delete() {
    boolean success = true;
    try {
      getClient().delete(getAbsolutePath());
    } catch (Throwable e) {
      success = false;
    }
    return success;
  }

  public void setDirectory(boolean isdirectory) {
    this.isDirectory = isdirectory;
  }

  @Override
  public List<RemoteFile> listFiles() {

    List<RemoteFile> files = new ArrayList<RemoteFile>();
    List array = this.DBJsonObjectMap.contents;
    for (int i = 0; i < array.size(); i++) {
      DropboxAPI.Entry json = (DropboxAPI.Entry) array.get(i);
      RemoteFileDropBox rfv = new RemoteFileDropBox(this.document);
      rfv.setlength(json.bytes);
      rfv.DBJsonObjectMap = DBJsonObjectMap;
      rfv.configDB = configDB;
      rfv.setTarget(json.path);
      rfv.setDirectory(json.isDir);
      files.add(rfv);
    }


    return files;  //To change body of implemented methods use File | Settings | File Templates.
  }

  @Override
  public InputStream getRemoteInputStream() throws IOException {
    try {

      DropboxAPI.DropboxInputStream resp = getClient().getFileStream(getAbsolutePath(), ""); //todo relative or absolute ?
      return resp;
    } catch (Throwable e) {
      return null;
    }
  }

  public void setlength(long length) {
    this.length = length;
  }


  public void upload(File fileToUpload) throws IOException {
    if (fileToUpload != null && fileToUpload.exists()) {
      try {
        getClient().putFile(getAbsolutePath(), new FileInputStream(fileToUpload), 1000, "", null);
      } catch (Throwable e) {
        e.printStackTrace();
      }

    } else {
      throw new IOException("Invalid file.");
    }
    throw new PlatformRuntimeException("to be developed");
  }


@Override
  public boolean setTarget(String relativePath) {
    boolean success = true;
    try {

      if (JSP.ex(relativePath)) {
        if (!relativePath.startsWith(getSeparator())) {
          relativePath = getSeparator() + relativePath;
        }
        String docContent = document.getContent();
        if (!docContent.startsWith(getSeparator())) {
          docContent = getSeparator() + docContent;
        }
        if (relativePath.toLowerCase().startsWith(docContent.toLowerCase()))
          relativePath = relativePath.substring(relativePath.lastIndexOf("/"));

        this.relativePath = relativePath;
        this.DBJsonObjectMap.path = relativePath;
      }
    } catch (Throwable e) {
      success = false;
    }
    return success;
  }


  @Override
  public boolean mkdir() {
    boolean success = true;
    try {
      getClient().createFolder(this.document.getContent() + relativePath);
    } catch (Throwable e) {
      success = false;
    }
    return success;
  }

  @Override
  public boolean renameTo(RemoteFile dest) {
    return false;  //To change body of implemented methods use File | Settings | File Templates.
  }

  public boolean disconnect() {
    return false;  //To change body of implemented methods use File | Settings | File Templates.
  }


  @Override
  public boolean canWrite() {
    return true;  //To change body of implemented methods use File | Settings | File Templates.
  }


  @Override
  public String getName() {
    if (DBJsonObjectMap == null)
      connect();
    String name = relativePath.substring(relativePath.lastIndexOf("/") + 1, relativePath.length());
    return name;
  }


  @Override
  public RemoteFile getParentFile() {
    RemoteFileDropBox parent = null;
    try {
      parent = new RemoteFileDropBox(document);
      if (relativePath != null && relativePath.length() > 0)
        if (!relativePath.startsWith(document.getContent()))
          parent.setTarget(relativePath.substring(0, relativePath.lastIndexOf("/")));
        else {
          int pos = relativePath.lastIndexOf("/");
          if (pos >= 0)
            parent.setTarget(relativePath.substring(0, relativePath.lastIndexOf("/")));
          else
            parent.setTarget(relativePath.substring(0, relativePath.lastIndexOf("/")));
        }

      parent.setDirectory((Boolean) getClient().metadata("", 0, getRelativePath() + "", true, "").isDir);
    } catch (Throwable e) {
    }
    return parent;
  }

  @Override
  public String getRelativePath() {
    return relativePath;
  }

  /**
   * @return full path including document.getContent()
   */
  private String getAbsolutePath() {
    return this.document.getContent() + getRelativePath();
  }


  public String getSeparator() {
    return "/";
  }

}
