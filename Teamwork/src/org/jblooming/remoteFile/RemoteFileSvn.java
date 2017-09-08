package org.jblooming.remoteFile;

import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.tmatesoft.svn.core.*;
import org.tmatesoft.svn.core.auth.ISVNAuthenticationManager;
import org.tmatesoft.svn.core.internal.io.dav.DAVRepositoryFactory;
import org.tmatesoft.svn.core.internal.io.svn.SVNRepositoryFactoryImpl;
import org.tmatesoft.svn.core.io.ISVNEditor;
import org.tmatesoft.svn.core.io.SVNRepository;
import org.tmatesoft.svn.core.io.SVNRepositoryFactory;
import org.tmatesoft.svn.core.io.diff.SVNDeltaGenerator;
import org.tmatesoft.svn.core.wc.SVNClientManager;
import org.tmatesoft.svn.core.wc.SVNCommitClient;
import org.tmatesoft.svn.core.wc.SVNWCUtil;

import java.io.*;
import java.util.*;


public class RemoteFileSvn extends RemoteFile {

  SVNRepository repository;
  private Boolean isdirectory = null;
  private long length = 0;
  private String host = null;
  private int port = 0;
  private String contentRoot = null;
  private String name = null;
  private String SVN = "svn://";
  private String HTTP = "http://";
  private String HTTPS = "https://";

  public RemoteFileSvn(Document document) {
    super(document);
  }

  public boolean connect() {

    if (Document.enabledConnectionTypes.contains(Document.ConnectionType.SVN)) {
      if (repository == null) {
        try {
          host = document.getConnectionHost();
          contentRoot = document.getContent();
          name = document.getConnectionUser();
          String password=document.getConnectionPwd();

          if (name == null || name.equals("")) {
            name = "anonymous";
            password = "anonymous";
          }
          String url = "";
          SVNURL surl = null;
          /* if you try to connect to you svn server on httpor https protocol*/
          if (Document.ConnectionType.SVN_Http.equals(document.getConnType())) {
            if (JSP.ex(host) && host.indexOf("http") == -1)
            host = HTTP + host;
             checkUrl();
            surl = SVNURL.parseURIDecoded( host + contentRoot);
            DAVRepositoryFactory.setup();
            repository = DAVRepositoryFactory.create(surl, null);
          } else if (Document.ConnectionType.SVN_Https.equals(document.getConnType())) { if (JSP.ex(host) && host.indexOf("https") == -1)
            host = HTTPS + host;
             checkUrl();
            surl = SVNURL.parseURIDecoded(host + contentRoot);
            DAVRepositoryFactory.setup();
            repository = DAVRepositoryFactory.create(surl, null);
          } else  if (Document.ConnectionType.SVN.equals(document.getConnType())){
            if (JSP.ex(host) && host.indexOf("svn") == -1)
              host = SVN + host;
            checkUrl();
            url = host + contentRoot;
            SVNRepositoryFactoryImpl.setup();
            surl = SVNURL.parseURIDecoded(url);
            repository = SVNRepositoryFactory.create(surl, null);
          }
          ISVNAuthenticationManager authManager = SVNWCUtil.createDefaultAuthenticationManager(name, password);
          repository.setAuthenticationManager(authManager);
          return true;
        }
        catch (SVNException e) {
          Tracer.platformLogger.error("SVNException ", e);
          return false;
        }
      } else {
        return true;
      }
    } else {
      return false;
    }
  }



  public boolean setTarget(String relativePath) {
    isdirectory = null;
    if (JSP.ex(relativePath)) {
      setRelativePath(relativePath);
      return true;
    } else
      return false;
  }



  private void readAllowed() {
    boolean canRead=false;try {
      Collection entries = repository.getDir(getRelativePath(), -1, null, (Collection) null);
      canRead = true;

    } catch (SVNException e) {
      canRead = false;
    }
  }

  private void writeAllowed() {
    boolean canWrite=false;try {
      String logMessage = "";
      ISVNEditor editor = repository.getCommitEditor(logMessage, null, true, null);
      canWrite = true;

    } catch (SVNException e) {
      canWrite = false;
    }
  }


  public RemoteFile getParentFile() {
    RemoteFileSvn parent;
    parent = new RemoteFileSvn(document);
    parent.setTarget(getParentRelativePath());
    return parent;
  }


  public List<RemoteFile> listFiles() {

    List<RemoteFile> rfs = new ArrayList<RemoteFile>();
    try {
      connect();
      List<SVNDirEntry> entries = (List)repository.getDir(getRelativePath(), -1, null, (Collection) null);
      Collections.sort(entries, new FileNameComparator());
      for (SVNDirEntry entry : entries) {
        RemoteFileSvn rfv = new RemoteFileSvn(document);
        if (entry.getKind() == SVNNodeKind.DIR) {
          rfv.setDirectory(true);
        } else {
          rfv.setDirectory(false);
          rfv.setlength(entry.getSize());
        }

        //todo controllare se le entries hanno il path dalla root e solo dalla cartella corrente
        rfv.setTarget(getRelativePath()+getSeparator()+ entry.getName());
        rfs.add(rfv);
      }
      repository.closeSession();
    }
    catch (SVNException e) {
      repository.closeSession();
      Tracer.platformLogger.error("SVNException ", e);
    }
    return rfs;
  }

  public InputStream getRemoteInputStream() throws IOException {
    connect();
    SVNProperties fileProperties = new SVNProperties();   // modified by Silvia chelazzi,19/01/09, because of the new svnkit release
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    try {
      repository.getFile(getRelativePath(), -1, fileProperties, out);
      repository.closeSession();
      return new ByteArrayInputStream(out.toByteArray());
    }
    catch (SVNException e) {
      repository.closeSession();
      return null;
    }
  }


  public boolean delete() {
    connect();
    try {
      String logMessage = "deleting a file or a directory - utente:" + this.name;
      //relativePath = correctPath(relativePath);
      ISVNEditor editor = repository.getCommitEditor(logMessage, null, true, null);
      editor.openRoot(-1);
      editor.deleteEntry(getRelativePath(), -1);
      editor.closeDir();
      editor.closeEdit();
      repository.closeSession();
      return true;
    }
    catch (SVNException svne) {
      Tracer.platformLogger.error("SVNException ", svne);
      repository.closeSession();
      return false;
    }
  }

  public boolean mkdir() {
    connect();
    //relativePath = correctPath(relativePath);
    String logMessage = "Create a new directory - utente:" + this.name;
    try {
      ISVNEditor editor = repository.getCommitEditor(logMessage, null, true, null);
      editor.openRoot(-1);
      editor.addDir(getRelativePath(), null, -1);
      editor.closeDir();
      editor.closeEdit();
      repository.closeSession();
      return true;
    }
    catch (SVNException e) {
      Tracer.platformLogger.error("SVNException ", e);
      repository.closeSession();
      return false;
    }
  }

  public void upload(File fileToUpload) throws IOException {
    if (fileToUpload != null && fileToUpload.exists()) {

      connect();
      SVNClientManager manager = SVNClientManager.newInstance(null);
      SVNCommitClient committer = manager.getCommitClient();
      try {
        committer.doImport(fileToUpload, SVNURL.parseURIEncoded(host + getAbsolutePath()), "importing a file - utente:" + this.name, false);
        repository.closeSession();
      } catch (SVNException e) {
        try {
          SVNProperties fileProperties = new SVNProperties();
          //Map fileProperties = new HashMap();
          ByteArrayOutputStream out1 = new ByteArrayOutputStream();
          repository.getFile(getRelativePath(), -1, fileProperties, out1);
          FileInputStream input = new FileInputStream(fileToUpload);
          int byteCount = input.available();
          byte[] newdata = new byte[byteCount];
          input.read(newdata);
          String logMessage = "updating a  file - utente:" + this.name;
          ISVNEditor editor = repository.getCommitEditor(logMessage, null, true, null);
          modifyFile(editor, getRelativePath(), getRelativePath(), out1.toByteArray(), newdata);
          repository.closeSession();
        } catch (SVNException ex) {
          repository.closeSession();
        }
      }
    } else {
      throw new IOException("Invalid file.");
    }

  }

  private void modifyFile(ISVNEditor editor, String dirPath, String filePath, byte[] oldData, byte[] newData) throws SVNException {
    editor.openRoot(-1);
    editor.openDir(dirPath, -1);

    editor.openFile(filePath, -1);

    editor.applyTextDelta(filePath, null);

    SVNDeltaGenerator deltaGenerator = new SVNDeltaGenerator();
    String checksum = deltaGenerator.sendDelta(filePath, new ByteArrayInputStream(oldData), 0, new ByteArrayInputStream(newData), editor, true);

    //Closes filePath.
    editor.closeFile(filePath, checksum);

    // Closes dirPath.
    editor.closeDir();

    //Closes the root directory.
    editor.closeDir();

    editor.closeEdit();
  }

  public void checkUrl() {
    if (host != null && host.equals("")) {
      if (host.endsWith("/")) {
        host = host.substring(0, host.length() - 1);
        document.setConnectionHost(host);
      }
    }
    if (contentRoot != null && !contentRoot.equals("")) {
      if (!contentRoot.startsWith("/")) {
        contentRoot = "/" + contentRoot;
        document.setContent(contentRoot);
      }
    }
  }

  public void setlength(long length) {
    this.length = length;
  }

  public String correctPath(String path) {
    if (path != null && !path.equals(""))
      path = path.replace("\\", "/");
    return path;
  }

  public List<SVNLog> getLog(Date startDate, Date endDate) throws SVNException {
    Set<String> id = null;
    return getLog(startDate, endDate, id);
  }


  public List<SVNLog> getLog(Date startDate, Date endDate, Set<String> logId) throws SVNException {
    connect();
    List<SVNLog> logList = new ArrayList();
    //  try {
    Collection logEntries = null;
    long startRevision = 0;
    long endRevision = -1;
    if (repository != null) {
      logEntries = repository.log(new String[]{""}, null, startRevision, endRevision, true, true);
      for (Iterator entries = logEntries.iterator(); entries.hasNext();) {
        SVNLogEntry logEntry = (SVNLogEntry) entries.next();
        boolean startedAfter = startDate == null || logEntry.getDate() == null || logEntry.getDate().after(startDate);
        boolean endedbefore = endDate == null || logEntry.getDate() == null || logEntry.getDate().before(endDate);

        if (logId == null) {
          if (logEntry.getAuthor()
              != null && logEntry.getAuthor().equals(name) && startedAfter && endedbefore) {
            SVNLog svnLog = this.getSingleLog(logEntry);
            logList.add(svnLog);
          }
        } else {
          for (String i : logId) {
            int c = -1;
            try {
              c = Integer.parseInt(i);
            } catch (NumberFormatException e) {
            }
            boolean isRigthLog = logEntry.getRevision() == c;
            if (logEntry.getAuthor() != null && logEntry.getAuthor().equals(name) && startedAfter && endedbefore && isRigthLog) {
              SVNLog svnLog = this.getSingleLog(logEntry);
              logList.add(svnLog);
            }

          }
        }
      }
    }
    return logList;

  }


  public SVNLog getSingleLog(SVNLogEntry logEntry) {
    String result = "";// = ("<br><b>log message:</b> " + logEntry.getMessage() + "<br>");
    SVNLog svnLog = null;
    Map changedPaths1 = logEntry.getChangedPaths();
    if (changedPaths1.size() > 0) {
      //result += ("<b>changed paths:</b>");
      Set<String> changedPathsSet = changedPaths1.keySet();
      for (Iterator changedPaths = changedPathsSet.iterator(); changedPaths.hasNext();) {
        SVNLogEntryPath entryPath = (SVNLogEntryPath) changedPaths1.get(changedPaths.next());

        boolean pathAllowed = entryPath.getPath().length() > getRelativePath().length() ? entryPath.getPath().substring(0, getRelativePath().length()).equals(getRelativePath()) : entryPath.getPath().equals(getRelativePath());
        if (pathAllowed) {
          result += (entryPath.getPath()
              + ((entryPath.getCopyPath() != null) ? " (from " + entryPath.getCopyPath() + " revision " + entryPath.getCopyRevision() + ")" : ""))
              + " (" + entryPath.getType() + ");";

        }
      }
      svnLog = new SVNLog();
      svnLog.setLog(result);
      svnLog.setLogMessage(logEntry.getMessage());
      svnLog.setDate(logEntry.getDate());
      svnLog.setLogId(logEntry.getRevision());
    }
    return svnLog;
  }

  public boolean renameTo(RemoteFile dest) {
    return false;
  }


  private void setDirectory(boolean isdirectory) {
    this.isdirectory = isdirectory;
  }


  public boolean isDirectory() {
    if (isdirectory == null) {
      try {
        connect();
        SVNNodeKind nodeKind = repository.checkPath(getRelativePath(), -1);
        if (SVNNodeKind.DIR.equals(nodeKind)) {
          this.isdirectory = true;
        } else if (SVNNodeKind.FILE.equals(nodeKind)) {
          this.isdirectory = false;
        } else {
          repository.closeSession();
          return false;
        }
        repository.closeSession();
        return this.isdirectory;
      }
      catch (SVNException e) {
        repository.closeSession();
        this.isdirectory = false;
      }
    }
    return isdirectory.booleanValue();
  }

  public boolean canWrite() {
    return true;
  }

  //todo implement this
  public long lastModified() {
    return 0;
  }

  public long length() {
    return this.length;
  }

  public boolean disconnect() {
    closeSession();
    return true;
  }

  public boolean exists() {
    boolean connected = connect();
    if (connected) {

      SVNNodeKind nodeKind = null;
      try {

        nodeKind = repository.checkPath(getRelativePath(), -1);

      } catch (SVNException e) {
        repository.closeSession();
        return false;
      }
      boolean exist = !SVNNodeKind.NONE.equals(nodeKind);
      repository.closeSession();
      return exist;
    } else {
      return false;
    }

  }

  public void closeSession() {
    if (repository != null)
      repository.closeSession();
  }

  private static class FileNameComparator implements Comparator {
     public int compare(Object f1, Object f2) {
      return ((SVNDirEntry)f1).getName().compareToIgnoreCase(((SVNDirEntry)f2).getName());
    }
  }

  public String getSeparator() {
    return "/";
  }


  public String getAbsolutePath(){
    return document.getContent()+ getRelativePath();
  }

}
