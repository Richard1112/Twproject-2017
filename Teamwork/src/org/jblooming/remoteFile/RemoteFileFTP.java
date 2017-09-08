package org.jblooming.remoteFile;

import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPFile;
import org.jblooming.tracer.Tracer;
import org.jblooming.waf.SessionState;
import org.jblooming.utilities.HashTable;

import java.io.*;
import java.util.*;

/**
 *  The remote location is always on the root folder
 */
public class RemoteFileFTP extends RemoteFile {

  private static Map<String, FTPClient> connectionsPool= new HashTable();



  private String rootFolder; //=document.getContent() -> ends with /

  private FTPData __metaData = null;
  FTPClient __ftpClient=null;

  private class FTPData {
    boolean isDirectory =false;
    long length = 0;
    boolean exists = false;
    long lastModified=0;
    //List<RemoteFileFTP> files=null;
  }


  public RemoteFileFTP(Document document) {
    super(document);
    rootFolder=document.getContent();
  }




  private FTPData getMetadata() {
    if (__metaData == null) {
      __metaData= new FTPData();
      try {
        if (document.getConnectionHost() != null) {
          FTPClient ftpClient = getFTPClient();

          //is a directory?
          __metaData.isDirectory = ftpClient.changeWorkingDirectory(getAbsolutePath());
          ftpClient.changeWorkingDirectory(rootFolder); //reset the root location


          if (__metaData.isDirectory){  //is a directory
            __metaData.exists= true;
            __metaData.lastModified=  0 ;
            __metaData.length= 0;

          } else { //is a file
            FTPFile[] files = ftpClient.listFiles(getAbsolutePath());
            if (files.length>=1){
              __metaData.exists=true;
              __metaData.isDirectory=files[0].isDirectory();
              __metaData.lastModified=files[0].getTimestamp().getTimeInMillis();
              __metaData.length=files[0].getSize();
            }
          }
        }

      } catch (Throwable e) {
        invalidateClient();
        Tracer.platformLogger.error("Cannot connect", e);
      }
    }
    return __metaData;
  }


  /**
   *  try to recover ftpCVlient attached to a http session if any
   * @return
   */
  private FTPClient getFTPClient() {
    FTPClient ftpClient = __ftpClient;

    if (ftpClient == null || !ftpClient.isConnected()) {


      //pool cleanUp: remove closed connections
      if (connectionsPool.size() > 0) {
        Set<String> keys = new HashSet();
        keys.addAll(connectionsPool.keySet());

        for (String k : keys) {
          if (!connectionsPool.get(k).isConnected())
            connectionsPool.remove(k);
        }
      }


      //try to recover the current session
      String sessionId = null;
      SessionState ss = SessionState.getCurrentSessionState();
      if (ss != null)
        sessionId = ss.sessionId;


      //if there is a session try to use pooling
      if (sessionId != null) {
        ftpClient = connectionsPool.get(sessionId);
        if (ftpClient!=null && !ftpClient.isConnected()) {
          ftpClient = null;
          connectionsPool.remove(sessionId);
        }
      }

      // if not in pool create a new connection
      if (ftpClient == null) {
        try {
          if (document.getConnectionHost() != null) {
            ftpClient= new FTPClient();
            ftpClient.connect(document.getConnectionHost());
            ftpClient.user(document.getConnectionUser());
            ftpClient.pass(document.getConnectionPwd());
            ftpClient.setFileTransferMode(FTPClient.BINARY_FILE_TYPE);
            ftpClient.setFileType(FTPClient.BINARY_FILE_TYPE);
            ftpClient.changeWorkingDirectory(rootFolder); // set current forlder as root
          }

        } catch (Throwable e) {
          invalidateClient();
          Tracer.platformLogger.error("Cannot connect", e);
        }
      }

      __ftpClient=ftpClient;

      //if session available put client on pool
      if (sessionId != null) {
        connectionsPool.put(sessionId, ftpClient);
      }

    }

    return __ftpClient;
  }

  private void invalidateClient() {
    __ftpClient=null;

    //try to recover the current session
    String sessionId = null;
    SessionState ss = SessionState.getCurrentSessionState();
    if (ss != null)
      sessionId = ss.sessionId;

    //if there is a session try to use pooling
    if (sessionId != null) {
      connectionsPool.remove(sessionId);
    }
  }



  public RemoteFile getParentFile() {
    RemoteFileFTP parent = null;
    parent = new RemoteFileFTP(document);
    parent.setTarget(getParentRelativePath());
    return parent;
  }

  public boolean canWrite() {
    return true;  //Todo implementation is not complete
  }

  public boolean exists() {
    return getMetadata().exists;
  }

  public boolean isDirectory() {
    return getMetadata().isDirectory;
  }

  public long lastModified() {
    return getMetadata().lastModified;
  }

  public long length() {
    return getMetadata().length;
  }

  public boolean delete() {
    boolean ret = false;
    try {
      FTPClient client = getFTPClient();
      if (isDirectory())
        client.removeDirectory(getAbsolutePath());
      else
        client.deleteFile(getAbsolutePath());
      //client.disconnect();
      ret = true;
    } catch (Throwable e) {
      Tracer.platformLogger.error("Cannot delete " + getDisplayName(), e);
      invalidateClient();
    }
    return ret;

  }

  public List<RemoteFile> listFiles() {
    List<RemoteFile> rf = new ArrayList<RemoteFile>();

    FTPFile[] files = new FTPFile[0];
    try {
      files = getFTPClient().listFiles(getAbsolutePath());
    for (FTPFile ftpf: files){

      RemoteFileFTP rfs = new RemoteFileFTP(document);
      rfs.setTarget(getRelativePath() + getSeparator() + ftpf.getName());

      //create metadata for listed file
      FTPData md= new FTPData();
      md.exists=true;
      md.isDirectory=ftpf.isDirectory();
      md.lastModified=ftpf.getTimestamp().getTimeInMillis();
      md.length=ftpf.getSize();
      rfs.__metaData=md;



      rf.add(rfs);
    }
    } catch (Throwable e) {
      invalidateClient();
    }
    return rf;
  }

  public boolean mkdir() {
    boolean ret = false;
    try {
      FTPClient client = getFTPClient();
      client.makeDirectory(getAbsolutePath());
      //client.disconnect();
      ret = true;
    } catch (IOException e) {
      Tracer.platformLogger.error("Cannot create directory " + getRelativePath(), e);
    }
    return ret;
  }

  public boolean renameTo(RemoteFile dest) {
    return false;  //To change body of implemented methods use File | Settings | File Templates.
  }


  public boolean setTarget(String relativePath) {
    setRelativePath(relativePath);
    return true;
  }

  public InputStream getRemoteInputStream() throws IOException {

    File tempFile = File.createTempFile("ftp_transfert_TEMP_FILE", ".tmp");
    tempFile.deleteOnExit();
    FileOutputStream fos = new FileOutputStream(tempFile);

    FTPClient cli = getFTPClient();
    cli.retrieveFile(getAbsolutePath(),fos);
    fos.close();

    return new FileInputStream(tempFile);                
    
  }

  public void upload(File fileToUpload) throws IOException {
    if (fileToUpload != null && fileToUpload.exists()) {
      FTPClient client = getFTPClient();
      InputStream inputStream = new FileInputStream(fileToUpload);
      client.storeFile(getAbsolutePath(), inputStream);
      inputStream.close();
      //client.disconnect();
    } else {
      throw new IOException("Invalid file.");
    }

  }

  public String getSeparator() {
    return "/";
  }


  /**
   *
   * @return full path including rootFolder
   */
  private String getAbsolutePath(){
    return rootFolder+getRelativePath();
  }

}
