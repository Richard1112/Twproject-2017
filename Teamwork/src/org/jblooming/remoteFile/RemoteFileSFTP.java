package org.jblooming.remoteFile;

import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpATTRS;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.HashTable;
import org.jblooming.waf.SessionState;

import java.io.*;
import java.util.*;

/**
 *  The remote location is always on the root folder
 */
public class RemoteFileSFTP extends RemoteFile {

  private static Map<String, ChannelSftp> connectionsPool= new HashTable();



  private String rootFolder; //=document.getContent() -> ends with /

  private SFTPData __metaData = null;
  ChannelSftp __sftpClient =null;

  private class SFTPData {


    boolean isDirectory =false;
    long length = 0;
    boolean exists = false;
    long lastModified=0;
    //List<RemoteFileFTP> files=null;
    public boolean canWrite;

    SFTPData (){}

    SFTPData (SftpATTRS attrs){
      isDirectory = attrs.isDir();
      exists= true;
      lastModified=  attrs.getMTime()*1000 ; //is in seconds
      length= attrs.getSize();
      canWrite=true; //todo attrs.getPermissions()
    }

  }


  public RemoteFileSFTP(Document document) {
    super(document);
    rootFolder=document.getContent();
  }


  private SFTPData getMetadata() {
    if (__metaData == null) {
      __metaData=new SFTPData();
      try {
        if (document.getConnectionHost() != null) {
          ChannelSftp sftpClient = getFTPClient();

          SftpATTRS attrs = sftpClient.lstat(getAbsolutePath());

          //caching data
          SFTPData metaData = new SFTPData(attrs);

          __metaData=metaData;
        }

      } catch (Exception e) {
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
  private ChannelSftp getFTPClient() {
    ChannelSftp sftpClient = __sftpClient;

    if (sftpClient == null || !sftpClient.isConnected()) {

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
      if (ss != null){
        sessionId = ss.sessionId;
      }

      //if there is a session try to use pooling
      if (sessionId != null) {
        sftpClient = connectionsPool.get(sessionId);
        if (sftpClient!=null && !sftpClient.isConnected()) {
          sftpClient = null;
          connectionsPool.remove(sessionId);
        }
      }

      // if not in pool create a new connection
      if (sftpClient == null) {
        try {
          if (document.getConnectionHost() != null) {

            JSch jsch = new JSch();
            sftpClient = null;
            Session ses= jsch.getSession(document.getConnectionUser(), document.getConnectionHost(), 22);
            ses.setConfig("StrictHostKeyChecking", "no");
            ses.setPassword(document.getConnectionPwd());
            ses.connect();

            sftpClient = (ChannelSftp) ses.openChannel("sftp");
            sftpClient.connect();
            //sftpClient.cd(rootFolder);
            //sftpClient.setFileTransferMode(FTPClient.BINARY_FILE_TYPE);
            //sftpClient.setFileType(FTPClient.BINARY_FILE_TYPE);

          }

        } catch (Throwable e) {
          invalidateClient();
          Tracer.platformLogger.error("Cannot connect", e);
        }
      }


      //if session available put client on pool
      if (sessionId != null && sftpClient!=null) {
        connectionsPool.put(sessionId, sftpClient);
      }

    }
    __sftpClient =sftpClient;

    return __sftpClient;
  }

  private void invalidateClient() {
    __sftpClient =null;

    //try to recover the current session
    String sessionId = null;
    SessionState ss = SessionState.getCurrentSessionState();
    if (ss != null){
      sessionId = ss.sessionId;
    }

    //if there is a session try to use pooling
    if (sessionId != null) {
      connectionsPool.remove(sessionId);
    }
  }



  public RemoteFile getParentFile() {
    RemoteFileSFTP parent = null;
    parent = new RemoteFileSFTP(document);
    parent.setTarget(getParentRelativePath());
    return parent;
  }

  public boolean canWrite() {
    return getMetadata().canWrite;
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
      ChannelSftp client = getFTPClient();
      if (isDirectory())
        client.rmdir(getAbsolutePath());
      else
        client.rm(getAbsolutePath());
      //client.disconnect();
      ret = true;
    } catch (Exception e) {
      Tracer.platformLogger.error("Cannot delete " + getDisplayName(), e);
      invalidateClient();
    }
    return ret;

  }

  public List<RemoteFile> listFiles() {
    List<RemoteFile> rf = new ArrayList<RemoteFile>();

    Vector<ChannelSftp.LsEntry> files;
    try {
      files = getFTPClient().ls(getAbsolutePath());
      for (ChannelSftp.LsEntry ftpf : files) {
        //must skip "." ".." in order to avoid escaping from jail
        if (".".equals(ftpf.getFilename()) || "..".equals(ftpf.getFilename()) )
          continue;

        SftpATTRS attrs = ftpf.getAttrs();

        //must skip link in order to avoid escaping from jail
        if (".".equals(ftpf.getFilename()))

          if (attrs.isLink())
          continue;

        RemoteFileSFTP rfs = new RemoteFileSFTP(document);
        rfs.setTarget(getRelativePath() + getSeparator() + ftpf.getFilename());

        //create metadata for listed file
        SFTPData md = new SFTPData(attrs);
        rfs.__metaData = md;
        rf.add(rfs);
      }
    } catch (Exception e) {
      invalidateClient();
    }
    return rf;
  }

  public boolean mkdir() {
    boolean ret = false;
    try {
      ChannelSftp client = getFTPClient();
      client.mkdir(getAbsolutePath());
      //client.disconnect();
      ret = true;
    } catch (Exception e) {
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
    try {
      ChannelSftp client = getFTPClient();
      client.get(getAbsolutePath(), fos);
    }catch (Exception e){
      Tracer.platformLogger.error("Cannot get the file " + getAbsolutePath(), e);
    }
    fos.close();
    return new FileInputStream(tempFile);
    
  }


  public void upload(File fileToUpload) throws IOException {
    if (fileToUpload != null && fileToUpload.exists()) {
      ChannelSftp client = getFTPClient();
      FileInputStream fis = new FileInputStream(fileToUpload);
      try {
        client.put(fis, getAbsolutePath());
      } catch (Exception e) {
        Tracer.platformLogger.error("Cannot upload:  " + getAbsolutePath(), e);
      }
      fis.close();
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
