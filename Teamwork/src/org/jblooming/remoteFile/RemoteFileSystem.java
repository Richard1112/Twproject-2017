package org.jblooming.remoteFile;

import org.jblooming.ontology.PlatformComparators;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.utilities.JSP;

import java.io.*;
import java.util.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class RemoteFileSystem extends RemoteFile {

  private File file;

  public RemoteFileSystem(Document document) {
    super(document);
  }

  private File getClient() {
	 
    if (file == null)
      connect();
    return file;
  }

  
  

  public RemoteFile getParentFile() {
    RemoteFileSystem parent = new RemoteFileSystem(document);
    parent.setTarget(getParentRelativePath());
    //parent.file = getClient().getParentFile();
    return parent;
  }

  public boolean canWrite() {
    return getClient().canWrite();
  }

  public boolean exists() {
    return getClient().exists();
  }

  public boolean isDirectory() {
    return getClient().isDirectory();
  }

  public long lastModified() {
    return getClient().lastModified();
  }

  public long length() {
    return getClient().length();
  }

  public boolean delete() {
    File client = getClient();
    if (client.isDirectory())
      FileUtilities.tryHardToDeleteDir(client);
    else
      FileUtilities.tryHardToDeleteFile(client);
    return true;
  }

  public List<String> list() {
    return Arrays.asList(getClient().list());
  }

  public List<RemoteFile> listFiles() {
    File[] fls = getClient().listFiles(new FileFilter() {
      @Override
      public boolean accept(File file) {
        return !file.isHidden();
      }
    });
    List<File> files = new ArrayList();
    Collections.addAll(files, fls);
    Collections.sort(files, new PlatformComparators.FileNameComparator());
    List<RemoteFile> rf = new ArrayList<RemoteFile>();
    for (File fl : files) {
      //try {
        RemoteFileSystem rfs = new RemoteFileSystem(document);
        rfs.file = fl;
        //rfs.setRelativePath(fl.getCanonicalPath().substring(document.getContent().length()));
        rfs.setRelativePath(getRelativePath()+getSeparator()+fl.getName());
        rf.add(rfs);
      //} catch (IOException e) {  }
    }
    return rf;
  }

  public boolean mkdir() {
    return getClient().mkdirs();
  }

  public boolean renameTo(RemoteFile dest) {
    RemoteFileSystem fileSystem = (RemoteFileSystem) dest;
    boolean ret = false;
    try {
      //check same root constraint
      //if (fileSystem.getClient().getCanonicalPath().toLowerCase().startsWith(document.getContent().toLowerCase())) {
      if (fileSystem.getClient().getCanonicalPath().toLowerCase().replace(File.separator, "/").startsWith(document.getContent().toLowerCase().replace(File.separator, "/"))) {
        getClient().renameTo(fileSystem.getClient());
        ret = true;
      }
    } catch (IOException o) {
    }
    return ret;
  }

  public boolean connect() {
    file = new File(getAbsolutePath());
    return file.exists();
  }

  public boolean disconnect() {
    file = null;
    return true;
  }

  public boolean setTarget(String relativePath) {
    boolean ret = false;
    if (JSP.ex(relativePath)) {
      //here using ../../ you may try to escape from root so we have to test

//      try {
//        String newPath = document.getContent() + relativePath;
//        File newfile = new File(newPath);
//        if (newPath.toLowerCase().replace(File.separator, "/").startsWith(document.getContent().toLowerCase().replace(File.separator, "/"))){
//          ret= true;
//          file=newfile;
//          setRelativePath(relativePath);
//        }
//      } catch (Throwable e) {
//      }

      try {
        File newfile = new File(document.getContent() + relativePath);
        if (newfile.getCanonicalPath().toLowerCase().replace(File.separator, "/").startsWith(document.getContent().toLowerCase().replace(File.separator, "/"))){
          ret= true;
          file=newfile;
          setRelativePath(relativePath);
        }
      } catch (IOException e) {
      }
    }
    return ret;
  }

  public InputStream getRemoteInputStream() throws IOException {
    return new FileInputStream(getClient());
  }


  public void upload(File fileToUpload) throws IOException {
    if (fileToUpload != null && fileToUpload.exists()) {
      FileInputStream inputStream = new FileInputStream(fileToUpload);
      FileUtilities.copy(inputStream, new FileOutputStream(getClient()));
    } else {
      throw new IOException("Invalid file.");
    }

  }


  public String getSeparator() {
    //return File.separator;
    return "/";
  }


  private String getAbsolutePath(){
    return document.getContent()+getRelativePath();
  }

}
