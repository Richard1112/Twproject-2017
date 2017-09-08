package org.jblooming.remoteFile;

import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;

import java.io.File;
import java.util.List;
import java.util.ArrayList;
import java.io.IOException;
import java.io.InputStream;

import net.sf.json.JSONObject;
import org.jblooming.utilities.file.FileUtilities;

import com.teamwork.expand.TaskReportBricks;

public abstract class RemoteFile {


  //informative commands
  public abstract boolean exists();

  public abstract boolean isDirectory();

  public abstract long lastModified();

  public abstract long length();

  public abstract boolean canWrite();

  public abstract String getSeparator(); // returns the directory separator for the remoteFile type


  // operative commands
  public abstract boolean delete();

  public abstract boolean mkdir();

  public abstract boolean setTarget(String relativePath); //this is a sort of cd command

  public abstract RemoteFile getParentFile();

  public abstract boolean renameTo(RemoteFile dest);

  public abstract List<RemoteFile> listFiles();


  /**
   *
   * @return the input stream to the file. Input stream MUST be closed by the caller.
   * @throws IOException
   */
  public abstract InputStream getRemoteInputStream() throws IOException;

  public abstract void upload(File fileToUpload) throws IOException;


  public Document document;
  private String relativePath="";  //starts without separator and eventually can end with (for instance for S3 directories)

  public RemoteFile(Document document) {
    this.document = document;
    //fix content root: content root MUST end with separator
    if( JSP.ex(document.getContent()) && !document.getContent().endsWith(getSeparator()))
      document.setContent(document.getContent()+getSeparator());
  }

  public static RemoteFile getInstance(Document document){
    RemoteFile rf = null;
    if (Document.ConnectionType.FS.equals(document.getConnType())) {
      rf = new RemoteFileSystem(document);
    } else if (Document.ConnectionType.FTP.equals(document.getConnType())) {
      try {
        rf = (RemoteFile) Class.forName("org.jblooming.remoteFile.RemoteFileFTP").getConstructor(Document.class).newInstance(document);
      }
      catch (Throwable e) {
      }

    } else if (Document.ConnectionType.SVN.equals(document.getConnType()) || Document.ConnectionType.SVN_Http.equals(document.getConnType()) || Document.ConnectionType.SVN_Https.equals(document.getConnType())) {
      try {
        rf = (RemoteFile) Class.forName("org.jblooming.remoteFile.RemoteFileSvn").getConstructor(Document.class).newInstance(document);
      } catch (Throwable e) {
      }
    }else if (Document.ConnectionType.DROPBOX.equals(document.getConnType())) {
      try {
        rf = (RemoteFile) Class.forName("org.jblooming.remoteFile.RemoteFileDropBox").getConstructor(Document.class).newInstance(document);
      } catch (Throwable e) {
      }
    }else if (Document.ConnectionType.S3.equals(document.getConnType())) {
      try {
        rf = (RemoteFile) Class.forName("org.jblooming.remoteFile.RemoteFileS3").getConstructor(Document.class).newInstance(document);
      } catch (Throwable e) {
      }
    }else if (Document.ConnectionType.SFTP.equals(document.getConnType())) {
      try {
        rf = (RemoteFile) Class.forName("org.jblooming.remoteFile.RemoteFileSFTP").getConstructor(Document.class).newInstance(document);
      } catch (Throwable e) {
      }
    }

    return rf;
  }


  /**
   *
    * @return  the name of the object without path and/or separatos. Returns "" in case of root. remove all separator
   */
  public String getDisplayName(){
    String s = getRelativePath();
    //if end with separator, last separator is removed
    s=s.endsWith(getSeparator())?s.substring(0,s.length()-getSeparator().length()):s;

    String[] dirs = StringUtilities.splitToArray(s, getSeparator());

    return dirs[dirs.length-1];
  }

  /**
   *
    * @return  the name of the object without path and/or separatos. Returns "" in case of root. Preserve the ending separator
   */
  public String getName(){
    String s = getRelativePath();
    //if end with separator, last separator is removed
    boolean endsWithSep=false;
    if (s.endsWith(getSeparator())){
      endsWithSep=true;
      s=s.endsWith(getSeparator())?s.substring(0,s.length()-getSeparator().length()):s;
    }

    String[] dirs = StringUtilities.splitToArray(s, getSeparator());

    s = dirs[dirs.length - 1]+(endsWithSep?getSeparator():"");
    return s;
  }

  public String getFileExtension() {
    return FileUtilities.getFileExt(getRelativePath());
  }

  /**
   *
   * @return  the relative path of parent. WARNING: do not end with separator. Separator may be added in the getParentFile() implementation.
   */
  protected String getParentRelativePath(){
    String s = getRelativePath();
    //if end with separator, last separator is removed
    s=s.endsWith(getSeparator())?s.substring(0,s.length()-2):s;
    List<String> paths = StringUtilities.splitToList(s, getSeparator());
    paths.remove(paths.size()-1);

    return StringUtilities.unSplit(paths,getSeparator());
  }

  /**
   *
   * @return  the path from document.getContent() excluded. Starting without separator. 
   */
  public String getRelativePath(){
    return relativePath;
  }

  /**
   *
   * @param relativePath  the trailing separator is always removed
   */
  protected void setRelativePath(String relativePath) {
    relativePath=relativePath.replace(getSeparator()+getSeparator(),getSeparator()); // added R 22/9/2016 perchè quando si naviga a partire da un folder si ha una doppia barra tra il folder ed il file
    this.relativePath = relativePath.startsWith(getSeparator())?relativePath.substring(getSeparator().length()):relativePath;
  }




  public boolean upload(File fileToUpload, boolean overrideIfExist) throws IOException {
    if (!overrideIfExist && exists()) {
      return false;
    } else {
      this.upload(fileToUpload);
      return true;
    }
  }

  public boolean isRoot() {
    return getRelativePath().length()==0;
  }


  public String getImageName() {
    String img = "";
    if (isDirectory())
      img = "directory.gif";
    else
      img = HttpUtilities.getContentType(getName()).replace('/', '_')+".gif";

    return img;
  }

  public List<RemoteFile> expandFileList() {
    return expandFileList(this);
  }

  private List<RemoteFile> expandFileList(RemoteFile toExpand) {

    List<RemoteFile> v = new ArrayList<RemoteFile>();
    if (toExpand.isDirectory()) {
      List<RemoteFile> content = toExpand.listFiles();
      v.addAll(content);
      for (RemoteFile rf : content) {
        v.addAll(expandFileList(rf));
      }
    }
    return v;
  }

  public JSONObject jsonify() {
    JSONObject ret = new JSONObject();
    boolean isDir = isDirectory();

    ret.element("fsId", document.getId()); //file storage id
    ret.element("name", getDisplayName());
    ret.element("isDirectory", isDir);
    ret.element("path", getRelativePath());
    ret.element("img", getImageName());
    if (!isDir){
      long bytes = length();
      ret.element("length", bytes);
      ret.element("downloadUrl", BasicDocumentBricks.getPageSeedForDownload (document, getRelativePath()).toLinkToHref());
    }
    ret.element("lastModified", lastModified());
    return ret;
  }
  public JSONObject jsonifyReport() {
	    JSONObject ret = new JSONObject();
	    boolean isDir = isDirectory();
        
	    ret.element("fsId", document.getId()); //file storage id
	    ret.element("name", getDisplayName());
	    ret.element("isDirectory", isDir);
	    ret.element("path", getRelativePath());
	    ret.element("img", getImageName());
	    if (!isDir){
	      long bytes = length();
	      ret.element("length", bytes);
	      String m=getRelativePath();
	      String user=TaskReportBricks.getDocumentUploadUser(m.substring(0,m.lastIndexOf("/")+1)+"\\"+getDisplayName());
	      
	      if(!JSP.ex(user)){
	    	  user=TaskReportBricks.getDocumentUploadHisUser(getDisplayName());
	      }
	      ret.element("uploadUser", user);
	      ret.element("downloadUrl", TaskReportBricks.getPageSeedForDownload (document, getRelativePath()).toLinkToHref());
	    }
	    
	    ret.element("lastModified", lastModified());
	    return ret;
	  }

  


}
