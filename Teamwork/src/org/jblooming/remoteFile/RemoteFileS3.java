package org.jblooming.remoteFile;

import org.jblooming.utilities.JSP;

import java.io.*;
import java.util.*;

import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class RemoteFileS3 extends RemoteFile {

  private String bucket; //document.getConnectionHost()   userId:document.getConnectionUser()  secret:document.getConnectionPwd()

  //S3 connection and object metadata are loaded lazily
  private S3Data __s3Data = null;
  private AmazonS3Client s3 = null;

  private class S3Data {
    long length = 0;
    boolean exists = false;
    Date lastModified=null;
  }




  public RemoteFileS3(Document document) {
    super(document);
    bucket = document.getConnectionHost();
  }


  private AmazonS3Client getClient() {
    if (s3 == null) {
      BasicAWSCredentials credentials = new BasicAWSCredentials(document.getConnectionUser(), document.getConnectionPwd());
      s3 = new AmazonS3Client(credentials);
    }
    return s3;
  }


  public RemoteFile getParentFile() {
    RemoteFileS3 parent = new RemoteFileS3(document);
    parent.setTarget(getParentRelativePath() + getSeparator() );  //S3 folder always end with separator
    parent.s3 = this.s3;
    return parent;
  }


  public boolean canWrite() {
    //todo implement checking grants
    //Set<Grant> grantSet = getClient().getObjectAcl(bucket, gets3Key()).getGrants();
    return true;
  }


  public boolean exists() {
    try{
    return getMeta().exists;
    }catch(Throwable e){
      return false;
    }
  }

  public boolean isDirectory() {
    return (getRelativePath().equals("")||getRelativePath().endsWith("/")) && exists();
  }

  public long lastModified() {
    return getMeta().lastModified.getTime();
  }

  public long length() {
    return getMeta().length;
  }

  public boolean delete() {
    boolean ret = false;
    try {
      List <String> keys= new ArrayList();

      //in case of directory (that not exists!) you must delete each file
      if (isDirectory()){
        ObjectListing objectListing = getClient().listObjects(new ListObjectsRequest().withBucketName(bucket).withPrefix(gets3Key()));
        for (S3ObjectSummary s: objectListing.getObjectSummaries()){
          keys.add(s.getKey());
        }
      } else {
        keys.add(gets3Key());
      }
      getClient().deleteObjects(new DeleteObjectsRequest(bucket).withKeys(keys.toArray(new String[0])));
      ret = true;
    } catch (AmazonS3Exception ae) {
    }
    return ret;
  }


  public List<RemoteFile> listFiles() {
    List<RemoteFile> rfs = new ArrayList<RemoteFile>();

    try {
      ObjectListing ol = getClient().listObjects(new ListObjectsRequest().withBucketName(bucket).withPrefix(gets3Key()).withMarker(gets3Key()).withDelimiter("/"));

      //dirs: che non esistono.... in quanto esistono solo i files
      for (String dir : ol.getCommonPrefixes()) {
        RemoteFileS3 rf = new RemoteFileS3(document);
        rf.s3 = s3;
        rf.setRelativePath(dir.substring(document.getContent().length()));// common prefix starts from root, so root is removed from relative path
        rfs.add(rf);
      }

      //files
      for (S3ObjectSummary s : ol.getObjectSummaries()) {
        RemoteFileS3 rf = new RemoteFileS3(document);
        rf.s3 = s3;
        rf.setRelativePath(s.getKey().substring(document.getContent().length())); // s3 key starts from root, so root is removed from relative path

        //fill meta immediately
        rf.__s3Data= new S3Data();
        rf.__s3Data.exists=true;
        rf.__s3Data.lastModified=s.getLastModified();
        rf.__s3Data.length=s.getSize();
        rfs.add(rf);
      }
    } catch (AmazonS3Exception ae) {
    }

    return rfs;
  }

  public boolean mkdir() {
    boolean ret = false;
    try {

      InputStream input = new ByteArrayInputStream(new byte[0]);
      ObjectMetadata metadata = new ObjectMetadata();
      metadata.setContentLength(0);
      String key = gets3Key();
      key+=(key.endsWith(getSeparator())?"":getSeparator());//in order to create a dir s3Key must end with "/"
      getClient().putObject(new PutObjectRequest(bucket, key, input, metadata));
      ret = true;
    } catch (AmazonS3Exception ae) {
    }

    return ret;
  }

  public boolean renameTo(RemoteFile dest) {
    return false; // renaming in s3 is not supported
  }

  public boolean connect() {
    boolean ret = false;
    if (document.getContent() != null) {
      ret = exists();
    }
    return ret;
  }

  public boolean clearMetadata() {
    __s3Data=null;
    return true;
  }


  /**
   *
   * @param relativePath  is always from document.getContent() on
   * @return
   */
  public boolean setTarget(String relativePath) {
    if (JSP.ex(relativePath)) {
      clearMetadata();
      setRelativePath(relativePath);
      return true;
    } else {
      return false;
    }
  }

  public InputStream getRemoteInputStream() throws IOException {
    return getClient().getObject(bucket, gets3Key()).getObjectContent();
  }


  public void upload(File fileToUpload) throws IOException {
    if (fileToUpload!= null && fileToUpload.exists()) {
      getClient().putObject(new PutObjectRequest(bucket, gets3Key(), fileToUpload));
      getClient().setObjectAcl(bucket, gets3Key(), CannedAccessControlList.PublicRead);
      clearMetadata();
    } else {
      throw new IOException("Invalid file.");
    }
  }


  public String getSeparator() {
    return "/";
  }


  private S3Data getMeta(){
    if (__s3Data==null){
      List<S3ObjectSummary> summaryList = getClient().listObjects(new ListObjectsRequest().withBucketName(bucket).withPrefix(gets3Key()).withMaxKeys(new Integer(1))).getObjectSummaries();
      __s3Data=new S3Data();
      if (summaryList.size()>0){
        S3ObjectSummary summary = summaryList.get(0);
        __s3Data.exists=true;
        __s3Data.lastModified=summary.getLastModified();
        __s3Data.length=summary.getSize();
      }
    }

    return __s3Data;
  }


  private String gets3Key(){
    String s3k = document.getContent() + getRelativePath();
    return s3k;
  }

}