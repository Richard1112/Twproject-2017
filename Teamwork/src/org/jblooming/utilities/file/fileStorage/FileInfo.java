package org.jblooming.utilities.file.fileStorage;

import java.io.File;

/**
 * @author Pietro Polsinelli ppolsinelli@twproject.com
 */
public class FileInfo {

  public String name = null,
  clientFileName = null,
  fileContentType = null;
  public File file = null;
  public StringBuffer sb = new StringBuffer(100);

  public void setFileContents(byte[] aByteArray) {
    byte[] fileContents=new byte[aByteArray.length];
    System.arraycopy(aByteArray, 0, fileContents, 0, aByteArray.length);
  }

}
