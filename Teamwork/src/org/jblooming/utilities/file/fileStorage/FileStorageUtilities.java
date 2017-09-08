package org.jblooming.utilities.file.fileStorage;

import org.jblooming.tracer.Tracer;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.system.SystemConstants;
import org.jblooming.utilities.StringUtilities;

import java.io.File;
import java.net.URLDecoder;
import java.util.Vector;
import java.util.List;

/**
 * @author Pietro Polsinelli ppolsinelli@twproject.com
 */
public class FileStorageUtilities {

  public static Vector getFileList(String[] files, boolean inclDirs) {
    Vector v = new Vector();
    if (files == null) return v;
    for (int i = 0; i < files.length; i++) {
      if (files[i] != null && files[i].trim().length() > 0)
        v.add(new File(URLDecoder.decode(files[i])));
    }
    return v;
  }

  public static Vector expandFileList(File toExpand) {
    Vector v = new Vector();
    if (toExpand.isDirectory()) {
      File listFiles[] = toExpand.listFiles();
      if (listFiles != null && listFiles.length > 0)
        for (int k = 0; k < listFiles.length; k++)
          v.addAll(expandFileList(listFiles[k]));
      else
        v.add(toExpand);
    } else
      v.add(toExpand);
    return v;
  }

  /**
   * @param files
   * @param inclDirs
   * @return
   * @deprecated It doesn't include empty directories. Use {@link #expandFileList(File) expandFileList(File)} instead.
   */
  public static Vector expandFileList(String[] files, boolean inclDirs) {
    Vector v = new Vector();
    if (files == null) return v;
    for (int i = 0; i < files.length; i++) {
      if (files[i] != null && files[i].trim().length() > 0)
        v.add(new File(URLDecoder.decode(files[i])));
    }
    for (int i = 0; i < v.size(); i++) {
      File f = (File) v.get(i);
      if (f.isDirectory()) {
        File[] fs = f.listFiles();
        for (int n = 0; n < fs.length; n++) v.add(fs[n]);
        if (!inclDirs) {
          v.remove(i);
          i--;
        }
      }
    }
    return v;
  }

  /**
   * @param files
   * @param inclDirs
   * @return
   * @deprecated It doesn't include empty directories. Use {@link #expandFileList(File) expandFileList(File)} instead.
   */
  public static Vector expandFileList(File[] files, boolean inclDirs) {
    String fileList[] = null;
    if (files != null) {
      fileList = new String[files.length];
      for (int k = 0; k < files.length; k++)
        fileList[k] = files[k].getPath();
    }
    return expandFileList(fileList, inclDirs);
  }

  public static String substr(String s, String search, String replace) {
    StringBuffer s2 = new StringBuffer();
    int i = 0, j = 0;
    int len = search.length();
    while (j > -1) {
      j = s.indexOf(search, i);
      if (j > -1) {
        s2.append(s.substring(i, j));
        s2.append(replace);
        i = j + len;
      }
    }
    s2.append(s.substring(i, s.length()));
    return s2.toString();
  }

  /**
   * Method to build an absolute path
   *
   * @param dir  the root dir
   * @param name the name of the new directory
   * @return if name is an absolute directory, returns name, else returns dir+name
   */
  public static String getDir(String dir, String name) {
    if (!dir.endsWith(File.separator)) dir = dir + File.separator;
    File mv = new File(name);
    String new_dir = null;
    if (!mv.isAbsolute()) {
      new_dir = dir + name;
    } else
      new_dir = name;
    return new_dir;
  }

  public static String convertFileSize(long size) {
    return Tracer.objectSize(size);
  }

  public static boolean validUrlToContent(String urlToContent) {
    boolean allowed = false;
    if (urlToContent!=null && urlToContent.indexOf("..") == -1) {
      String spa = ApplicationState.getApplicationSetting(SystemConstants.STORAGE_PATH_ALLOWED);
      if (spa == null || spa.trim().length() == 0) {
        Tracer.platformLogger.warn("STORAGE_PATH_ALLOWED value not found in global settings - global.properties");
      } else {
        List<String> allowedPaths = StringUtilities.splitToList(spa, ",");
        for (String s : allowedPaths) {
          //windows case insnsitivity and \ usage
          if (System.getProperty("os.name").toUpperCase().indexOf("WINDOWS") > -1) {
            if (urlToContent.toLowerCase().startsWith(s.toLowerCase())) {
              allowed = true;
              break;
            }
          } else if (urlToContent.startsWith(s)) {
            allowed = true;
            break;
          }
        }
      }
    }
    return allowed;
  }
}
