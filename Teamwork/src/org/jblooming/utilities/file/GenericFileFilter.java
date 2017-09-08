package org.jblooming.utilities.file;

import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;

/**
 * Date: 4-mar-2003
 * Time: 13.15.12
 *
 * @author Pietro Polsinelli dev@open-lab.com
 */
public class GenericFileFilter implements FileFilter, FilenameFilter {
  private String pattern;
  private boolean includeDirs;

  public GenericFileFilter(String pattern) {
    this(pattern, true);
  }

  public GenericFileFilter(String pattern, boolean includeDirs) {
    this.pattern = pattern;
    this.includeDirs = includeDirs;
  }

  public boolean accept(File pathname) {
    if (pathname.isDirectory()) return includeDirs;

    String name = pathname.getName();
    if (pattern.equals("*.*")) {
      return true;

    } else if (pattern.startsWith("*.")) {
      String patternExt = pattern.substring(pattern.lastIndexOf(".") + 1);
      return name.endsWith('.' + patternExt);

    } else if (!pattern.equals("*.*") && pattern.endsWith("*.*")) {
      String patternName = pattern.substring(0, pattern.indexOf("*.*"));
      return name.startsWith(patternName);

    } else if (pattern.endsWith(".*")) {
      String patternName = pattern.substring(0, pattern.lastIndexOf("."));
      return name.startsWith(patternName + '.');

    } else if (!pattern.endsWith(".*") && !pattern.startsWith("*.")) {
      return name.equals(pattern);

    } else {
      return false;
    }
  }

  public boolean accept(File dir, String name) {
    return accept(new File(dir,name));
  }
}


