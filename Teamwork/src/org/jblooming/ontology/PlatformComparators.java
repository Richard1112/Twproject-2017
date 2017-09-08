package org.jblooming.ontology;

import org.jblooming.utilities.JSP;
import org.jblooming.utilities.NumberUtilities;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.ClientEntries;

import java.io.File;
import java.util.Comparator;

/**
 * Created by rbicchierai on 27/05/2015.
 */
public class PlatformComparators {




  // ---------------------- version comparator by number, roman or string
  public static class VersionComparator implements Comparator<String> {
    public int compare(String s1, String s2) {
      if (s1 == null || s2 == null)
        return 0;

      // numeric case
      try {
        return new Integer(Integer.parseInt(s1)).compareTo(new Integer(Integer.parseInt(s2)));
      } catch (NumberFormatException e) {

        // roman case
        try {
          return new Integer(NumberUtilities.romanToInt(s1)).compareTo(new Integer(NumberUtilities.romanToInt(s2)));
        } catch (NumberFormatException e1) {

          // alpha case
          return s1.compareTo(s2);
        }
      }
    }
  }

  //------------------------  LoggableIdentifiable by creationdate
  public static class CreationDateComparator implements Comparator<LoggableIdentifiable> {
    public int compare(LoggableIdentifiable o1, LoggableIdentifiable o2) {

      int ret = 1;
      if (o1.getCreationDate() == null)
        ret = 1;
      else
        ret = o1.getCreationDate().compareTo(o2.getCreationDate());

      ret = ret == 0 ? o1.getName().compareTo(o2.getName()) : ret;
      return ret;
    }
  }

  //------------------------ Identifiable by name
  public static class NameComparator implements Comparator<Identifiable> {
    public int compare(Identifiable o1, Identifiable o2) {
      if (o1==null || o2==null)
        return 0;

      return (JSP.w(o1.getName())+o1.getId()).compareTo(JSP.w(o2.getName())+o2.getId());
    }
  }

  //------------------------ Identifiable by displayName name
  public static class DisplayNameComparator implements Comparator<Identifiable> {
    public int compare(Identifiable o1, Identifiable o2) {
      return o1.getDisplayName().compareTo(o2.getDisplayName());
    }
  }

  //----------------------- Number in reverse order
  public static class InverseNumberComparator implements Comparator<Number> {
    public int compare(Number p1, Number p2) {
      return (int) Math.signum(p2.doubleValue() - p1.doubleValue());
    }
  }


  //--------------------- File by name
  public static class FileNameComparator implements Comparator<File> {
    public int compare(File f1, File f2) {
      return f1.getName().compareToIgnoreCase(f2.getName());
    }
  }


  public static class IgnoreCaseComparator implements Comparator<String> {
    public int compare(String o1, String o2) {
      return o1.compareToIgnoreCase(o2);
    }
  }



  //---------------------- sort client entries by value give the ce.name
  public static class ClientEntryComparator implements Comparator {
    ClientEntries ces;

    public ClientEntryComparator(ClientEntries ces) {
      super();
      this.ces = ces;
    }

    public int compare(Object a, Object b) {
      String aS = (String) a;
      String bS = (String) b;
      return JSP.w(ces.getEntry(aS).stringValueNullIfEmpty()).compareToIgnoreCase(JSP.w(ces.getEntry(bS).stringValueNullIfEmpty()));
    }
  }
}
