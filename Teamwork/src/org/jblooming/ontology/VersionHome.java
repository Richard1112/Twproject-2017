package org.jblooming.ontology;

import org.jblooming.utilities.JSP;
import org.jblooming.utilities.NumberUtilities;

import java.util.Collection;

/**
 * @author pietro polsinelli ppolsinelli@twproject.com
 */
public class VersionHome {

  public static String VERSION_ROOT = "01";

  public static String nextVersion(String currentVersion) {
    String nextVersion = VERSION_ROOT;

    if (JSP.ex(currentVersion)) {
      try {
        int current = (Integer.parseInt(currentVersion));
        nextVersion = increase(current, nextVersion);
      } catch (NumberFormatException e) {
        try {
          nextVersion =
            NumberUtilities.intToRoman(Integer.parseInt(increase(NumberUtilities.romanToInt(currentVersion),
              (VERSION_ROOT.equals(nextVersion) ? "1" : NumberUtilities.romanToInt(nextVersion) + "")))) + "";
        } catch (NumberFormatException e1) {

          char c = currentVersion.toCharArray()[currentVersion.length() - 1];
          c = (char) (c + 1);
          if (c > nextVersion.toCharArray()[nextVersion.length() - 1])
            nextVersion = currentVersion.substring(0, currentVersion.length() - 1) + c;

        }
      }
    }
    return nextVersion;
  }

  public static String nextVersion(Collection<String> versions) {

    String nextVersion = VERSION_ROOT;
    if (versions != null && versions.size() > 0) {
      for (String version : versions) {
        if (JSP.ex(version)) {
          try {
            int current = (Integer.parseInt(version));
            nextVersion = increase(current, nextVersion);
          } catch (NumberFormatException e) {
            try {
              nextVersion = NumberUtilities.intToRoman(Integer.parseInt(increase(NumberUtilities.romanToInt(version), (VERSION_ROOT.equals(nextVersion) ? "1" : NumberUtilities.romanToInt(nextVersion) + "")))) + "";
            } catch (NumberFormatException e1) {
              char c = version.toCharArray()[version.length() - 1];
              c = (char) (c + 1);
              if (c > nextVersion.toCharArray()[nextVersion.length() - 1])
                nextVersion = version.substring(0, version.length() - 1) + c;

            }
          }
        }

      }
    }
    return nextVersion;
  }


  private static String increase(int current, String nextVersion) {
    if (current >= (nextVersion.equals("I") ? 0 : Integer.parseInt(nextVersion)))
      nextVersion = NumberUtilities.padd((current + 1) + "", 2, "0");
    //nextVersion = (Math.max(current, (nextVersion.equals("I") ? 0 : Integer.parseInt(nextVersion))) + 1) + "";
    return nextVersion;
  }


}
