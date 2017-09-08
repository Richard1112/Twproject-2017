/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.util;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

/**
 * Utility class with string manipulation methods.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class StringUtil {

  /**
   * Method that splits a string with delimited fields
   * into an array of field strings.
   *
   * @param str String with delimited fields.
   * @param delim String that represents the delimiter.
   *
   * @return String[] holding all fields.
   */
  public static String[] split(String str, String delim) {
    //System.out.println(str);
    StringTokenizer strtok = null;
    ArrayList list = null;

    if (delim.equals(";")) {
      strtok = new StringTokenizer(str, delim, true);
      list = new ArrayList(strtok.countTokens());
      boolean flag = false;
      int cnt = 0;
      while (strtok.hasMoreElements()) {
        String tok = strtok.nextToken();
        //System.out.println(tok);
        //filter delimiters without value?
        if (tok.equals(delim)) {
          if (cnt == 0 || cnt == strtok.countTokens() - 1) {
            list.add("");
            continue;
          }
          if (!flag) {
            flag = true;
            continue;
          } else {
            //add one empty string
            list.add("");
            flag = false;
            continue;
          }
        } else {
          list.add(tok);
          flag = false;
          cnt = 0;
        }
        cnt++;
      }

    } else {
      strtok = new StringTokenizer(str, delim);
      list = new ArrayList(strtok.countTokens());
      while (strtok.hasMoreElements()) {
        list.add(strtok.nextToken());
      }
    }


    //check list for String with / at the end
    String tmpstr = null;
    for (int i = 0; i < list.size(); i++) {
      tmpstr = (String) list.get(i);
      if (tmpstr.length() > 0 && tmpstr.charAt(tmpstr.length() - 1) == 92) {
        //join into next
        if (i < list.size()) {
          tmpstr += delim + (String) list.get(i + 1);
          list.set(i, tmpstr);
          list.remove(i + 1);
        }
      }
    }
    return listToStringArray(list);
  }//split(String,String)

  /**
   * Method that splits a string with delimited fields
   * into an array of field strings.
   *
   * @param str String with delimited fields.
   * @param delim char that represents the delimiter.
   *
   * @return String[] holding all fields.
   */
  public static String[] split(String str, char delim) {
    return StringUtil.split(str, String.valueOf(delim));
  }//split(String,char)


  public static String[] listToStringArray(List list) {
    String[] strs = new String[list.size()];
    return (String[]) list.toArray(strs);
  }//listToStringArray

  /**
   *
   */
  public static String[] splitList(String str) {
    return split(str, ",");
  }//splitList

  public static String joinList(String[] items) {
    StringBuffer sbuf = new StringBuffer();
    for (int i = 0; i < items.length; i++) {
      sbuf.append(items[i]);
      if (i < items.length - 1) {
        sbuf.append(",");
      }
    }
    return sbuf.toString();
  }//joinList

  public static String joinList(String[] items, char separator) {
    StringBuffer sbuf = new StringBuffer();
    for (int i = 0; i < items.length; i++) {
      sbuf.append(items[i]);
      if (i < items.length - 1) {
        sbuf.append(separator);
      }
    }
    return sbuf.toString();
  }//joinList

  public static String removeWhiteSpace(String datastr) {
    byte[] data = datastr.getBytes();
    ByteArrayOutputStream bout = new ByteArrayOutputStream(data.length);
    for (int i = 0; i < data.length; i++) {
      if (data[i] == 10 || data[i] == 13 || data[i] == 9 || data[i] == 32) {
        //ignore whitespace
        continue;
      } else {
        bout.write(data[i]);
      }
    }
    return new String(bout.toByteArray());
  }//removeWhiteSpace

  public static boolean isValidString(String str) {
    return (str != null && str.length() > 0);
  }//validString

  public static boolean isValidStringArray(String[] str) {
    return (str != null && str.length > 0);
  }//validStringArray

}//class StringUtil

