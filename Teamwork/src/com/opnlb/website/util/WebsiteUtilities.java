package com.opnlb.website.util;

import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.system.SystemConstants;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.waf.settings.ApplicationState;

import java.io.File;

/**
 * WebsiteUtilities (c) 2005 - Open Lab - www.open-lab.com
 */
public class WebsiteUtilities {

  public static String showPreviewScript(String pfFileLocationWithoutContextPath, String underlyingText) {

    String fileStorage = ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL);
    String text = StringUtilities.replaceAllNoRegex(underlyingText, "\"", "");
    text = JSP.javascriptEncode(text);

    if(PersistentFile.TYPE_WEBAPP_FILESTORAGE.equals(PersistentFile.DEFAULT_STORAGE_TYPE)) {
      pfFileLocationWithoutContextPath = StringUtilities.replaceAllNoRegex(pfFileLocationWithoutContextPath,"\\","/");
      pfFileLocationWithoutContextPath = (pfFileLocationWithoutContextPath.startsWith("/") ? pfFileLocationWithoutContextPath : "/"+pfFileLocationWithoutContextPath);
      return "onMouseOver=\"showPreview('"+ pfFileLocationWithoutContextPath +"','" + text +"');\"";

    } else // if (PersistentFile.TYPE_FILESTORAGE.equals(pfType))
      return "onMouseOver=\"showPreview('"+ fileStorage + (fileStorage.endsWith(File.separator) ? "" : File.separator) + pfFileLocationWithoutContextPath +"','" + JSP.javascriptEncode(text) +"');\"";
  }

  public static String normalizePageName(String text){
    if (JSP.ex(text)){
      //text = StringUtilities.replaceAllNoRegex(text, " ", "_");
      text = camelPageName(text);
      text = StringUtilities.replaceAllNoRegex(text, "-", "");
      text = StringUtilities.replaceAllNoRegex(text, "/", "");
      text = StringUtilities.replaceAllNoRegex(text, "\\", "");
      text = StringUtilities.replaceAllNoRegex(text, "\"", "");
      text = StringUtilities.replaceAllNoRegex(text, ",", "");
      text = StringUtilities.replaceAllNoRegex(text, ";", "");
      text = StringUtilities.replaceAllNoRegex(text, "'", "");
      text = StringUtilities.replaceAllNoRegex(text, "+", "");
      text = StringUtilities.replaceAllNoRegex(text, "°", "");
      text = StringUtilities.replaceAllNoRegex(text, "§", "");
      text = StringUtilities.replaceAllNoRegex(text, "@", "");
      text = StringUtilities.replaceAllNoRegex(text, "&", "&amp;");
      text = StringUtilities.replaceAllNoRegex(text, ">", "&gt;");
      text = StringUtilities.replaceAllNoRegex(text, "<", "&lt;");
      text = normalizeAccentedLetters(text);
    }
    return text;
  }

  public static String normalizeAccentedLetters(String text) {
    if(JSP.ex(text)) {
      text = StringUtilities.replaceAllNoRegex(text, "à", "a");
      text = StringUtilities.replaceAllNoRegex(text, "á", "a");
      text = StringUtilities.replaceAllNoRegex(text, "æ", "a");
      text = StringUtilities.replaceAllNoRegex(text, "å", "a");
      text = StringUtilities.replaceAllNoRegex(text, "ä", "a");
      text = StringUtilities.replaceAllNoRegex(text, "ã", "a");
      text = StringUtilities.replaceAllNoRegex(text, "â", "a");
      text = StringUtilities.replaceAllNoRegex(text, "è", "e");
      text = StringUtilities.replaceAllNoRegex(text, "é", "e");
      text = StringUtilities.replaceAllNoRegex(text, "ë", "e");
      text = StringUtilities.replaceAllNoRegex(text, "ê", "e");
      text = StringUtilities.replaceAllNoRegex(text, "ì", "i");
      text = StringUtilities.replaceAllNoRegex(text, "í", "i");
      text = StringUtilities.replaceAllNoRegex(text, "ï", "i");
      text = StringUtilities.replaceAllNoRegex(text, "î", "i");
      text = StringUtilities.replaceAllNoRegex(text, "ò", "o");
      text = StringUtilities.replaceAllNoRegex(text, "ó", "o");
      text = StringUtilities.replaceAllNoRegex(text, "ö", "o");
      text = StringUtilities.replaceAllNoRegex(text, "õ", "o");
      text = StringUtilities.replaceAllNoRegex(text, "ô", "o");
      text = StringUtilities.replaceAllNoRegex(text, "ù", "u");
      text = StringUtilities.replaceAllNoRegex(text, "ú", "u");
      text = StringUtilities.replaceAllNoRegex(text, "ü", "u");
      text = StringUtilities.replaceAllNoRegex(text, "û", "u");
      text = StringUtilities.replaceAllNoRegex(text, "ç", "c");
      text = StringUtilities.replaceAllNoRegex(text, "ÿ", "y");
      text = StringUtilities.replaceAllNoRegex(text, "ý", "y");
      text = StringUtilities.replaceAllNoRegex(text, "ñ", "n");
      text = StringUtilities.replaceAllNoRegex(text, "ß", "ss");
    }
    return text;
  }

  public static String camelPageName(String text){
    if (text!=null) {
      StringBuffer result = new StringBuffer();
      boolean jumpToNext = false;
      for (int i = 0; i < text.length(); i++) {
        Character myChar = text.charAt(i);
        if(!jumpToNext) {
          if(Character.isSpaceChar( myChar )) {
            char nextChar = text.charAt(i+1);
            result.append(Character.toUpperCase(nextChar));
            jumpToNext = true;
          } else {
            if(i==0)
              result.append(myChar.toString().toLowerCase());
            else
              result.append(myChar);
          }

        } else
          jumpToNext = false;
      }
      return result.toString();
    } else {
      return text;
    }
  }

}