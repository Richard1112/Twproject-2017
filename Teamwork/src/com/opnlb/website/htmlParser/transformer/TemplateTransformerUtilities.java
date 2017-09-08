package com.opnlb.website.htmlParser.transformer;

import com.opnlb.website.htmlParser.HtmlDocument;

import java.util.Map;

/**
 * TemplateTransformerUtilities (c) 2005 - Open Lab - www.open-lab.com
 */
public class TemplateTransformerUtilities {

  public static String getAttribute(HtmlDocument.Tag tag, String name, boolean upperTrim ) {
    Map map = tag.attributeList.attribsMap;
    HtmlDocument.Attribute nameAttribute = (HtmlDocument.Attribute) map.get( name );
    if( nameAttribute==null || nameAttribute.value==null || nameAttribute.value.trim().length()==0 )
      return null;
    String value = nameAttribute.value;
    if( upperTrim )
      value = value.toUpperCase().trim();

    value = decodeTagValue( value );
    return value;
  }

  public static boolean isTagContainingAnArea (HtmlDocument.Tag tag) {
    String a = getAttribute(tag, "AREANAME", true);
    if (a==null)
      return false;
    else
      return true;
  }

  public static String decodeTagValue( String tagValue ) {
    if( tagValue.startsWith("\"") )
      tagValue=tagValue.substring( 1 );
    if( tagValue.endsWith("\"") )
      tagValue=tagValue.substring( 0 , tagValue.length()-1 );
    return tagValue;
  }

  public static String checkQuotes(String value) {
    if( value==null )
      return "\"\"";
    if( value.startsWith("\"") && value.startsWith("\"") )
      value = value.substring( 1 , value.length()-1 );
    if( value.indexOf('"')>=0 || value.indexOf('\\')>=0 )
      throw new RuntimeException( "Invalid parameter name: "+value );
    return "\""+value+"\"";
  }

  /*
  public static boolean isHtmlCode (HtmlDocument.HtmlElement element) {
    boolean isHtmlCode = true;
    if (    ( element instanceof HtmlDocument.Newline )     ||
            ( element instanceof HtmlDocument.Text )        ||
            ( element instanceof HtmlDocument.Annotation )  ||
            ( element instanceof HtmlDocument.Comment )     ) {
      isHtmlCode = false;
    }
    return isHtmlCode;
  }
  */
  
//  public static boolean isStartOfBlockToReplace( HtmlDocument.Tag tag ) {
//    boolean a = tag.tagName != null;
//    boolean b = tag.tagName.trim().equalsIgnoreCase( "PORTALAREAREGION" );
//    return a && b;
//  }

//  public static boolean isToBeReplaced( HtmlDocument.Tag tag ) {
//    boolean a = tag.tagName != null;
//    boolean b = tag.tagName.trim().equalsIgnoreCase( "PORTALAREA" );
//    return a && b;
//  }

//  public static boolean isEndOfBlockToReplace( HtmlDocument.HtmlElement element ) {
//    if( !(element instanceof HtmlDocument.EndTag) )
//      return false;
//    HtmlDocument.EndTag tag  = (HtmlDocument.EndTag) element;
//    boolean a = tag.tagName != null;
//    boolean b = tag.tagName.trim().equalsIgnoreCase( "PORTALAREAREGION" );
//    return a && b;
//  }

}
