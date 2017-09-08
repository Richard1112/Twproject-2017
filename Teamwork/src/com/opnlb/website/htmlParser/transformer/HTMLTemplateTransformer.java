package com.opnlb.website.htmlParser.transformer;

import com.opnlb.website.htmlParser.HtmlVisitor;
import com.opnlb.website.htmlParser.HtmlDocument;
import com.opnlb.website.htmlParser.parser.HtmlParser;
import com.opnlb.website.htmlParser.parser.ParseException;

import java.util.*;
import java.io.*;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.StringUtilities;

/**
 * HTMLTemplateTransformer (c) 2005 - Open Lab - www.open-lab.com
 */
// previously WW3 HTMLScreenTransform
public class HTMLTemplateTransformer extends  HtmlVisitor {

  protected boolean modified = false;

  static interface Acceptor {
    public void accept(HtmlDocument.HtmlElement t);
    public void accept(HtmlDocument.TagBlock tb);
    //public void accept(HtmlDocument.HtmlElement t, boolean modifyHtml);
  }

  public static class EmptyTag extends HtmlDocument.Tag {
    public EmptyTag(String t, HtmlDocument.AttributeList a) {
      super( t , a );
    }

    public String toString() {
      String s = "<" + tagName;
      for (Enumeration ae=attributeList.elements(); ae.hasMoreElements(); ) {
        s += " " + ae.nextElement().toString();
      }
      s += "/>";
      return s;
    }
  }

  public static class ScriptletTag extends HtmlDocument.Tag {
    public ScriptletTag(String content) {
      super( content , new HtmlDocument.AttributeList() );
    }
    public String toString() {
      //return "<%" + tagName + "%>";
      return  tagName;
    }
  }

  Acceptor acceptor;
  Map areas;

  protected void acceptElement( HtmlDocument.HtmlElement t ) {
    acceptor.accept(t);
  }

  protected void acceptElement( HtmlDocument.TagBlock tb ) {
    acceptor.accept(tb);
  }

  boolean repaired     = false;
  boolean transforming = false;

  class WriterAcceptor implements Acceptor {
    Writer writer;
    WriterAcceptor(Writer writer) {
      this.writer = writer;
    }
    public void accept(HtmlDocument.HtmlElement t) {
      try {                           

        // as it was before
        //writer.write( t.toString() );

        // contextPath management start
        if (t instanceof HtmlDocument.Tag) {
          HtmlDocument.Tag tag = (HtmlDocument.Tag) t;
          if ("link".equals(tag.tagName.toLowerCase()) ||
              "script".equals(tag.tagName.toLowerCase()) ||
              "img".equals(tag.tagName.toLowerCase())
              ) {
            HtmlDocument.AttributeList al = tag.attributeList;
            HtmlDocument.AttributeList cleanedList = new HtmlDocument.AttributeList();
            for (Enumeration ae=al.elements(); ae.hasMoreElements(); ) {
              HtmlDocument.Attribute attribute = (HtmlDocument.Attribute)ae.nextElement();
              if (
                  ( "href".equals(attribute.name.toLowerCase()) || "src".equals(attribute.name.toLowerCase())) &&
                    attribute.value.toLowerCase().indexOf("applications")>-1
                  )  {
                String oldValue = StringUtilities.replaceAllNoRegex(attribute.value, "\"", "");
                String replacedValue = "\"<%=request.getContextPath()%>"+oldValue+"\"";
                HtmlDocument.Attribute newAtt = new HtmlDocument.Attribute(attribute.name, replacedValue);
                cleanedList.addAttribute(newAtt);
              } else {
                cleanedList.addAttribute(attribute);
              }
            }
            HtmlDocument.Tag originalCleanedTag = new HtmlDocument.Tag(tag.tagName, cleanedList);
            // original area tag is accepted with no changes
            writer.write(originalCleanedTag.toString());
          } else {
            writer.write( t.toString() );
          }
        } else {
          writer.write( t.toString() );
        }
        // contextPath management end

      } catch ( IOException e )  {
        throw new PlatformRuntimeException (e);
      }
    }

    public void accept(HtmlDocument.TagBlock tb) {
      try {
        writer.write( tb.toString() );
      } catch ( IOException e )  {
        throw new PlatformRuntimeException (e);
      }
    }

    /*
    public void accept(HtmlDocument.HtmlElement t, boolean modifyHtml) {
      try {
        writer.write( t.toString() );
      } catch ( IOException e )  {
        throw new PlatformRuntimeException (e);
      }
    }
    */
  }

  /**
   * preview file creation
   * @param tag
   */
  protected void  insertActiveArea( HtmlDocument.Tag tag ) {
    String name = TemplateTransformerUtilities.getAttribute( tag , "AREANAME" , true );
    String custom = TemplateTransformerUtilities.getAttribute( tag, "CUSTOM", true);
    if( name==null )
      throw new RuntimeException("Can not transform a tag without a name");
    if( name.indexOf('"') >= 0 || name.indexOf('\\')>=0 )
      throw new RuntimeException("Invalid name "+name+": names can't contain '\"' or '\\'");
    if( custom==null || custom.trim().length()==0)
      custom = "YES";

    Map map = tag.attributeList.attribsMap;
    Iterator iter = map.keySet().iterator();

    boolean tagIdPresent = false;
    HtmlDocument.AttributeList tagTmpList = new HtmlDocument.AttributeList();
    while (iter.hasNext())  {
      String element = (String)iter.next();  // AREANAME
      HtmlDocument.Attribute attribute = null;
      try {
        attribute = (HtmlDocument.Attribute) map.get( element );
      } catch(ClassCastException a) {
        Tracer.platformLogger.error(a);
        String tagError = StringUtilities.replaceAllNoRegex(tag.toString(), "<", "&lt;");
        tagError = tagError.replaceAll(">", "&gt;").replaceAll("\"", "").replaceAll("'", "");
        throw new PlatformRuntimeException("INVALID HTML CODE: THE ATTRIBUTE " + element.toLowerCase() +" IS DUPLICATED IN \\n" + tagError + "\\n Template can not be saved!");
      }

      HtmlDocument.AttributeList tagList = new HtmlDocument.AttributeList();
      tagList.addAttribute( new HtmlDocument.Attribute("name", TemplateTransformerUtilities.checkQuotes(element) ) );

    // id = nome area
      if ("id".equalsIgnoreCase(element)) {
        tagIdPresent = true;
        tagTmpList.addAttribute(new HtmlDocument.Attribute("id", "\"" + name + "\""));
      } else {
        tagTmpList.addAttribute(attribute);
      }
    }

    // required by mbDrop_js' drag and drop (tag "custom" can't be missing!!)
    if (!tag.attributeList.attribsMap.containsKey("CUSTOM")) {
      tagTmpList.addAttribute(new HtmlDocument.Attribute("custom", "\"yes\""));
    }

    if(!tagIdPresent) {
      HtmlDocument.Attribute id = new HtmlDocument.Attribute("id", "\""+name+"\"" );
      tagTmpList.addAttribute(id);
    }

    //class="dropArea"
    tagTmpList.addAttribute(new HtmlDocument.Attribute("class", "\"dropArea\"" ));

    // input hidden making
    HtmlDocument.AttributeList inputList = new HtmlDocument.AttributeList();
    HtmlDocument.Attribute type = new HtmlDocument.Attribute("type", "\"hidden\"");
    inputList.addAttribute(type);
    HtmlDocument.Attribute inputName = new HtmlDocument.Attribute("name", "\"wsa_"+ name+"\"" );
    HtmlDocument.Attribute inputValue = new HtmlDocument.Attribute("value", "\"" + name + "\"" );
    inputList.addAttribute(inputName);
    inputList.addAttribute(inputValue);

    HtmlDocument.Tag originalTag = new HtmlDocument.Tag(tag.tagName, tagTmpList);
    acceptElement(originalTag);

    HtmlDocument.Tag inputHid = new HtmlDocument.Tag("input", inputList);
    acceptElement(inputHid);

    // added to have a space in html preview file
    //acceptElement(new HtmlDocument.Text("&nbsp;"));
    acceptElement(new HtmlDocument.Text("<img src='.' width='1' height='1'>"));

    modified=true;
}

  public HTMLTemplateTransformer(Map areas) {
    if( areas==null ) {
      //areas = new TreeMap( String.CASE_INSENSITIVE_ORDER );
      //areas = new HashTable();
      areas = new HashMap();
    }
    this.areas=areas;
  }

  public HTMLTemplateTransformer () {
    this(null);
  }

  protected Stack passed = new Stack();

  public void visit(HtmlDocument.Tag t) {
    if( (!t.tagName.startsWith("PORTAL:") && t.tagName.indexOf(':')>=0) || t.tagName.indexOf('%')>=0 )
      throw new SecurityException( "Forbidden tag " + t );

    if ( TemplateTransformerUtilities.isTagContainingAnArea( t )) {
      insertActiveArea( t );
    } else {
      String tagName = t.tagName.toLowerCase();
      if (tagName.indexOf("script")==-1)
        acceptElement(t);
    }
  }

  protected void conditionalAccept( HtmlDocument.HtmlElement e ) {
    if( !this.transforming )
      acceptElement( e );
  }

  public void visit(HtmlDocument.TagBlock tb)      {
    if (tb.startTag.tagName.toLowerCase().indexOf("script")==-1)
      acceptElement(tb);
  }

  public void visit(HtmlDocument.EndTag t)      {
    if (t.tagName.toLowerCase().indexOf("script")==-1)
      acceptElement(t);
  }

  public void visit(HtmlDocument.Comment c)     {
    conditionalAccept(c);
  }

  public void visit(HtmlDocument.Text t)        {
    conditionalAccept(t);
  }

  public void visit(HtmlDocument.Newline n)     {
    conditionalAccept(n);
  }

  public void visit(HtmlDocument.Annotation a)  {
    conditionalAccept(a);
  }

  public void start()  {
    passed.clear();
  }

  public void finish() {
  }

  public Map transform (Reader r, Writer w) {
    try {
      modified = false;
      acceptor = new WriterAcceptor( w );
      HtmlParser parser = new HtmlParser( r );
      read( parser );
      //w.write("<%}catch( Throwable t) { throw t; }%>");
      w.flush();
      if( transforming )
        throw new RuntimeException( "AREANAME opened but not closed" );

      return areas;

    } catch ( IOException e )  {
      throw new PlatformRuntimeException(e);
    }
  }

  protected void read( HtmlParser parser ) { //throws ControllerException {
    try {
      parser.HtmlDocument().accept( this );
    } catch( ParseException e ) {
      //throw new ControllerException( e , "HTMLConditioner::read");
      throw new PlatformRuntimeException(e);
    }
  }

  public boolean isModified() {
    return modified;
  }

  /*
  protected void  replaceWithArea( HtmlDocument.Tag tag ) {
    String name = TemplateTransformerUtilities.getAttribute( tag , "NAME" , true );
    String custom = TemplateTransformerUtilities.getAttribute( tag, "CUSTOM", true);
    if( name==null )
      throw new RuntimeException("Can not transform a tag without a name");
    if( name.indexOf('"') >= 0 || name.indexOf('\\')>=0 )
      throw new RuntimeException("Invalid name "+name+": names can't contain '\"' or '\\'");
    if( custom==null )
      custom = "YES";
    Map map = tag.attributeList.attribsMap;
    Iterator iter = map.keySet().iterator();
    //Map attributes = (Map)areas.get( name );
    Map<String, String>attributes = (Map<String, String>) areas.get(name);
    if( !areas.keySet().contains( name ) ) {
      //attributes = new HashMap();
      //areas.put( name , attributes );
    }
    HtmlDocument.AttributeList inputList = new HtmlDocument.AttributeList();
    HtmlDocument.Attribute type = new HtmlDocument.Attribute("type", "hidden");
    inputList.addAttribute(type);
    while (iter.hasNext())  {
      String element = (String)iter.next();  // AREANAME
      HtmlDocument.Attribute attribute = (HtmlDocument.Attribute) map.get( element );
      HtmlDocument.AttributeList list = new HtmlDocument.AttributeList();
      list.addAttribute( new HtmlDocument.Attribute("name", TemplateTransformerUtilities.checkQuotes(element) ) );
      HtmlDocument.Attribute inputName = new HtmlDocument.Attribute("name", "wsa_"+element );
      HtmlDocument.Attribute inputValue = new HtmlDocument.Attribute("value", element );
      inputList.addAttribute(inputName);
      inputList.addAttribute(inputValue);
    }
    HtmlDocument.Tag inputHid = new HtmlDocument.Tag("input", inputList);
    acceptElement(inputHid);
//    String script = "";
//    ScriptletTag newTag  = new ScriptletTag( script );
//    acceptElement( newTag );
    modified=true;
  }
  */
}
