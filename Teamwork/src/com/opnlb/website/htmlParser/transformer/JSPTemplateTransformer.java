package com.opnlb.website.htmlParser.transformer;

import com.opnlb.website.htmlParser.HtmlVisitor;
import com.opnlb.website.htmlParser.HtmlDocument;
import com.opnlb.website.htmlParser.parser.HtmlParser;

import java.util.*;
import java.io.Writer;
import java.io.IOException;
import java.io.Reader;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.StringUtilities;

/**
 * HTMLTemplateTransformer (c) 2005 - Open Lab - www.open-lab.com
 * previously WW3 HTMLScreenTransform
 */
public class JSPTemplateTransformer extends  HtmlVisitor {

  protected boolean modified = false;

  /**
   * Acceptor
   * It accepts html elements
   */
  static interface Acceptor {
    public void accept(HtmlDocument.HtmlElement t);
    public void accept(HtmlDocument.TagBlock tb);
  }


  /**
   * EmptyTag
   *
   * NOT USED
   */
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

  /**
   * ScriptletTag
   *
   * jsp active area script
   */
  public static class ScriptletTag extends HtmlDocument.Tag {
    public ScriptletTag(String content) {
      super( content, new HtmlDocument.AttributeList() );
    }
    public String toString() {
      return "<%" + tagName + "%>";
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

  /**
   * WriterAcceptor
   *
   * accepted elements are passed to the writer which build the jsp code
   */
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
        if (t instanceof HtmlDocument.Tag ) {
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

  }

  /**
   * AreaTagsMatcher
   *
   * simple object to preserve
   *    tagBlock
   *    end "areaName" tag position
   * while looping the html document's element sequence
   */
  private class AreaTagsMatcher {

    String body;
    ScriptletTag scriptTag;
    int endTagPosition;

    private AreaTagsMatcher(ScriptletTag script, String tagBody, int endTagPos) {
      this.scriptTag = script;
      this.body = tagBody;
      this.endTagPosition = endTagPos;
    }
  }

  public JSPTemplateTransformer(Map areas) {
    if( areas==null ) {
      areas = new HashMap();
    }
    this.areas=areas;
  }

  protected Stack passed = new Stack();

  /**
   * step 01
   *
   * template's html code is parsed and a new jsp file is created and written
   * Such jsp will be used by application
   * @param reader
   * @param writer
   * @return
   * @throws IOException
   */
  public Map writeJSP( Reader reader , Writer writer ) {
    try {
      //modified = false;
      writer.write("<%@ page import=\"org.jblooming.waf.view.PageState,\n" +
              "                 org.jblooming.operator.Operator,\n" +
              "                 org.jblooming.waf.SessionState,\n" +
              "                 java.util.List,\n" +
              "                 com.opnlb.website.template.Template,\n" +
              "                 com.opnlb.website.page.WebSitePage,\n" +
              "                 org.jblooming.waf.html.core.JspIncluderSupport,\n" +
              "                 com.opnlb.website.util.TemplateManager\"%>\n" +
              "<%@page pageEncoding=\"UTF-8\"%><%\n" +
              "try{\n" +
              "    PageState pageState = PageState.getCurrentPageState(request);\n" +
              "    SessionState sessionState = SessionState.getCurrentSessionState();\n" +
              "    Operator logged = (Operator)pageState.getLoggedOperator();\n" +
              "    Template template = (Template) JspIncluderSupport.getCurrentInstance(request);\n" +
              "    String pageId = template.pageIdParam;\n" +
              "    if (pageId == null || pageId.trim().length()==0) {\n" +
              "      pageId = pageState.getEntry(\"WS_PAGEID\").stringValue();\n" +
              "    }\n" +
              "    List choosenContents = TemplateManager.choosenContents(logged, pageId);\n" +
              "    WebSitePage wsp = (WebSitePage) pageState.getAttribute(\"WW_MAIN_OBJ\");\n\n" +
              "%>");
                // per ww4 ==>    WebworkPage wsp = FrontOfficeBricks.getCurrentPage(pageState, request);
      acceptor = new WriterAcceptor( writer );

      HtmlParser parser = new HtmlParser( reader );
      // active areas dynamic codes is streamed
      read( parser );

      writer.write("<%\n" +
              "} catch( Throwable t) { \n" +
              "  throw t; \n" +
              "}\n%>");
      writer.flush();
      return areas;

    } catch ( IOException e )  {
      throw new PlatformRuntimeException(e);
    }
  }

/*--------------------------------------------------------------------------------------------------------------------------------------------------------------

  Html text handling
  WW4 CUSTOMIZED

--------------------------------------------------------------------------------------------------------------------------------------------------------------*/

  /**
   * this object preserve the script while looping in the tagBlock to see if something present between opening end closing area tag
   */
  private static ScriptletTag scriptArea = new ScriptletTag("") ;
  /**
   * this value is the absolute position of the an area end tag in doc element sequence
   * comment, annotation, plain text, new line aren't considered
   * see TemplateTransformerUtilities.isHtmlCode (HtmlDocument.HtmlElement element)
   */
  private static int endTagPosition = 0;

  /**
   * step 02
   *
   * html file character streams reading via a parser
   * @param parser
   */
  protected void read( HtmlParser parser ) {
    try {
      parser.HtmlDocument().accept( this );
    } catch( com.opnlb.website.htmlParser.parser.ParseException e ) {
      throw new PlatformRuntimeException(e);
    }
  }

  /**
   * step 03
   *
   * visit HtmlDocument document
   * @param doc
   */
  public void visit (HtmlDocument doc) {
    start();
    visit(doc.elements);
    /**
     * variables must be resetted only at the end of the parsing process because its previous value (i.e. the value of the previous area endTag)
     * is used to identify the successive end tag
     */
    scriptArea = new ScriptletTag("");
    endTagPosition = 0;
    finish();
  }

  /**
   * step 04
   *
   * it simply visits doc's element sequence throughout a loop which call step 03
   * @param es
   */
  public void visit (HtmlDocument.ElementSequence es) {
    int elementIndex=0;
    AreaTagsMatcher mat = null;
    /**
     * process main loop on html document's elements
     */
    for (Enumeration e = es.elements(); e.hasMoreElements();) {
      HtmlDocument.HtmlElement element = (HtmlDocument.HtmlElement)e.nextElement();
      visit(es, element, mat, elementIndex);
      elementIndex++;
    }
  }

  /**
   * step 05
   *
   * method called inside the document's elementSequence loop (step 04)
   */
  public void visit(HtmlDocument.ElementSequence es , HtmlDocument.HtmlElement element, AreaTagsMatcher mat, int elementIndex) {
    try {
      HtmlDocument.Tag tag = (HtmlDocument.Tag) element;
      if ( TemplateTransformerUtilities.isTagContainingAnArea( tag ) ) {
        // area script is stored but not accepted by visitor
        mat = prepareActiveAreaScript( es, tag, elementIndex );
        endTagPosition = mat.endTagPosition;
        scriptArea  = mat.scriptTag;
      } else {
        acceptElement(tag);
      }
    } catch(ClassCastException ex) {
      // check to accept script before end tag
      if (elementIndex>0 && elementIndex==endTagPosition) {
        // empty script are ignored
        if (scriptArea!=null && scriptArea.tagName!=null && scriptArea.tagName.trim().length()>0) {
          // area script is accepted and thus written by visitor after its tagblock has already been accepted (if present)
          acceptElement(scriptArea);
        }
      }
      // inserted to accept TagBlock "<script>"
      if (element instanceof HtmlDocument.TagBlock) {
        HtmlDocument.TagBlock tb = (HtmlDocument.TagBlock) element;
        acceptElement(tb.startTag);
        HtmlDocument.ElementSequence body = tb.body;
        for (Enumeration e = body.elements(); e.hasMoreElements();) {
          HtmlDocument.HtmlElement el = (HtmlDocument.HtmlElement)e.nextElement();
          acceptElement(el);
        }
        acceptElement(tb.endTag);
      } else
        acceptElement(element);
    }
  }

  /**
   * script is prepared but not passed to visitor
   * it must be passed only after its tagBlock
   * @param es (ElementSequence)
   * @param tag
   * @return AreaTagsMatcher
   */
  protected AreaTagsMatcher prepareActiveAreaScript( HtmlDocument.ElementSequence es, HtmlDocument.Tag tag, int startTagPosition ) {
    String areaName = TemplateTransformerUtilities.getAttribute( tag , "AREANAME" , true );
    String customArea = TemplateTransformerUtilities.getAttribute( tag, "CUSTOM", true);
    /**
     * exceptions
     */
    if( areaName==null || areaName.trim().length()==0 )
      throw new RuntimeException("Application can not handle an active area without a name");
    if( areaName.indexOf('"') >= 0 || areaName.indexOf('\\')>=0 )
      throw new RuntimeException("Invalid name "+areaName+": active area name can't contain character like '\"' or '\\'");

    // default value
    if ( customArea==null)
      customArea = "YES";

    // building script
    String script = "";
    if ("YES".equals(customArea.trim().toUpperCase())) {
      script="TemplateManager.display(choosenContents, \""+areaName+"\", pageContext, pageState);";
    } else {
      script="TemplateManager.displayAdmin(pageId, \""+areaName+"\", pageContext, pageState);";
    }
    ScriptletTag scriptTag = new ScriptletTag( script );

    // verifying duplicated area names
    Map<String, String>attributes = (Map<String, String>) areas.get(areaName);
    if( !areas.keySet().contains( areaName ) ) {
      attributes = new HashMap<String, String>();
      areas.put( areaName, attributes );
    } else {
      throw new PlatformRuntimeException("\\nERROR:: invalid duplicated area name.\\n"
              + areaName + " is used at least twice. Please modify your template code.\\n" +
              "JSP file has not been created.");
    }

    Map map = tag.attributeList.attribsMap;
    for (Object o : map.keySet()) {
      String element = (String) o;  // AREANAME
      HtmlDocument.Attribute attribute = (HtmlDocument.Attribute) map.get(element);
      HtmlDocument.AttributeList list = new HtmlDocument.AttributeList();
      list.addAttribute(new HtmlDocument.Attribute("name", TemplateTransformerUtilities.checkQuotes(element)));
      if (attribute.hasValue) {
        attributes.put(element, attribute.value);
      } else {
        attributes.put(element, "\"AREANAME_ABSENT\"");
      }
    }

    // W3C required (areaname is not liked) - areaname attribute is not accepted
    HtmlDocument.AttributeList originalList = tag.attributeList;
    HtmlDocument.AttributeList cleanedList = new HtmlDocument.AttributeList();
    for (Enumeration ae=originalList.elements(); ae.hasMoreElements(); ) {
      HtmlDocument.Attribute attribute = (HtmlDocument.Attribute)ae.nextElement();
      if (!"areaname".equals(attribute.name.toLowerCase()))
        cleanedList.addAttribute(attribute);
    }
    HtmlDocument.Tag originalCleanedTag = new HtmlDocument.Tag(tag.tagName, cleanedList);

    // original area tag is accepted with no changes
    acceptElement(originalCleanedTag);
    // if script accepted here ==> written before its tagblock
    //acceptElement(scriptTag);

    // tagBlock is stored in a temporary object (MatchAreaTags)
    HtmlDocument.TagBlock tagBlock = new HtmlDocument.TagBlock(tag.tagName, cleanedList, es);
    AreaTagsMatcher mat = getBodyTag(tagBlock, scriptTag, startTagPosition);
    modified=true;

    return mat;
  }

  /**
   * html code included into the area tag is parsed
   * thus it can be accepted before script
   * @param tagBlock
   * @param scriptTag
   * @return AreaTagsMatcher
   */
  protected AreaTagsMatcher getBodyTag (HtmlDocument.TagBlock tagBlock, ScriptletTag scriptTag, int startTagPosition ) {
    String body = "";
    boolean inTagBlockBody = false;
    boolean alreadyVerified = false;
    int endTagIndex = 0;
    int elementIndex=0;
    HtmlDocument.EndTag endAreaTag = tagBlock.endTag;
    HtmlDocument.ElementSequence es = tagBlock.body;
    /**
     * process secondary loop on html document's elements to build area tag tagBlock
     */
    for (Enumeration e = es.elements(); e.hasMoreElements();) {
      HtmlDocument.HtmlElement element = (HtmlDocument.HtmlElement)e.nextElement();
      try {
        HtmlDocument.Tag tag = (HtmlDocument.Tag) element;
        if (tag!=null) {
          boolean checkName = tag.tagName.toUpperCase().equalsIgnoreCase(tagBlock.startTag.tagName.toUpperCase());
          boolean checkAttributes = tag.attributeList.attribsMap.equals(tagBlock.startTag.attributeList.attribsMap);
          boolean isThisTag = checkName && checkAttributes;
          if (isThisTag) {
            // next element will be part of tagblock body
            inTagBlockBody = true;
          } else if (inTagBlockBody) {
            // if tag element, tagblock body is stored
            body = body+tag;
          }
        }
      } catch (ClassCastException ex) {
        try {
          HtmlDocument.EndTag endTag = (HtmlDocument.EndTag) element;
          boolean isThisEndTag = endTag.tagName.toUpperCase().equalsIgnoreCase(endAreaTag.tagName.toUpperCase());
          /**
           *  in case of more than an active area 'endTagPosition' will be previous area endTag position value
           */
          if ( isThisEndTag && !alreadyVerified && elementIndex>startTagPosition && elementIndex>endTagPosition ) {
            alreadyVerified = true;
            endTagIndex = elementIndex;
            break;
          }
        } catch (ClassCastException exx) {
          // if the tagBlock body is a text or any other object it must be intercepted as well
          if (inTagBlockBody) {
            body = body+element;
          }
        }
      }
      elementIndex++;
    }
    return new AreaTagsMatcher(scriptTag, body, endTagIndex);
  }

  public void visit(HtmlDocument.EndTag t) {
    acceptElement(t);
  }

  public void visit(HtmlDocument.Comment c) {
    conditionalAccept(c);
  }

  public void visit(HtmlDocument.Text t) {
    conditionalAccept(t);
  }

  public void visit(HtmlDocument.Newline n) {
    conditionalAccept(n);
  }

  public void visit(HtmlDocument.Annotation a) {
    conditionalAccept(a);
  }

  public void start()  {
    passed.clear();
  }

  public void finish() {
  }

  protected void conditionalAccept( HtmlDocument.HtmlElement e ) {
    if( !this.transforming )
      acceptElement( e );
  }

}
