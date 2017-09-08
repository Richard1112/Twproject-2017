package com.opnlb.website.htmlParser;

import java.util.Enumeration;
import java.util.LinkedList;
import java.util.TreeMap;
import java.util.Vector;


public class HtmlDocument implements java.io.Serializable {

  public ElementSequence elements;

  public HtmlDocument(ElementSequence s) {
    elements = s;
  }

  public void accept(HtmlVisitor v) {                       
    v.visit(this);
  }


  public static abstract class HtmlElement  implements java.io.Serializable  {

    protected int             positionInSequence=-1;
    protected ElementSequence ownerSequence=null;
    public abstract void accept(HtmlVisitor v);

    public int getPositionInSequence() {
      return positionInSequence;
    }

    public void setPositionInSequence(int p) {
      positionInSequence = p;
    }

    public ElementSequence getOwnerSequence() {
      return ownerSequence;
    }

    public void setOwnerSequence(ElementSequence p) {
      ownerSequence = p;
    }
  }

  public static class Tag extends HtmlElement {

    public String tagName;
    public AttributeList attributeList;

    public Tag(String t, AttributeList a) {
      tagName = t;
      attributeList = a;
    }

    public void accept(HtmlVisitor v) {
      v.visit(this);
    }

    public int getLength() {
      int length = 0;
      for (Enumeration ae=attributeList.elements(); ae.hasMoreElements(); ) {
        length += 1 + ((Attribute) ae.nextElement()).getLength();
      }
      return length + tagName.length() + 2;
    }

    public String toString() {
      String s = "<" + tagName;
      for (Enumeration ae=attributeList.elements(); ae.hasMoreElements(); ) {
        s += " " + ((Attribute) ae.nextElement()).toString();
      }
      s += ">";
      return s;
    }
  }

  public static class EndTag extends HtmlElement {

    public String tagName;
    public EndTag(String t) {
      tagName = t;
    }
    public void accept(HtmlVisitor v) {
      v.visit(this);
    }
    public int getLength() {
      return 3 + tagName.length();
    }

    public String toString() {
      return "</" + tagName + ">";
    }
  }

  public static class TagBlock extends HtmlElement {

    public Tag startTag;
    public EndTag endTag;
    public ElementSequence body;

    public TagBlock(String name, AttributeList aList, ElementSequence b) {
      startTag = new Tag(name, aList);
      endTag   = new EndTag(name);
      body = b;
    }

    public void accept(HtmlVisitor v) {
      v.visit(this);
    }
  }

  /** HTML comments. */
  public static class Comment extends HtmlElement {

    public String comment;

    public Comment(String c) {
      comment = c;
    }

    public void accept(HtmlVisitor v) {
      v.visit(this);
    }

    public int getLength() {
      return 3 + comment.length();
    }

    public String toString() {
      return "<!" + comment + ">";
    }
  }

  /** Plain text */
  public static class Text extends HtmlElement {

    public String text;

    public Text(String t) {
      text = t;
    }

    public void accept(HtmlVisitor v) {
      v.visit(this);
    }

    public int getLength() {
      return text.length();
    }

    public String toString() {
      return text;
    }
  }

  public static class Newline extends HtmlElement {

    public static final String NL = System.getProperty("line.separator");

    public void accept(HtmlVisitor v) {
      v.visit(this);
    }

    public int getLength() {
      return NL.length();
    }

    public String toString() {
      return NL;
    }
  }

  public static class ElementSequence  implements java.io.Serializable  {

    protected Vector elements;

    public ElementSequence(int n) {
      elements = new Vector(n);
    }

    public ElementSequence() {
      elements = new Vector();
    }

    public void addElement(HtmlElement o) {
      elements.addElement(o);
    }

    public void setElementAt(HtmlElement o,int idx) {
      elements.setElementAt(o,idx);
    }

    public Enumeration elements() {
      return elements.elements();
    }
  }


  public static class Annotation extends HtmlElement {

    String type, text;

    public Annotation(String type, String text) {
      this.type = type;
      this.text = text;
    }

    public void accept(HtmlVisitor v) {
      v.visit(this);
    }

    public int getLength() {
      return 14 + type.length() + text.length();
    }

    public String toString() {
      return "<!--NOTE(" + type + ") " + text + "-->";
    }
  }

  public static class Attribute  implements java.io.Serializable  {

    public String name, value;
    public boolean hasValue;

    public Attribute(String n) {
      name = n;
      hasValue = false;
    }

    public Attribute(String n, String v) {
      name = n;
      value = v;
      hasValue = true;
    }

    public int getLength() {
      return (hasValue? name.length() + 1 + value.length() : name.length());
    }

    public String toString() {
      return (hasValue? name+"="+value : name);
    }
  }

  public static class AttributeList  implements java.io.Serializable {

    public TreeMap attribsMap;
    public Vector  attributes;

    public AttributeList() {
      attribsMap = new TreeMap();
      attributes = new Vector(5,15);
    }

    public void addAttribute(Attribute a) {
      String     key     = a.name.toUpperCase();
      Object     element = attribsMap.get(key);
      LinkedList col     = null;

      if(element == null) {
        attribsMap.put(key, a);
      } else if (element.getClass() == Attribute.class) {
        col = new LinkedList();
        col.add(element);
        col.add(a);
        attribsMap.put(key, col);
      } else {
        ((LinkedList)element).add(a);
      }
      attributes.addElement(a);
    }

    public Object getAttribute(String aName) {
      return attribsMap.get(aName.toUpperCase());
    }

    public Enumeration elements() {
      return attributes.elements();
    }
  }

}
