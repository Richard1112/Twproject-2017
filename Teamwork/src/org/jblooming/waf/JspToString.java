package org.jblooming.waf;

import javax.servlet.jsp.PageContext;
import javax.servlet.jsp.JspWriter;
import javax.servlet.jsp.tagext.BodyContent;
import java.io.Writer;
import java.io.StringWriter;
import java.io.IOException;

/**
 * @author Federico Soldani - fsoldani@open-lab.com
 * @author Roberto Bicchierai - rbicchierai@open-lab.com
 *         Date: 26-nov-2009
 *         Time: 12.53.39
 *
 * Example to use:
 *
 * <code>
 * JspToString jspToString = new JspToString();
 * jspToString.start(pageContext);
 * out = jspToString.getOut();
 *
 * // BEGIN CODE ON THE WRITER //
 * ButtonJS js = new ButtonJS("addRow()");
 * js.toHtml(pageContext); // This code is on the writer
 *
 * %>bla bla bla before<% // This code is on the writer
 * // END CODE ON THE WRITER //
 *
 * Writer writer = jspToString.end(pageContext);
 * out = jspToString.getOut();
 *
 * // BEGIN CODE ON THE JSP //
 * %>bla bla bla after<% // This code is on the jsp
 * // END CODE ON THE JSP //
 *
 * System.out.println(writer.toString());
 * </code>
 */
public class JspToString {

  private Writer writer;
  private BodyContent content;
  public JspWriter out;

  /**
   * Default constructor with StringWriter writer.
   */
  public JspToString() {
    this(new StringWriter());
  }

  public JspToString(Writer writer) {
    this.writer = writer;
    this.content = null;
  }

  public void start(PageContext pageContext) {
    this.content = pageContext.pushBody();
    this.out = pageContext.getOut();
  }

  public Writer end(PageContext pageContext) throws IOException{
    this.content.writeOut(this.writer);
    pageContext.popBody();

    this.out = pageContext.getOut();

    return this.writer;
  }

  public Writer getWriter() {
    return this.writer;
  }

  public JspWriter getOut() {
    return this.out;
  }
}