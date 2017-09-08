package org.jblooming.waf.html.core;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;
import java.io.IOException;


public class TextEmbedder implements JspIncluder{

  public String textToEmbed;

  public TextEmbedder(String textToEmbed) {
    this.textToEmbed = textToEmbed;
  }


  public void toHtml(PageContext pageContext) throws IOException, ServletException {
    pageContext.getOut().print(textToEmbed);
  }
}