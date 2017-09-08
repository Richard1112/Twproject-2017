package org.jblooming.waf.html.input;

import org.jblooming.ApplicationException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.container.Container;
import org.jblooming.waf.html.container.ButtonBar;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.jsp.PageContext;
import java.io.IOException;

public class TextFileEditor extends JspHelper{

  public static final String init = TextFileEditor.class.getName();
  public static final String DRAW = "DRAW";
  public static final String SAVE = "SAVE";
 
  public Form form;
  public ActionController controller;
  public String fileName = "openlab.txt";
  public PageSeed backToPage;
  public Container container;
  public String name;
  public String label;
  public int rows=20;
  public int cols=80;
  public ButtonBar bb = new ButtonBar();

  public TextFileEditor(String fileName, PageSeed backToPage, PageState pageState) {
    this.fileName=fileName;
    this.urlToInclude = "/commons/layout/partTextFileEditor.jsp";
    this.backToPage=backToPage;
  }


  public void toHtml(PageContext pageContext) {

    pageContext.getRequest().setAttribute(ACTION, DRAW);
    super.toHtml(pageContext);
  }

  public String getId() {
    return id;
  }

  public String getDiscriminator() {
    return getId();
  }



    public PageState perform(HttpServletRequest request, HttpServletResponse response) throws
            ApplicationException, IOException, PersistenceException,
            ActionException, org.jblooming.security.SecurityException {
      PageState pageState = PageState.getCurrentPageState(request);
      final String command = pageState.getCommand();
      if (Commands.SAVE.equals(command)) {
        save(pageState);
      } else {
        edit(pageState);
      }
      if (controller!=null)
        controller.perform( request,  response);
      return pageState;
    }

    private void edit(PageState pageState) {
      String value;
      try {
        value = FileUtilities.readTextFile(fileName);
      } catch (IOException e) {
        value="";
      }
      pageState.addClientEntry(name,value);
    }

    private void save(PageState pageState)  {
      String value=pageState.getEntry(name).stringValueNullIfEmpty();
      if (value!=null){
        FileUtilities.writeToFile(fileName,value,"UTF-8"); //was in old times value.replace('\\','/'), who knows why ? ppolsinelli@open-lab.com
      }
    }



}
