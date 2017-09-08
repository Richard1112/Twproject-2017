package org.jblooming.waf.html.state;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.core.UrlComposer;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;
import java.io.IOException;
import java.util.Set;
import java.util.HashSet;

public class Form extends UrlComposer {

  public boolean w3cCompliant = false;
  private boolean closeFormCalled;
  private boolean openFormCalled;
  private Drawer drawer;
  public String target;
  public String additionalParams;


  public static final String MULTIPART_FORM_DATA = "multipart/form-data";
  public static final String APPLICATION_X_WWW_FORM_URLENCODED = "application/x-www-form-urlencoded";

  public static final String FORM_START = "FORM_START";
  public static final String FORM_END = "FORM_END";


  /**
   * do not use, if you need to add CE use form.url not this
   */
  public Set<String> entriesCarrier = null;

  public boolean usePost = true;
  public String encType = Form.APPLICATION_X_WWW_FORM_URLENCODED;

  public boolean alertOnChange = false;

  public static final String FLD_FORM_ORDER_BY = "OB_";

  public Form(PageSeed v) {
    super(v);
    if (v instanceof PageState)
      throw new PlatformRuntimeException("NO PAGESTATE BUT PAGESEED IN THE FORM COSTRUCTOR!");
    outputModality = UrlComposer.OUTPUT_AS_FORM;
    this.drawer = new Drawer(this);

  }

  /**
   * @deprecated use start and end
   */
  public String toHtml() {
    return toHtmlStringBuffer().toString();
  }

  /**
   * @deprecated use start and end
   */
  public StringBuffer toHtmlStringBuffer() {

    StringBuffer bv = new StringBuffer(512);

    bv.append(" enctype=\"").append(encType + "\"");

    bv.append(" method=\"").append((this.usePost ? "POST" : "GET")).append("\" action=\"");

    if (this.debug)
      bv.append(request.getContextPath() + "/commons/administration/debug.jsp");
    else
      bv.append(url.getHref());

    bv.append("\" name=\"").append(getUniqueName()).append('\"');

    if (alertOnChange)
      bv.append("\" alertOnChange=\"true\"");

    if (launchedJsOnActionListened != null) {
      bv.append(' ' + actionListened + "= \"if (event.keyCode==" + keyToHandle + " && " + (checkCtrlKey ? " event.ctrlKey==true " : " event.ctrlKey==false ") + ") { " + launchedJsOnActionListened);
      bv.append("return false;}\"");
    }

    if (JSP.ex(additionalParams))
      bv.append(" "+additionalParams);

    // bv.append(" id=\"").append(getUniqueName()).append("\" savedAction=\"\" savedTarget=\"\">\n");
    bv.append(" id=\"").append(getUniqueName());
    if(! w3cCompliant)
      bv.append("\" savedAction=\"\" savedTarget=\"\"");
    bv.append(">\n");

    final String href = getHref();
    if (href.endsWith(">"))
      bv.append(href.substring(0, href.length() - 1));
    else if (href.endsWith(">\n")) {
      bv.append(href.substring(0, href.length() - 2)).append("\n");
    } else
      throw new PlatformRuntimeException("Form must end with >");


    return bv;
  }

  public void start(PageContext pageContext) {

    PageState.getCurrentPageState(request).htmlBootstrappers.add(this.drawer);
    openFormCalled = true;
    pageContext.getRequest().setAttribute(Drawer.ACTION, FORM_START);
    drawer.toHtml(pageContext);

    entriesCarrier = new HashSet(url.getClientEntries().getEntryKeys());

  }

  public void end(PageContext pageContext) {
    if (!openFormCalled)
      throw new PlatformRuntimeException("Call start before end. Form id:" + id);

    if (!closeFormCalled) {
      closeFormCalled = true;
      pageContext.getRequest().setAttribute(Drawer.ACTION, FORM_END);

      Set tmp = new HashSet(url.getClientEntries().getEntryKeys());
      tmp.removeAll(entriesCarrier);
      entriesCarrier = tmp;

      drawer.toHtml(pageContext);
    }
  }


  public class Drawer extends JspHelper implements HtmlBootstrap {

    public Form form;

    public Drawer(Form f) {
      this.form = f;
      this.urlToInclude = "/commons/layout/partForm.jsp";

    }

    public String getId() {
      return id;
    }

    public String getDiscriminator() {
      return getId();
    }


    public boolean validate(PageState pageState) throws IOException, ServletException {
      return form.openFormCalled && form.closeFormCalled;
    }


  }


}
