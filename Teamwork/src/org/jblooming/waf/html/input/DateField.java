package org.jblooming.waf.html.input;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.view.PageState;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;
import java.io.IOException;
import java.util.Date;

/**
 * calendarField(className,str_inputDateName,str_EventInput)
 * str_className  : Nome della classe da mettere sul backqround es: "class=""tabella"" "
 * str_inputDateName  : Nome dell'input text per la data
 * reqValue                   : input value
 * str_html                  : stringa html da mettere per il size ... es: " size=12  maxlength=12 " onclick.. , Onchange.. ,  onblur-->no title da inserire
 * onblurOnDateValid    : aggiunge cosa si deve fare quando la data è giusta sull onblur con ";" se � necessario
 * onblurOnDateInvalid    : aggiunge cosa si deve fare quando la data è falsa sull onblur con ";" se � necessario
 * bool_Enabled           = true per permettere la selezione:
 * readOnly         = false per permettere la scrittura:
 * bool_searchField     = true se è un campo di ricerca: se non è di ricerca fa la validazione
 */
public class DateField extends JspHelper  {
  public static final String init = DateField.class.getName();
  public static final String INITIALIZE = "DATEFLD_INIT";
  public static final String FINALIZE = "FINALIZE";

  public String fieldName;
  public boolean disabled = false;
  public boolean required = false;
  public boolean readOnly = false;
  public int size = 8;
  public String labelstr;
  public String innerLabel;

  private boolean searchField = false;
  public String className="formElements dateField";
  public String classLabelName;
  public String separator;
  public String onblurOnDateValid;
  public String dateFormat=null;

  public Date minDate;
  public Date maxDate;

  public boolean preserveOldValue = true;
  public String script;

  private String actionListened;
  private int keyToHandle;
  private String launchedJsOnActionListened;


  public DateField(String fieldName) {
    this(fieldName,"/commons/layout/dateField/partDateField.jsp");
  }


  public DateField(String fieldName, String urlToInclude) {
    this.urlToInclude = urlToInclude;
    this.fieldName = fieldName;
    this.id=fieldName;
  }


  @Deprecated
  public DateField(String fieldName, PageState pageState) {
    this(fieldName,"/commons/layout/dateField/partDateField.jsp");
  }
  @Deprecated
  public DateField(String fieldName, PageState pageState, String urlToInclude) {
    this.urlToInclude = urlToInclude;
    this.fieldName = fieldName;
    this.id=fieldName;
  }
  public void addKeyPressControl(int keyToHandle, String launchedJs, String actionListened) {
    this.setKeyToHandle(keyToHandle);
    this.setLaunchedJsOnActionListened(launchedJs);
    this.setActionListened(actionListened);
  }

  public String getDiscriminator() {
    return DateField.class.getName();
  }

  public boolean validate(PageState pageState) throws IOException, ServletException {
    return true;
  }


//  public void init(PageContext pageContext) {
//    PageState ps = PageState.getCurrentPageState((HttpServletRequest) pageContext.getRequest());
//    if (!ps.initedElements.contains(init)) {
//      pageContext.getRequest().setAttribute(ACTION, INITIALIZE);
//      super.toHtml(pageContext);
//      ps.initedElements.add(init);
//    }
//  }

  public void toHtml(PageContext pageContext) {
    //init(pageContext);
    pageContext.getRequest().setAttribute(ACTION, "");
    super.toHtml(pageContext);
  }

  public String getActionListened() {
    return actionListened;
  }

  public void setActionListened(String actionListened) {
    this.actionListened = actionListened;
  }

  public int getKeyToHandle() {
    return keyToHandle;
  }

  public void setKeyToHandle(int keyToHandle) {
    this.keyToHandle = keyToHandle;
  }

  public String getLaunchedJsOnActionListened() {
    return launchedJsOnActionListened;
  }

  public void setLaunchedJsOnActionListened(String launchedJsOnActionListened) {
    this.launchedJsOnActionListened = launchedJsOnActionListened;
  }

  public boolean isSearchField() {
    return searchField;
  }

  public void setSearchField(boolean searchField) {
    this.searchField = searchField;
    this.preserveOldValue=!searchField;
    if (this.searchField && size==10)
      size = 12;
  }

  public void toHtmlI18n(PageContext pageContext) {
    PageState pageState = PageState.getCurrentPageState(pageContext);
    if (labelstr == null)
    labelstr = pageState.getI18n(fieldName);
    else
      labelstr = pageState.getI18n(labelstr);
    toHtml(pageContext);
  }

}
