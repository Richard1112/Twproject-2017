package org.jblooming.waf.html.input;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageState;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.utilities.DateUtilities;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;
import java.util.List;
import java.util.ArrayList;
import java.io.IOException;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 16-feb-2006 : 15.05.54
 */
public class ComboBox extends JspHelper {

  public String fieldName;
  /**
   * type is used for te init. An hidden div will be created for each different group.
   * If in the same page there will two or more ComboBox with same dropdown they will share the popup.
   */
  private String type;

  public List<String> values = new ArrayList();

  public boolean disabled = false;
  public boolean readOnly = false;
  public boolean required = false;

  public String label;
  public String innerLabel; // printed inside the field
  public String separator = "";
  public String htmlClass = "formElements smartCombo";
  public boolean preserveOldValue = true;
  public String style;
  public int fieldSize = 5;
  public int divWidth = 50;
  public int divHeight = 0;
  public boolean forQBE = false;
  public String script = ""; // warning onFocus, onBlur, OnKeyDown onKeyUp already used!!!!!!!
  public String onSelectScript = "";

  public InputElement.EntryType entryType;


  public ComboBox(String fieldName, String label, String type, PageState pageState) {
    this.urlToInclude = "/commons/layout/comboBox/partComboBox.jsp";
    this.fieldName = fieldName;
    this.type = type;
    this.label = label;
  }


  public void addValue(String value) {
    values.add(value);
  }

  public String getDiscriminator() {
    return ComboBox.class.getName() + type;
  }

  public boolean validate(PageState pageState) throws IOException, ServletException {
    return true;
  }

  public void init(PageContext pageContext) {
    PageState ps = PageState.getCurrentPageState(pageContext);
    if (!ps.initedElements.contains( getDiscriminator())) {
      pageContext.getRequest().setAttribute(ACTION, INITIALIZE);
      super.toHtml(pageContext);
      ps.initedElements.add( getDiscriminator());
    }
  }

  public void toHtml(PageContext pageContext) {
    init(pageContext);
    pageContext.getRequest().setAttribute(ACTION, "DRAW_INPUT");
    super.toHtml(pageContext);
  }


  public String getType() {
    return type;
  }

  //Sep 3, 2008: Pietro: 0 -> (int)(CompanyCalendar.MILLIS_IN_MINUTE*15): it doesn't make sense to set 0!
  public static ComboBox getTimeInstance(String fieldName, String label, PageState pageState) {
    return getTimeInstance(fieldName, label, "TIMECMB", (int) (CompanyCalendar.MILLIS_IN_MINUTE * 15), pageState);
  }

  public static ComboBox getTimeInstance(String fieldName, String label, String type, int startMillis, PageState pageState) {
    return getTimeInstance(fieldName, label, type, startMillis, CompanyCalendar.MILLIS_IN_DAY, CompanyCalendar.MILLIS_IN_MINUTE * 15, pageState);
  }

  public static ComboBox getTimeInstance(String fieldName, String label, String type, long startMillis, long endMillis, long incrementInMillis, PageState pageState) {
    ComboBox cb = new ComboBox(fieldName, label, type, pageState);
    cb.fieldSize = 5;
    cb.divHeight = 150;
    cb.divWidth = 75;
    cb.style = "width:80px;";
    for (long i = startMillis; i < endMillis; i += incrementInMillis)
      cb.addValue(DateUtilities.getMillisInHoursMinutes(i,"00","00"));

    cb.entryType = InputElement.EntryType.TIME;
    return cb;
  }

  public void toHtmlI18n(PageContext pageContext) {
    if (label == null)
      label = I18n.get(fieldName);
    else
      label = I18n.get(label);
    toHtml(pageContext);
  }

  public void finalize(PageContext pageContext) {
    pageContext.getRequest().setAttribute(ACTION, "FINALIZE");
    super.toHtml(pageContext);
  }


}
