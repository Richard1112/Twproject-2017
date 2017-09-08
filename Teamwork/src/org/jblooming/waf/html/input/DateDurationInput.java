package org.jblooming.waf.html.input;

import org.jblooming.utilities.JSP;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.view.PageState;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;
import java.io.IOException;

public class DateDurationInput extends JspHelper implements HtmlBootstrap {

  public DateField startField;
  public String onBlurStartAdditionalScript;
  public CheckField startMilestoneField;

  public DateField endField;
  public CheckField endMilestoneField;

  public TextField durationField;

  public static final String COMPUTE_FIELDS = "CMPF";

  public DateDurationInput(String prefix, PageState pageState) {
    this(prefix + "START", prefix + "STARTISMILES", prefix + "END", prefix + "ENDISMILES", prefix + "DURATION", pageState);
  }

  public DateDurationInput(String startField, String startMilestoneField, String endField, String endMilestoneField, String durationField, PageState pageState) {
    this( startField,  startMilestoneField,  endField,  endMilestoneField,  durationField,  "", pageState);
  }

  public DateDurationInput(String startField, String startMilestoneField, String endField, String endMilestoneField, String durationField, String onBlurAdditionalScript, PageState pageState) {

    this.startField = new DateField(startField);
    this.startMilestoneField = new CheckField(startMilestoneField, "&nbsp;", false);
    this.endField = new DateField(endField);
    this.endMilestoneField = new CheckField(endMilestoneField, "&nbsp;", false);
    this.durationField = TextField.getDurationInDaysInstance(durationField);

    this.startField.onblurOnDateValid =  "resynchDates('START','" + this.startField.id + "','" + this.startMilestoneField.id + "','" + this.durationField.id + "','" + this.endField.id + "','" + this.endMilestoneField.id + "');" +
      JSP.w(onBlurAdditionalScript);

    this.startMilestoneField.additionalOnclickScript =  "resynchDates('MILES','" + this.startField.id + "','" + this.startMilestoneField.id + "','" + this.durationField.id + "','" + this.endField.id + "','" + this.endMilestoneField.id + "');";
    this.endField.onblurOnDateValid =  "resynchDates('END','" + this.startField.id + "','" + this.startMilestoneField.id + "','" + this.durationField.id + "','" + this.endField.id + "','" + this.endMilestoneField.id + "');"+
      JSP.w(onBlurAdditionalScript);
    this.endMilestoneField.additionalOnclickScript = "resynchDates('MILES','" + this.startField.id + "','" + this.startMilestoneField.id + "','" + this.durationField.id + "','" + this.endField.id + "','" + this.endMilestoneField.id + "');";
    this.durationField.script = " autocomplete=\"off\" onBlur=\"" + "resynchDates('TASK_DURATION','" + this.startField.id + "','" + this.startMilestoneField.id + "','" + this.durationField.id + "','" + this.endField.id + "','" + this.endMilestoneField.id + "');"+
      JSP.w(onBlurAdditionalScript)+"\" ";

    this.urlToInclude =null;

  }

  public String getDiscriminator() {
    return DateDurationInput.class.getName();
  }

  public boolean validate(PageState pageState) throws IOException, ServletException {
    return true;
  }

  public void drawStart(PageContext pageContext) {
    startField.toHtml(pageContext);
  }

  public void drawStartMiles(PageContext pageContext) {
    startMilestoneField.toHtml(pageContext);
  }

  public void drawDuration(PageContext pageContext) {
    durationField.toHtml(pageContext);
  }


  public void drawEnd(PageContext pageContext) {
    endField.toHtml(pageContext);
  }

  public void drawEndMiles(PageContext pageContext) {
    endMilestoneField.toHtml(pageContext);
  }

  /**
   * @deprecated
   */
  public void toHtml(PageContext pageContext) {
    throw new RuntimeException("Call drawStart,drawStartMiles,drawDuration,drawEnd and drawEndMiles");
  }

}
