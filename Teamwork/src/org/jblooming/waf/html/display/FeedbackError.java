package org.jblooming.waf.html.display;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.html.input.HtmlElement;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageState;

import javax.servlet.jsp.PageContext;
import java.io.IOException;

/**
 * @author Pietro Polsinelli : ppolsinelli@open-lab.com
 */
public class FeedbackError extends HtmlElement {
  public boolean translateError;
  public String errorCode;
  public String suggestedValue;

  public FeedbackError() {

  }

  public void toHtml(PageContext pageContext) {

    if (errorCode != null && errorCode.length() > 0) {
      try {
        if (suggestedValue != null && suggestedValue.length() > 0)
          errorCode = errorCode + ' ' + suggestedValue;

        if (translateError) {
          errorCode = I18n.get(errorCode);
        }
        String errorCodeAlert = JSP.javascriptEncode(errorCode);

        pageContext.getOut().write("<span id=\""+id+"error\" class=\"formElementExclamation\" error=\"1\" onclick=\"alert($(this).attr('title'))\" border='0' align='absmiddle' title=\""+errorCodeAlert+"\"></span>");

      } catch (IOException e) {
        throw new PlatformRuntimeException(e);

      }
    }

  }

  public StringBuffer toHtmlStringBuffer() {
    throw new PlatformRuntimeException("Do not use this");
  }
}
