package org.jblooming.waf.html.state;

import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.operator.Operator;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.JSP;

import java.util.Hashtable;
import java.util.Map;


public class ScreenElementStatus {

  public String containerId;
  public int x, y;
  public String w, h;
  public boolean collapsed;
  public boolean hidden;
  public boolean iconized;
  public String status;
  public String focusedId;

  public final static String SES_QUALIFIER = "__SESQL__";

  public ScreenElementStatus(String id) {
    validate(id);
    containerId = id;
  }

  private void validate(String candidate) {
    if (candidate.indexOf("|") > -1 || candidate.indexOf(SES_QUALIFIER) > -1)
      throw new PlatformRuntimeException("You should not use | or " + SES_QUALIFIER + "in keys");
  }

  public String toPersistentString(String key) throws ApplicationException {
    validate(key + containerId);
    return key + '|' + containerId + '|' + x + '|' + y + '|' + JSP.w(w) + '|' + JSP.w(h) + '|' + collapsed + '|' + hidden + '|' + iconized + '|' + status + '|' + focusedId;
  }

  public static Map getInstanceFromOptions(Operator op) throws ApplicationException {

    Map screenElementsStatus = new Hashtable();

    if (JSP.ex(op.getOptions())) {

        for (String opkey: op.getOptions().keySet()){

        if (opkey.indexOf(SES_QUALIFIER) == 0) {
          String s[] = StringUtilities.splitToArray((String) op.getOption(opkey), "|");
          String key = s[0];
          String containerId = s[1];
          ScreenElementStatus ses = new ScreenElementStatus(containerId);
          ses.x = 0;
          ses.y = 0;
          try { ses.x = Integer.parseInt(s[2]); } catch (Throwable t){}
          try { ses.y = Integer.parseInt(s[3]); } catch (Throwable t){}
          ses.w = s[4];
          ses.h = s[5];
          ses.collapsed = Boolean.valueOf(s[6]).booleanValue();
          ses.hidden = Boolean.valueOf(s[7]).booleanValue();
          ses.iconized = Boolean.valueOf(s[8]).booleanValue();
          ses.status = s[9];
          ses.focusedId = s[10];
          screenElementsStatus.put(key, ses);
        }
      }
    }
    return screenElementsStatus;
  }

}
