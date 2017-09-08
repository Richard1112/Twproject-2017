package org.jblooming.waf.html.input;

import org.jblooming.waf.constants.Fields;
import org.jblooming.utilities.JSP;

/**
 * @deprecated use CheckField
 */
public class CheckBox extends InputElement {

  public String fieldClass;
  public boolean putLabelFirst;
  public String valueCB ="on";



  public CheckBox(String fieldName, String separator, boolean putLabelFirst) {

    this(fieldName, fieldName, separator, null, false, false, null, putLabelFirst);
  }

  public CheckBox(String label, String fieldName, String separator, String htmlClass, boolean disabled, boolean readOnly, String script, boolean putLabelFirst) {
    id=fieldName;
    this.fieldName = fieldName;
    this.separator = separator;
    this.label = label;
    this.fieldClass = htmlClass;
    this.disabled = disabled;
    this.readOnly = readOnly;
    this.script = script;
    this.putLabelFirst = putLabelFirst;

  }

  public void addKeyPressControl(int keyToHandle, String launchedJs, String actionListened) {
    this.keyToHandle = keyToHandle;
    this.launchedJsOnActionListened = launchedJs;
    this.actionListened = actionListened;
  }

  public StringBuffer toHtmlStringBuffer() {

    StringBuffer sb = new StringBuffer(512);

    String fieldValue = value!=null ? value.stringValueNullIfEmpty() : null;

    if (putLabelFirst)
      sb.append(getLabelElement());

    if (value!=null && value.errorCode != null)
      sb.append(JSP.makeTag("font", "color=\"#ff0000\" size=\"1\"", " error"));

    if (putLabelFirst)
      sb.append(separator);

    sb.append("<input type=\"checkbox\"");
    if (fieldName != null && !fieldName.trim().equals(""))
      sb.append(" id=\"" + fieldName + "\"");
    if (fieldName != null && !fieldName.trim().equals(""))
      sb.append(" NAME=\"" + fieldName + "\"");

    if (fieldClass != null && !fieldClass.trim().equals(""))
      sb.append(" CLASS=\"" + fieldClass + '\"');

    if (fieldValue != null && !fieldValue.equals("") && !fieldValue.equals(Fields.FALSE))
      sb.append(" checked ");

    sb.append(" value=\""+valueCB+"\"" );
    sb.append(disabled ? " disabled" : "");
    sb.append(readOnly ? " readonly" : "");

    if (JSP.ex(toolTip))
      sb.append(" title=\""+toolTip+"\"");

    sb.append(script != null && script.length() > 0 ? ' ' + script : "");

    if (tabIndex != 0) {
      sb.append(" tabindex=\"").append(tabIndex).append('\"');
    }

    if (launchedJsOnActionListened != null) {
      sb.append(' ' + actionListened + "= \"if (event.keyCode==" + keyToHandle + ") {" + launchedJsOnActionListened);
      sb.append("}\n\"");
    }

    //if (preserveOldValue)
    //   sb. append(" oldValue=\"").append(JSP.w(JSP.ex(value.errorCode) ? "" :valueCB)).append('\"');
    if (preserveOldValue && !JSP.ex(value.errorCode)) {
      sb.append(" oldValue='1'");
    }


    sb.append('>');

    if (!putLabelFirst) {
      sb.append(separator);
      sb.append(getLabelElement());
    }


    return sb;
  }

}
