package org.jblooming.waf.html.input;

import org.jblooming.utilities.CodeValue;
import org.jblooming.utilities.CodeValueList;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.CodeValueScriptList;
import org.jblooming.ontology.Node;
import org.jblooming.ontology.LookupSupport;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.constants.Fields;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.FindException;

import java.util.Iterator;
import java.util.TreeMap;
import java.util.List;
import java.util.Map;

public class Combo extends InputElement {

  public String initialSelectedCode;

  /**
   * if 0 its ignored
   */
  public int maxLenght;
  public CodeValueList cvl;
  public boolean sortEntries = false;
  public String additionalStyle;
  public String setJsOnChange;
  public String onBlurScript;
  public static final String OPTION_GROUP_START = "__OPTION_GROUP_START__";
  public static final String OPTION_GROUP_END = "__OPTION_GROUP_END__";


  private Combo() {
  }

  public Combo(String fieldName, String separator, String htmlClass, int maxLenght, CodeValueList cvl, String script) {
    this(null, fieldName, separator, htmlClass, maxLenght, null, cvl, script);
  }

  /**
   * This builds label from fieldName and view's client entries using i18n.
   */
  public Combo(String fieldName, String separator, String htmlClass, int maxLenght, String initialSelectedCode, CodeValueList cvl, String script) {
    this(fieldName, fieldName, separator, htmlClass, maxLenght, initialSelectedCode, cvl, script);
  }

  /**
   * initialSelectedCode is used only if  fieldName has no value in currentUrl's client entries.
   *
   * @param label
   * @param fieldName
   * @param separator
   * @param htmlClass
   * @param maxLenght
   * @param initialSelectedCode
   * @param cvl
   * @param script
   */
  public Combo(String label, String fieldName, String separator, String htmlClass, int maxLenght, String initialSelectedCode, CodeValueList cvl, String script) {
    this.label = label;
    this.fieldName = fieldName;
    this.id = fieldName;
    this.separator = separator;
    this.fieldClass = htmlClass;
    this.maxLenght = maxLenght;
    this.initialSelectedCode = initialSelectedCode;
    this.cvl = cvl;
    this.script = script;
  }

  public Combo(String label, String fieldName, String separator, String htmlClass, int maxLenght, String initialSelectedCode, List<Node> roots, String script) {
    this.label = label;
    this.fieldName = fieldName;
    this.id = fieldName;
    this.separator = separator;
    this.fieldClass = htmlClass;
    this.maxLenght = maxLenght;
    this.initialSelectedCode = initialSelectedCode;
    this.script = script;
  }


  public void addKeyPressControl(int keyToHandle, String launchedJs, String actionListened) {
    this.keyToHandle = keyToHandle;
    this.launchedJsOnActionListened = launchedJs;
    this.actionListened = actionListened;
  }

  public StringBuffer toHtmlStringBuffer() {

    StringBuffer sb = new StringBuffer();

    if (value != null && value.stringValueNullIfEmpty() != null && !JSP.ex(initialSelectedCode))
      initialSelectedCode = value.stringValueNullIfEmpty();

    if (label == null)
      label = fieldName;

    sb.append(getLabelElement());

    if (separator != null)
      sb.append(separator);

    sb.append("<SELECT").append(" NAME=\"").append(fieldName).append("\" ").append(" id=\"").append(fieldName).append('\"');

    if (JSP.ex(innerLabel))
      sb.append(" placeholder=\""+innerLabel+"\"");

    if (disabled || readOnly)
      sb.append(" disabled ");

    sb.append(script != null && script.length() > 0 ? " " + script : "");


    if (!JSP.ex(fieldClass)) {
      fieldClass = "formElements";
    }


    if (fieldClass != null && !fieldClass.equals("")) sb.append(" CLASS=\"").append(fieldClass).append('\"');

    if (tabIndex != 0) {
      sb.append(" tabindex=\"").append(tabIndex).append('\"');
    }

    if (launchedJsOnActionListened != null) {
      sb.append(' ' + actionListened + "= \"if (event.keyCode==" + keyToHandle + ") {" + launchedJsOnActionListened);
      sb.append("}\n\"");
    }
    if (setJsOnChange != null) {
      sb.append(" onChange=\"" + setJsOnChange + "\"");
    }
    if (JSP.ex(onBlurScript)) {
      sb.append(" onBlur=\"" + onBlurScript + "\"");
    }
    if (additionalStyle != null) {
      sb.append(" style=\"" + additionalStyle + "\"");
    }


    if (required)
      sb.append(" required=\"true\" ");

//    if (excludeFromAlert)
//      sb.append(" excludeFromAlert=\"true\" ");

    //if (preserveOldValue)
    //  sb.append(" oldValue=\"").append(JSP.w(JSP.ex(value.errorCode) ? "" : initialSelectedCode)).append('\"');
    if (preserveOldValue && !JSP.ex(value.errorCode)) {
      sb.append(" oldValue='1'");
    }

    if (JSP.ex(toolTip))
     sb.append(" title=\"").append(JSP.w(toolTip)).append("\"");

    sb.append(">\n");

    if (cvl != null && cvl.size() > 0) {
      printCVL(sb);
    }

    sb.append("</SELECT>");

    return sb;
  }

  private void printCVL(StringBuffer sb) {

     Map<String ,String> cvlsm=null;
    if (cvl instanceof CodeValueScriptList) {
      CodeValueScriptList cvls=(CodeValueScriptList)cvl;
      cvlsm=cvls.getCodeValuesScript();
    }

    if (sortEntries) {
      TreeMap<String, String> sm = new TreeMap<String, String>();

      CodeValue cv;
      Iterator i = cvl.iterator();
      while (i.hasNext()) {
        cv = (CodeValue) i.next();
        String display = cv.value;
        sm.put(display, (cv.code == null ? "" : cv.code));
      }

      i = sm.keySet().iterator();
      while (i.hasNext()) {
        String display = (String) i.next();
        String value = (String) sm.get(display);
        sb.append("<option ");

        if(cvlsm!=null){
          if(cvlsm.containsKey(value)){
            String script= (String)cvlsm.get(value);
            if(script!=null && script.trim().length()>0)
              sb.append(" "+script+" ");
          }
        }

        if (value.equals(initialSelectedCode)) {
          sb.append("selected ");
          //if (selectedStyle!=null)
          //  sb.append(" style=\"").append(selectedStyle).append("\"");
        }
        sb.append("value=\"").append(value).append("\">");
        if (maxLenght > 0 && display != null && display.length() > maxLenght)
          display = display.substring(0, maxLenght - 2) + "..";
        sb.append(display);
        sb.append("</option>\n");
      }

    } else {

      Iterator i = cvl.iterator();
      while (i.hasNext()) {
        CodeValue cv = (CodeValue) i.next();
        String code = (cv.code == null ? "" : cv.code);
        String display = cv.value;

        if (cv.value != null && cv.value.equals(OPTION_GROUP_START)){
          sb.append("<optgroup " );

          if(cvlsm!=null){
            if(cvlsm.containsKey(code)){
              String script= (String)cvlsm.get(code);
              if(script!=null && script.trim().length()>0)
                sb.append(" "+script+" ");
            }
          }

          sb.append(" label=\"").append(code).append("\">\n");
        }
        else if (cv.value != null && cv.value.equals(OPTION_GROUP_END))
          sb.append("</optgroup>\n");
        else if (cv.value != null) {
          sb.append("<option ");

          if(cvlsm!=null){
            if(cvlsm.containsKey(code)){
              String script= (String)cvlsm.get(code);
              if(script!=null && script.trim().length()>0)
                sb.append(" "+script+" ");
            }
          }

          sb.append(code.equalsIgnoreCase(initialSelectedCode) ? "selected " : "");
          sb.append("value=\"").append(code).append("\">");
          if (maxLenght > 0 && display != null && display.length() > maxLenght)
            display = display.substring(0, maxLenght - 2) + "..";
          sb.append(display);
          sb.append("</option>\n");
        }
      }
    }
  }


  public static Combo getYesNoInstance (String fieldName, String label, PageState pageState){
    return getYesNoInstance(fieldName, label, 0, pageState);
  }

  public static Combo getYesNoInstance (String fieldName, String label, int colspan, PageState pageState){
    CodeValueList cvl= new CodeValueList();
    cvl.add("","");
    cvl.add(Fields.TRUE,pageState.getI18n(Fields.TRUE));
    cvl.add(Fields.FALSE,pageState.getI18n(Fields.FALSE));
    String col = colspan>0 ? " colspan=\""+colspan+"\" " : "";
    Combo c=new Combo(fieldName,"</td><td "+col+">",null,3,cvl,null);
    c.label=label;
    return c;
  }

  public static Combo getLookupInstance(String fieldName, String label, Class<? extends LookupSupport> clazz) throws FindException {
    List <LookupSupport> ll= new OqlQuery("select a from "+clazz.getName()+" as a order by a.description").list();
    CodeValueList cvl = CodeValueList.getInstanceForIdentifiables(ll);
    Combo c=new Combo(fieldName,"</td><td>",null,30,cvl,null);
    c.label=label;
    return c;
  }

}
