package org.jblooming.waf.html.input;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.constants.Fields;

import javax.servlet.jsp.PageContext;
import java.util.List;
import java.util.Arrays;

public class CheckField extends JspHelper {

  public String label;
  public String fieldName;
  public boolean putLabelFirst;
  public String script;
  public String additionalOnclickScript="";
  public String separator;
  public boolean disabled = false;
  public boolean preserveOldValue = true;

  public String[] trueFalseValues = {Fields.TRUE, Fields.FALSE};

  public String selector = "";




  public CheckField(String fieldName, String separator, boolean putLabelFirst) {
    this(fieldName, fieldName, separator, putLabelFirst);
  }

  public CheckField(String label, String fieldName, String separator, boolean putLabelFirst) {
    this.urlToInclude = "/commons/layout/partCheckField.jsp";
    this.label = label;
    this.fieldName = fieldName;
    this.putLabelFirst = putLabelFirst;
    this.separator = separator;
    this.id = fieldName;
  }

  public void toHtmlI18n(PageContext pageContext) {
    PageState pageState = PageState.getCurrentPageState(pageContext);
    if (label == null)
      label = pageState.getI18n(fieldName);
    else
      label = pageState.getI18n(label);
    toHtml(pageContext);
  }


  public static CheckField getMasterCheckField(String fieldName, String... checkBoxesFieldPrefixes) {
    return getMasterCheckField(fieldName,Arrays.asList(checkBoxesFieldPrefixes));    
  }

  public static CheckField getMasterCheckField(String fieldName, List<String> checkBoxesFieldPrefixes) {

    CheckField toReturn = new CheckField(fieldName, "", false);

    boolean isFirst = true;
    for (String checkBoxesFieldName : checkBoxesFieldPrefixes) {

      if (!isFirst)
       toReturn.selector = toReturn.selector+",";

      isFirst = false;
      toReturn.selector = toReturn.selector +"input[id*=ck_" + checkBoxesFieldName + "][type=checkbox]:enabled ";
    }

    return toReturn;

  }



 }
