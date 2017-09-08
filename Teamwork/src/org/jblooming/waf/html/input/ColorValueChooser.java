package org.jblooming.waf.html.input;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.layout.HtmlColors;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageState;

import javax.servlet.jsp.PageContext;
import javax.servlet.ServletException;
import java.util.List;
import java.util.ArrayList;
import java.io.IOException;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 16-feb-2006 : 15.05.54
 */
public class ColorValueChooser extends JspHelper {

  /**
   * in case you want to use several ColorValueChooser on the same page, you will need to have distinct div in the initialize phase
   */  
  protected String type = "STATUS";

  public String fieldName;

  /**
   * this is used in the hidden fieed of this componente
   */
  public String script;

  public int height = 26;
  public int width= 120;

  public boolean showOpener=false;

  /**
   * list of color like "#ff6655"
   */
  public List<CodeColorValue> codeColorValues = new ArrayList();

  public boolean disabled = false;
  public boolean readOnly = false;
  public boolean displayValue = true;
  public String label;
  public String separator;

  // in the onChangeScript you can use a var called "hidden" to acces the hidden field and
  // "data" = {code:[id],value:[text content of selected row], color:[color of selected row], textColor: [tasx color],index:[position of the row]}
  public String onChangeScript;
  public boolean preserveOldValue = true;
  public String style;
  public boolean alreadyInited=false;

  public boolean multiSelect=false;

  protected ColorValueChooser() {
  }

  @Deprecated
  public ColorValueChooser(String fieldName, String type, PageState pageState) {
    this(fieldName,type);
  }

  public ColorValueChooser(String fieldName, String type) {
    this.urlToInclude = "/commons/layout/colorValueChooser/partColorValueChooser.jsp";
    this.fieldName = fieldName;
    this.type=type;
  }


  public void addCodeColorValue(String code, String color, String value){
    CodeColorValue ccv = new CodeColorValue();
    ccv.code = code;
    ccv.color = color;
    ccv.value=value;
    codeColorValues.add(ccv);
  }

   public String getDiscriminator() {
    return this.getClass().getName()+type;
  }

  public boolean validate(PageState pageState) throws IOException, ServletException {
    return true;
  }

  public void init(PageContext pageContext) {
    PageState ps = PageState.getCurrentPageState(pageContext);
    if (!ps.initedElements.contains(getDiscriminator()) &&!alreadyInited) {
      pageContext.getRequest().setAttribute(ACTION, INITIALIZE);
      super.toHtml(pageContext);
      ps.initedElements.add(getDiscriminator());
      alreadyInited=true;
    }
  }

  public void toHtml(PageContext pageContext) {
    init(pageContext);
    pageContext.getRequest().setAttribute(ACTION, "VAI");
    super.toHtml(pageContext);
  }

  public void toHtmlI18n(PageContext pageContext) {
    label= I18n.get(fieldName);
    toHtml(pageContext);
  }


  public class CodeColorValue {
    public String code;
    public String color;
    public String value;

    public JSONObject jsonify(){
      JSONObject ret= new JSONObject();
      ret.element("code",code);
      ret.element("color",color);
      ret.element("value",value);
      ret.element("textColor", HtmlColors.contrastColor(color));
      return ret;
    }
  }


  public String getType() {
    return type;
  }
  public void setType(String type) {
    this.type=type;
  }

  public JSONArray getCodeColorValues(){
    //JSONObject ret= new JSONObject();
    JSONArray ret= new JSONArray();
    int i=0;
    for (CodeColorValue ccv: codeColorValues) {
      i++;
      JSONObject ccvJs = ccv.jsonify();
      ccvJs.element("index",i);
      ret.add(ccvJs);
    }
    return ret;
  }


  public void addChoose(){
    CodeColorValue ccv = new CodeColorValue();
    ccv.code = "";
    ccv.color = "#808080";
    ccv.value= I18n.get("EDITOR_CHOOSE");
    codeColorValues.add(0,ccv);
  }

}
