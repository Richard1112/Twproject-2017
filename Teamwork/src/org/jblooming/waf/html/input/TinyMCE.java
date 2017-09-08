package org.jblooming.waf.html.input;

import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.html.button.ButtonSubmit;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.PageContext;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class TinyMCE extends JspHelper  {

  public static final String init = TinyMCE.class.getName();
  public TextArea textArea;          
  private String theme = THEME_SIMPLE;
  public String mode = "textareas"; // exact
  public String fieldName;
  public String width;
  public boolean disable = false;
  public String height;
  public String label;
  public String separator;
  public String sitePrefix = "";
  public String pluginBar = "theme_advanced_buttons3";
  public int fieldCols;
  public int fieldRows;
  public String external_image_list_url;  // "example_data/example_image_list.js"
  public String external_link_list_url;   // "example_data/example_link_list.js",
  public String content_css;              //"/example_data/example_full.css",
  public List<String> additionalPlugins = new ArrayList();  //"usableTags",
  public Map<String, String> initParameters = new HashMap();

  public boolean resize = false;
  public boolean limitTextSize = false;
  public boolean relativeUrls = true;
  public boolean includeScript = true;
  public boolean showHTMLButton = false;
  public boolean forceNewLinesAsParagraph = false;

  public boolean useTinyCustomSetupJSFunction = false;  // when true in the setup: parameter will call tinyCustomSetup function that MUST be in the scope

  public static String baseUrl;

  public static final String DRAW = "DRAW";
  public static final String CLOSE = "CLOSE";
  public static final String DISABLE = "DISABLE";

  public static final String MODE_EXACT = "exact";  // see also textareas
  public static final String THEME_ADVANCED = "advanced";
  // in this case tiny configuration MUST be manually built
  public static final String THEME_CUSTOMIZED = "customized";
  public static final String THEME_SIMPLE = "simple";

  /**
   * added for images deletion in imagesUploader.jsp (OL TinyMCE plugin)
   */
  public String objectClass;
  public String imageSearchField;
  public boolean readOnly;
  public boolean required;

  // WARNING!! in case tiny is inserted into an unfocused tab the textarea MUST have setted width and height

  // Attenzione!!! allo stato attuale lo script della textarea può essere sovrascritto

  public TinyMCE(String label, String fieldName, String separator, String width, String height, PageState pageState) {
    if (id != null)
      this.id = fieldName;
    this.setTheme(TinyMCE.THEME_SIMPLE);
    this.fieldName = fieldName;
    this.urlToInclude = "/commons/layout/partTinyMCE.jsp";
    this.textArea = new TextArea(label, fieldName, separator, fieldCols, fieldRows, "");
    this.textArea.preserveOldValue = false;
    this.textArea.id = this.id;
    if (JSP.ex(width,height))
      this.textArea.script = " style=\"width:" + width + "; height:" + height + ";\" ";

    this.textArea.script+=" withTinyMCE";
  }

  public String getDiscriminator() {
    return init;
  }

  private void init(PageContext pageContext) {
    PageState ps = PageState.getCurrentPageState(pageContext);
    if (!ps.initedElements.contains(init)) {
      pageContext.getRequest().setAttribute(ACTION, INITIALIZE);
      super.toHtml(pageContext);
      ps.initedElements.add(init);
    }
  }

  /**
   * @param f
   * @return a ButtonSubmit with the additionalOnClickScript initialized as needed by HtmlArea to persist content while browsing through tabs
   */
  public static ButtonSubmit getButtonSubmit(Form f) {
    ButtonSubmit submit = new ButtonSubmit(f);
    return submit;
  }

  public void toHtml(PageContext pageContext) {
    if (this.disable) {
      pageContext.getRequest().setAttribute(ACTION, DISABLE);
    } else {
      init(pageContext);
      pageContext.getRequest().setAttribute(ACTION, DRAW);
    }
    super.toHtml(pageContext);
  }

  public TinyMCE() {
  }

  public static String tinyCodeCleaner(String text) {
    return tinyCodeCleaner(text, null);
  }

  public static String tinyCodeCleaner(String text, String pathIncludedContext) {
    if (text != null) {
      if (baseUrl == null)
        baseUrl = "";
      // href code
      text = StringUtilities.replaceAllNoRegex(text, baseUrl.toLowerCase(), "");
      text = StringUtilities.replaceAllNoRegex(text, baseUrl.toUpperCase(), "");
      text = StringUtilities.replaceAllNoRegex(text, "class=\"mceVisualAid\"", "");
      text = StringUtilities.replaceAllNoRegex(text, "<P mce_keep=\"true\">&nbsp;</P>", "");
      text = StringUtilities.replaceAllNoRegex(text, "mce_keep=\"true\"", "");
      text = StringUtilities.replaceAllNoRegex(text, "mceItemAnchor", "");
      text = StringUtilities.replaceAllNoRegex(text, "´", "'");

      // tinyMCE added code
      text = text.replaceAll("mce[\\w]+=\\\"[^> ]+\\\"", "");
      // empty fields
      if ("<br>".equalsIgnoreCase(text))
        text = "";

      text = StringUtilities.replaceAllNoRegex(text, "\r\n", "\n");
      text = text.replaceAll("\\n{2,}","\n");
    }
    return text;
  }

  public static String getApplicationBaseURL(HttpServletRequest request) {
    String protocol = request.getScheme();
    String server = request.getServerName();
    String port = "" + request.getServerPort();
    String contextPath = request.getContextPath();
    String slash = "";
    if ((!("/".equals(contextPath)) || "".equals(contextPath.trim()))) {
      slash = "/";
    }
    if ("80".equals(port))
      port = "";
    else
      port = ":" + port;

    String redirect = protocol + "://" + server + port + request.getContextPath() + slash;

    return redirect;
  }


  public String getTheme() {
    return theme;
  }

  public void setTheme(String theme) {
    this.theme = theme;

    this.initParameters=new HashMap<String, String>();

    addParameter("theme", "advanced"); // era scritto a fuoco nella jsp

    addParameter("valid_elements", "*[*]");
    addParameter("element_format", "html");
                            
    addJSParameter("cleanup","true");

    // moved to partTinyMCE.jsp : it can be setted while init each single tiny
//    addJSParameter("force_br_newlines","true");
//    addJSParameter("forced_root_block","false");

    addParameter("theme_advanced_toolbar_location", "top");
    addParameter("theme_advanced_toolbar_align", "left");

    addParameter("theme_advanced_buttons1", "bold,italic,underline");


    if (THEME_ADVANCED.equals(theme)) {
      addParameter("theme_advanced_statusbar_location", "bottom");
      addParameter("external_image_list_url", external_image_list_url);
      addParameter("external_link_list_url", external_link_list_url);
      addParameter("content_css", content_css);
      addParameter("extended_valid_elements", "hr[class|width|size|noshade],font[face|size|color|style],span[class|align|style],style");
      // preview doesn't work
      //addParameter("plugins","emotions,imagesUploader,videoEmbedder,inlinepopups, style,table,preview,searchreplace,print,contextmenu,paste,noneditable,fullscreen");
      addParameter("plugins","inlinepopups, style,table,searchreplace,print,contextmenu,paste,noneditable,fullscreen");
      addParameter("theme_advanced_blockformats", "p,h1,h2,h3,h4,h5,h6");
      addParameter("theme_advanced_buttons1", "strikethrough,separator,justifyleft,justifycenter,justifyright,justifyfull,bullist,numlist,outdent,indent,separator,sub,sup,charmap,separator");
      addParameter("theme_advanced_buttons2", "tablecontrols,separator,formatselect,forecolor,cut,copy,paste");
      // preview doesn't work
      //addParameter("theme_advanced_buttons3", "pastetext,pasteword,separator,link,unlink,anchor,separator,imagesUploader,videoEmbedder,separator,undo,redo,preview,fullscreen,emotions");
      addParameter("theme_advanced_buttons3", "pastetext,pasteword,separator,link,unlink,anchor,separator,imagesUploader,videoEmbedder,separator,undo,redo,fullscreen");
      additionalPlugins.add("FCode");

      // simple configuration
    } else if (THEME_SIMPLE.equals(theme)) {
      //addParameter("plugins", "paste");
      //addParameter("theme_advanced_buttons1", "FCode, separator, pastetext,pasteword, separator,link,unlink, separator");
      addParameter("theme_advanced_buttons1", "FCode, separator,link,unlink, separator"); 
      addParameter("theme_advanced_buttons2", "");
      addParameter("theme_advanced_buttons3", "");
      addParameter("theme_advanced_buttons4", "");
    }
  }


  /**
   * append or create a parameter
   * inside a comma delimited string escape the parameter if it not already exists
   * if the initial value is theme_advanced_buttons1 : "bold,italic,underline,strikethrough" and you appendEscapedParameter("theme_advanced_buttons1","link"), the result will be
   * theme_advanced_buttons1 : "bold,italic,underline,strikethrough,link"
   * <p/>
   * if you want to add variable parameter use the setJSParameter() that will not escape the string
   *
   * @param param
   * @param value
   */
  public void addParameter(String param, String value) {
    if (value != null) {
      String val = initParameters.get(param);
      if (JSP.ex(val)&& !"\"\"".equals (val)) {
        initParameters.put(param, val.substring(0, val.length() - 1) + "," + value + "\"");
      } else {
        initParameters.put(param, "\"" + value + "\"");
      }
    }
  }

  /**
   * add or replace a initParameter e.g.: initParameters.put("apply_source_formatting","false") this will be rendered as "apply_source_formatting : false" or
   * setup : myFunctionToDoSomething
   * @param param
   * @param value
   */
  public void addJSParameter(String param, String value) {
    if (value != null) {
      String val = initParameters.get(param);
      if (JSP.ex(val)) {
        initParameters.put(param, val + "," + value );
      } else {
        initParameters.put(param, value );
      }
    }
  }
}
