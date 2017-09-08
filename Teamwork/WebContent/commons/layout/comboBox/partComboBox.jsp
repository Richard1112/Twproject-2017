<%@ page import="org.jblooming.utilities.JSP,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.input.ComboBox,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageState, net.sf.json.JSONObject" %><%

  ComboBox comboBox = (ComboBox) JspIncluderSupport.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);
  String choosenCode = pageState.getEntry(comboBox.fieldName).stringValueNullIfEmpty();

if (ComboBox.INITIALIZE.equals(request.getAttribute(ComboBox.ACTION))) {

  JSONObject json=new JSONObject();
  json.element("values",comboBox.values);
  json.element("type",comboBox.getType());

  String style = "display:none;position:absolute;background-color:#ffffff;border:#808080 1px solid;cursor:pointer;z-index:10;overflow:auto;";
  if (comboBox.style!=null)
    style= style +comboBox.style;
  style = style +(comboBox.divHeight>0 ?"height:" + comboBox.divHeight +"px;":"")+ (comboBox.divWidth>0? "width:"+comboBox.divWidth+"px;":"");
  json.element("style",style);
  json.element("onSelectScript",comboBox.onSelectScript);



%><script type="text/javascript">

  $(function() {
    var cdata=<%=json%>;
    _comboBoxData[cdata.type]=cdata;

  });
</script>
<%
    //----------------------------------------------------------------- INPUT HIDDEN and BOX ----------------------------------------------------------------- --

  } else if ("DRAW_INPUT".equals(request.getAttribute(ComboBox.ACTION))) {
    String script = "onfocus=\"cb_initializeCombo($(this)); \" "+JSP.w(comboBox.script);
    script += " onblur=\"cb_inputBlur($(this));\"";
    script += " onkeyDown=\"cb_keyDown ($(this),event.keyCode);\"";
    script += " onkeyUp=\"cb_keyUp ($(this),event.keyCode);\"";
    script += " autocomplete=\"off\"";
    script += " comboType=\""+comboBox.getType()+"\"";

    TextField textField = new TextField("text", comboBox.label, comboBox.fieldName, comboBox.separator, comboBox.fieldSize, comboBox.disabled, comboBox.readOnly, (comboBox.readOnly || comboBox.disabled) ? "" : script );
    textField.toolTip = comboBox.toolTip;
    textField.fieldClass=comboBox.htmlClass;
    textField.preserveOldValue = comboBox.preserveOldValue;
    textField.searchField=comboBox.forQBE;
    textField.entryType=comboBox.entryType;
    textField.required=comboBox.required;
    textField.innerLabel=comboBox.innerLabel;
    textField.script=textField.script + ( JSP.ex(comboBox.style)? " style=\""+comboBox.style+"\"":"");
    textField.toHtml(pageContext);

    String spanScript="";

    if(!(comboBox.readOnly || comboBox.disabled))
      spanScript = " onMouseDown=\"cb_clickOpener($(this));return false;\"";

    %><span class="teamworkIcon menuArrow" <%=spanScript%> style="cursor:pointer; margin-left: -15px;" title="<%=(comboBox.forQBE ? I18n.get("USE_QBE_IN_THIS_FIELD") : "")%>"><%=(comboBox.forQBE ? "<font style='font-family:arial;font-size:9px'>QBE</font>&ugrave;" : "&ugrave;")%></span><%

  }

%>
