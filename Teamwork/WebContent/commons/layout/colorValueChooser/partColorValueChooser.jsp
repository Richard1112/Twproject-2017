<%@ page import="org.jblooming.utilities.JSP, org.jblooming.utilities.StringUtilities, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.input.ColorValueChooser, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.layout.HtmlColors, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.List"%>
<%@ page import="java.util.Set" %>
<%

  ColorValueChooser cs = (ColorValueChooser) JspIncluderSupport.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);

  if (ColorValueChooser.INITIALIZE.equals(request.getAttribute(ColorValueChooser.ACTION))) {

  %><script type="text/javascript">

  (function() {
    var cvd={values:<%=cs.getCodeColorValues()%>};

    <%if (JSP.ex(cs.onChangeScript)){%>
    var callback=function(hidden,data){
      <%=JSP.w(cs.onChangeScript)%>
    };
    cvd.callback=callback;
    <%}%>
    _colorValueData["<%=cs.getType()%>"]=cvd;

  })();
</script>
<%
 //----------------------------------------------------------------- INPUT HIDDEN and BOX ----------------------------------------------------------------- --

} else {

  if (cs.label!=null) {
    %><label for="<%=cs.id%>"><%=cs.label%></label><%
  }
  if (cs.separator!=null) {
    %><%=cs.separator%><%
  }
%> <div class="cvcComponent <%=cs.multiSelect?"cvcMultiSelect":""%> <%=cs.displayValue?"cvcDisplayValue":""%> <%=cs.showOpener?"cvcShowOpener":""%>" style="<%=!cs.readOnly?"cursor:pointer;":""%><%=JSP.w(cs.style)%>" cvcType="<%=cs.getType()%>" <%=cs.displayValue?"displayValue='true'":""%>> <%

  String ceValue = pageState.getEntry(cs.fieldName).stringValue();

  boolean containsValue=false;

  //occore stare attenti se la ce è farlocca e viene da un JST
  if (JSP.ex(ceValue) && (ceValue.contains("(#=")||ceValue.contains("##"))){
    containsValue=true;
  } else {
    Set<String> ceValues = StringUtilities.splitToSet(JSP.w(ceValue), ",");
    for (ColorValueChooser.CodeColorValue ccv : cs.codeColorValues) {
      if (ceValues.contains(ccv.code)) {
        containsValue = true;
        break;
      }
    }
  }

  if (!cs.disabled) {

    if (!containsValue) {
      ceValue=cs.codeColorValues.get(0).code;
      pageState.addClientEntry(cs.fieldName, ceValue);
    }

    TextField hidden = new TextField("hidden", cs.fieldName, "", 2);
    hidden.label = "";
    hidden.preserveOldValue = cs.preserveOldValue;
    hidden.script= cs.script;
    hidden.toHtml(pageContext);
  }

  String choosenCode = JSP.w(ceValue).toUpperCase();
  List<String>choosenCodes=StringUtilities.splitToList(choosenCode,",");
  List<ColorValueChooser.CodeColorValue> codeColorValueChosen = new ArrayList<ColorValueChooser.CodeColorValue>();
  for (ColorValueChooser.CodeColorValue codeColorValue : cs.codeColorValues) {
    if (codeColorValue!=null && codeColorValue.code!=null && choosenCodes.contains(codeColorValue.code.toUpperCase())) {
      codeColorValueChosen.add(codeColorValue);
    }
  }
  if (codeColorValueChosen.size()==0) {
    codeColorValueChosen.add(cs.codeColorValues.get(0));
  }

    %><div class="cvcColorSquare"  <%if (!cs.disabled && !cs.readOnly) {%> onClick="cvc_clickColValSel($(this),event);" <%}%>><div class="cvcStatuses">
      <%
for (ColorValueChooser.CodeColorValue codeColorValue :codeColorValueChosen){
  %><div class="cvcSelBox" title="<%=codeColorValue.value%>" code="<%=codeColorValue.code%>" <%=cs.multiSelect||!cs.displayValue?"":"style=\"color:"+ HtmlColors.contrastColor(codeColorValue.color)+";background-color:"+codeColorValue.color+"\""%>><%
  if (cs.multiSelect||!cs.displayValue){
    %><span class="teamworkIcon" style="color:<%=codeColorValue.color%>">&#169;</span><%
  }
  if (cs.displayValue){
    %><span class="cvcDescription"><%=codeColorValue.value%></span><%
  }
%></div><%

}
%></div><%

  if (cs.showOpener) {
    %><span class="teamworkIcon menuArrow">&ugrave;</span><%
  }
%>
  </div>

</div><%

  }

%>
