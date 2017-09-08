<%@ page import="org.jblooming.utilities.JSP,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.view.PageState"%><%

  CheckField cf = (CheckField) JspIncluderSupport.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);

  if(pageState.getEntry(cf.fieldName).stringValueNullIfEmpty()==null)
    pageState.addClientEntry(cf.fieldName, cf.trueFalseValues[1]);
  TextField tf = new TextField("hidden", cf.fieldName,"",1);
  tf.id = cf.id;
  tf.preserveOldValue = cf.preserveOldValue;
  tf.label = "";
  tf.toolTip="";
  tf.fieldClass="";
  tf.script="data-checkfield";

  if (JSP.ex(cf.selector)) {


  String script = " var selector=$('"+cf.selector+"');";

  String scriptCheckAll = script + "selector.each(function() {" +
          "this.checked = true;var name = this.id; name = name.substring('ck_'.length,name.length); var hidden = $('input#'+name);hidden.val('"+Fields.TRUE+"');});";

  String scriptUnCheckAll = script + "selector.each(function() {" +
          "this.checked = false;var name = this.id; name = name.substring('ck_'.length,name.length); var hidden = $('input#'+name);hidden.val('"+Fields.FALSE+"');});";


  cf.additionalOnclickScript = JSP.w(cf.additionalOnclickScript)+"if (this.checked==true) { "+scriptCheckAll+ " };";
  cf.additionalOnclickScript = cf.additionalOnclickScript+"if (this.checked==false) { "+scriptUnCheckAll+ " };";

  
  }

  if (cf.putLabelFirst && JSP.ex(cf.label)) {
    %><label <%=cf.generateToolTip()%> for="ck_<%=cf.id%>" <%=cf.generateToolTip()%>><%=JSP.w(cf.label)%></label><%=JSP.w(cf.separator)%><%
  }
  tf.toHtml(pageContext);

%><input type="checkbox" id="ck_<%=cf.id%>" name="ck_<%=cf.fieldName%>" <%=cf.trueFalseValues[0].equals(pageState.getEntry(cf.fieldName).stringValueNullIfEmpty()) ? "checked" : ""%> value="<%=cf.trueFalseValues[0]%>" <%=cf.disabled ? "disabled" : ""%> <%=cf.script==null || cf.script.trim().length()<=0 ? "" : cf.script%> <%=cf.generateToolTip()%>
onClick="$(this).prevAll('[type=hidden]:first').val(this.checked ? '<%=Fields.TRUE%>' : '<%=Fields.FALSE%>');<%=JSP.w(cf.additionalOnclickScript)%>" >
<%
  if (!cf.putLabelFirst && JSP.ex(cf.label)) {
    %><%=JSP.w(cf.separator)%><label <%=cf.generateToolTip()%> for="ck_<%=cf.id%>" <%=cf.generateToolTip()%>><%=JSP.w(cf.label)%></label><%=JSP.w(cf.separator)%><%
  }

%>