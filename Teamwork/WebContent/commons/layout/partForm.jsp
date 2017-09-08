<%@ page import="org.jblooming.utilities.JSP, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.state.Form" %><%

  Form.Drawer drawer = (Form.Drawer) JspIncluderSupport.getCurrentInstance(request);
  Form f = drawer.form;

  if (Form.FORM_START.equals(request.getAttribute(Form.Drawer.ACTION))) {
    %><form enctype="<%=f.encType%>" method="<%=(f.usePost ? "POST" : "GET")%>"
  action="<%=f.url.getHref()%>" name="<%=f.getUniqueName()%>" <%=f.alertOnChange ? "alertOnChange=\"true\"" : ""%> id="<%=f.getUniqueName()%>"  <%=JSP.w(f.additionalParams)%>
<%if(!f.w3cCompliant) {%> savedAction="" <%=JSP.ex(f.target)? "target=\""+f.target+"\"":""%>  savedTarget="" <%}%>  <%if (f.launchedJsOnActionListened != null) {%><%=JSP.w(f.actionListened)%>="if (event.keyCode==<%=JSP.w(f.keyToHandle)%>&&<%=(f.checkCtrlKey ? " event.ctrlKey==true " : " event.ctrlKey==false ")%>) { <%=JSP.w(f.launchedJsOnActionListened)%>;return false; event.preventDefault()}"<%}%>><%

    if(f.w3cCompliant) {
     %><div><%
    }

    %><%=f.getHref()%><%

  } else if (Form.FORM_END.equals(request.getAttribute(Form.Drawer.ACTION))) {

    StringBuffer st= new StringBuffer();
    for (String key:f.entriesCarrier){
      f.generateHiddenInput(st,key,f.url.getEntry(key).stringValueNullIfEmpty()); //shoudn't be f.generateHiddenInput(st,key,"")
    }

    %><%=st.toString()%><%
    if(f.w3cCompliant) {
      %></div><%
    }

    %></form><%
  }
%>
