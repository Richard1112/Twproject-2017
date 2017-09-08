<%@ page import="org.jblooming.utilities.JSP,
                 org.jblooming.waf.html.button.ButtonImg,
                 org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.core.JspIncluderSupport" %><%
  ButtonImg buttonImg = (ButtonImg) JspIncluderSupport.getCurrentInstance(request);
  ButtonSupport button=buttonImg.button;

  String focus=button.hasFocus?"focused":"";
  String disabled = !button.enabled ? "disabled" : "";
  String style = JSP.w(button.style)+" "+ (JSP.ex(button.width) ? "width:" + JSP.w(button.width) + ";" : "");

  if (buttonImg.image != null) {
    %><span class="button buttonImg <%=focus%> <%=JSP.w(button.additionalCssClass)%>" <%=disabled%> id="<%=button.getId()%>" style="<%=style%>" <%=button.generateToolTip()%> <%=button.generateLaunchJs()%>><%buttonImg.image.toHtml(pageContext);%><%=JSP.ex(button.label)?" "+button.label:""%></span><%
  }
%>
