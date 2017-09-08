<%@ page import="org.jblooming.utilities.JSP, org.jblooming.waf.SessionState, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonsOnMouseover,
                 org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluder, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.input.HtmlElement" %><%

  ButtonsOnMouseover buttonsOnMouseover = (ButtonsOnMouseover) JspIncluderSupport.getCurrentInstance(request);
   //get the launcher id
  String launcherId = "";
  JspIncluder opener = buttonsOnMouseover.opener;
  if (opener instanceof HtmlElement)
     launcherId = ((HtmlElement) opener).id;
  else
    launcherId = ((JspHelper) opener).id;

  String divId = "butsomo_"+ buttonsOnMouseover.id;
  String additionalScript = "onmouseover=\"bjs_showButtonsOMO('" + divId + "', '" + launcherId + "');  \" " +
          " onmouseout=\"bjs_hideButtonsOMO('"+divId+"'); \"";


%> <div class="buttonsOnMouseOverWrapper">
  <span <%=additionalScript%>><%
  if (opener instanceof ButtonSupport && buttonsOnMouseover.openerInTextOnly)
    ((ButtonSupport)opener).toHtmlInTextOnlyModality(pageContext);
  else 
    opener.toHtml(pageContext);
%></span>
<div id="<%=divId%>" onmouseleave="div_hideButtonsOMOWithTimeout('<%=divId%>');"
     onmouseover="div_showButtonsOMO('<%=divId%>');" class="buttonsomo" style="display: none;position: absolute;" >

  <%
    if (JSP.ex(buttonsOnMouseover.buttonList)){
      for (JspIncluder jh: buttonsOnMouseover.buttonList)  {

      if (jh instanceof ButtonSupport) {
          ((ButtonSupport)jh).toHtmlInTextOnlyModality(pageContext);
      } else {
        jh.toHtml(pageContext);
      }
    }
   }
  %>
</div>
  </div>