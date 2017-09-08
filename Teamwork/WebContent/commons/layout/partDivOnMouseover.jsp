<%@ page import="org.jblooming.utilities.JSP,
                 org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.waf.html.container.DivOnMouseover,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.core.JspIncluder, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.input.HtmlElement" %><%

  DivOnMouseover divOnMouseover = (DivOnMouseover) JspIncluderSupport.getCurrentInstance(request);

  //get the launcher id
  String launcherId = "";
  JspIncluder opener = divOnMouseover.opener;
  boolean arrowOpener = divOnMouseover.arrowOpener;
  if (opener instanceof HtmlElement)
    launcherId = ((HtmlElement) opener).id;
  else
    launcherId = ((JspHelper) opener).id;

  String divId = "divdomo_"+ divOnMouseover.id;
  String eventType = arrowOpener ? "onmouseenter" : "onclick";
  String functionName = arrowOpener ? "bjs_showMenuDivOnMouseOver" : "bjs_showMenuDiv";
  String additionalScript = eventType + "=\""+functionName+"('" + divId + "', '" + launcherId + "');  \" " ;
  String additionalScriptarrowOpener = "onclick=\"bjs_showMenuDiv('" + divId + "', 'arrowOpener_" + divId + "'); event.stopPropagation(); event.preventDefault(); return false; \" " ;

%><span <%=!arrowOpener ? "class=\"menuArrow\"" : ""%> <%=!arrowOpener ? additionalScript : ""%>><%
  if (opener instanceof ButtonSupport && divOnMouseover.openerInTextOnly)
    ((ButtonSupport)opener).toHtmlInTextOnlyModality(pageContext);
  else
    opener.toHtml(pageContext);

  if(arrowOpener){
%><span id="arrowOpener_<%=divId%>"  class="arrowOpener menuArrow" <%=additionalScriptarrowOpener%>><span></span></span>
  <%}%>
</span>

<div id="<%=divId%>" class="divomo <%=JSP.w(divOnMouseover.additionalDropDownClass)%>"
     style=" position: absolute;left:-10000px; top:-10000px;">

  <div class="divomoArrow" ></div>

  <div><%
    if (divOnMouseover.content!=null)
      divOnMouseover.content.toHtml(pageContext);
    if (JSP.ex(divOnMouseover.buttonList)){
      for (JspIncluder jh:divOnMouseover.buttonList)  {

        if (jh instanceof ButtonSupport) {
          ((ButtonSupport)jh).toHtmlInTextOnlyModality(pageContext);
        } else {
          jh.toHtml(pageContext);
        }
      }
    }
  %></div>
</div>