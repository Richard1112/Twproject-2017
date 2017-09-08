<%@ page import ="org.jblooming.utilities.JSP, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.button.ButtonLink"%><%
  ButtonSupport button = (ButtonSupport) JspIncluderSupport.getCurrentInstance(request);
  String textual=ButtonSupport.GRAPHICAL.equals(button.outputModality)?"":"textual";
  String focus=button.hasFocus?"focused":"";
  String disabled = !button.enabled ? "disabled" : "";
  String style = JSP.w(button.style) + (JSP.ex(button.width) ? "width:" + JSP.w(button.width) + ";" : "");
  String icon = JSP.ex(button.iconChar) && !JSP.ex(button.label) ? "icon" : "";

  String htmlTag="span";
  String action;
  if (button instanceof ButtonLink && (!JSP.ex(((ButtonLink)button).popup_width) && button.enabled)){
    htmlTag="a";
    ButtonLink bl=(ButtonLink)button;
    action=bl.pageSeed.getHref();
    if (!bl.inhibitParams) {
      action+="?"+bl.getCEHref();
    }

    action="href=\""+action+"\"";
    action+= JSP.ex(button.target)?" target=\""+button.target+"\"":"";
  } else {
    action=button.generateLaunchJs();
  }

%><<%=htmlTag%> class="button <%=button.noPrint?"noprint":""%> <%=textual%> <%=icon%> <%=focus%> <%=JSP.w(button.additionalCssClass)%>" <%=disabled%> id="<%=button.getId()%>" style="<%=JSP.w(style)%>" <%=button.generateToolTip()%> <%=action%>><%
  if (JSP.ex(button.iconChar)){
    %><span class='teamworkIcon <%=JSP.ex(button.label)?"withLabel":""%>'><%=button.iconChar%></span><%
  }
%><%=button.label%></<%=htmlTag%>>

