<%@ page import=" org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.core.JspIncluder"
        %><%

  if (JspIncluder.INITIALIZE.equals(request.getParameter(Commands.COMMAND))) {
    /**
     * custom permissions MUST start with "Custom_"
     */
    //ApplicationState.getPermissions().add(new Permission("Custom_Permission 1"));

  }
%>