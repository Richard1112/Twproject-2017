<%@ page import="com.twproject.worklog.Worklog, org.jblooming.designer.DesignerField, org.jblooming.waf.html.input.TextArea, org.jblooming.waf.settings.I18n,org.jblooming.waf.view.PageState, com.twproject.task.Assignment, org.jblooming.waf.html.core.JspHelper, org.jblooming.utilities.JSP, com.twproject.worklog.businessLogic.WorklogBricks" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  Assignment ass = null;
  Worklog wl = null;

  JspHelper wlDesFrom = (JspHelper) JspHelper.getCurrentInstance(request);
  if (wlDesFrom != null) {

    ass = (Assignment) wlDesFrom.parameters.get("assig");
    wl = (Worklog) wlDesFrom.parameters.get("worklog");

  } else {
    ass = Assignment.load(pageState.getEntry("assId").intValueNoErrorCodeNoExc() + "");
    int wlId = pageState.getEntry("wlId").intValueNoErrorCodeNoExc();
    if(wlId!=0)
       wl = Worklog.load(wlId + "");

  }

if(JSP.ex(wl)) {
  pageState.addClientEntry("WORKLOG_ACTION", wl.getAction());
  ass=wl.getAssig();
} else {
  //se sto aggiungendo creo un wl fittizio in modo da rendere possibile il isVisibleIf dei custom fields
  wl = new Worklog();
  wl.setIdAsNew();
  wl.setAssig(ass);
}

%><table class="table"> <tr><td style="padding: 5px 0 0;">
  <%
    TextArea tfDescr = new TextArea("WORKLOG_ACTION", "", 20, 3, "formElements wlDescr");
    tfDescr.label = "";
    tfDescr.fieldSize = 12;
    tfDescr.maxlength = 1900;
    tfDescr.toolTip = I18n.get("WORKLOG_ACTION");
    tfDescr.innerLabel =  I18n.get("WORKLOG_ACTION");
    tfDescr.toHtml(pageContext);

  %></td></tr><%

  if (DesignerField.hasCustomField("WORKLOG_CUSTOM_FIELD_",4)){
%><tr><td><%


  for (int i = 1; i < 5; i++) {
    DesignerField dfStr = DesignerField.getCustomFieldInstance("WORKLOG_CUSTOM_FIELD_", i, wl, false, false, false, pageState);

    if (dfStr != null) {
      %><div style="display: inline-block"><%
      dfStr.separator = "<br>";
      dfStr.toHtml(pageContext);
      %></div><%
    }
    %>&nbsp;&nbsp;<%
  }
%></td></tr><%
  }
%></table>
