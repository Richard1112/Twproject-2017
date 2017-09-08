<%@ page import="com.twproject.resource.Person,
                 com.twproject.resource.Resource,
                 com.twproject.resource.businessLogic.ResourceController,
                 com.twproject.utilities.TeamworkComparators,
                 com.twproject.waf.TeamworkPopUpScreen,
                 org.jblooming.anagraphicalData.AnagraphicalData,
                 org.jblooming.ontology.PerformantNodeSupport,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.display.Img,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.ApplicationState" %>
<%@ page import="org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Collections, java.util.Iterator" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new ResourceController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {


    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.EDIT);
    self.mainObjectId = pageState.mainObjectId;

    Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);

    Resource resource = (Resource) pageState.getMainObject();

    Img log = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");
%>
<table border="0" width="100%" align="center" cellpadding="5" cellspacing="0">
  <tr>
    <td align="left" width="90%"><h3><%log.toHtml(pageContext);%></h3></td>
    <td align="right"><%

      ButtonJS print = new ButtonJS("window.print();");
      print.label = "";
      print.toolTip = I18n.get("PRINT_PAGE");
      print.iconChar = "p";
      print.toHtmlInTextOnlyModality(pageContext);

    %></td>
  </tr>
</table>
<%
  JspHelper helper = new JspHelper("/applications/teamwork/resource/part/partResourceEditorPrint.jsp");
  helper.parameters.put("resource", resource);
  helper.toHtml(pageContext);

  %><%

  if (pageState.getEntry("PRINT_ASSIGNMENTS").checkFieldValue()){
    helper = new JspHelper("/applications/teamwork/resource/part/partPrintAssignments.jsp");
    helper.parameters.put("resource", resource);
    helper.toHtml(pageContext);
    %><%
  }

%>
<table border="0" width="99%" align="center" cellpadding="3" cellspacing="0">

  <%
    if (resource.getChildrenSize() > 0) {

 %><tr>
  <td colspan="6"><hr><h2><%=I18n.get("CHILDREN")%></h2></td>
</tr><%
      for (Iterator<PerformantNodeSupport> iterator = resource.getChildrenIterator(); iterator.hasNext();) {
        PerformantNodeSupport child = iterator.next();
  %>
  <tr>
    <%
      String type = null;
      if (child instanceof Person)
        type = I18n.get("PERSON");
      else
        type = I18n.get("COMPANY");
    %>
    <td style="padding-top: 20px">
      <h3 style="margin-bottom: 5px"><%=child.getName()%> (<%=type%>)</h3>
    </td>
  </tr>

  <tr>
    <td style="border-bottom:1px dotted #999999;">
      <%
        ArrayList<AnagraphicalData> orderAnagraphicalData = new ArrayList<AnagraphicalData>(((Resource) child).getAnagraphicalDatas());
        Collections.sort(orderAnagraphicalData, new TeamworkComparators.AnagraphicalDataComparator());

        for (AnagraphicalData data : orderAnagraphicalData) {
      %><b><%=JSP.w(data.getLocationDescription())%></b><%
      if (JSP.ex(data.getTelephone())){
    %> - t. <%=data.getTelephone()%>&nbsp;&nbsp;&nbsp;&nbsp;<%
      }
      if (JSP.ex(data.getMobile())){
    %> - m. <%=data.getMobile()%>&nbsp;&nbsp;&nbsp;&nbsp;<%
      }
      if (JSP.ex(data.getEmail())){
    %> - <%=data.getEmail()%><%
        }

      } %>

    </td>
  </tr>
  <%
      }
    }
  %>
</table>
<%
    f.end(pageContext);

  }
%>
