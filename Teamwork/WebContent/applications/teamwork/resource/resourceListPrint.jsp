<%@ page import="com.twproject.operator.TeamworkOperator,com.twproject.resource.Resource, com.twproject.resource.businessLogic.ResourceController, com.twproject.utilities.TeamworkComparators, com.twproject.waf.TeamworkPopUpScreen,
org.jblooming.anagraphicalData.AnagraphicalData, org.jblooming.messaging.MailHelper, org.jblooming.page.Page, org.jblooming.system.SystemConstants, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea,
org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n,
org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Collections, java.util.List"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(new ResourceController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);

    self.mainObjectId = pageState.mainObjectId;

    Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);

    Page resources = pageState.getPage();

    Img log = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");
    log.align="absmiddle";
    %><table border="0" width="100%" align="center" cellpadding="5" cellspacing="0">
          <tr>
            <td valign="middle"><%log.toHtml(pageContext);%></td>
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
    if (resources != null || pageState.getCommand() != null) {
       %><table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" class="table dataTable">
          <%

      for (Resource resource : (List<Resource>)resources.getAllElements()) {
        %><tr class="alternate">
            <td><%=JSP.w(resource.getCode())%>&nbsp;<%=JSP.w(resource.getName())%></td>



            <td><%
              boolean isEmpty = true;
              ArrayList<AnagraphicalData> orderAnagraphicalData = new ArrayList<AnagraphicalData>(resource.getAnagraphicalDatas());
              if (JSP.ex(orderAnagraphicalData.size())){  %>
              <div class="listData"><%
                Collections.sort(orderAnagraphicalData, new TeamworkComparators.AnagraphicalDataComparator());
                for (AnagraphicalData data : orderAnagraphicalData) {
              %><b><%=JSP.w(data.getLocationDescription())%></b>&nbsp;<%
                if (JSP.ex(data.getTelephone())){
              %>t. <%=data.getTelephone()%>&nbsp;&nbsp;&nbsp;&nbsp;<%
                }
                if (JSP.ex(data.getMobile())){
              %>m. <%=data.getMobile()%>&nbsp;&nbsp;&nbsp;&nbsp;<%
                }
                if (JSP.ex(data.getEmail())){
              %><a href="<%=MailHelper.mailToUrl(logged.getDefaultEmail(),data.getEmail(),"","")%>" target="_blank"><%=data.getEmail()%></a><%
                  }
                 %><br><%
                } %></div><%
                }
              %>
            </td>


          </tr><%
      } %>
   </table>
<%
    }
    f.end(pageContext);

  }
%>