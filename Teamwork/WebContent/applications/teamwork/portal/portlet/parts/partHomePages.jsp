<%@ page import="com.opnlb.website.page.WebSitePage, com.twproject.operator.TeamworkOperator, org.jblooming.oql.OqlQuery, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.Container, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List" %><%

    PageState pageState = PageState.getCurrentPageState(request);
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


    Container homeCont = new Container();
    homeCont.title = I18n.get("INTRO_PICK_YOUR_HOME");
    homeCont.start(pageContext);

  %>
    <table width="100%" border="0">
      <tr>
        <td>
        <%

          String hql = "from " + WebSitePage.class.getName() + " as page where page.name!='ROOT'";
          OqlQuery oql = new OqlQuery(hql);
          List<WebSitePage> wsp = oql.list();

          boolean showSome = false;
          if (wsp.size() > 0) {
            %><table cellpadding="3" cellspacing="0" width="100%"><%
            for (WebSitePage p : wsp) {

              if ("introduction".equalsIgnoreCase(p.getName()) || "firstStart".equalsIgnoreCase(p.getName()))
                continue;

              if (!p.hasPermissionToSee(logged))
                continue;

             showSome = true;
        %>
         <tr><td style="border-bottom:1px solid gray; vertical-align:top" align="center">
            <i><%=p.getName()%></i></td><td ><%

            %><%=I18n.get(p.getDescription())%></td>

           <td nowrap style="border-bottom:1px solid gray" align="center"><%
            PageSeed see = new PageSeed(request.getContextPath() + "/" + p.getName() + ".page");
            see.setPopup(true);
            ButtonJS js = new ButtonJS("centerPopup('" + see.toLinkToHref() + "', 'sysCheckWindow', '1280', '600', 'yes','yes')");
            js.label= I18n.get("INTRO_SEE_IT");
            js.toHtmlInTextOnlyModality(pageContext);
            %> - <%
            see.command = "SET_AS_MY_HOME";
            see.setPopup(false);
            ButtonLink bl = new ButtonLink(I18n.get("INTRO_SET_AS_MY_HOME") , see);
            bl.toHtmlInTextOnlyModality(pageContext);

          %></td></tr><%
          }
          %></table>
          <%
          }

          if (!showSome) {
            %><%=I18n.get("INTRO_NO_CUSTOM_PAGE")%>.<%
          }
          %>

        </td>

      </tr>
    </table>
    <%

      homeCont.end(pageContext);

    %>