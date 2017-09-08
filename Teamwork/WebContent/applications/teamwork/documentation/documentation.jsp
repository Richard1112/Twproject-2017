<%@ page import="com.twproject.waf.TeamworkHBFScreen" %>
<%@ page import="org.jblooming.utilities.HttpUtilities" %>
<%@ page import="org.jblooming.waf.ScreenArea" %>
<%@ page import="org.jblooming.waf.SessionState" %>
<%@ page import="org.jblooming.waf.constants.Fields" %>
<%@ page import="org.jblooming.waf.html.button.ButtonLink" %>
<%@ page import="org.jblooming.waf.html.button.ButtonSubmit" %>
<%@ page import="org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 java.io.File,
                 java.io.FileInputStream,
                 java.util.Properties, org.jblooming.security.License, org.jblooming.utilities.JSP" %>
<%
    PageState pageState = PageState.getCurrentPageState(request);
    if (!pageState.screenRunning) {
        pageState.screenRunning = true;

        final ScreenArea body = new ScreenArea(null, request);
        TeamworkHBFScreen lw = new TeamworkHBFScreen(body);

        lw.register(pageState);
        pageState.perform(request, response).toHtml(pageContext);
    } else {

%>

<style>
    html {
        background: #2F97C6 url(/img/twprojectIcon.svg) no-repeat right bottom;
        background-size: 50%;
        width: 100%;
        height: 100%;
    }

    .inlineContainerWrapper, .container {
      background-color: transparent;
    }

    h1, .inlineContainerWrapper .container, .inlineContainerWrapper TBODY {
        color: #fff;
    }


    .container.threeColumns a {
        color: #fff;
        text-decoration: underline
    }

    .aboutPage span.button.textual {
      text-decoration: underline;
      color: #fff;
      font-size: .8em;
      margin-top-top: 25px;
    }
    .aboutPage span.button.big {
      line-height: 17px;
      padding: 13px 0;
    }


</style>

<h1><%=I18n.get("TEAMWORK_HELP_LINKS")%>
</h1>

<div class="inlineContainerWrapper pTB-30 aboutPage">


    <div class="container threeColumns" style="padding-left: 0">
        <div class="doSearch">
            <%
                PageSeed pageSeed = new PageSeed("http://twproject.com/support/");
                pageSeed.disableCache=false;
                Form f = new Form(pageSeed);
                f.usePost=false;
                f.target = "_blank";
                f.start(pageContext);

                TextField wws = new TextField("s", "");
                //wws.innerLabel=I18n.get("TW_HELP_SEARCH");
                wws.fieldClass = "formElements";
                wws.fieldSize = 35;
                wws.label = "";
            %><h2><%=I18n.get("TW_HELP_SEARCH")%>
        </h2><%wws.toHtml(pageContext);%> <%

            ButtonSubmit doSearch = new ButtonSubmit(I18n.get("GO"), "", f);
            doSearch.additionalCssClass = "big first";
            doSearch.toHtml(pageContext);

            f.end(pageContext);
            // label added for adding some cusotm help references
            if (!I18n.get("CUSTOMIZED_HELP_CONTACT").equals("CUSTOMIZED_HELP_CONTACT")) {
        %>
            <hr>
            <%=I18n.get("CUSTOMIZED_HELP_CONTACT")%>
            <hr>
            <%
                }

            %>

        </div>
        <div class="panel pTB-30">
            <ul>
                <li><%=I18n.get("TW_HELP_INFOSITE")%> <a href="http://twproject.com" target="_blank" title="Twproject web site">twproject.com</a></li>
                <li><%=I18n.get("TW_HELP_GUIDE")%>: <a href="http://twproject.com/support/" target="_blank" title="Twproject user guide">twproject.com/support</a></li>
                <li><%=I18n.get("TW_HELP_FAQ")%>: <a href="http://twproject.com/support/category/faq/" target="_blank" title="Teamwork's FAQ">twproject.com/support/category/faq</a></li>
                <li><%=I18n.get("TW_HELP_ASK")%>: <a href="http://twproject.com/support/submit-a-ticket/" target="_blank" title="Twproject contacts">twproject.com/support/submit-a-ticket</a></li>
                <li><%=I18n.get("TW_HELP_NEWS")%>: <a href="http://twproject.com/blog/" target="_blank" title="Twproject news">twproject.com/blog</a></li>
            </ul>
        </div>
    </div>
    <div class="container threeColumns" style="padding-left: 35px">
        <div class="panel">

            <b>Direct contact:</b> <br>
            E-mail: info@twproject.com<br>
            Phone +39 055 5522779<br>


            <p>Twproject is produced since 2001 by <a href="http://www.open-lab.com" target="_blank"
                                                      title="Open Lab's site">Open Lab</a>.<br>
                <br><b>Company's HQ:</b><br>Open Lab, Via Venezia 18b 50121 Florence, Italy

                <b>Twproject's project directors:</b><br>Roberto Bicchierai, Pietro Polsinelli<br>
                <br>
                <b><%=I18n.get("APP_VERSION")%></b>
                <%=ApplicationState.getApplicationVersion()%><br><br><%



                    if (!Fields.TRUE.equals(ApplicationState.applicationParameters.get("TEAMWORK_ASP_INSTANCE"))) {
                        %>
                          <b>License:</b><br>
                          Customer code: <%=License.getLicense().customerCode%><br>
                          Number of clients: <%=License.getLicense().licenses%><br>
                          Expires: <%=JSP.w(License.getLicense().expires)%><%
                    }


                %>
                <br><%
                    ButtonLink.getBlackInstance("See trademarks used.", 400, 500, pageState.pageInThisFolder("legal.jsp", request)).toHtmlInTextOnlyModality(pageContext);%>
            </p>

        </div>
    </div>

</div>


<%
    }
%>
