<%@ page buffer="164kb" %>
<%@ page import="com.twproject.setup.businessLogic.CreateEnvironmentActionController,
                 com.twproject.waf.TeamworkLoginScreen,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.OperatorConstants,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.display.Img,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new CreateEnvironmentActionController(pageState), request);
    TeamworkLoginScreen lw = new TeamworkLoginScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

  } else {

    PageSeed psc = pageState.thisPage(request);
    psc.setCommand("FORGOT_PWD");
    Form form = new Form(psc);
    form.alertOnChange = false;
    pageState.setForm(form);

    form.start(pageContext);

%>
  <table align="center" border="0">
    <tr><td style="max-width: 280px"><h4 style="font-weight: 300; text-transform: uppercase">Password recovery</h4><%


        if ("FORGOT_PWD".equals(pageState.command) && pageState.validEntries()) {
      %>An e-mail with a new password has been sent to your account.<br><br><%
      PageSeed loginPage = pageState.pageFromRoot("index.jsp");
      ButtonLink bl = new ButtonLink("Back to Twproject", loginPage);
      bl.additionalCssClass = "first big access full";
      bl.toHtml(pageContext);

    } else if ("resetPassword".equals(pageState.command) && pageState.validEntries()) {
      PageSeed login = pageState.pageFromRoot("security/login.jsp");
      ButtonLink lobu = new ButtonLink("login", login);
    %>
      Your password has been succesfully reset.<br>
      Prooceed with <%lobu.toHtmlInTextOnlyModality(pageContext);%>
      <%
      } else {



      %>This simple procedure will help you to recover your password.<br><br><%
      //Insert your login name and an e-mail will be sent to your account.

    %></td></tr><tr><td><%

      TextField login = new TextField(OperatorConstants.FLD_LOGIN_NAME, "");
      login.label = "";
      login.innerLabel = "Your login name";
      login.fieldSize = 20;
      login.required = true;
      login.fieldClass="formElements formElementsBig light";
      login.toHtml(pageContext);



  %></tr><tr><td height="70"><%
      //ButtonBar bb = new ButtonBar();
      ButtonSubmit forgot = new ButtonSubmit(form);
      forgot.label = "Get your password";
      forgot.additionalCssClass = "first big access full";
      forgot.variationsFromForm.command = "FORGOT_PWD";

      //bb.addButton(forgot);
      forgot.toHtml(pageContext);
    %></td><tr><td><%
        PageSeed loginPage = pageState.pageFromRoot("index.jsp");
        new ButtonLink("Maybe I remember it", loginPage).toHtmlInTextOnlyModality(pageContext);
      }


    %></td></tr></table>
<%

    form.end(pageContext);

  }
%>
