<%@ page import="org.jblooming.operator.Operator, org.jblooming.utilities.JSP, org.jblooming.utilities.StringUtilities,
org.jblooming.waf.ScreenBasic, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar,
org.jblooming.waf.html.container.Container, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form,
org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  Operator logged = pageState.getLoggedOperator();
  if (!logged.hasPermissionAsAdmin())
    throw new SecurityException("Hahahahahahahaha!!!!");

  if (!pageState.screenRunning) {

    ScreenBasic.preparePage(pageContext);

    pageState.perform(request, response).toHtml(pageContext);

  } else {

    ClientEntry valueCE = pageState.getEntry("TEXT");
    String value = valueCE.stringValueNullIfEmpty();
    String result = "";

    if (JSP.ex(value)) {
      if ("ENC".equals(pageState.command)) {
        result = StringUtilities.encrypt(value);
      } else if ("DEC".equals(pageState.command)) {
        try {
          result = StringUtilities.decrypt(value);
        } catch (Throwable t) {
          valueCE.errorCode=t.getMessage();
        }
      }
    }
    PageSeed self = pageState.thisPage(request);
    self.setCommand("ENC");
    Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);

    Container encDec = new Container();
    encDec.title = "EncDec";

    encDec.start(pageContext);

%>
<table cellspacing="5" height="50">
  <tr>
    <td>
      <%
      TextField tf = new TextField("TEXT", "text", "TEXT", "</td><td>", 30, false);
      pageState.setFocusedObjectDomId(tf.id);
      tf.toHtml(pageContext);
      %>
    <td>
  </tr>
  <td colspan="2"><%=result%>
  </td>
  <tr>

  </tr>
</table>
<%
    ButtonBar bb = new ButtonBar();

    ButtonSubmit enc = new ButtonSubmit(f);
    enc.label = "encript";
    enc.variationsFromForm.command = "ENC";

    bb.addButton(enc);

    ButtonSubmit dec = new ButtonSubmit(f);
    dec.label = "decript";
    dec.variationsFromForm.command = "DEC";
    bb.addButton(dec);

    bb.toHtml(pageContext);


    encDec.end(pageContext);

    f.end(pageContext);

  }
%>