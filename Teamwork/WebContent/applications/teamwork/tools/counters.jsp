<%@ page import="com.twproject.waf.TeamworkPopUpScreen, org.jblooming.oql.OqlQuery, org.jblooming.uidgen.Counter, org.jblooming.uidgen.CounterHome, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.html.table.ListHeader, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, org.jblooming.operator.Operator, org.jblooming.security.PlatformPermissions, com.twproject.security.TeamworkPermissions, org.jblooming.ontology.businessLogic.DeleteHelper" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  Operator logged = pageState.getLoggedOperator();

  boolean canCreate = logged.hasPermissionFor(TeamworkPermissions.classificationTree_canManage);
  boolean canManage = logged.hasPermissionFor(TeamworkPermissions.classificationTree_canManage);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    if (Commands.SAVE.equals(pageState.getCommand())) {
      if (canCreate) {
        String cName = pageState.getEntry("NEWCOUNTER").stringValueNullIfEmpty();
        if (cName != null) {
          String prefix = pageState.getEntry("COUNTERPREFIX").stringValueNullIfEmpty();
          if (prefix != null)
            cName += "_" + prefix;

          //find if existing
          Counter c = (Counter) new OqlQuery("from " + Counter.class.getName() + " as c where c.name = 'COUNT_" + cName + "'").uniqueResultNullIfEmpty();
          if (c != null) {
            pageState.getEntry("NEWCOUNTER").errorCode = "Name is already in use";
          } else {
            int startValue = pageState.getEntry("NEWCOUNTERVALUE").intValueNoErrorCodeNoExc();
            CounterHome.setCounterSeed("COUNT_" + cName, startValue);
          }
        }
      }
    } else if ("GENERATE".equals(pageState.getCommand())) {
      String cName = pageState.mainObjectId.toString();
      CounterHome.next(cName);
    } else if ("RESET".equals(pageState.getCommand())) {
      if (canManage) {
        String cName = pageState.mainObjectId.toString();
        CounterHome.setCounterSeed(cName, 0);
      }
    } else if ("DELETE".equals(pageState.getCommand())) {
      if (canManage) {
        String cId = pageState.mainObjectId.toString();
        Counter counter = CounterHome.findByPrimaryKey(cId);
        //counter.remove();
        DeleteHelper.cmdDelete(counter, pageState);
      }
    }

    pageState.toHtml(pageContext);

  } else {

    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);
    self.mainObjectId = "-";
    Form f = new Form(self);
    f.alertOnChange = false;
    pageState.setForm(f);
    f.start(pageContext);
%>
<h1><%=I18n.get("COUNTERS")%>
</h1>
<table class="table dataTable" cellpadding="4"><%

  ListHeader lh = new ListHeader("COUNTLH", f);
  lh.addHeader(I18n.get("COUNTER_NAME"));
  lh.addHeaderFitAndCentered(I18n.get("COUNTER_VALUE"));
  lh.addHeaderFitAndCentered(I18n.get("GENERATE"));
  if (canManage) {
    lh.addHeaderFitAndCentered(I18n.get("RESET"));
    lh.addHeaderFitAndCentered(I18n.get("DELETE_SHORT"));
  }
  lh.toHtml(pageContext);

  String hql = "from " + Counter.class.getName() + " as counter where counter.name like :cprefix";
  OqlQuery oql = new OqlQuery(hql);
  oql.getQuery().setString("cprefix", "COUNT_%");

  List<Counter> cntrs = oql.list();

  ButtonSubmit gen = new ButtonSubmit(f);
  gen.variationsFromForm.setCommand("GENERATE");
  gen.label = "<span style='font-size:30px'>+1</span>";

    ButtonSubmit reset = new ButtonSubmit(f);
    reset.variationsFromForm.setCommand("RESET");
    reset.confirmRequire = true;
    reset.iconChar = "N";
    reset.label = "";
    reset.enabled= canManage;

    ButtonSubmit delete = new ButtonSubmit(f);
    delete.variationsFromForm.setCommand("DELETE");
    delete.confirmRequire = true;
    delete.iconChar = "d";
    delete.label = "";
    delete.enabled= canManage;


  for (Counter c : cntrs) {

    pageState.addClientEntry("COUNTER" + c.getId(), c.getValue());

    TextField aCounter = new TextField("COUNTER" + c.getId(), "");
    aCounter.fieldSize = 3;
    aCounter.label = "";

    String name = c.getName().substring("COUNT_".length());
    String prefix = "";
    if (name.indexOf("_") > -1) {
      prefix = name.substring(name.indexOf("_") + 1);
      name = name.substring(0, name.indexOf("_"));

    }

%>
  <tr class="alternate"><%
  %>
    <td><%=name%>
    </td>
    <td align="center" nowrap><span style="font-size:30px"><%=prefix + c.getValue()%></span></td>
    <td align="center"><%
      gen.variationsFromForm.mainObjectId = c.getName();
      gen.toHtmlInTextOnlyModality(pageContext);
    %></td><%
      if (canManage){
    %>
    <td align="center"><%
      reset.variationsFromForm.mainObjectId = c.getName();
      reset.toHtmlInTextOnlyModality(pageContext);
    %></td>
    <td align="center"><%
      delete.variationsFromForm.mainObjectId = c.getName();
      delete.additionalCssClass = "delete";
      delete.toHtmlInTextOnlyModality(pageContext);
    %></td><%
      }
    %>
  </tr>
  <tr>
    <td colspan="5" style="height: 30px"></td>
  </tr>
  <%
      }


    if (canCreate) {

      TextField newCounter = new TextField("NEWCOUNTER", "<br>");
    newCounter.script = "style=width:100%";
    pageState.addClientEntry("NEWCOUNTERVALUE", "1");
    TextField counterPrefix = new TextField("COUNTERPREFIX", "<br>");
    counterPrefix.fieldSize = 5;
    counterPrefix.script = "style=width:100%";
    TextField startValue = new TextField("NEWCOUNTERVALUE", "<br>");
    startValue.fieldSize = 5;
  %>
  <tr class="highlight">
    <td colspan="5" align="left">
      <table class="table fixFoot">
        <tfoot>
        <tr>
          <td><%newCounter.toHtmlI18n(pageContext);%></td>
          <td><%counterPrefix.toHtmlI18n(pageContext);%></td>
          <td><%startValue.toHtmlI18n(pageContext);%></td>

          <td valign="bottom" style="margin-left: 20px"><%
            ButtonSubmit.getSaveInstance(f, I18n.get("SAVE")).toHtml(pageContext);%></td>
        </tr>
        </tfoot>
      </table>
    </td>
  </tr>
  <%
    }
  %>
</table>
<%

    f.end(pageContext);
  }
%>
