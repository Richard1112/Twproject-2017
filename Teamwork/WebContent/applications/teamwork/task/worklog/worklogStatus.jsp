 <%@ page import="com.twproject.operator.TeamworkOperator,  com.twproject.security.TeamworkPermissions, com.twproject.waf.TeamworkPopUpScreen, com.twproject.worklog.WorklogStatus, org.jblooming.ontology.businessLogic.DeleteHelper, org.jblooming.oql.OqlQuery, org.jblooming.persistence.exceptions.PersistenceException, org.jblooming.waf.ActionUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, java.util.Map, org.jblooming.waf.html.button.ButtonJS" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    if (Commands.SAVE.equals(pageState.command)) {

      Map<String, ClientEntry> map = pageState.getClientEntries().getEntriesStartingWithStripped("DESC_");
      for (String id : map.keySet()) {
          WorklogStatus t = WorklogStatus.load(id);
          ActionUtilities.setInt(pageState.getEntryAndSetRequired("CODE_"+id ),t,"intValue");
          ActionUtilities.setString(pageState.getEntryAndSetRequired("DESC_" + id),t,"description");
          ActionUtilities.setString(pageState.getEntryAndSetRequired("COLOR_" + id),t,"color");

          if (pageState.validEntries())
            t.store();
      }

      String newDesc = pageState.getEntry("DESC").stringValueNullIfEmpty();
      if (newDesc != null) {

          WorklogStatus t = new WorklogStatus();
          ActionUtilities.setInt(pageState.getEntryAndSetRequired("CODE" ),t,"intValue");
          ActionUtilities.setString(pageState.getEntryAndSetRequired("DESC" ),t,"description");
          ActionUtilities.setString(pageState.getEntryAndSetRequired("COLOR"),t,"color");

          pageState.removeEntry("CODE");
          pageState.removeEntry("DESC");
          pageState.removeEntry("COLOR");

          if (pageState.validEntries())
            t.store();

      }
    }

    pageState.toHtml(pageContext);

  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    logged.testPermission(TeamworkPermissions.classificationTree_canManage);


%><script>$("#TIMESHEET_MENU").addClass('selected');</script><%

    String hql = "from " + WorklogStatus.class.getName() + " as tt order by intValue";
    OqlQuery oql = new OqlQuery(hql);
    List<WorklogStatus> tts = oql.list();

    PageSeed ps = pageState.thisPage(request);
    ps.mainObjectId = pageState.mainObjectId;
    Form form = new Form(ps);
    pageState.setForm(form);
    form.start(pageContext);

%><h1><%=I18n.get("WORKLOG_STATUS")%></h1>
<table class="table">
  <tr>
    <th width="5%" class="tableHead">id</th>
    <th width="5%" nowrap="" class="tableHead"><%=I18n.get("CODE")%></th>
    <th class="tableHead"><%=I18n.get("DESCRIPTION")%>*</th>
    <th width="12%" class="tableHead"><%=I18n.get("COLOR")%>*</th>
    <th class="tableHead"><%=I18n.get("DELETE_SHORT")%></th>
  </tr><%

    for (WorklogStatus tt : tts) {

      pageState.addClientEntry("DESC_"+tt.getId(),tt.getDescription());
      pageState.addClientEntry("CODE_"+tt.getId(),tt.getIntValue());
      pageState.addClientEntry("COLOR_"+tt.getId(),tt.getColor());

        %> <tr class="alternate" objid="<%=tt.getId()%>" >
       <td><%=tt.getId()%></td><%

      TextField tf = TextField.getIntegerInstance("CODE_"+tt.getId());
      tf.fieldSize=3;
      tf.label="";
      tf.separator="";
      %><td><%tf.toHtml(pageContext);%></td><%

      tf = new TextField("TEXT","","DESC_"+tt.getId(),"",30,false);
      tf.label="";
      tf.separator="";
      %><td><%tf.toHtml(pageContext);%></td><%

      tf = new TextField("TEXT","","COLOR_"+tt.getId(),"",7,false);
      tf.label="";
      tf.separator="";
      tf.script = " colorField='1'";
      %><td><%tf.toHtml(pageContext);%></td>
      <td align="center"><%
        ButtonJS delLink = new ButtonJS("delRow($(this))");
        delLink.iconChar = "d";
        delLink.label = "";
        delLink.additionalCssClass = "delete";
        delLink.toHtmlInTextOnlyModality(pageContext);
      %></td></tr><%

    }

  %> <tr class="alternate highlight">
  <td><span class="sectionTitle"><%=I18n.get("NEW")%></span></td><%

      TextField tf = new TextField("TEXT","","CODE","",3,false);
      tf.label="";
      tf.separator="";
      %><td><%tf.toHtml(pageContext);%></td><%

      tf = new TextField("TEXT","","DESC","",30,false);
      tf.label="";
      tf.separator="";
      %><td><%tf.toHtml(pageContext);%></td><%
      tf = new TextField("TEXT","","COLOR","",7,false);
      tf.label="";
      tf.separator="";
      tf.script = " colorField='1'";
      %><td colspan="2"><%tf.toHtml(pageContext);%></td><%
    %></table><%
    ButtonBar bb = new ButtonBar();

    ButtonSubmit save = ButtonSubmit.getSaveInstance(form, I18n.get("SAVE"));
    save.additionalCssClass="first";
    bb.addButton(save);

    bb.toHtml(pageContext);


    form.end(pageContext);

   new DeletePreviewer("OBJ_DEL",WorklogStatus.class.getName(), pageState);

 %>
 <script type="text/javascript">

   function delRow(el) {
     var issueRow = $(el).closest("[objid]");
     var issueId = issueRow.attr("objid");
     deletePreview("OBJ_DEL", issueId, function (response) { // callback function
       if (response && response.ok) {
         issueRow.fadeOut(500, function () {$(this).remove();});
       }
     });
   }

 var defs = [initialize(contextPath + "/commons/js/jquery/minicolors/jquery.miniColors.css", "css"),
     initialize(contextPath + "/commons/js/jquery/minicolors/jquery.miniColors.min.js", "script")];

   $.when.apply(null, defs).done(function () {
     $(":input[colorField]").minicolors();

   });
 </script>
<%
  }
%>