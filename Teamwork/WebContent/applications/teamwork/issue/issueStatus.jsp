<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.task.IssueStatus, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.ontology.businessLogic.DeleteHelper, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.exceptions.PersistenceException, org.jblooming.security.Area, org.jblooming.waf.ActionUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.HeadBar, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, java.util.Map, java.util.Set, org.jblooming.waf.html.button.ButtonJS" %>
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
          IssueStatus t = IssueStatus.load(id);
        ActionUtilities.setInt(pageState.getEntryAndSetRequired("ORDE_"+id ),t,"orderBy");
          ActionUtilities.setString(pageState.getEntryAndSetRequired("DESC_" + id),t,"description");
          ActionUtilities.setString(pageState.getEntryAndSetRequired("COLOR_" + id),t,"color");
          ActionUtilities.setBoolean(pageState.getEntryAndSetRequired("ASOP_" + id),t,"behavesAsOpen");
          ActionUtilities.setBoolean(pageState.getEntryAndSetRequired("ASCL_" + id),t,"behavesAsClosed");
          ActionUtilities.setBoolean(pageState.getEntryAndSetRequired("ASKC_" + id),t,"askForComment");
          ActionUtilities.setBoolean(pageState.getEntryAndSetRequired("ASKW_" + id),t,"askForWorklog");

          if (pageState.validEntries())
            t.store();
      }

      String newDesc = pageState.getEntry("DESC").stringValueNullIfEmpty();
      if (newDesc != null) {

          IssueStatus t = new IssueStatus();
          ActionUtilities.setInt(pageState.getEntryAndSetRequired("ORDE" ),t,"orderBy");
          ActionUtilities.setString(pageState.getEntryAndSetRequired("DESC" ),t,"description");
          ActionUtilities.setString(pageState.getEntryAndSetRequired("COLOR"),t,"color");
          ActionUtilities.setBoolean(pageState.getEntryAndSetRequired("ASOP" ),t,"behavesAsOpen");
          ActionUtilities.setBoolean(pageState.getEntryAndSetRequired("ASCL" ),t,"behavesAsClosed");
          ActionUtilities.setBoolean(pageState.getEntryAndSetRequired("ASKC" ),t,"askForComment");
          ActionUtilities.setBoolean(pageState.getEntryAndSetRequired("ASKW" ),t,"askForWorklog");

          pageState.removeEntry("ORDE");
          pageState.removeEntry("DESC");
          pageState.removeEntry("COLOR");
          pageState.removeEntry("ASOP");
          pageState.removeEntry("ASCL");
          pageState.removeEntry("ASKC");
          pageState.removeEntry("ASKW");

          if (pageState.validEntries())
            t.store();

      }

    }

    pageState.toHtml(pageContext);

  } else {


%><script>$("#ISSUES_MENU").addClass('selected');</script><%


    String hql = "from " + IssueStatus.class.getName() + " as tt order by orderBy";
    OqlQuery oql = new OqlQuery(hql);
    List<IssueStatus> tts = oql.list();


    PageSeed ps = pageState.thisPage(request);
    ps.mainObjectId = pageState.mainObjectId;
    Form form = new Form(ps);
    pageState.setForm(form);
    form.start(pageContext);

%>
<%

    HeadBar hb = new HeadBar();

    ButtonLink ii = new ButtonLink(I18n.get("ISSUE_IMPACT_MENU"), pageState.pageFromRoot("issue/issueImpact.jsp"));
    hb.addButton(ii);

    hb.addSeparator(30);

    ButtonLink it = new ButtonLink(I18n.get("ISSUE_TYPE_MENU"), pageState.pageFromRoot("issue/issueType.jsp"));
    hb.addButton(it);

    hb.toHtml(pageContext);

%><h1><%=I18n.get("ISSUE_STATUS_MENU")%></h1>
<table class="table">
  <tr>
    <th class="tableHead" width="5%">id</th>
    <th class="tableHead" width="5%">order</th>
    <th class="tableHead"><%=I18n.get("DESCRIPTION")%>*</th>
    <th class="tableHead" width="8%">color*</th>
    <th class="tableHead">as 'open'</th>
    <th class="tableHead">as 'close'</th>
    <th class="tableHead">ask comment</th>
    <th class="tableHead">ask wl</th>
    <th class="tableHead"><%=I18n.get("DELETE_SHORT")%></th>
  </tr><%


    for (IssueStatus tt : tts) {

      pageState.addClientEntry("DESC_"+tt.getId(),tt.getDescription());
      pageState.addClientEntry("ORDE_"+tt.getId(),tt.getOrderBy());
      pageState.addClientEntry("ASOP_"+tt.getId(),tt.isBehavesAsOpen());
      pageState.addClientEntry("ASCL_"+tt.getId(),tt.isBehavesAsClosed());
      pageState.addClientEntry("ASKC_"+tt.getId(),tt.isAskForComment());
      pageState.addClientEntry("ASKW_"+tt.getId(),tt.isAskForWorklog());
      pageState.addClientEntry("COLOR_"+tt.getId(),tt.getColor());

        %> <tr class="alternate" objid="<%=tt.getId()%>" >
       <td><%=tt.getId()%></td><%

      TextField tf = new TextField("TEXT","","ORDE_"+tt.getId(),"",3,false);
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
      %><td><%tf.toHtml(pageContext);%></td><%

      %><td align="center"><%new CheckField("","ASOP_"+tt.getId(),"",false).toHtml(pageContext);%></td><%
      %><td align="center"><%new CheckField("","ASCL_"+tt.getId(),"",false).toHtml(pageContext);%></td><%
      %><td align="center"><%new CheckField("","ASKC_"+tt.getId(),"",false).toHtml(pageContext);%></td><%
      %><td align="center"><%new CheckField("","ASKW_"+tt.getId(),"",false).toHtml(pageContext);%></td>


      <td align="center"><%
        ButtonJS delLink = new ButtonJS("delRow($(this))");
        delLink.iconChar = "d";
        delLink.label = "";
        delLink.additionalCssClass = "delete";
        delLink.toHtmlInTextOnlyModality(pageContext);
      %></td></tr><%

    }

  %><tr class="alternate highlight"><td><span class="sectionTitle"><%=I18n.get("NEW")%></span></td><%

      TextField tf = new TextField("TEXT","","ORDE","",3,false);
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
      %><td nowrap><%tf.toHtml(pageContext);%></td><%

      %><td align="center"><%new CheckField("","ASOP","",false).toHtml(pageContext);%></td><%
      %><td align="center"><%new CheckField("","ASCL","",false).toHtml(pageContext);%></td><%
      %><td align="center"><%new CheckField("","ASKC","",false).toHtml(pageContext);%></td><%
      %><td align="center"><%new CheckField("","ASKW","",false).toHtml(pageContext);%></td>
  <td></td><%


    %></table><%
    ButtonBar bb = new ButtonBar();

  ButtonSubmit save = ButtonSubmit.getSaveInstance(form, I18n.get("SAVE"));
  save.additionalCssClass="first";
  bb.addButton(save);

    bb.toHtml(pageContext);


    form.end(pageContext);
  new DeletePreviewer("OBJ_DEL",IssueStatus.class.getName(), pageState);

%>
<link rel="stylesheet" href="<%=request.getContextPath()+"/commons/js/jquery/minicolors/jquery.miniColors.css"%>" type="text/css"/>

<script type="text/javascript">
  var defs=[initialize(contextPath + "/commons/js/jquery/minicolors/jquery.miniColors.css","css"),
    initialize(contextPath + "/commons/js/jquery/minicolors/jquery.miniColors.min.js","script")];
  $.when.apply(null,defs).done(function(){
    $(":input[colorField]").minicolors();
  });


  function delRow(el) {
    var issueRow = $(el).closest("[objid]");
    var issueId = issueRow.attr("objid");
    deletePreview("OBJ_DEL", issueId, function(response) {  // callback function
      if (response && response.ok){
        issueRow.fadeOut(500, function () {$(this).remove();});
      }
    });
  }


</script>



<%



  }
%>
