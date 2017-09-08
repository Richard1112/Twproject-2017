<%@ page pageEncoding="UTF-8" %><%@ page import="com.twproject.meeting.DiscussionPointType,com.twproject.operator.TeamworkOperator, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.ontology.businessLogic.DeleteHelper,
org.jblooming.operator.Operator, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.exceptions.PersistenceException, org.jblooming.waf.ActionUtilities,
 org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar,
 org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n,
 org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Date,
  java.util.Map, com.twproject.agenda.DiscussionPointStatus, org.jblooming.waf.html.button.ButtonJS" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    if (Commands.SAVE.equals(pageState.command)) {
      Operator loggedOperator = pageState.getLoggedOperator();

      Map<String, ClientEntry> map = pageState.getClientEntries().getEntriesStartingWithStripped("DP_DESCRIPTION_");

      // save eventual changes in the page
      for (String id : map.keySet()) {
        DiscussionPointStatus dpt = (DiscussionPointStatus) PersistenceHome.findByPrimaryKey(DiscussionPointStatus.class, id);
        ActionUtilities.setString(pageState.getEntry("DP_DESCRIPTION_" + id), dpt, "description");
        dpt.setLastModifier(loggedOperator.getFullname());
        dpt.setLastModified(new Date());
        dpt.store();
      }

      // if there is a new discussion Point type then I'll go to save it
      String newDesc = pageState.getEntry("DP_DESCRIPTION").stringValueNullIfEmpty();
      if (newDesc != null) {
        DiscussionPointStatus dpt = new DiscussionPointStatus();
        ActionUtilities.setString(pageState.getEntry("DP_DESCRIPTION"), dpt, "description");
        dpt.store();
        pageState.removeEntry("DP_DESCRIPTION");
      }
    }

    pageState.toHtml(pageContext);

  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    // obtain DiscussionPointTypes objects list
    String hql = "select dpType from " + DiscussionPointStatus.class.getName() + " as dpType";
    OqlQuery oqlQuery = new OqlQuery(hql);
    ArrayList<DiscussionPointStatus> dpTypeLst = (ArrayList<DiscussionPointStatus>) oqlQuery.list();

    PageSeed ps = pageState.thisPage(request);
    ps.mainObjectId = pageState.mainObjectId;
    Form form = new Form(ps);
    form.alertOnChange = true;
    pageState.setForm(form);


    %>
<h1><%=I18n.get("DISCUSSION_POINT_STATUS")%></h1><%
    form.start(pageContext);
%>
<table class="table">
  <tr>
    <th class="tableHead">Id</th>
    <th class="tableHead"><%=I18n.get("DESCRIPTION")%>*</th>
    <th class="tableHead"><%=I18n.get("DELETE_SHORT")%></th>
  </tr>

  <%

    if (dpTypeLst != null && !dpTypeLst.isEmpty()) {
      for (DiscussionPointStatus dpt : dpTypeLst) {
  %>
  <tr class="alternate" objid="<%=dpt.getId()%>">
    <td><%=dpt.getId()%>
    </td>
    <td>
      <%
        pageState.addClientEntry("DP_DESCRIPTION_" + dpt.getId().toString(), dpt.getDescription());
        TextField tf = new TextField("TEXT", "", "DP_DESCRIPTION_" + dpt.getId().toString(), "", 15, false);
        tf.label = "";
        tf.required = true;
        tf.separator = "";
        tf.toHtml(pageContext);
      %>
    </td>
    <td align="center"><%
      ButtonJS delLink = new ButtonJS("delRow($(this))");
      delLink.iconChar = "d";
      delLink.label = "";
      delLink.additionalCssClass = "delete";
      delLink.toHtmlInTextOnlyModality(pageContext);
    %></td></tr>
  <%
      }
    }
  %>
  <tr class="alternate highlight">
    <td>
      <span class="sectionTitle"><%=I18n.get("DP_TYPE_NEW")%></span>
    </td>
    <td>
      <%
        TextField tf = new TextField("TEXT", "", "DP_DESCRIPTION", "", 15, false);
        tf.label = "";
        tf.required = true;
        tf.separator = "";
        tf.toHtmlI18n(pageContext);
      %>
    </td>
    <td></td>
  </tr>
</table>
<%
    ButtonBar bb = new ButtonBar();
    ButtonSubmit save = ButtonSubmit.getSaveInstance(form, I18n.get("SAVE"));
    save.additionalCssClass="first";
    bb.addButton(save);
    bb.toHtml(pageContext);

    form.end(pageContext);
  new DeletePreviewer("OBJ_DEL",DiscussionPointStatus.class.getName(), pageState);

%>
<script>

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