<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.security.SecurityBricks, com.twproject.security.TeamworkPermissions, com.twproject.task.TaskBricks, com.twproject.task.TaskType, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.ontology.businessLogic.DeleteHelper, org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.exceptions.PersistenceException, org.jblooming.waf.ActionUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.exceptions.ActionException, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, java.util.Map, org.jblooming.waf.html.button.ButtonJS" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  TaskType justAdded = (TaskType) pageState.getAttribute("justAdded");
  boolean once = pageState.getEntry("once").checkFieldValue();

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    if (Commands.SAVE.equals(pageState.command)) {
      try {

        Map<String, ClientEntry> map = pageState.getClientEntries().getEntriesStartingWithStripped("DESC_");
        for (String id : map.keySet()) {

          TaskType t = (TaskType) PersistenceHome.findByPrimaryKey(TaskType.class, id);
          t.setStringValue(pageState.getEntryAndSetRequired("CODE_" + id).stringValue());
          t.setDescription(pageState.getEntryAndSetRequired("DESC_" + id).stringValue());
          ActionUtilities.setIdentifiable(pageState.getEntryAndSetRequired("AREA_" + id), t, "area");
          if (pageState.validEntries())
            t.store();
        }

        String newDesc = pageState.getEntry("DESC").stringValueNullIfEmpty();
        if (newDesc != null) {
          TaskType t = new TaskType();
          t.setStringValue(pageState.getEntryAndSetRequired("CODE").stringValue());
          t.setDescription(pageState.getEntryAndSetRequired("DESC").stringValue());
          ActionUtilities.setIdentifiable(pageState.getEntryAndSetRequired("AREA"), t, "area");
          if (pageState.validEntries()){
            t.store();
            pageState.removeEntry("CODE");
            pageState.removeEntry("DESC");
            pageState.removeEntry("AREA");
            pageState.setAttribute("justAdded",t);
          }

        }

      } catch (ActionException e) {
      }
    }

    pageState.toHtml(pageContext);

  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    List<TaskType> tts = TaskBricks.getTaskTypes(logged,TeamworkPermissions.classificationTree_canManage,logged.getPerson().getArea());

    if (once && justAdded !=null){
      %><script>closeBlackPopup({id:<%=justAdded.getId()%>,name:"<%=justAdded.getName()%>"})</script><%
    }

    PageSeed ps = pageState.thisPage(request);
    ps.addClientEntry(pageState.getEntry("once"));
    ps.mainObjectId = pageState.mainObjectId;
    Form form = new Form(ps);
    pageState.setForm(form);
    form.start(pageContext);

%><h1><%=I18n.get("TASK_TYPE_MENU")%></h1>
<table class="table">
  <tr>
    <th class="tableHead">Id</th>
    <th class="tableHead"><%=I18n.get("CODE")%>*</th>
    <th class="tableHead"><%=I18n.get("DESCRIPTION")%>*</th>
    <th class="tableHead <%=SecurityBricks.isSingleArea()?"displayNone":""%>"><%=I18n.get("AREA")%>*</th>
    <th class="tableHead"><%=I18n.get("DELETE_SHORT")%></th>
  </tr>
  <%


    for (TaskType tt : tts) {
     
        %> <tr class="alternate" objid="<%=tt.getId()%>">
       <td><%=tt.getId()%></td><%

      pageState.addClientEntry("CODE_"+tt.getId(),tt.getStringValue());
      TextField tf = new TextField("TEXT","","CODE_"+tt.getId(),"",15,false);
      tf.label="";
      tf.separator="";
      tf.required=true;

      %><td><%tf.toHtml(pageContext);%></td><%

      pageState.addClientEntry("DESC_"+tt.getId(),tt.getDescription());
      tf = new TextField("TEXT","","DESC_"+tt.getId(),"",30,false);
      tf.label="";
      tf.required = true;
      tf.separator="";  
      %><td><%tf.toHtml(pageContext);%></td><%

      Combo a = SecurityBricks.getAreaCombo("AREA_"+tt.getId(), TeamworkPermissions.classificationTree_canManage, pageState);
      //if (a.cvl.size()>1)
      //  a.cvl.addChoose(pageState);
      a.label = "";
      a.required = true;
      a.separator = "";
      if (tt.getArea()!=null)
        pageState.addClientEntry("AREA_"+tt.getId(),tt.getArea().getId());      
      %><td class="<%=SecurityBricks.isSingleArea()?"displayNone":""%>"><%a.toHtml(pageContext);%></td>

      <td align="center"><%
        ButtonJS delLink = new ButtonJS("delRow($(this))");
        delLink.iconChar = "d";
        delLink.label = "";
        delLink.additionalCssClass = "delete";
        delLink.toHtmlInTextOnlyModality(pageContext);
      %></td></tr><%

    }

  %><tr class="alternate highlight"><td><span class="sectionTitle"><%=I18n.get("TASK_TYPE_NEW")%></span></td><%

      TextField tf = new TextField("TEXT","","CODE","",15,false);
      tf.label="";
      tf.separator="";
      //tf.required=true;
      %><td><%tf.toHtml(pageContext);%></td><%

      tf = new TextField("TEXT","","DESC","",30,false);
      tf.label="";
      tf.separator="";
      //tf.required=true;
      %><td><%tf.toHtml(pageContext);%></td><%

      Combo a = SecurityBricks.getAreaCombo("AREA", TeamworkPermissions.classificationTree_canManage, pageState);
      a.label = "";
      a.separator = "";
      a.fieldName = "AREA";
      %><td class="<%=SecurityBricks.isSingleArea()?"displayNone":""%>"><%a.toHtml(pageContext);%></td><td></td></tr><%


    %></table><%
    ButtonBar bb = new ButtonBar();

    ButtonSubmit save = ButtonSubmit.getSaveInstance(form, I18n.get("SAVE"));
    save.additionalCssClass="first";
    bb.addButton(save);
    bb.toHtml(pageContext);

    form.end(pageContext);

  new DeletePreviewer("OBJ_DEL",TaskType.class.getName(), pageState);

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