<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.security.SecurityBricks, com.twproject.security.TeamworkPermissions,
com.twproject.task.TaskCustomerField,com.twproject.task.TaskBricks, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.ontology.businessLogic.DeleteHelper, org.jblooming.oql.OqlQuery,
 org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.exceptions.PersistenceException, org.jblooming.security.Area, org.jblooming.waf.ActionUtilities,
  org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.SecurityConstants, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit,
   org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.HeadBar, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.Combo,
    org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed,
     org.jblooming.waf.view.PageState, java.util.List, java.util.Map, java.util.Set, org.jblooming.waf.html.button.ButtonJS" %>
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

    	TaskCustomerField t = (TaskCustomerField) PersistenceHome.findByPrimaryKey(TaskCustomerField.class, id);
        t.setName(pageState.getEntry("NAME_" + id).stringValue());
        t.setDescription(pageState.getEntry("DESC_" + id).stringValue());
        ActionUtilities.setIdentifiable(pageState.getEntry("DATATYPE_" + id), t, "dataType");
        t.store();
      }

      String newDesc = pageState.getEntry("DESC").stringValueNullIfEmpty();
      if (newDesc != null) {
    	  TaskCustomerField t = new TaskCustomerField();
        t.setName(pageState.getEntry("NAME").stringValue());
        t.setDescription(pageState.getEntry("DESC").stringValue());
        ActionUtilities.setIdentifiable(pageState.getEntry("DATATYPE"), t, "dataType");
        t.store();
        pageState.removeEntry("NAME");
        pageState.removeEntry("DESC");
        pageState.removeEntry("DATATYPE");
      }
    }

    pageState.toHtml(pageContext);

  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Set<Area> areas = null;

    if (!logged.hasPermissionAsAdmin()) {
      areas = logged.getAreasForPermission(TeamworkPermissions.classificationTree_canManage);
      if (areas.size() == 0)
        throw new org.jblooming.security.SecurityException(SecurityConstants.I18N_PERMISSION_LACKING, TeamworkPermissions.classificationTree_canManage);
      
    }


    String hql = "from " + TaskCustomerField.class.getName() + " as tt ";
    
    OqlQuery oql = new OqlQuery(hql);
    List<TaskCustomerField> tts = oql.list();


    PageSeed ps = pageState.thisPage(request);
    ps.mainObjectId = pageState.mainObjectId;
    Form form = new Form(ps);
    pageState.setForm(form);
    form.start(pageContext);

%><h1>DEFINE CUSTOMER FIELD</h1>

<table class="table">
  <tr>
    <th class="tableHead">id</th>
    <th class="tableHead">Name</th>
    <th class="tableHead">Type</th>
    <th class="tableHead">Description</th>
    <th class="tableHead"><%=I18n.get("DELETE_SHORT")%></th>
  </tr><%


    for (TaskCustomerField tt : tts) {
     
        %> <tr class="alternate" objid="<%=tt.getId()%>">
       <td><%=tt.getId()%></td><%

      pageState.addClientEntry("NAME_"+tt.getId(),tt.getName());
      TextField tf = new TextField("TEXT","","NAME_"+tt.getId(),"",15,false);
      tf.label="";
      tf.separator="";

      %><td><%tf.toHtml(pageContext);%></td><%

      Combo a = TaskBricks.getDataTypeCombo("DATATYPE_"+tt.getId(), "DataType");
      a.cvl.addChoose(pageState);
      a.label = "";
      a.separator = "";
      if (tt.getDataType()!=null)
        pageState.addClientEntry("DATATYPE_"+tt.getId(),tt.getDataType().getId());
      %><td><%a.toHtml(pageContext);%></td><%

      pageState.addClientEntry("DESC_"+tt.getId(),tt.getDescription());
      tf = new TextField("TEXT","","DESC_"+tt.getId(),"",30,false);
      tf.label="";
      tf.separator="";  
      %><td><%tf.toHtml(pageContext);%></td>

  <td align="center"><%
    ButtonJS delLink = new ButtonJS("delRow($(this))");
    delLink.iconChar = "d";
    delLink.label = "";
    delLink.additionalCssClass = "delete";
    delLink.toHtmlInTextOnlyModality(pageContext);
  %></td></tr><%
    }

    %><tr class="alternate highlight"><td><span class="sectionTitle"><%=I18n.get("NEW")%></span></td><%

    TextField tf = new TextField("TEXT","","NAME","",15,false);
    tf.label="";
    tf.separator="";
    %><td><%tf.toHtml(pageContext);%></td><%

    Combo a = TaskBricks.getDataTypeCombo("DATATYPE", "DataType");
    a.cvl.addChoose(pageState);
    a.label = "";
    a.separator = "";
    a.fieldName = "DATATYPE";
    %><td><%a.toHtml(pageContext);%></td><%

    tf = new TextField("TEXT","","DESC","",30,false);
    tf.label="";
    tf.separator="";
    %><td><%tf.toHtml(pageContext);%></td><td></td></tr> <%

    %></table><%
    ButtonBar bb = new ButtonBar();

    ButtonSubmit save = ButtonSubmit.getSaveInstance(form, I18n.get("SAVE"));
    save.additionalCssClass="first";
    bb.addButton(save);

    bb.toHtml(pageContext);

    form.end(pageContext);
  new DeletePreviewer("OBJ_DEL",TaskCustomerField.class.getName(), pageState);

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