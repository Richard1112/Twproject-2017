<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.flowork.FieldAvailable, org.jblooming.flowork.FlowPermissions, org.jblooming.ontology.businessLogic.DeleteHelper, org.jblooming.oql.OqlQuery, org.jblooming.persistence.exceptions.PersistenceException, org.jblooming.utilities.CodeValueList, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.exceptions.ActionException, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, java.util.Map, org.jblooming.waf.html.button.ButtonJS" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    if (Commands.SAVE.equals(pageState.command)) {

      try {

        Map<String, ClientEntry> map = pageState.getClientEntries().getEntriesStartingWithStripped("DESC_");
        for (String id : map.keySet()) {

          FieldAvailable fa = FieldAvailable.load(id);
          fa.setKind(pageState.getEntryAndSetRequired("KIND_" + id).stringValue());
          fa.setName(pageState.getEntryAndSetRequired("NAME_" + id).stringValue());
          fa.setLabel(pageState.getEntry("LABEL_" + id).stringValue());
          fa.setInitialValue(pageState.getEntry("INVAL_" + id).stringValueNullIfEmpty());
          fa.setColumnLength(pageState.getEntry("COLLEN_" + id).intValueNoErrorCodeNoExc());
          fa.setCardinality(pageState.getEntry("CARD_" + id).intValueNoErrorCodeNoExc());

          if (pageState.validEntries())
            fa.store();
        }

        String newKind = pageState.getEntry("KIND").stringValueNullIfEmpty();
        if (newKind != null) {
          FieldAvailable fa = new FieldAvailable();
          fa.setKind(pageState.getEntryAndSetRequired("KIND" ).stringValue());
          fa.setName(pageState.getEntryAndSetRequired("NAME").stringValue());
          fa.setLabel(pageState.getEntry("LABEL").stringValue());
          fa.setInitialValue(pageState.getEntry("INVAL").stringValueNullIfEmpty());
          fa.setColumnLength(pageState.getEntry("COLLEN").intValueNoErrorCodeNoExc());
          fa.setCardinality(pageState.getEntry("CARD").intValueNoErrorCodeNoExc());

          if (pageState.validEntries()){
            fa.store();
            pageState.removeEntry("NAME");
            pageState.removeEntry("LABEL");
            pageState.removeEntry("KIND");
            pageState.removeEntry("INVAL");
            pageState.removeEntry("COLLEN");
            pageState.removeEntry("CARD");
          }
        }

      } catch (ActionException e) {
      }
    }

    pageState.toHtml(pageContext);

  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    boolean isFlowManager = logged!=null && (logged.hasPermissionFor(FlowPermissions.canManageFlows));
    if (logged==null || !isFlowManager)
     throw new SecurityException("No permission for accessing page "+request.getRequestURI());


    List<FieldAvailable> fas =new OqlQuery("select fa from "+FieldAvailable.class.getName()+" as fa order by fa.name").list();



    CodeValueList availKinds= new CodeValueList();
    availKinds.add("java.lang.String","String");
    availKinds.add("java.lang.Integer","Integer");
    availKinds.add("java.lang.Double","Double");
    availKinds.add("java.util.Date","Date");



    
    PageSeed ps = pageState.thisPage(request);
    ps.mainObjectId = pageState.mainObjectId;
    Form form = new Form(ps);
    pageState.setForm(form);
    form.start(pageContext);

%><h1><%=I18n.get("FIELD_DECLARATION_MENU")%></h1>
<table class="table">
  <tr>
    <th>id</th>
    <th><%=I18n.get("NAME")%>*</th>
    <th><%=I18n.get("LABEL")%></th>
    <th><%=I18n.get("CLASS_NAME")%></th>
    <th><%=I18n.get("INITIAL_VALUE")%></th>
    <th><%=I18n.get("LENGHT")%></th>
    <th><%=I18n.get("CARDINALITY")%></th>
    <th><%=I18n.get("DELETE_SHORT")%></th>
  </tr><%


    for (FieldAvailable fa : fas) {
     
        %> <tr class="alternate" objid="<%=fa.getId()%>">
       <td><%=fa.getId()%></td><%

      pageState.addClientEntry("NAME_"+fa.getId(),fa.getName());
      TextField tf = new TextField("TEXT","","NAME_"+fa.getId(),"",15,false);
      tf.label="";
      tf.required=true;
      tf.separator="";

      %><td><%tf.toHtml(pageContext);%></td><%

      pageState.addClientEntry("LABEL_"+fa.getId(),fa.getLabel());
      tf = new TextField("TEXT","","LABEL_"+fa.getId(),"",30,false);
      tf.label="";
      tf.separator="";
      %><td><%tf.toHtml(pageContext);%></td><%

      Combo a = new Combo("KIND_"+fa.getId(),"" ,null, 30,availKinds,"");
      a.label = "";
      a.required = true;
      a.separator = "";
      pageState.addClientEntry("KIND_"+fa.getId(),fa.getKind());
      %><td><%a.toHtml(pageContext);%></td><%

      pageState.addClientEntry("INVAL_"+fa.getId(),fa.getInitialValue());
      tf = new TextField("TEXT","","INVAL_"+fa.getId(),"",20,false);
      tf.label="";
      tf.separator="";
      %><td><%tf.toHtml(pageContext);%></td><%

      pageState.addClientEntry("COLLEN_"+fa.getId(),fa.getColumnLength());
      tf = TextField.getIntegerInstance("COLLEN_"+fa.getId());
      tf.fieldSize=5;
      tf.label="";
      tf.separator="";
      %><td><%tf.toHtml(pageContext);%></td><%

      pageState.addClientEntry("CARD_"+fa.getId(),fa.getCardinality());
      tf = TextField.getIntegerInstance("CARD_"+fa.getId());
      tf.fieldSize=3;
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

  %><tr><td><small><%=I18n.get("NEW")%></small></td><%


    TextField tf = new TextField("TEXT","","NAME","",15,false);
    tf.label="";
    tf.separator="";

    %><td><%tf.toHtml(pageContext);%></td><%

    tf = new TextField("TEXT","","LABEL","",30,false);
    tf.label="";
    tf.separator="";
    %><td><%tf.toHtml(pageContext);%></td><%


    Combo a = new Combo("KIND","" ,null, 30,availKinds,"");
    a.label = "";
    a.separator = "";
    %><td><%a.toHtml(pageContext);%></td><%

    tf = new TextField("TEXT","","INVAL","",20,false);
    tf.label="";
    tf.separator="";
    %><td><%tf.toHtml(pageContext);%></td><%

    tf = TextField.getIntegerInstance("COLLEN");
    tf.label="";
    tf.fieldSize=5;
    tf.separator="";
    %><td><%tf.toHtml(pageContext);%></td><%

    tf = TextField.getIntegerInstance("CARD");
    tf.fieldSize=3;
    tf.label="";
    tf.separator="";
    %><td><%tf.toHtml(pageContext);%></td>

    </tr>
  </table><%

    ButtonBar bb = new ButtonBar();

    ButtonSubmit save = ButtonSubmit.getSaveInstance(form, I18n.get("SAVE"));
    save.additionalCssClass="first big";
    bb.addButton(save);
    bb.toHtml(pageContext);

    form.end(pageContext);
  new DeletePreviewer("OBJ_DEL",FieldAvailable.class.getName(), pageState);

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