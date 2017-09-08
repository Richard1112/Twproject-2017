<%@ page import="com.twproject.operator.TeamworkOperator,
          com.twproject.waf.TeamworkHBFScreen,
          org.jblooming.messaging.Message,
          org.jblooming.operator.Operator,
          org.jblooming.persistence.objectEditor.FieldFeature,
          org.jblooming.persistence.objectEditor.ObjectEditor,
          org.jblooming.persistence.objectEditor.businessLogic.ObjectEditorController,
          org.jblooming.waf.ScreenArea,
          org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    logged.testIsAdministrator();
        
    // definizione object editor
    ObjectEditor objectEditor = new ObjectEditor(I18n.get("MESSAGE_MANAGEMENT"), Message.class, pageContext);
    objectEditor.query = "from "+Message.class.getName()+" as message";
    objectEditor.defaultOrderBy="message.id";
    objectEditor.mainHqlAlias = "message";
    objectEditor.bulkActions=true;
    {
    objectEditor.addDisplayField("subject", I18n.get("MESSAGE_SUBJECT"));
    objectEditor.addDisplayField("messageBody", I18n.get("MESSAGE_BODY"));
    FieldFeature ff = FieldFeature.getIdentifiableInstance("fromOperator", I18n.get("MESSAGE_FROM_OPERATOR"), Operator.class, new String[]{"name", "surname"});
    objectEditor.addDisplayField(ff);

    // Operator combo to
    String hql = "select operator.id, operator.name || ' ' || operator.surname from " + Operator.class.getName() + " as operator ";
    String whereForId = "where operator.id = :" + SmartCombo.FILTER_PARAM_NAME;
    String whereForFiltering =
    " where operator.name || ' ' || operator.surname like :" + SmartCombo.FILTER_PARAM_NAME +
    " or operator.surname || ' ' || operator.name like :" + SmartCombo.FILTER_PARAM_NAME+ " order by operator.surname, operator.name";

    SmartCombo ops = new SmartCombo("MESSAGE_TO", hql, whereForFiltering, whereForId);
    FieldFeature ffo = new FieldFeature("toOperator",I18n.get("MESSAGE_TO"));
    ffo.smartCombo = ops;
    ffo.smartComboClass = Operator.class;
    objectEditor.addDisplayField(ffo);

    // Operator combo from
    SmartCombo opsf = new SmartCombo("MESSAGE_FROM", hql, whereForFiltering, whereForId);
    ffo = new FieldFeature("fromOperator",I18n.get("MESSAGE_FROM"));
    ffo.smartCombo = opsf;
    ffo.smartComboClass = Operator.class;
    objectEditor.addDisplayField(ffo);


    objectEditor.addDisplayField("media", I18n.get("MESSAGE_MEDIA"));
    objectEditor.addDisplayField("lastTry", I18n.get("MESSAGE_LAST_TRY"));
    objectEditor.addDisplayField("status", I18n.get("MESSAGE_STATUS"));
    objectEditor.addDisplayField("numberOfTries", I18n.get("MESSAGE_NUMBER_OF_TRIES"));
    objectEditor.addDisplayField("expires", I18n.get("MESSAGE_EXPIRES"));
    objectEditor.addDisplayField("received", I18n.get("MESSAGE_RECEIVED"));
    objectEditor.addDisplayField("lastError", I18n.get("MESSAGE_LAST_ERROR"));
    }

    // edit == list
    objectEditor.editFields=objectEditor.displayFields;


    PageSeed eventManager = new PageSeed(request.getContextPath() + "/applications/teamwork/messaging/eventManager.jsp");
    PageSeed listenerManager = new PageSeed(request.getContextPath() + "/applications/teamwork/messaging/listenerManager.jsp");
    PageSeed stickyManager = new PageSeed(request.getContextPath() + "/applications/teamwork/messaging/stickyManager.jsp");
    objectEditor.additionalButtons.add( new ButtonLink(I18n.get("EVENT_MANAGEMENT"), eventManager));
    objectEditor.additionalButtons.add( new ButtonLink(I18n.get("LISTENER_MANAGEMENT"), listenerManager));
    objectEditor.additionalButtons.add( new ButtonLink(I18n.get("STICKYNOTE"), stickyManager));


    ObjectEditorController oec = new ObjectEditorController(objectEditor);
    //
    final ScreenArea body = new ScreenArea(oec, request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    pageState.getMainJspIncluder().toHtml(pageContext);
%><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script><%
  }
%>


