<%@ page import="com.twproject.messaging.stickyNote.StickyNote,
          com.twproject.operator.TeamworkOperator,
          com.twproject.resource.Person,
          com.twproject.waf.TeamworkHBFScreen,
          org.jblooming.messaging.Message,
          org.jblooming.persistence.objectEditor.FieldFeature,
          org.jblooming.persistence.objectEditor.ObjectEditor,
          org.jblooming.persistence.objectEditor.businessLogic.ObjectEditorController,
          org.jblooming.waf.ScreenArea, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    logged.testIsAdministrator();
        
    // definizione object editor
    ObjectEditor objectEditor = new ObjectEditor(I18n.get("STICKYNOTE"), StickyNote.class, pageContext);
    objectEditor.query = "from "+StickyNote.class.getName()+" as sticky";
    objectEditor.defaultOrderBy="sticky.id";
    objectEditor.mainHqlAlias = "sticky";
    objectEditor.bulkActions=true;

    objectEditor.addDisplayField("title", I18n.get("MESSAGE_SUBJECT"));
    //objectEditor.addDisplayField("message", I18n.get("MESSAGE_BODY"));
    FieldFeature ff = FieldFeature.getIdentifiableInstance("author", I18n.get("MESSAGE_FROM_OPERATOR"), Person.class, new String[]{"personName", "personSurname"});
    objectEditor.addDisplayField(ff);

    ff = FieldFeature.getIdentifiableInstance("receiver", I18n.get("MESSAGE_TO"), Person.class, new String[]{"personName", "personSurname"});
    objectEditor.addDisplayField(ff);

    objectEditor.addDisplayField("readOn", I18n.get("READ_ON"));
    objectEditor.addDisplayField("type", I18n.get("TYPE"));

    // edit == list
    objectEditor.editFields.putAll(objectEditor.displayFields);
    objectEditor.addEditField("message", I18n.get("MESSAGE_BODY"));

    PageSeed eventManager = new PageSeed(request.getContextPath() + "/applications/teamwork/messaging/eventManager.jsp");
    PageSeed listenerManager = new PageSeed(request.getContextPath() + "/applications/teamwork/messaging/listenerManager.jsp");
    PageSeed messageManager = new PageSeed(request.getContextPath() + "/applications/teamwork/messaging/messageManager.jsp");

    objectEditor.additionalButtons.add( new ButtonLink(I18n.get("LISTENER_MANAGEMENT"), listenerManager));
    objectEditor.additionalButtons.add( new ButtonLink(I18n.get("MESSAGING_MANAGEMENTS"), messageManager));
    objectEditor.additionalButtons.add( new ButtonLink(I18n.get("EVENT_MANAGEMENT"), eventManager));


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


