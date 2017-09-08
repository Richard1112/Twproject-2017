<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.waf.TeamworkHBFScreen,
                 org.jblooming.messaging.SomethingHappened,
                 org.jblooming.operator.Operator,
                 org.jblooming.persistence.objectEditor.FieldFeature,
                 org.jblooming.persistence.objectEditor.ObjectEditor,
                 org.jblooming.persistence.objectEditor.businessLogic.ObjectEditorController, org.jblooming.waf.ScreenArea, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState"
%>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    logged.testIsAdministrator();

    // definizione object editor

    /*
    private SerializedMap<String, String> messageParams = new SerializedMap();
  private String theClass;
  private String identifiableId;
  private String eventType;
  private Date happenedAt = new Date();
  private Date happeningExpiryDate;
  private String messageTemplate;
  private String link;
  private Operator whoCausedTheEvent;

    */

    ObjectEditor objectEditor = new ObjectEditor(I18n.get("EVENT_MANAGEMENT"), SomethingHappened.class, pageContext);
    objectEditor.defaultOrderBy="event.id";
    objectEditor.query = "from "+SomethingHappened.class.getName()+" as event";
    objectEditor.mainHqlAlias = "event";
    objectEditor.bulkActions=true;

    objectEditor.addDisplayField("theClass", I18n.get("EVENT_EVENT_TYPE"));
    objectEditor.addDisplayField("identifiableId", I18n.get("EVENT_EVENT_OBJECT"));
    objectEditor.addDisplayField("eventType", I18n.get("EVENT_COMMAND"));
    objectEditor.addDisplayField("happenedAt", I18n.get("EVENT_HAPPENED_AT"));
    objectEditor.addDisplayField("happeningExpiryDate", I18n.get("EVENT_HAPPENING_EXPIRY_DATE"));

    // Operator combo
    String hql = "select operator.id, operator.name || ' ' || operator.surname from " + Operator.class.getName() + " as operator";
    String whereForId = "where operator.id = :" + SmartCombo.FILTER_PARAM_NAME;
    String whereForFiltering =
    " where operator.name || ' ' || operator.surname like :" + SmartCombo.FILTER_PARAM_NAME +
    " or operator.surname || ' ' || operator.name like :" + SmartCombo.FILTER_PARAM_NAME + " order by operator.surname, operator.name";
    SmartCombo ops = new SmartCombo("owner", hql, whereForFiltering, whereForId);

    FieldFeature ffo = new FieldFeature("whoCausedTheEvent",I18n.get("EVENT_WHO_CAUSED"));
    ffo.smartCombo = ops;
    ffo.smartComboClass = Operator.class;
    objectEditor.addDisplayField(ffo);

    // edit == list
    objectEditor.editFields=objectEditor.displayFields;

    PageSeed listenerManager = new PageSeed(request.getContextPath() + "/applications/teamwork/messaging/listenerManager.jsp");
    PageSeed messageManager = new PageSeed(request.getContextPath() + "/applications/teamwork/messaging/messageManager.jsp");

    objectEditor.additionalButtons.add( new ButtonLink(I18n.get("LISTENER_MANAGEMENT"), listenerManager));
    objectEditor.additionalButtons.add( new ButtonLink(I18n.get("MESSAGING_MANAGEMENTS"), messageManager));

    ObjectEditorController oec = new ObjectEditorController(objectEditor);
    //
    final ScreenArea body = new ScreenArea(oec, request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {
    pageState.getMainJspIncluder().toHtml(pageContext);
%><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>

<%
  }
%>


