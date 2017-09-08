<%@ page import="com.twproject.document.businessLogic.TeamworkDesignerController, com.twproject.security.TeamworkPermissions, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.designer.Designer,
                org.jblooming.ontology.Documentable, org.jblooming.persistence.PersistenceHome, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState"%><%

  PageState pageState = PageState.getCurrentPageState(request);

  String urlToInclude = pageState.getEntry("DESIGNER_URL_TO_INCLUDE").stringValueNullIfEmpty();

  String designerName = pageState.getEntry("DESIGNER_NAME").stringValue();

  Class clazz = Class.forName(pageState.getEntryAndSetRequired("CLAZZ").stringValue());

  Documentable documentable= (Documentable) PersistenceHome.findByPrimaryKey(clazz, pageState.mainObjectId);

  Designer designer = new Designer(urlToInclude, designerName, clazz, pageState.mainObjectId);
  designer.readOnly = !documentable.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.document_canWrite);
  designer.exportable = pageState.getEntry("EXPORT_MODALITY").checkFieldValue();


  designer.configFields(pageContext);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TeamworkDesignerController(designer), "/applications/teamwork/document/drawModule.jsp");
    body.areaHtmlClass="lreq30 lreqPage";
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    PageSeed me = pageState.thisPage(request);
    me.addClientEntry("DESIGNER_URL_TO_INCLUDE", urlToInclude);
    me.addClientEntry("DESIGNER_NAME", designerName);
    me.addClientEntry("CLAZZ",clazz.getName());
    me.addClientEntry("PRINT",false);
    me.setPopup(true);
    me.mainObjectId = documentable.getId();
    me.addClientEntry("EXPORT_MODALITY", Fields.FALSE);

    Form form = new Form(me);
    form.alertOnChange = true;
    form.encType = Form.MULTIPART_FORM_DATA;
    form.start(pageContext);

    designer.drawDesigner(form, pageContext);

    ButtonBar bb = designer.buttonBar;
    if (!designer.readOnly && !designer.exportable) {
      ButtonSubmit saveBt = ButtonSubmit.getSaveInstance(designer.form, I18n.get("SAVE"));
      saveBt.id="saveFormButton";
      saveBt.alertOnRequired = false;
      saveBt.additionalCssClass = "big first";
      bb.addButton(saveBt);
    }

    if (!designer.exportable) {
      ButtonSubmit printBt = new ButtonSubmit(designer.form);
      printBt.variationsFromForm.setCommand(Commands.EDIT);
      printBt.variationsFromForm.addClientEntry("EXPORT_MODALITY", Fields.TRUE);
      printBt.variationsFromForm.addClientEntry("PRINT", true);
      printBt.label = I18n.get("PRINT");
      printBt.additionalCssClass = "big";
      bb.addButton(printBt);
    }

    bb.toHtml(pageContext);

    form.end(pageContext);

    if (pageState.getEntry("PRINT").checkFieldValue()){
      %><script type="text/javascript">print()</script><%
    }
  }
%>
