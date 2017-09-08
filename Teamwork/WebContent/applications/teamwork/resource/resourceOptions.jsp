<%@ taglib prefix="pd4ml" uri="http://pd4ml.com/tlds/pd4ml/2.5" %>
<%@ page import="com.opnlb.website.page.WebSitePage, com.opnlb.website.waf.WebSiteConstants, com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.resource.Resource,
                 com.twproject.resource.businessLogic.ResourceController, com.twproject.waf.TeamworkHBFScreen,  com.twproject.waf.html.ResourceHeaderBar, org.jblooming.messaging.MessagingSystem,
                 org.jblooming.operator.businessLogic.OptionController, org.jblooming.oql.OqlQuery,  org.jblooming.system.SystemConstants,
                 org.jblooming.utilities.CodeValueList, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.OperatorConstants, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.display.Img,org.jblooming.waf.html.input.*,
                 org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Collections, java.util.List, org.jblooming.waf.html.button.*, com.twproject.security.TeamworkPermissions" %>
<%@page pageEncoding="UTF-8" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new ResourceController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    pageState.toHtml(pageContext);

  } else {
    Person person = (Person) pageState.getMainObject();

    boolean isNew = person.isNew();


    TeamworkOperator myself = person.getMyself();


    PageSeed seed = pageState.thisPage(request);
    seed.setMainObjectId(person.getId());
    seed.setCommand(Commands.EDIT);
    Form form = new Form(seed);
    pageState.setForm(form);
    form.alertOnChange = true;
    form.encType = Form.MULTIPART_FORM_DATA;
    form.start(pageContext);

    boolean canWrite = person.hasPermissionFor(pageState.getLoggedOperator(),TeamworkPermissions.resource_canWrite) || person.equals(Person.getLoggedPerson(pageState));
%>

<div class="mainColumn">
<%

  //---------------------------------------- HEAD BAR -------------------------------------------
  pageState.addClientEntry("RESOURCE_TABSET","GENERAL_OPTIONS_TAB");
  ResourceHeaderBar head = new ResourceHeaderBar(person);
  head.pathToObject.destination=pageState.pageInThisFolder("resourceEditor.jsp",request);
  head.pathToObject.destination.command=Commands.EDIT;
  head.toHtml(pageContext);


  String email = person.getDefaultEmail();
  boolean emailDisabled = !JSP.ex(email);

%>
<table cellpadding="10" class="table">
<tr>
  <td align="right" valign="top" nowrap><%
    Combo cb = I18n.getLocaleCombo(OperatorConstants.FLD_SELECT_LANG, pageState);
    cb.label=I18n.get("SELECT_LANGUAGE_AND_LOCALE");
    cb.readOnly=!canWrite;

    cb.toHtml(pageContext);

    %><br><%=JSP.wHelp(I18n.get("SELECT_LANGUAGE_AND_LOCALE_HELP"))%></td>
</tr>
<%

  String hql = "from " + WebSitePage.class.getName() + " as page where page.name!='ROOT' ";
  OqlQuery oql = new OqlQuery(hql);
  List<WebSitePage> wsp = oql.list();

  if (wsp.size() > 0) {

    CodeValueList cvl = new CodeValueList();
    for (WebSitePage p : wsp) {
      if(p.isActive())
      cvl.add(p.getName() + ".page", p.getFrontOfficeTitle());
    }
    cvl.addChoose(pageState);
    Combo c = new Combo(WebSiteConstants.HOME_PAGE, "</td><td>", null, 25, cvl, null);
    c.label = "PICK_HOME_PAGE";
    c.readOnly=!canWrite;
%>
<tr>
  <td align="right" valign="top" nowrap><%
    c.toHtmlI18n(pageContext);%><br><%=JSP.wHelp(I18n.get("PICK_HOME_PAGE_HELP"))%>
  </td>
</tr>
<%
  }
%>
<tr>
  <td align="right" valign="top" nowrap><%
    new TextField(I18n.get(OperatorConstants.OP_PAGE_SIZE), OperatorConstants.OP_PAGE_SIZE, "</td><td>", 3, !canWrite).toHtml(pageContext);%>
    <br><%=JSP.wHelp(I18n.get("OP_PAGE_SIZE_HELP"))%>
  </td>

</tr>
<tr>
  <td align="right" valign="top" nowrap><%
    ColorChooser colorChooser = new ColorChooser("PREFERRED_COLOR");
    colorChooser.colorSquareWidth = 40;
    colorChooser.colorSquareHeight = 20;
    colorChooser.fieldSize = 2;
    colorChooser.label=I18n.get("CHOOSE_YOUR_PREFERRED_COLOR");
    colorChooser.separator="</td><td>";
    colorChooser.disabled=!canWrite;
    colorChooser.toHtml(pageContext);
    %><br><%=JSP.wHelp(I18n.get("CURRENT_SKIN_HELP"))%>
  </td>
</tr>


<%-- ------------------- AGENDA --------------------------------------------------------------%>

<tr class="alternate"><td colspan="3"></td></tr>
<%--tr style="display:none;">
  <td align="right" valign="top" nowrap><label><%=I18n.get("AGENDA_HOURS")%></label></td>
  <td colspan="2" valign="top"><%
    ComboBox cbWHB = ComboBox.getTimeInstance(OperatorConstants.FLD_WORKING_HOUR_BEGIN, I18n.get(OperatorConstants.FLD_WORKING_HOUR_BEGIN), pageState);
    cbWHB.separator = "&nbsp;";
    cbWHB.fieldSize = 5;
    cbWHB.readOnly=!canWrite;
    cbWHB.toHtml(pageContext);
     %><span class="block pTB-2"></span><%
    ComboBox cbWHE = ComboBox.getTimeInstance(OperatorConstants.FLD_WORKING_HOUR_END, I18n.get(OperatorConstants.FLD_WORKING_HOUR_END), pageState);
    cbWHE.separator = "&nbsp;";
    cbWHE.fieldSize = 5;
    cbWHE.readOnly=!canWrite;
    cbWHE.toHtml(pageContext);%><span class="block pTB-2"></span><%=JSP.wHelp(I18n.get("WORKING_HOUR_END_HELP"))%>
  </td>
</tr--%>
<tr>
  <td align="right" valign="top"><%
    CheckField genIcal = new CheckField(OperatorConstants.SEND_EVENT_BY_ICAL, "</td><td valign=\"top\">", true);
    genIcal.label = I18n.get("ICAL_GENERATE_EVENT");
    genIcal.disabled = emailDisabled || !canWrite;
    genIcal.toHtml(pageContext);
  %><br><%=JSP.wHelp(I18n.get("WORKING_CAL_OPTIONS_HELP"))%>
    <%
      String pop3 = ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_HOST);
      String twMailAddr = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_FROM);

      if (JSP.ex(pop3) && JSP.ex(twMailAddr)) {
    %><br><%=JSP.wHelp(I18n.get("ICAL_INVITE_ADDRESS_%%_HELP", twMailAddr))%><%
    }
  %><br><%
      if (emailDisabled) {
    %><br><%=JSP.wHelp(I18n.get("CLIENT_DISABLED_HELP"))%><%
    }
  %>
  </td>
</tr>
<tr>
  <td align="right" valign="top" nowrap><label><%=I18n.get(OperatorConstants.MEDIA_PREFERRED_CHANNEL)%></label></td>
<td><%

    for (MessagingSystem.Media media : MessagingSystem.activeMedia) {
      String subscrField = OperatorConstants.MEDIA_PREFERRED_CHANNEL + "_" + media.toString().toUpperCase();
      CheckField mediaCf = new CheckField(subscrField, "", false);
      mediaCf.label = I18n.get(media.toString().toUpperCase());
      mediaCf.disabled=!canWrite;
      mediaCf.toHtml(pageContext);
  %>&nbsp;<%
    }
  %><br><%=JSP.wHelp(I18n.get("MEDIA_PREFERRED_CHANNEL_HELP"))%></td>
</tr>


<%-- -----------------------------------GOOGLE CALENDAR------------------------------------------------- --%>

<tr class="alternate"><td colspan="3"></td></tr>
<%--<tr>
  <td align="right" valign="top"><%
    CheckField cfGooogle = new CheckField(I18n.get("SEND_TO_GOOGLE"), "SEND_TO_GOOGLE", "</td><td colspan='2'>", true);
    cfGooogle.toHtml(pageContext);
  %>&nbsp;&nbsp;Google account: <%
    new TextField("user", "GOOGLE_LOGIN_USER", "&nbsp;", 15, !canWrite).toHtml(pageContext);
  %>&nbsp;&nbsp; See <a href="http://www.google.com/calendar" target="_blank">Google calendar</a>©
  </td>
</tr>--%>
    <tr>
<td align="right" valign="top"><label><%=I18n.get("TW_CALENDAR_EXTERNAL_ACCESS")%></label></td>
        <td>

            <%
                String href = JSP.encode(ApplicationState.serverURL + "/agendaInIcal.ics?" + Commands.COMMAND + "=EXTERNAL&USR=" + myself.getId() + "&CK=" + myself.getPassword());
            %>

            <textarea class="formElements" rows="2" cols="40" style="width: 60%;font-size:10pt;" readonly><%=href%></textarea><br>
            <%=JSP.wHelp(I18n.get("TW_CALENDAR_%%_LINK_DESCRIPTION", href))%></td>
    </tr>
<tr>
  <td align="right" valign="top">
    <%
      new TextField(I18n.get("DEFAULT_EXT_CALENDAR"), "DEFAULT_EXT_CALENDAR", "</td><td colspan=\"2\">", 70, !canWrite).toHtml(pageContext);
    %>
  </td>
</tr>

<%-- ----------------------------------- SUBSCRIPTIONS ------------------------------------------------- --%>
<tr class="alternate"><td colspan="3"></td></tr>
<tr>
  <td align="right" valign="top"><label><%=I18n.get(OperatorConstants.NOTIFY_MY_EVENTS_TO_MYSELF)%></label></td>

  <td><%
      CheckField mediaCf = new CheckField( OperatorConstants.NOTIFY_MY_EVENTS_TO_MYSELF, "", false);
      mediaCf.label = "";
      mediaCf.disabled=!canWrite;
      mediaCf.toHtml(pageContext);
  %><br><%=JSP.wHelp(I18n.get("NOTIFY_MY_EVENTS_TO_MYSELF_HELP"))%> </td>
</tr>



<%-- ----------------------------------- HINTS ------------------------------------------------- --%>
<tr class="alternate"><td colspan="3"></td></tr>
<tr>
  <td align="right" valign="top" nowrap><label><%=I18n.get("HINTS")%></label></td><td colspan="3">
  <%
    ButtonJS bjs = new ButtonJS(I18n.get("SKIP_ALL_HINTS"), "skipAllHints();");
    bjs.enabled=canWrite;
    bjs.toHtmlInTextOnlyModality(pageContext);
    %>&nbsp;&nbsp;&nbsp;&nbsp;<%
    bjs=new ButtonJS(I18n.get("REDO_ALL_HINTS"),"redoAllHints();");
    bjs.enabled=canWrite;
    bjs.toHtmlInTextOnlyModality(pageContext);
%>

</td></tr>
<tr class="alternate"><td colspan="3"></td></tr>

</table>
<%
  Container allOpt = new Container();
  allOpt.level = 1;
  allOpt.title = I18n.get("ALL_OPTIONS");
  allOpt.status = Container.HIDDEN;
  allOpt.centeredOnScreen = true;
  allOpt.width = "800px";
  allOpt.height = "600px";
  allOpt.overflow = "auto";
  allOpt.closeable = true;

  ButtonBar buttonBar = new ButtonBar();
  buttonBar.loggableIdentifiableSupport = person;

  ButtonSubmit saveButton= ButtonSubmit.getSaveInstance(form,I18n.get("SAVE"));
  saveButton.variationsFromForm.command="SAVE_OPT";
  saveButton.additionalCssClass="first big";
  saveButton.enabled=canWrite;
  buttonBar.addButton(saveButton);

  if (!isNew && !person.equals(Person.getLoggedPerson(pageState))) {
    DeletePreviewer deletePreviewer = new DeletePreviewer("RES_DEL",ResourceController.class, pageState);
    deletePreviewer.normalizeInstanceToSuperclass = Resource.class;

    ButtonSupport delPrev = deletePreviewer.getDeleteButton(I18n.get("DELETE"), person.getId() );
    delPrev.enabled = person.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.resource_canDelete);
    delPrev.additionalCssClass="big delete";
    buttonBar.addButton(delPrev);
  }

  ButtonJS allOptB = allOpt.getOpenerButton(false);
  allOptB.label = I18n.get("ALL_OPTIONS");
  allOptB.additionalCssClass="big";
  buttonBar.addButton(allOptB);

  buttonBar.toHtml(pageContext);

    allOpt.start(pageContext);
%>
<table class="table"><%
  List<String> optsSort = new ArrayList<String>(myself.getOptions().keySet());
  Collections.sort(optsSort);

  for (String key : optsSort) {
%>
  <tr class="alternate"  id="tr_option_<%=key%>">
    <td><%=key%>
    </td>
    <td><%=myself.getOption(key)%>
    </td>
    <td><%

      ButtonJS removeRowAndRefresh = new ButtonJS("executeCommand('CALLCONTR', 'CTCL=" + OptionController.class.getName() + "&CTRM=REMOVE_OPTION&OBID=" + myself.getId() + "___" + key + "');");
      removeRowAndRefresh.confirmRequire = true;
      removeRowAndRefresh.confirmQuestion = I18n.get("FLD_CONFIRM_DELETE");
      removeRowAndRefresh.additionalOnClickScript = "$('#tr_option_" + key + "').fadeOut('slow',function(){$(this).remove()});";

      removeRowAndRefresh.iconChar="d";
      removeRowAndRefresh.additionalCssClass="delete";
      removeRowAndRefresh.label="";
      removeRowAndRefresh.toolTip="" + key;
      removeRowAndRefresh.toHtmlInTextOnlyModality(pageContext);

    %></td>
  </tr>
  <%
    }
  %></table>
<%


  allOpt.end(pageContext);

%></div><%

    //---------------------------------------- HEAD BAR -------------------------------------------
    JspHelper side = new JspHelper("part/partResourceSideBar.jsp");
    side.parameters.put("RESOURCE", person);
    side.toHtml(pageContext);

form.end(pageContext);


%><script type="text/javascript">

  function skipAllHints() {
    var req = {CM:"SKIPALLHINTS"};
    showSavingMessage();
    $.getJSON(contextPath + "/applications/teamwork/resource/resourceAjaxController.jsp", req, function (response) {
      hideSavingMessage();
      showFeedbackMessage("OK", "<%=I18n.get("DONE")%>");
    });
  }

  function redoAllHints() {
    var req = {CM:"REDOALLHINTS"};
    showSavingMessage();
    $.getJSON(contextPath + "/applications/teamwork/resource/resourceAjaxController.jsp", req, function (response) {
      hideSavingMessage();
      showFeedbackMessage("OK", "<%=I18n.get("DONE")%>");
    });
  }




</script>
<%
  }
%>