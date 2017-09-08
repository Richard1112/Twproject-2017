<%@ page
    import="com.twproject.exchange.contacts.businessLogic.ResourcesImportControllerAction,
            com.twproject.operator.TeamworkOperator,
            com.twproject.resource.Resource,
            com.twproject.security.TeamworkPermissions,
            com.twproject.waf.TeamworkHBFScreen,
            org.jblooming.page.ListPage,
            org.jblooming.utilities.CodeValueList,
            org.jblooming.utilities.HashTable,
            org.jblooming.utilities.JSP,
            org.jblooming.waf.ScreenArea,
            org.jblooming.waf.SessionState,
            org.jblooming.waf.constants.Commands,
            org.jblooming.waf.html.button.ButtonLink,
            org.jblooming.waf.html.button.ButtonSubmit,
            org.jblooming.waf.html.container.ButtonBar,
            org.jblooming.waf.html.display.Paginator,
            org.jblooming.waf.html.input.CheckField,
            org.jblooming.waf.html.input.Combo,
            org.jblooming.waf.html.input.Uploader,
            org.jblooming.waf.html.state.Form,
            org.jblooming.waf.html.table.ListHeader,
            org.jblooming.waf.settings.I18n,
            org.jblooming.waf.view.PageSeed,
            org.jblooming.waf.view.PageState,
            java.util.Iterator,
            java.util.List,
            java.util.Map" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new ResourcesImportControllerAction(pageState), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    SessionState sessionState = pageState.getSessionState();
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


    Exception exc = (Exception) pageState.getAttribute("IMPORT_EXC");
    Map<String, Integer> columnsPositions = new HashTable();
    List<String[]> lines = null;

    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);
    Form f = new Form(self);
    f.encType = Form.MULTIPART_FORM_DATA;
    pageState.setForm(f);

    f.start(pageContext);

%>
<script type="text/javascript">
  function showDivByValue(divId) {
    var div = $('#' + divId);
    var value = $('#TYPE').val();
    if (value != ''){
      div.show();
    } else {
      div.hide();
    }
  }
</script>
<script>$("#RESOURCE_MENU").addClass('selected');</script>

<h1><%=I18n.get("IMPORT_RESOURCES")%></h1>

<hr>
<table class="table" cellspacing="0" border="0" cellpadding="5">
<%
    pageState.removeEntry("FILE_TO_IMPORT");
%>
<tr>
  <td>
    <span style="display: block; width: 30%"><%=I18n.get("CONTACT_FILE_IMPORT_DESCRIPTION")%></span>

      <code class="import">
      First Name, Last name, e-mail, Business phone, Mobile phone, Street, City, State, Postal code, Country, Username, Password</code>
  </td>
</tr>
  <tr>
    <td nowrap align="left" >
        <br><%
      Uploader u = new Uploader("FILE_TO_IMPORT", pageState);
      u.label =  I18n.get("RESOURCE_FILE_TO_IMPORT") + ":";
      u.separator = "<br>";
      u.size = 60;
      u.toHtml(pageContext);
    %><br></td>
  </tr>
  <tr><td>
    <label><%=I18n.get("SELECT_FILE_SOURCE")%></label>
    <%
      CodeValueList cvl = new CodeValueList();
      cvl.add(ResourcesImportControllerAction.FORMATS.OUTLOOK2003 + "", "Outlook 2003 (English)");
      cvl.add(ResourcesImportControllerAction.FORMATS.OUTLOOK2007 + "", "Outlook 2007 (English)");
      cvl.add(ResourcesImportControllerAction.FORMATS.THUNDERBIRD15 + "", "Mozilla Thunderbird (1.5)");
      cvl.addChoose(pageState);
      Combo cb = new Combo("TYPE", "", "", 50, cvl, null);
      cb.label = "";//I18n.get(cb.fieldName);
      cb.fieldSize = 10;
      cb.separator = "<br>";
      cb.setJsOnChange = "showDivByValue('test')";
      cb.toHtml(pageContext);
    %>
  </td></tr>
</table>
<div id="test" style="display:none; padding: 7px">
  Given the differences of the application underlying model, not everything can be imported.<br>
  Warn: in case of Outlook only data from Outlook in English can be imported.<br>
  We suggest to revise all resources modified after import; they are all marked with "imported" in notes.<br>

  <%//=I18n.get("IMPORT_RESOURCES_FROM_FILE_DISCLAIM")%><br>
  <small>Microsoft Outlook is (c) Microsoft Corporation. Mozilla Thunderbird is (c) the Mozilla Corporation.<br>
    Ical4J is Copyright (c) 2006, Ben Fortuna. All rights reserved. See http://ical4j.sourceforge.net <br>
  </small>
</div>


<%


  if (lines == null) {
    lines = (List<String[]>) sessionState.getAttribute("RESOURCE_IMPORT_LINES");
    columnsPositions = (Map<String, Integer>) sessionState.getAttribute("RESOURCE_IMPORT_CP");
  }

  ButtonBar bb2 = new ButtonBar();

  ButtonSubmit saveInstance = ButtonSubmit.getSaveInstance(f, I18n.get("READ_CONTACTS_FROM_FILE"));
  saveInstance.variationsFromForm.command = "READ_FILE";
  saveInstance.additionalCssClass="big first";
  bb2.addButton(saveInstance);

  if (lines != null) {

    /* ButtonSubmit importAll = ButtonSubmit.getSaveInstance(f, "import all page");
   importAll.variationsFromForm.command = "IMPORT_ALL";
   bb2.addButton(importAll); */

    ButtonSubmit importSel = ButtonSubmit.getSaveInstance(f,I18n.get("IMPORT_SELECTED"));
    importSel.variationsFromForm.command = "IMPORT_SELECTED";
    importSel.additionalCssClass="big";
    bb2.addButton(importSel);
  }

  bb2.toHtml(pageContext);



  if (exc != null) {

%><br><span class="warning"><%=I18n.get("RESOURCE_IMPORT_FORMAT_FAILED")%>:<%=exc.getMessage()%></span><%




} else if (lines != null) {

  String importType = pageState.getEntry("RESOURCE_IMPORT_FORMAT").stringValueNullIfEmpty();
  if (!JSP.ex(importType))
    importType = "CSV";
  ResourcesImportControllerAction.FORMATS csvFormat = ResourcesImportControllerAction.FORMATS.valueOf(importType);

  ListPage listPage = new ListPage(lines, Paginator.getWantedPageNumber(pageState), Paginator.getWantedPageSize("RESIMPORT", pageState));
  pageState.setPage(listPage);






%>
<table border="0" cellpadding="0" class="table">
  <tr>
    <td><%new Paginator("RESIMP", f).toHtml(pageContext);%></td>
  </tr>
</table>

<table border="0" class="table"><%
  ListHeader lh = new ListHeader("RESIMPLH", f);

  lh.addHeaderFitAndCentered(I18n.get("NAME"));
  lh.addHeaderFitAndCentered(I18n.get("FLD_SURNAME"));
  if ("CSV".equals(importType)) {
    lh.addHeaderFitAndCentered(I18n.get("USERNAME"));
    lh.addHeaderFitAndCentered(I18n.get("PASSWORD"));
  }
  lh.addHeaderFitAndCentered("e-mail");
  lh.addHeaderFitAndCentered(I18n.get("MOBILE"));
  lh.addHeaderFitAndCentered(I18n.get("ADDRESS"));

  lh.addHeaderFitAndCentered(I18n.get("WHAT_FOUND_AND_ACTION"));
  lh.toHtml(pageContext);


  int lineCounterInPage = 0;
  PageSeed ps = pageState.pageInThisFolder("resourceEditor.jsp", request);
  ps.setCommand(Commands.EDIT);

  for (Iterator iterator = pageState.getPage().getThisPageElements().iterator(); iterator.hasNext();) {

    String[] line = (String[]) iterator.next();

    String firstName = line[columnsPositions.get(ResourcesImportControllerAction.realName(ResourcesImportControllerAction.COLUMNS.First_Name, csvFormat))];
    String lastName = line[columnsPositions.get(ResourcesImportControllerAction.realName(ResourcesImportControllerAction.COLUMNS.Last_Name, csvFormat))];
    String email = line[columnsPositions.get(ResourcesImportControllerAction.realName(ResourcesImportControllerAction.COLUMNS.E__mail_Address, csvFormat))];
    String userName = line[columnsPositions.get(ResourcesImportControllerAction.realName(ResourcesImportControllerAction.COLUMNS.Username, csvFormat))];
    String password = line[columnsPositions.get(ResourcesImportControllerAction.realName(ResourcesImportControllerAction.COLUMNS.Password, csvFormat))];
    String Business_Phone = line[columnsPositions.get(ResourcesImportControllerAction.realName(ResourcesImportControllerAction.COLUMNS.Business_Phone, csvFormat))];
    String Mobile_Phone = line[columnsPositions.get(ResourcesImportControllerAction.realName(ResourcesImportControllerAction.COLUMNS.Mobile_Phone, csvFormat))];


    String Business_Street = line[columnsPositions.get(ResourcesImportControllerAction.realName(ResourcesImportControllerAction.COLUMNS.Business_Street, csvFormat))];
    String Business_City = line[columnsPositions.get(ResourcesImportControllerAction.realName(ResourcesImportControllerAction.COLUMNS.Business_City, csvFormat))];
    String Business_State = line[columnsPositions.get(ResourcesImportControllerAction.realName(ResourcesImportControllerAction.COLUMNS.Business_State, csvFormat))];
    String Business_Postal_Code = line[columnsPositions.get(ResourcesImportControllerAction.realName(ResourcesImportControllerAction.COLUMNS.Business_Postal_Code, csvFormat))];
    String Business_Country = line[columnsPositions.get(ResourcesImportControllerAction.realName(ResourcesImportControllerAction.COLUMNS.Business_Country, csvFormat))];

%>
<tr class="alternate" >
  <td><%=JSP.w(firstName)%>&nbsp;</td>
  <td><%=JSP.w(lastName)%>&nbsp;</td>
  <%
    if ("CSV".equals(importType)) {
  %>
  <td><%=JSP.w(userName)%>&nbsp;</td>
  <td><%=JSP.w(password)%>&nbsp;</td>
  <%
    }
  %>
  <td><%=JSP.w(email)%>&nbsp;</td>

  <td>
    <%=JSP.w(Business_Phone)%>
    <%=JSP.w(Mobile_Phone)%>
  </td>

  <td>
    <%=JSP.w(Business_Street)%>
    <%=JSP.w(Business_City)%>
    <%=JSP.w(Business_State)%>
    <%=JSP.w(Business_Postal_Code)%>
    <%=JSP.w(Business_Country)%>
  </td>

  <td nowrap>
    <%

      List<Resource> found = ResourcesImportControllerAction.findResource(firstName, lastName, email, pageState);
      if (found.size() == 0) {

        CheckField createAll = new CheckField("RES_CREATE_ALL_" + lineCounterInPage, "&nbsp;", false);
        createAll.label = I18n.get("RES_CREATE_ALL");
    %>
    <table width="100%">
      <tr>
        <td><%createAll.toHtml(pageContext);%></td>
      </tr>
    </table>
    <%

    } else if (found.size() == 1) {
    %>
    <table width="100%">
      <tr>
        <td width="50%"><%
          Resource resource = found.get(0);
          ps.mainObjectId = resource.getId();
          ButtonLink edit = new ButtonLink(JSP.w(resource.getDisplayName()) + " " + resource.getDefaultEmail(), ps);
          edit.toHtmlInTextOnlyModality(pageContext);
        %></td>
        <td><%
          if (resource.hasPermissionFor(logged, TeamworkPermissions.resource_canWrite)) {
            CheckField updateName = new CheckField("RES_NAME_UPD_" + lineCounterInPage, "&nbsp;", false);
            updateName.label = I18n.get("RES_NAME_UPD");
            updateName.toHtml(pageContext);
        %><br><%
          CheckField updateEmail = new CheckField("RES_EMAIL_UPD_" + lineCounterInPage, "&nbsp;", false);
          updateEmail.label = I18n.get("RES_EMAIL_UPD");
          updateEmail.toHtml(pageContext);%></td>
      </tr>
    </table>
    <%
    } else {
    %>You don't have permissions' to update this resource.<%
    }

  } else {
  %>Multiple resources found: non determined update.<br><%
    for (Resource resource : found) {
      ButtonLink edit = new ButtonLink(JSP.w(resource.getDisplayName()) + " " + resource.getDefaultEmail(), ps);
      //edit.additionalCssClass="big first";
      edit.toHtmlInTextOnlyModality(pageContext);
  %><br><%
      }
    }


  %>&nbsp;</td>


</tr>
<%

    //add email to
    //update email to
    //create resource
    //as child of

    lineCounterInPage++;
  }
%></table>
<%
    }

    //list

    f.end(pageContext);

  }
%>