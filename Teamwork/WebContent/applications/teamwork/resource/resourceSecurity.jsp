<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.resource.businessLogic.ResourceController, com.twproject.waf.TeamworkHBFScreen, com.twproject.waf.html.ResourceHeaderBar,
org.jblooming.ldap.LdapUtilities, org.jblooming.operator.Operator, org.jblooming.system.SystemConstants, org.jblooming.utilities.JSP,org.jblooming.waf.ScreenArea,
org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.OperatorConstants, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.Selector, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, com.twproject.resource.Company, org.jblooming.waf.html.button.ButtonSupport, com.twproject.security.TeamworkPermissions, org.jblooming.security.PlatformPermissions, org.jblooming.agenda.CompanyCalendar" %>
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
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    Resource resource = (Resource) pageState.getMainObject();

    if (resource instanceof Company){
      %><script>location.href="resourceEditor.jsp?CM=ED&OBJID=<%=resource.getId()%>";</script><%
    }


    TeamworkOperator resourceOperator = resource.getMyself();


    //boolean canWrite =resource.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite)||resource.equals(logged.getPerson());
    boolean canWrite =resource.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite);

    boolean disabledBecauseOfLDAP = SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION.toString().equals(ApplicationState.getApplicationSetting(SystemConstants.AUTHENTICATION_TYPE)) && !logged.hasPermissionAsAdmin();
    // in case of fallback you may want to create new users by hand  ||  SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION_WITH_FALLBACK_ON_STANDARD.toString().equals(ApplicationState.getApplicationSetting(SystemConstants.AUTHENTICATION_TYPE));

    PageSeed seed = pageState.thisPage(request);
    seed.setMainObjectId(resource.getId());
    seed.setCommand(Commands.EDIT);

  Form form = new Form(seed);
  pageState.setForm(form);
  form.alertOnChange = true;
  form.start(pageContext);

%>
<div class="mainColumn">
<%
   //---------------------------------------- HEAD BAR -------------------------------------------
   pageState.addClientEntry("RESOURCE_TABSET","RESOURCE_SECURITY_TAB");
   ResourceHeaderBar head = new ResourceHeaderBar(resource);
   head.toHtml(pageContext);

 %>

<div class="inlineContainerWrapper">
<div class="container">
<table class="table">
<tr>
<td style="padding-right:50px; padding-top: 15px">
  <table class="table">
    <tr><%
      TextField tf = new TextField(I18n.get("FLD_LOGIN_NAME"), "LOGIN_NAME", "<br>", 15, false);

      if (pageState.getEntry("LOGIN_NAME").stringValueNullIfEmpty() != null)
        tf.script = " onChange=\"alert('" + I18n.get("ALERT_CHANGE_ALSO_PSW") + "');obj('PWD').value='';obj('PWD_RETYPE').value='a';obj('PWD').focus();\" ";
      tf.readOnly = disabledBecauseOfLDAP || !canWrite;
      tf.required=true;

      tf.script = tf.script + " autocomplete=\"off\"";


      %><td style="width: 20%"><%tf.toHtml(pageContext);%></td><%
      tf = new TextField("PASSWORD", I18n.get("FLD_PWD"), "PWD", "<br>", 15, false);
      tf.readOnly = !canWrite || disabledBecauseOfLDAP;
      tf.script = "autocomplete=\"off\"";

      %><td style="width: 20%"><%tf.toHtml(pageContext);%></td><%

      tf = new TextField("PASSWORD", I18n.get("FLD_PWD_RETYPE"), "PWD_RETYPE", "<br>", 15, false);
      tf.readOnly = !canWrite || disabledBecauseOfLDAP;
      tf.script = "autocomplete=\"off\"";

      %><td style="width: 50%"><%tf.toHtml(pageContext);%></td>

    </tr>
    <tr>
        <td colspan="3" style="padding-top: 30px">
            <div>
                <%
                    CheckField checkBox = new CheckField(OperatorConstants.FLD_ADMINISTRATOR, "&nbsp;", false);
                    checkBox.label = "ADMINISTRATOR";
                    checkBox.disabled = !logged.hasPermissionAsAdmin();

                    checkBox.toHtmlI18n(pageContext);
                %>&nbsp;&nbsp;
                <%
                    CheckField dis = new CheckField(OperatorConstants.FLD_IS_ENABLED, "&nbsp;", false);
                    dis.disabled = !canWrite;
                    dis.additionalOnclickScript="alertDisabledUser($(this));";
                    dis.toHtmlI18n(pageContext);
                %>&nbsp;&nbsp;
                <%
                    CheckField cf = new CheckField("USER_HIDDEN", "&nbsp;", false);
                    cf.disabled = !canWrite;
                    cf.toHtmlI18n(pageContext);%>
            </div>

        </td>
       </tr>
    <%

      String auth_type = ApplicationState.getApplicationSetting(SystemConstants.AUTHENTICATION_TYPE);

      if (SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION.toString().equals(auth_type)) {
        %><tr><td colspan="5"><%
        if (resourceOperator==null || LdapUtilities.getLdapUser(resourceOperator.getLoginName(), LdapUtilities.getDefaultContext()) != null){
          %>&nbsp;<%=I18n.get("USER_FOUND_IN_LDAP")%>&nbsp;<%
        } else {
          %>&nbsp;<%=I18n.get("USER_NOT_FOUND_IN_LDAP")%>&nbsp;<%
        }
        %></td></tr><%
      }

  %>
    <tr>
      <td valign="top" colspan="4" style="padding-top: 20px"><%

      Selector c = new Selector("roles", pageState.getForm());
        c.disabled = !resource.hasPermissionFor(logged, PlatformPermissions.area_canManage);
        c.label = I18n.get("DIRECT_ROLES");
        c.height = "250px";
        c.selectedOnTop = true;
        c.toHtml(pageContext);


        Operator owner = resource.getOwner();
        %><hr><small>
          <%=I18n.get("OPERATOR_ID")%>: <%=(resourceOperator != null) ? resourceOperator.getId() : "-"%>
          &nbsp;&nbsp;&nbsp;
          <%=I18n.get("OWNER")%>: (id: <%=owner != null ? owner.getId() : "-"%>) <%=owner != null ? owner.getDisplayName() : ""%><%

          if (resourceOperator!=null && resourceOperator.getLastLoggedOn()!=null){
        %><br><%=I18n.get("LAST_LOGIN")+": "+JSP.timeStamp(resourceOperator.getLastLoggedOn())%><br><%
          }
        %></small>




        </td>
    </tr>
  </table>
</td>
</tr>
</table>

     <%

  ButtonBar buttonBar = new ButtonBar();
  buttonBar.loggableIdentifiableSupport = resource;

  ButtonSubmit saveButton=ButtonSubmit.getSaveInstance(form,I18n.get("SAVE"));
  saveButton.variationsFromForm.command="SV_SECURITY";
  saveButton.additionalCssClass="first big";
  saveButton.enabled=canWrite;
  buttonBar.addButton(saveButton);


  //si mette il bottono di notifica se la risorsa è creata da meno di 3 giornio
  if (System.currentTimeMillis()-resource.getCreationDate().getTime()< CompanyCalendar.MILLIS_IN_DAY*3) {
    ButtonJS sendAccountNotification = new ButtonJS();
    sendAccountNotification.label = I18n.get("SEND_ACCOUNT_NOTIFICATION");
    if (resourceOperator != null && JSP.ex(resource.getDefaultEmail())) {
      sendAccountNotification.onClickScript = "sendAccountNotifications('" + resourceOperator.getId() + "','" + sendAccountNotification.getId() + "', '" + I18n.get("ACCOUNT_NOTIFICATION_SENT") + "');";
    } else {
      sendAccountNotification.enabled = false;
      sendAccountNotification.toolTip = I18n.get("DISABLED_FOR_MISSINGEMAIL");
    }
    sendAccountNotification.additionalCssClass = "big";
    buttonBar.addButton(sendAccountNotification);
  }

  //se non hai i permessi di write e sei te si mette un bottone changePassword
  if (!canWrite && !disabledBecauseOfLDAP && resource.equals(logged.getPerson())){
    ButtonSupport changePassword = ButtonLink.getBlackInstance(I18n.get("CHANGE_PASSWORD"), 350, 500, pageState.pageInThisFolder("changePassword.jsp", request));
    changePassword.additionalCssClass = "big";
    buttonBar.addButton(changePassword);
  }

  if (!resource.equals(logged.getPerson())) {
    DeletePreviewer deletePreviewer = new DeletePreviewer("RES_DEL",ResourceController.class, pageState);
    deletePreviewer.normalizeInstanceToSuperclass = Resource.class;
    ButtonSupport delPrev = deletePreviewer.getDeleteButton(I18n.get("DELETE"), resource.getId() );
    delPrev.enabled = resource.hasPermissionFor(logged, TeamworkPermissions.resource_canDelete);
    delPrev.additionalCssClass="big delete";
    buttonBar.addButton(delPrev);
  }

  buttonBar.toHtml(pageContext);



%>



     </div>
    <div class="container" style="width: 300px">
        <%if (resourceOperator == null) {%><b><%=JSP.wHelp(I18n.get("HELP_CREATE_LOGIN"))%></b><%}%>
        <span class="descrEl"><%=I18n.get("HELP_SECURITY")%><%ButtonLink.getDescriptiveLinkInstance(I18n.get("HELP_MORE"), "http://twproject.com/support/security-faq/").toHtml(pageContext);%></span>
    </div>
</div>
</div>
<%
  //---------------------------------------- SIDE BAR -------------------------------------------
  JspHelper side = new JspHelper("part/partResourceSideBar.jsp");
  side.parameters.put("RESOURCE", resource);
  side.toHtml(pageContext);
%>

<script type="text/javascript">
  function sendAccountNotifications(receiverId,btnId,label) {
    $.getJSON("resourceAjaxController.jsp",{CM:"SENDNOTIFACCOUNT",RECEIVER:receiverId},function(response){
      jsonResponseHandling(response);
      if (response.ok) {
        $('#'+btnId).replaceWith('<del class=\"button noprint\"><span>'+label+'</span></del>');
      }
    });
  }


  function alertDisabledUser(el){
    //console.debug("alertDisabledUser")
    if (!el.is(":checked"))
      if (!confirm("<%=I18n.get("WARNING_DISABLING_USER")%>"))
        el.click();
  }
</script>

<%
    form.end(pageContext);
  }
%>    
