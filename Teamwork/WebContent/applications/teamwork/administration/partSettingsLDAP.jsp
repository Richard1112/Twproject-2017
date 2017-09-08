<%@ page import="org.jblooming.ldap.LdapUtilities, org.jblooming.security.AreaBricks,
 org.jblooming.system.SystemConstants, org.jblooming.tracer.Tracer, org.jblooming.utilities.CodeValueList, org.jblooming.utilities.JSP" %>
<%@ page import="org.jblooming.utilities.StringUtilities" %>
<%@ page import="org.jblooming.waf.constants.Commands" %>
<%@ page import="org.jblooming.waf.html.button.ButtonJS" %>
<%@ page import="org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.input.*, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.io.File" %>
<%@ page pageEncoding="UTF-8" %>


<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!Commands.SAVE.equals(pageState.command)) {
    
    pageState.addClientEntry(SystemConstants.AUTHENTICATION_TYPE, ApplicationState.getApplicationSetting(SystemConstants.AUTHENTICATION_TYPE));
    pageState.addClientEntry(LdapUtilities.BASE_DN, ApplicationState.getApplicationSetting(LdapUtilities.BASE_DN));
    pageState.addClientEntry(LdapUtilities.PROVIDER_URL, ApplicationState.getApplicationSetting(LdapUtilities.PROVIDER_URL));
    //pageState.addClientEntry(LdapUtilities.DOMAIN_NAME, ApplicationState.getApplicationSetting(LdapUtilities.DOMAIN_NAME));
    pageState.addClientEntry(LdapUtilities.SECURITY_AUTHENTICATION, ApplicationState.getApplicationSetting(LdapUtilities.SECURITY_AUTHENTICATION));
    pageState.addClientEntry(LdapUtilities.CREATE_USERS_ON_LOGIN, ApplicationState.getApplicationSetting(LdapUtilities.CREATE_USERS_ON_LOGIN));
    pageState.addClientEntry(LdapUtilities.CREATE_USERS_IN_AREA, ApplicationState.getApplicationSetting(LdapUtilities.CREATE_USERS_IN_AREA));
      pageState.addClientEntry(LdapUtilities.ENABLE_LDAP_SSL, ApplicationState.getApplicationSetting(LdapUtilities.ENABLE_LDAP_SSL));


    String secCr = ApplicationState.getApplicationSetting(LdapUtilities.SECURITY_CREDENTIALS);
    if (JSP.ex(secCr)) {
      try {
      pageState.addClientEntry(LdapUtilities.SECURITY_CREDENTIALS, StringUtilities.decrypt(secCr));
      pageState.addClientEntry("RETYPE_"+LdapUtilities.SECURITY_CREDENTIALS, StringUtilities.decrypt(secCr));
      } catch (Throwable t) {
        Tracer.platformLogger.error(t);
      }
    }
    pageState.addClientEntry(LdapUtilities.SECURITY_PRINCIPAL, ApplicationState.getApplicationSetting(LdapUtilities.SECURITY_PRINCIPAL));
    pageState.addClientEntry(LdapUtilities.LDAP_CONFIG_FILE, ApplicationState.getApplicationSetting(LdapUtilities.LDAP_CONFIG_FILE));

  }
  String typeAuthentication = pageState.getEntry(SystemConstants.AUTHENTICATION_TYPE).stringValueNullIfEmpty();
  if (typeAuthentication == null) {
    pageState.addClientEntry(SystemConstants.AUTHENTICATION_TYPE, SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_STANDARD_AUTHENTICATION);
    
  }
  boolean isLdapAuth = SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION.toString().equals(pageState.getEntry(SystemConstants.AUTHENTICATION_TYPE).stringValueNullIfEmpty())||
          SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION_WITH_FALLBACK_ON_STANDARD.toString().equals(pageState.getEntry(SystemConstants.AUTHENTICATION_TYPE).stringValueNullIfEmpty());
%>

<tr>
  <th colspan="99" class="tableHead secondRow lreq30 lreqLabel">Users authentication</th>
</tr>
<tr class="AUTH lreq30">
   <td nowrap="yes"><%
       RadioButton rb_std = new RadioButton(StringUtilities.replaceAllNoRegex(SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_STANDARD_AUTHENTICATION.toString(), "_", " ").toLowerCase(),SystemConstants.AUTHENTICATION_TYPE,SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_STANDARD_AUTHENTICATION.toString(),"&nbsp;",null,false, "");
       //rb_std.script = "$('#ldapConnection').hide();$('#httpAuthentication').hide();";
       rb_std.toHtml(pageContext);
      %>
  </td>
   <td nowrap="yes"><%
        RadioButton rb_http = new RadioButton(StringUtilities.replaceAllNoRegex(SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_HTTP_AUTHENTICATION.toString(), "_", " ").toLowerCase(),SystemConstants.AUTHENTICATION_TYPE,SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_HTTP_AUTHENTICATION.toString(),"&nbsp;",null,false, "");
        //rb_http.script = "$('#ldapConnection').hide();$('#httpAuthentication').show();";
        rb_http.toHtml(pageContext);
     %>
  </td>
  <td nowrap="yes"><%
    RadioButton rb_ldap = new RadioButton(StringUtilities.replaceAllNoRegex(SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION.toString(), "_", " ").toLowerCase(),SystemConstants.AUTHENTICATION_TYPE,SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION.toString(),"&nbsp;",null,false, "");
    //rb_ldap.script = "$('#ldapConnection').show();$('#httpAuthentication').hide();";
    rb_ldap.toHtml(pageContext);
      %>    
  </td>
  <td nowrap="yes"><%
    RadioButton rb_ldapf = new RadioButton(StringUtilities.replaceAllNoRegex(SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION_WITH_FALLBACK_ON_STANDARD.toString(), "_", " ").toLowerCase(),SystemConstants.AUTHENTICATION_TYPE,SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION_WITH_FALLBACK_ON_STANDARD.toString(),"&nbsp;",null,false, "");
    //rb_ldapf.script = "$('#ldapConnection').show();$('#httpAuthentication').hide();";
    rb_ldapf.toHtml(pageContext);
      %>
  </td>
</tr>


</table>
<div class="lreq30 lreqLabel">
<h2>LDAP Parameters</h2>
<table width="100%" cellpadding="3" id="help" class="table edged">
<tr>
    <th width="15%" class="tableHead">Parameter</th>
    <th width="30%%" class="tableHead">Current value</th>
    <th width="30%" class="tableHead">Sample value</th>
    <th class="tableHead">Description</th>
</tr>

<tr class="LDAP">
  <td><%


  CodeValueList cvl = new CodeValueList();
  File skinf = new File(request.getSession().getServletContext().getRealPath("/").replaceAll("\\\\", "/") + "/commons/settings/ldap");
  File[] files = skinf.listFiles();
  for (int i = 0; i < files.length; i++) {
    File file = files[i];
    if (!file.isDirectory() && (file.getName().toLowerCase().indexOf("properties") > -1))
      cvl.add(file.getName(),file.getName().substring(0,file.getName().lastIndexOf(".")));
  }
Combo combo = new Combo(StringUtilities.replaceAllNoRegex(LdapUtilities.LDAP_CONFIG_FILE, "_", " ").toLowerCase(), LdapUtilities.LDAP_CONFIG_FILE, "</td><td>", null, 20, null, cvl, "");
    combo.preserveOldValue=false;
    combo.toHtml(pageContext);%></td>

  <td>activeDirectory</td>
   <td>Select the type of LDAP server you are connecting to. Lists the ".properties" files in <i><%=skinf.getAbsolutePath()%></i>.</td>
</tr>

<tbody id="ldapConnection">
<tr >
  <td><%

    TextField textField = new TextField(StringUtilities.replaceAllNoRegex(LdapUtilities.BASE_DN, "_", " ").toLowerCase(), LdapUtilities.BASE_DN, "</td><td>", 40, false);
    textField.toolTip = LdapUtilities.BASE_DN;
    textField.toHtml(pageContext);
  %>
  </td>  
  <td>CN=Users,DC=open-lab,DC=com</td>
  <td>See the web page<br><a href="https://twproject.com/support/twproject-advanced-usage/connections/ldap-active-directory/" target="_blank">http://twproject.com/support/ldapactive-directory/</a>.</td>
</tr>
<tr>
  <td><%
    textField = new TextField(StringUtilities.replaceAllNoRegex(LdapUtilities.PROVIDER_URL, "_", " ").toLowerCase(), LdapUtilities.PROVIDER_URL, "</td><td>", 40, false);
    textField.toolTip = LdapUtilities.PROVIDER_URL;
    textField.toHtml(pageContext);
  %>
  </td>
  <td>myserver.mydomain.com</td>
  <td>&nbsp;</td>
  
</tr>
<%--
<tr>
  <td><%
    textField = new TextField(StringUtilities.replaceAllNoRegex(LdapUtilities.DOMAIN_NAME, "_", " ").toLowerCase(), LdapUtilities.DOMAIN_NAME, "</td><td>", 40, false);
    textField.toolTip = LdapUtilities.DOMAIN_NAME;
    textField.toHtml(pageContext);
  %>
  </td>
  <td>open-lab.com</td>
  <td>&nbsp;</td>

</tr>
--%>
<tr>
  <td><%
    textField = new TextField(StringUtilities.replaceAllNoRegex(LdapUtilities.SECURITY_AUTHENTICATION, "_", " ").toLowerCase(), LdapUtilities.SECURITY_AUTHENTICATION, "</td><td>", 40, false);
    textField.toolTip = LdapUtilities.SECURITY_AUTHENTICATION;

    textField.toHtml(pageContext);
  %>
  </td>
  <td>simple</td>
  <td>&nbsp;</td>
</tr>
  <tr>
  <td><%

    textField = new TextField(StringUtilities.replaceAllNoRegex(LdapUtilities.SECURITY_PRINCIPAL, "_", " ").toLowerCase(), LdapUtilities.SECURITY_PRINCIPAL, "</td><td>", 40, false);
    textField.toolTip = LdapUtilities.SECURITY_PRINCIPAL;
    textField.script="autocomplete=\"off\"";
    textField.toHtml(pageContext);
  %>
  </td>

  <td>testldap@mydomain.com</td>
  <td>&nbsp;</td>
</tr>
<tr>
  <td><%

    textField = new TextField(StringUtilities.replaceAllNoRegex(LdapUtilities.SECURITY_CREDENTIALS, "_", " ").toLowerCase(), LdapUtilities.SECURITY_CREDENTIALS, "</td><td>", 40, false);
    textField.toolTip = LdapUtilities.SECURITY_CREDENTIALS;
    textField.type="PASSWORD";
    textField.script="autocomplete=\"off\"";
    textField.toHtml(pageContext);
  %>
  </td>

  <td>somepwd</td>
  <td>&nbsp;</td>

</tr>
 <tr>
  <td><%

    textField = new TextField("retype "+StringUtilities.replaceAllNoRegex(LdapUtilities.SECURITY_CREDENTIALS, "_", " ").toLowerCase(), "RETYPE_"+LdapUtilities.SECURITY_CREDENTIALS, "</td><td>", 40, false);
     textField.type="PASSWORD";
    textField.script="autocomplete=\"off\"";
    textField.toHtml(pageContext);
  %>
  </td>

  <td>confirm somepwd</td>
   <td>&nbsp;</td>
</tr>

<tr>
  <td><%=StringUtilities.replaceAllNoRegex(LdapUtilities.CREATE_USERS_ON_LOGIN, "_", " ").toLowerCase()%></td><td><%

    SmartCombo cbb = AreaBricks.getAllAreas(LdapUtilities.CREATE_USERS_IN_AREA,null);
    cbb.label= I18n.get("AREA");
    cbb.required=false;
    cbb.separator="&nbsp;";
    cbb.fieldSize = 15;
    cbb.toHtml(pageContext);

    CheckField cluotf = new CheckField("create", LdapUtilities.CREATE_USERS_ON_LOGIN, "&nbsp;", false);
    cluotf.toHtml(pageContext);

  %>
  </td>

  <td>&nbsp;</td>
   <td>Whether any valid LDAP login will trigger a Twproject user creation if the user is missing, in the area selected (area is required).</td>
</tr>

 <tr>
  <td><%

    CheckField cf = new CheckField(StringUtilities.replaceAllNoRegex(LdapUtilities.ENABLE_LDAP_SSL, "_", " ").toLowerCase(), LdapUtilities.ENABLE_LDAP_SSL, "</td><td>", true);
    cf.toolTip = LdapUtilities.ENABLE_LDAP_SSL;
    cf.script="autocomplete=\"off\"";
    cf.toHtml(pageContext);
  %>
  </td>

  <td>no (default its false)</td>
  <td>Enable SSL over LDAP (LDAPS protocol)</td>
</tr>
</tbody>

 </table>

  <%
  ButtonBar bb = new ButtonBar();
  String relativePath = request.getContextPath() + "/applications/teamwork/administration/ldapCheck.jsp";
  String data = "'"+LdapUtilities.SECURITY_PRINCIPAL+"='+$('#"+LdapUtilities.SECURITY_PRINCIPAL+"').val()"+"+'&"+LdapUtilities.SECURITY_CREDENTIALS+"='+$('#"+LdapUtilities.SECURITY_CREDENTIALS+"').val()"
          +"+'&"+LdapUtilities.SECURITY_AUTHENTICATION+"='+$('#"+LdapUtilities.SECURITY_AUTHENTICATION+"').val()"+"+'&"+LdapUtilities.PROVIDER_URL+"='+$('#"+LdapUtilities.PROVIDER_URL+"').val()"
          +"+'&"+LdapUtilities.BASE_DN+"='+$('#"+LdapUtilities.BASE_DN+"').val()"
          +"+'&"+LdapUtilities.DOMAIN_NAME+"='+$('#"+LdapUtilities.DOMAIN_NAME+"').val()";
  String scriptSel = "$('#LDAP_CHECK_DIV').load('"+relativePath+"',"+data+");";
  PageSeed imp = pageState.pageInThisFolder("importLdapUsers.jsp",request);

  Container cldap = new Container("LDAPAUTH");
  cldap.title = "Twproject LDAP connection check";
  cldap.closeable=true;
  cldap.status=Container.HIDDEN;
  cldap.draggable=true;
  cldap.width="300px";
  cldap.height="90px";

  cldap.start(pageContext);%>
  <div id="LDAP_CHECK_DIV" align="center" ><br>
    <h3>Loading ...</h3>
   </div>
  <%
  cldap.end(pageContext);
  ButtonJS ocb = cldap.getOpenerButton(true);

  ocb.label="test LDAP connection";
  ocb.additionalOnClickScript=scriptSel;

  bb.addButton(ocb);
  bb.addSeparator(20);

  ButtonLink bimp = new ButtonLink("go to LDAP imports",imp);
  bb.addButton(bimp);
  bb.toHtml(pageContext);
 %></div>