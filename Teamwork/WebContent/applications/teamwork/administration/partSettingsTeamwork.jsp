<%@ page import="com.twproject.exchange.msproject.businessLogic.ProjectImportExportControllerAction, org.jblooming.utilities.CodeValueList, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!Commands.SAVE.equals(pageState.command)) {

    pageState.addClientEntry("DEFAULT_PROJECT_MANAGER_ROLE_NAME", ApplicationState.getApplicationSetting("DEFAULT_PROJECT_MANAGER_ROLE_NAME"));
    pageState.addClientEntry("DEFAULT_WORKER_ROLE_NAME", ApplicationState.getApplicationSetting("DEFAULT_WORKER_ROLE_NAME"));
    pageState.addClientEntry("DEFAULT_CUSTOMER_ROLE_NAME", ApplicationState.getApplicationSetting("DEFAULT_CUSTOMER_ROLE_NAME"));

    pageState.addClientEntry("ROLE_SCRUM_MASTER_NAME", ApplicationState.getApplicationSetting("ROLE_SCRUM_MASTER_NAME"));
    pageState.addClientEntry("ROLE_SCRUM_TEAM_NAME", ApplicationState.getApplicationSetting("ROLE_SCRUM_TEAM_NAME"));

    pageState.addClientEntry("MILESTONE_ALERT_DELTA", ApplicationState.getApplicationSetting("MILESTONE_ALERT_DELTA"));


    pageState.addClientEntry("MPXJ_LOCALE", ApplicationState.getApplicationSetting("MPXJ_LOCALE"));
    pageState.addClientEntry("SET100ONCLOSE", ApplicationState.getApplicationSetting("SET100ONCLOSE"));
    pageState.addClientEntry("GENTASKCODES", ApplicationState.getApplicationSetting("GENTASKCODES"));
    pageState.addClientEntry("USEUNIQUECODES", ApplicationState.getApplicationSetting("USEUNIQUECODES"));
    pageState.addClientEntry("DISABLEWORKFLOW", ApplicationState.getApplicationSetting("DISABLEWORKFLOW"));

  }
%>
<tr>
  <td class="lreq30 lreqLabel"><%
    TextField textField = new TextField("DEFAULT_PROJECT_MANAGER_ROLE_NAME", "DEFAULT_PROJECT_MANAGER_ROLE_NAME", "</td><td class=\"lreq30\">", 20, false);
    textField.toolTip = "DEFAULT_PROJECT_MANAGER_ROLE_NAME";
    textField.toHtmlI18n(pageContext);%>
 </td>
    
  <td class="lreq30">Project manager</td>
  <td class="lreq30">Used for finding a default project manager role in automated role setting. Change this value only if you are SURE of what you are doing.</td>
</tr>
<tr>
  <td class="lreq30 lreqLabel"><%

    textField = new TextField("DEFAULT_WORKER_ROLE_NAME", "DEFAULT_WORKER_ROLE_NAME", "</td><td class=\"lreq30\">", 20, false);
    textField.toolTip = "DEFAULT_WORKER_ROLE_NAME";
    textField.toHtmlI18n(pageContext);%>
 </td>
  <td class="lreq30">Worker</td>
  <td class="lreq30">Used for finding a default worker role in automated role setting. Change this value only if you are SURE of what you are doing.
</td>
</tr>
<tr>
  <td class="lreq30 lreqLabel"><%
    textField = new TextField("DEFAULT_CUSTOMER_ROLE_NAME", "DEFAULT_CUSTOMER_ROLE_NAME", "</td><td class=\"lreq30\">", 20, false);
    textField.toolTip = "DEFAULT_CUSTOMER_ROLE_NAME";
    textField.toHtmlI18n(pageContext);%>
 </td>
  <td class="lreq30">Customer</td>
  <td class="lreq30">Used for finding a default customer role in automated role setting. Change this value only if you are SURE of what you are doing.<br>
  <%
    ButtonLink bl = new ButtonLink("Create new area and roles.",pageState.pageFromRoot("security/security.jsp"));
    bl.toHtmlInTextOnlyModality(pageContext);
  %></td>
</tr>
<tr>
  <td class="lreq30 lreqLabel"><%
    textField = new TextField("ROLE_SCRUM_MASTER_NAME", "ROLE_SCRUM_MASTER_NAME", "</td><td class=\"lreq30\">", 20, false);
    textField.toolTip = "ROLE_SCRUM_MASTER_NAME";
    textField.toHtmlI18n(pageContext);%>
  </td>
  <td class="lreq30">Scrum Master</td>
  <td class="lreq30">Name of the Scrum master role, used by the Scrum master wizard.</td>
</tr>
<tr>
  <td class="lreq30 lreqLabel"><%
    textField = new TextField("ROLE_SCRUM_TEAM_NAME", "ROLE_SCRUM_TEAM_NAME", "</td><td class=\"lreq30\">", 20, false);
    textField.toolTip = "ROLE_SCRUM_TEAM_NAME";
    textField.toHtmlI18n(pageContext);%>
  </td>
  <td class="lreq30">Scrum Team</td>
  <td class="lreq30">Name of the Scrum team member role, used by the Scrum master wizard.&nbsp;
    <%bl.toHtmlInTextOnlyModality(pageContext);%></td>
</tr>
<tr>
  <td><%
    textField = TextField.getIntegerInstance("MILESTONE_ALERT_DELTA");
    textField.fieldSize=3;
    textField.toolTip = "MILESTONE_ALERT_DELTA";
    textField.toHtmlI18n(pageContext);%>
 </td>    
  <td>3</td>
  <td>How many days before the milestone deadline you want to get warnings.</td>
</tr>



<tr>
  <td class="lreq20 lreqLabel"><%
    //mpxj supported
    CodeValueList cvl = ProjectImportExportControllerAction.getLocalizations();
    cvl.addChoose(pageState);
    
    Combo cb = new Combo("MPXJ_LOCALE", "</td><td class=\"lreq20\">", null, 20, cvl, null);
    cb.toolTip = "MPXJ_LOCALE";
    cb.toHtmlI18n(pageContext);%>
 </td>
  <td class="lreq20" >English</td>
  <td class="lreq20">Default Microsoft localization for which projects will be exported and imported.<br>
    <%
    bl = new ButtonLink("Import a project.",pageState.pageFromRoot("task/taskImportMPX.jsp"));
    bl.toHtmlInTextOnlyModality(pageContext);
  %>
  </td>
</tr>

<tr>
  <td><%
    //set 100% on completed projects
    CheckField set100 = new CheckField("SET100ONCLOSE","</td><td>",true);
    set100.toHtmlI18n(pageContext);%>
 </td>
  <td>no</td>
  <td>Set progress to 100% when closing a task. </td>
</tr>

<tr>
  <td><%
    CheckField uuc = new CheckField("USEUNIQUECODES","</td><td>",true);
    uuc.toHtmlI18n(pageContext);%>
 </td>
  <td>no</td>
  <td>When true, codes on task, resources and eventually issues should be "unique" or empty.</td>
</tr>

<tr>
  <td><%
    //CheckField genTaskCodes = new CheckField("GENTASKCODES","</td><td>",true);
    //genTaskCodes.toHtmlI18n(pageContext);
    CodeValueList cvll= new CodeValueList();
    cvll.add(Fields.FALSE,"no");
    cvll.add(Fields.TRUE,"yes");
    cvll.add("yes_year", I18n.get("GENTASKCODES_YEAR"));

    Combo c=new Combo("GENTASKCODES","</td><td>",null,20,cvll,null);
    c.toHtmlI18n(pageContext);
     %>
 </td>
  <td>no</td>
  <td>Generate task codes as a progressive number based on the type </td>
</tr>

<tr>
  <td class="lreq30 lreqLabel"><%

    CheckField dwf = new CheckField("DISABLEWORKFLOW","</td><td  class=\"lreq30\">",true);
    dwf.toHtmlI18n(pageContext);%>
 </td>
  <td class="lreq30">no</td>
  <td class="lreq30">When unchecked you will be able to create project as process.</td>
</tr>
