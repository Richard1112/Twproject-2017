<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.security.RoleTeamwork, com.twproject.security.TeamworkPermissions,
com.twproject.security.businessLogic.LdapController, com.twproject.security.businessLogic.LdapImportUsers, com.twproject.waf.TeamworkHBFScreen,
org.jblooming.agenda.ScheduleSupport, org.jblooming.ldap.LdapUtilities, org.jblooming.oql.OqlQuery, org.jblooming.page.Page, org.jblooming.scheduler.Job, org.jblooming.security.Area, org.jblooming.security.LdapUser, org.jblooming.system.SystemConstants, org.jblooming.utilities.CodeValueList, org.jblooming.utilities.JSP, org.jblooming.utilities.ReflectionUtilities, org.jblooming.utilities.StringUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.container.TabSet, org.jblooming.waf.html.input.*, org.jblooming.waf.html.state.Form, org.jblooming.waf.html.table.ListHeader, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, javax.naming.directory.DirContext, java.util.*" %>

<%
    PageState pageState = PageState.getCurrentPageState(request);
    String command = pageState.getCommand();
    if(command == null){
      //get all ldap parameters from global properties file
        pageState.addClientEntry(SystemConstants.AUTHENTICATION_TYPE, ApplicationState.getApplicationSetting(SystemConstants.AUTHENTICATION_TYPE));
        pageState.addClientEntry(LdapUtilities.BASE_DN, ApplicationState.getApplicationSetting(LdapUtilities.BASE_DN));
        pageState.addClientEntry(LdapUtilities.INITIAL_CONTEXT_FACTORY,"com.sun.jndi.ldap.LdapCtxFactory");
        pageState.addClientEntry(LdapUtilities.PROVIDER_URL, ApplicationState.getApplicationSetting(LdapUtilities.PROVIDER_URL));
        pageState.addClientEntry(LdapUtilities.DOMAIN_NAME, ApplicationState.getApplicationSetting(LdapUtilities.DOMAIN_NAME));
        pageState.addClientEntry(LdapUtilities.SECURITY_AUTHENTICATION, ApplicationState.getApplicationSetting(LdapUtilities.SECURITY_AUTHENTICATION));
      String secCr = ApplicationState.getApplicationSetting(LdapUtilities.SECURITY_CREDENTIALS);
      if (JSP.ex(secCr)) {
        pageState.addClientEntry(LdapUtilities.SECURITY_CREDENTIALS, StringUtilities.decrypt(secCr));
      }
        pageState.addClientEntry(LdapUtilities.SECURITY_PRINCIPAL, ApplicationState.getApplicationSetting(LdapUtilities.SECURITY_PRINCIPAL));
    }
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    if (!pageState.screenRunning) {
      pageState.screenRunning = true;
      final ScreenArea body = new ScreenArea(new LdapController(), request);
      body.areaHtmlClass="lreq30 lreqPage";
      TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
      lw.register(pageState);
      pageState.perform(request, response);
      pageState.toHtml(pageContext);

    } else { %>
<%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
%>
<%adminLink.toHtmlInTextOnlyModality(pageContext);%>
<h1>LDAP user import</h1>
<%
      PageSeed self = pageState.thisPage(request);
      self.setCommand(Commands.FIND);
      Form f = new Form(self);
      pageState.setForm(f);
      Container search = new Container();
      search.title = I18n.get("LDAP_USERS_LIST");
      search.level = 3;
      search.collapsable = true;
      search.start(pageContext);
      f.start(pageContext);

 %><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>

<table class="table"cellpadding="3" cellspacing="0" border="0">
  <tr>
    <td valign="top"><table width="100%">
      <tr class="formSearchLabel">
      <td><%
          TextField tf = new TextField(StringUtilities.replaceAllNoRegex(LdapUtilities.BASE_DN, "_", " ").toLowerCase(), LdapUtilities.BASE_DN, "<br>", 40, false);
          tf.addKeyPressControl(13,"obj('"+ pageState.getForm().getUniqueName() +"').submit();", "onkeyup");
          pageState.setFocusedObjectDomId(tf.id);
          tf.required=true;
          tf.toHtml(pageContext);
          %>
      </td>
       <td><%

          tf = new TextField(StringUtilities.replaceAllNoRegex(LdapUtilities.PROVIDER_URL, "_", " ").toLowerCase(), LdapUtilities.PROVIDER_URL, "<br>", 40, false);
          tf.addKeyPressControl(13,"obj('"+ pageState.getForm().getUniqueName() +"').submit();", "onkeyup");
          tf.required=true;
          pageState.setFocusedObjectDomId(tf.id);
          tf.toHtml(pageContext);
        %>
        </td>
       </tr>
       <tr>
        <td><%
          String filter =  pageState.getEntry("ldapfilter").stringValueNullIfEmpty();

          if(filter == null)
            pageState.addClientEntry("ldapfilter", LdapUtilities.USERSEARCHFILTER);
          tf = new TextField(I18n.get("LDAP_FILTER"),"ldapfilter", "<br>", 40, false);
          tf.addKeyPressControl(13,"obj('"+ pageState.getForm().getUniqueName() +"').submit();", "onkeyup");
          tf.required=true;
          pageState.setFocusedObjectDomId(tf.id);
          tf.toHtml(pageContext);
        %>
        </td>
         <td><%
          tf = new TextField(StringUtilities.replaceAllNoRegex(LdapUtilities.SECURITY_AUTHENTICATION, "_", " ").toLowerCase(), LdapUtilities.SECURITY_AUTHENTICATION, "<br>", 40, false);
          tf.addKeyPressControl(13,"obj('"+ pageState.getForm().getUniqueName() +"').submit();", "onkeyup");
          tf.required=true;
          tf.toHtml(pageContext);
          %>
        </td>
      </tr>
      <tr>

        <td><%

          tf = new TextField(StringUtilities.replaceAllNoRegex(LdapUtilities.SECURITY_CREDENTIALS, "_", " ").toLowerCase(), LdapUtilities.SECURITY_CREDENTIALS, "<br>", 40, false);
          tf.addKeyPressControl(13,"obj('"+ pageState.getForm().getUniqueName() +"').submit();", "onkeyup");
          tf.type="PASSWORD";
          tf.required=true;
          tf.toHtml(pageContext);
        %>
        </td>
        <td>
          <%
          tf = new TextField(StringUtilities.replaceAllNoRegex(LdapUtilities.SECURITY_PRINCIPAL, "_", " ").toLowerCase(), LdapUtilities.SECURITY_PRINCIPAL, "<br>", 40, false);
          tf.addKeyPressControl(13,"obj('"+ pageState.getForm().getUniqueName() +"').submit();", "onkeyup");
          tf.toHtml(pageContext);
          tf.required=true;
        %>
        </td>
      </tr>
      <tr>
        <td colspan="2">
          <%CheckField cf = new CheckField(I18n.get("LDAP_SYNCRONIZE_DISABLED_USERS"),"TAKE_DISABLE_LDAPUSERS", "&nbsp;", false);
          cf.toHtml(pageContext);%>
        </td>
      </tr>

      </table></td>
      <td><%
      Selector sgroups = new Selector("ldapGroups",f);
      sgroups.label = I18n.get("LDAP_GROUPS");
      sgroups.height="150px";
      sgroups.selectedOnTop=true;
      sgroups.toHtml(pageContext);
      %>
    </td>
  </tr>
 </table>

  
<%

    ButtonBar bb = new ButtonBar();

    LoadSaveFilter lsf = new LoadSaveFilter("LDAPFILTER", f);
    bb.addButton(lsf);


    ButtonSupport qbe = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("HELP")), 700, 800, pageState.pageFromCommonsRoot("help/qbe.jsp"));
    qbe.toolTip = I18n.get("HELP_QBE");
    bb.addButton(qbe);
    bb.addSeparator(10);


    ButtonSubmit bs = ButtonSubmit.getSearchInstance(f, pageState);
    bs.preserveFormStatus=true;
    bs.alertOnRequired = true;
    bb.addButton(bs);

    bb.toHtml(pageContext);


  search.end(pageContext);

    Set<String> chosenGroupIds = Selector.chosen("ldapGroups", pageState).keySet();
    String filterGroup="";
    for (String groupName : chosenGroupIds) {
       filterGroup+="(memberOf="+groupName+")";
    }
    if(filterGroup.length() >0)
      filterGroup="(|"+filterGroup+")";
  
    TreeMap<String, String> chosenUsers = Collector.chosen("ldapUsers", pageState);
    TreeMap<String, String> candidateUsers = new TreeMap<String, String>();
    Collector.clearClientEntry("ldapUsers", pageState);


    Resource res = null;



    String basedn = pageState.getEntry(LdapUtilities.BASE_DN).stringValueNullIfEmpty();
    filter =  pageState.getEntry("ldapfilter").stringValueNullIfEmpty();

    try{
      DirContext ctx = LdapUtilities.getContext(pageState.getEntry(LdapUtilities.PROVIDER_URL).stringValueNullIfEmpty(),
              pageState.getEntry(LdapUtilities.SECURITY_AUTHENTICATION).stringValueNullIfEmpty(),pageState.getEntry(LdapUtilities.SECURITY_PRINCIPAL).stringValueNullIfEmpty(),pageState.getEntry(LdapUtilities.SECURITY_CREDENTIALS).stringValueNullIfEmpty());
      Vector<LdapUser> users = LdapUtilities.getUsers(filterGroup,filter,basedn,ctx);
      if (users!=null) {
        for (LdapUser user : users) {
          if (user.getUserPrincipalName() != null && chosenUsers.get(user.getUserPrincipalName()) == null)
            candidateUsers.put(user.getUserPrincipalName(), user.getDisplayUser());
        }
      }

    } catch (Throwable e) {
      pageState.getEntry("ldapfilter").errorCode=e.getMessage();
    }

    Collector.make("ldapUsers", candidateUsers, chosenUsers, pageState);
    Collector col = new Collector("ldapUsers", 200, f);
   // add Local Roles
    if(chosenUsers.size() >0) {
       List<RoleTeamwork> rts = null;
      String hql = "from "+ RoleTeamwork.class.getName()+" as role where role.localToAssignment = :localToAssignment";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setBoolean("localToAssignment",false);
      rts = oql.list();

      TreeMap<String, String> mroles = new TreeMap<String, String>();
      // add TeamWork Administration Role
      mroles.put("Administrator","administrator");
      for(RoleTeamwork role:rts)
        mroles.put(role.getDisplayName(),role.getId().toString());
        col.checkBoxes =mroles;
    } 
    col.toHtml(pageContext);


%>
<%
  Container action = new Container();
  action.collapsable=true;
  action.title = I18n.get("LDAP_IMPORT_ACTION");
  action.level = 3;
  action.start(pageContext);%>
<table class="table"cellpadding="3" cellspacing="0" border="0">
  <tr class="formSearchLabel">

    <td><%

        String area = pageState.getEntry("AREA").stringValueNullIfEmpty();
        //select default area for logged operator
        if(area == null){
          Set<Area> areas =  logged.getAreasOwned();
          if(areas.size() >0){

           Area[] arr_areas = areas.toArray(new Area[0]);
           Area firstArea = arr_areas[0];
           pageState.addClientEntry("AREA",firstArea.getId());
          }
        }

      Set<Area> areas = logged.getAreasForPermission(TeamworkPermissions.resource_canRead);
      CodeValueList cvl = CodeValueList.getI18nInstanceForIdentifiables(areas,pageState);
      Combo cbb = new Combo("AREA", "</td><td colspan=3>","", 30,null,cvl, "");
      cbb.label=I18n.get("AREA");
      cbb.required=false;
      cbb.toHtml(pageContext);
           
    %></td>
    <td><%
      cf = new CheckField(I18n.get("LDAP_UPDATE_EXISTING_USERS"),"UPDATE_LDAPUSERS", "&nbsp;", false);
      cf.toHtml(pageContext);
    %>
  </td>
  <td>
    <%TextField txt_pwd = new TextField(I18n.get("DEFAULT_PWD"),"DEFAULT_PWD","</td><td>",20,false);
      txt_pwd.toHtml(pageContext);
    %>
  </td>
    <td><%
      cf = new CheckField(I18n.get("LDAP_UPDATE_ADDITIONAL_PROPERTIS"),"ADDITIONAL_PROPERTIES", "&nbsp;", false);
      cf.additionalOnclickScript="$('#ldapAdditionalProperties').toggle();";
      cf.toHtml(pageContext);
    %>
  </td>
  
  </tr>
</table>
<%

  String ldapFileName = ApplicationState.getApplicationSetting(LdapUtilities.LDAP_CONFIG_FILE);
  if (!JSP.ex(ldapFileName))
    ldapFileName = "activeDirectory.properties";


  action.end(pageContext);
  Container pdapProperties = new Container();
  pdapProperties.collapsable=true;
  pdapProperties.level = 3;
  pdapProperties.title = I18n.get("LDAP_CUSTOM_PROPERTIES")+ " <small>(file:"+ldapFileName+")</small>";
  pdapProperties.start(pageContext);

  Properties hldapProperties = LdapUtilities.loadLdapMappingFromFile();

  Iterator iter =  hldapProperties.keySet().iterator();
%>
<div id="ldapAdditionalProperties"  style="display:<%= pageState.getEntry("ADDITIONAL_PROPERTIES").checkFieldValue()?"block":"none"%>">
<table class="table"cellpadding="3" cellspacing="0" border="0">
  <%
    int row=1;
    while(iter.hasNext()){
      int pos = row % 2;
      String name = (String)iter.next();
      String property =  hldapProperties.getProperty(name);
      if(pos ==1){
       %><tr class="formSearchLabel"><%
      }
      %><td><%
      if(pageState.getEntry(name).stringValueNullIfEmpty() == null)
        pageState.addClientEntry(name,property);
      tf = new TextField(I18n.get(name),name, "</td><td>",20, false);
      tf.toHtml(pageContext);

      %></td><%
      if(pos ==0){
        %></tr><%
      }
      row++;
    }%>
</table>  
</div>
<%
  pdapProperties.end(pageContext);

  ButtonBar bbimp = new ButtonBar();

  Container cjob = new Container();
  cjob.title = I18n.get("LIST_LDAP_JOBS");
  cjob.status = Container.HIDDEN;
  cjob.centeredOnScreen = true;
  cjob.width="400px";
  ButtonJS btb_job = cjob.getOpenerButton(false);
  btb_job.label = I18n.get("LDAP_JOBS");

  bbimp.addButton(btb_job);
  cjob.start(pageContext);
  String hql = "from " + Job.class.getName() + " as job where job.executable=:ldapClass";
  OqlQuery oql = new OqlQuery(hql);
  oql.getQuery().setString("ldapClass", LdapImportUsers.class.getName());
  List<Job> jobList = oql.list();
  PageSeed ps = pageState.pageFromCommonsRoot("scheduler/jobEditor.jsp");
  ps.command= Commands.EDIT;
  if(jobList.size() >0) {

%>
   <table class="table" cellpadding="2" cellspacing="1" border="0">
     <tr><td>Job Name</td><td>Job Scheduled</td></tr>

  <%for(Job job: jobList){

    %> <tr valign="top" class="alternate" >
        <td><%ps.mainObjectId= job.getId();
              new ButtonLink(job.getName(),ps).toHtmlInTextOnlyModality(pageContext);%></td>
        <td><%=((ScheduleSupport) ReflectionUtilities.getUnderlyingObject(job.getSchedule())).getScheduleDescription("")%></td>

      </tr> <%
     }
  } else {
   %><tr><td colspan="2" align="center">No import is currently scheduled.</td></tr><%
 }
 %>
    <%ps.command= Commands.ADD;
      ps.addClientEntry("LAUNCHER_CLASS",LdapImportUsers.class.getName());%>
      <tr><td colspan="2" align="right"><%new ButtonLink("Create scheduled import",ps).toHtml(pageContext);%></td></tr> 
    </table>
   <%
   cjob.end(pageContext);

    PageSeed configPage = pageState.pageFromRoot("administration/teamworkGlobalSettings.jsp");
    TabSet.pointToTab("genTabSet", "secTS", configPage);
    bbimp.addButton(new ButtonLink("LDAP settings", configPage));

    bbimp.addSeparator(20);

    ButtonSubmit bimp = new ButtonSubmit(f);
    bimp.label = "<b>"+I18n.get("LDAP_IMPORT")+"</b>";
    bimp.variationsFromForm.setCommand("IMPORT");
    bbimp.addButton(bimp);

    bimp.enabled= chosenUsers.size()>0;

    bbimp.toHtml(pageContext);
%>
<%
  if("IMPORT".equalsIgnoreCase(command)){
%>

 <%
 String updUsers = pageState.getEntry("updateUsers").stringValueNullIfEmpty();
 String addUsers = pageState.getEntry("addUsers").stringValueNullIfEmpty();
 boolean update_users = pageState.getEntry("UPDATE_LDAPUSERS").checkFieldValue();
   
 Container cdata = new Container();
 cdata.closeable = true;
 cdata.width = "600";
 cdata.centeredOnScreen = true;
 cdata.title = "Import Ldap Result";
 cdata.start(pageContext);%>
 <table id="help" align="center">
    <tr><td height="30px">&nbsp;</td></tr>
    <tr>
      <td align="center"><h2><%=update_users ? "Updated users "+updUsers : "No users updated as not selected"%>; added users <%=addUsers%>.</h2></td>
   </tr>
   <tr>

   </tr>
</table>
<div id="ldap_list_detail" style="height :200px; overflow : auto;">
    <table class="table" cellpadding="2" cellspacing="1" border="0">
      <tr>
         <%
            ListHeader lh = new ListHeader("LDAPUSER", f);
            lh.addHeaderFitAndCentered(I18n.get("FLD_LOGIN_NAME"));
            lh.addHeaderFitAndCentered(I18n.get("FLD_NAME"));
            lh.addHeaderFitAndCentered(I18n.get("FLD_SURNAME"));
            lh.addHeaderFitAndCentered(I18n.get("FLD_PWD"));
            lh.toHtml(pageContext);
        %>
     </tr>
     <%
        Page ldapUsers = pageState.getPage();
        if (ldapUsers != null) {
          for (Object o : ldapUsers.getAllElements()) {
            LdapUser ldapUser = (LdapUser) o;
      %>
      <tr valign="top" class="alternate" >
        <td><%=ldapUser.getUsername()%></td>
        <td><%=JSP.w(ldapUser.getName())%></td>
         <td><%=JSP.w(ldapUser.getSurname())%></td>
         <td><%= pageState.getEntry("DEFAULT_PWD").stringValueNullIfEmpty() != null ? pageState.getEntry("DEFAULT_PWD").stringValueNullIfEmpty(): ldapUser.getUsername()%></td>
      </tr>
      <%  }
        }

      %>
    </table>
   </div>
 <%cdata.end(pageContext);
 %>

<%}
    f.end(pageContext);
  }
%>