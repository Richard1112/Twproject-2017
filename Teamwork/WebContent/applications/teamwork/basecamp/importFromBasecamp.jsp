<%@ page import="com.twproject.exchange.BasecampControllerAction,
                 com.twproject.resource.Company,
                 com.twproject.resource.Person,
                 com.twproject.resource.ResourceBricks,
                 com.twproject.security.RoleTeamwork,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Task,
                 com.twproject.task.TaskBricks,
                 com.twproject.waf.TeamworkHBFScreen,
                 org.jblooming.ontology.Pair,
                 org.jblooming.persistence.PersistenceHome,
                 org.jblooming.utilities.HashTable,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.constants.RoleConstants,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.input.RadioButton,
                 org.jblooming.waf.html.input.SmartCombo,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 org.w3c.dom.Node,
                 org.w3c.dom.NodeList,
                 java.util.HashSet,
                 java.util.List,
                 java.util.Map,
                 java.util.Set" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    BasecampControllerAction bh = new BasecampControllerAction(pageState);
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(bh, request);
    pageState.attributes.put("BasecampControllerAction", bh);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    BasecampControllerAction bh = (BasecampControllerAction) pageState.attributes.get("BasecampControllerAction");

    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = pageState.mainObjectId;
    self.setCommand("CONNECT");
    Form form = new Form(self);
    form.encType = Form.MULTIPART_FORM_DATA;
    form.alertOnChange = true;
    pageState.setForm(form);

    form.start(pageContext);

    %><script>$("#TASK_MENU").addClass('selected');</script>
<h1><%=(bh.connected ? "Connected to Basecamp. <small>Inspect your current account: <a href=\"" + pageState.getEntry("BASECAMP_HOST_URL").stringValue() + "\"  target=\"_blank\">" + pageState.getEntry("BASECAMP_HOST_URL").stringValue() + "</a><small>" : "Connect to BaseCamp" )%></h1>
<hr>
<table cellpadding="5">
  <tr>
    <td nowrap><%
      //["http://intergeo.updatelog.com/"]
      TextField tf = new TextField("BASECAMP_HOST_URL", "<br>");
      tf.label = "Host url";
      tf.required = true;
      tf.fieldSize = 30;
      tf.preserveOldValue = false;
      tf.toHtmlI18n(pageContext);
    %></td>
    <td><%
      tf = new TextField("BASECAMP_HOST_USER", "<br>");
      tf.label = "User";
      tf.required = true;
      tf.fieldSize = 20;
      tf.preserveOldValue = false;
      tf.script = " autocomplete=\"false\"";

      tf.toHtmlI18n(pageContext);
    %></td>
    <td><%
      tf = new TextField("BASECAMP_HOST_PSW", "<br>");
      tf.fieldSize = 20;
      tf.label = "Password";
      tf.type = "PASSWORD";
      tf.required = true;
      tf.addKeyPressControl(13, "obj('" + form.getUniqueName() + "').submit();", "onkeyup");
      tf.preserveOldValue = false;
      tf.script = " autocomplete=\"false\"";        
      tf.toHtmlI18n(pageContext);
    %></td>
  </tr>
</table>
<%
  ButtonBar bb2 = new ButtonBar();

  ButtonSubmit saveInstance = ButtonSubmit.getSaveInstance(form, I18n.get("CONNECT"));
  saveInstance.additionalCssClass = "big first";
  saveInstance.variationsFromForm.command = "CONNECT";
  bb2.addButton(saveInstance);
  bb2.toHtml(pageContext);
  form.end(pageContext);

  /*
  ________________________________________________________________________________________________________________________________________________________________________


    begin read

  ________________________________________________________________________________________________________________________________________________________________________

  */
  if (bh.connected) {
%>
<hr>
<hr>
<table width="100%">
  <tr>
    <td align="top" width="20%">
      <p>
        <%=I18n.get("BASECAMP_INFO_ABOUT_ENTITY")%>
      </p>
    </td>
    <td width="70%">
      <%

        /*
        ________________________________________________________________________________________________________________________________________________________________________


          info table on associations between object in teamwork and basecamp

        ________________________________________________________________________________________________________________________________________________________________________

        */
        Container entity = new Container();
        entity.title = I18n.get("BS_ENTITY");
        entity.level = 2;
        entity.start(pageContext);
      %>
      <table width="100%">
        <tr>
          <th>
            Basecamp
          </th>
          <th>
            Twproject
          </th>
        </tr>
        <tr class="alternate" >
          <td width="50%"><%=I18n.get("PROJECT")%>
          </td>
          <td width="50%"><%=I18n.get("TASK")%>
          </td>
        </tr>
        <tr class="alternate" >
          <td width="50%"><%=I18n.get("MILESTONES")%>
          </td>
          <td width="50%"><%=I18n.get("SUBTASKS")%>
          </td>
        </tr>
        <tr class="alternate" >
          <td width="50%">todo
          </td>
          <td width="50%"><%=I18n.get("ISSUE")%>
          </td>
        </tr>
        <tr class="alternate" >
          <td width="50%"><%=I18n.get("PEOPLE_ON_PROJECT")%>
          </td>
          <td width="50%"><%=I18n.get("ASSIGNMENT")%>
          </td>
        </tr>
        <tr class="alternate" >
          <td width="50%"><%=I18n.get("WRITEBOARD")%>
          </td>
          <td width="50%"><%=I18n.get("BOARD")%>
          </td>
        </tr>
        <tr class="alternate" >
          <td width="50%"><%=I18n.get("COMPANY")%>
          </td>
          <td width="50%"><%=I18n.get("COMPANY")%>
          </td>
        </tr>
        <tr class="alternate" >
          <td width="50%"><%=I18n.get("PERSON")%>
          </td>
          <td width="50%"><%=I18n.get("PERSON")%>
          </td>
        </tr>
      </table>
      <%
        entity.end(pageContext);
      %>
    </td>
  </tr>
</table>
<br>
<%

  /*
  ________________________________________________________________________________________________________________________________________________________________________


    companies

  ________________________________________________________________________________________________________________________________________________________________________

  */

  Container peopleInvolved = new Container();
  peopleInvolved.title = I18n.get("PEOPLE_ON_PROJECT");
  peopleInvolved.level = 2;
  peopleInvolved.start(pageContext);
%>
<table class="table">
<tr>
  <th><%=I18n.get("DATA_IN_BASECAMP")%>
  </th>
  <th><%=I18n.get("BS_ASSOCIATE_OR_CREATRE_NEW")%>
  </th>
</tr>
<%

  //loop companies and get people
  Set<String> companiesIds = new HashSet();

  Map<String, Map<String, String>> allCompanyInXML = bh.getContents("companies.xml", "company");
  for (String mapCompanyKey : allCompanyInXML.keySet()) {
    Map<String, String> aCompany = allCompanyInXML.get(mapCompanyKey);
    Pair<String, Company> companyPair = bh.buildCompanyFromMap(aCompany);
    Company bsCompanyModel = companyPair.second;
    String companyXmlId = companyPair.first;
    companiesIds.add(companyXmlId);
%>
<tr class="alternate" >
  <td>
    <%=bsCompanyModel.getName()%>
  </td>
  <td nowrap>
    <%
      // guess company in tw, and keep map of mapped
      List<Company> all = ResourceBricks.guessCompanyFromName(bsCompanyModel.getName());
      SmartCombo sc = ResourceBricks.getCompanyCombo("ASSOCIATE_TO_COMPANY_" + mapCompanyKey, TeamworkPermissions.resource_canRead, null, pageState);   //bsCompanyModel.getId()
      sc.separator = "&nbsp;";
      sc.fieldSize = 20;
      sc.label = "";
      if (all.size() > 0) {
        Company c = all.get(0);
        pageState.addClientEntry("ASSOCIATE_TO_COMPANY_" + mapCompanyKey, c.getId());  // bsCompanyModel.getId()
      }
    %>
    <table>
      <tr>
        <td width="50%">
          <%
            RadioButton companyRadio = new RadioButton(I18n.get("CREATE_NEW"), "OPERATION_COMPANY_" + mapCompanyKey, "new", "", "", false, "");
            if (all.size() != 1)
              pageState.addClientEntry("OPERATION_COMPANY_" + mapCompanyKey, "new");
            companyRadio.toHtml(pageContext);
          %>
        </td>
        <td width="20%">
          <%
            companyRadio = new RadioButton(I18n.get("ASSOCIATE_TO"), "OPERATION_COMPANY_" + mapCompanyKey, "old", "", "", false, "");
            if (all.size() > 0)
              pageState.addClientEntry("OPERATION_COMPANY_" + mapCompanyKey, "old");
            companyRadio.toHtml(pageContext);
          %>
        </td>
        <td width="30%">
          <%
            sc.toHtmlI18n(pageContext);
          %>
        </td>
      </tr>
    </table>
  </td>
</tr>
<%

  /*
  ________________________________________________________________________________________________________________________________________________________________________


    people in the company

  ________________________________________________________________________________________________________________________________________________________________________

  */
  Map<String, Map<String, String>> allPersonInXML = bh.getContents("contacts/people/" + companyXmlId, "person");

  for (String mapPersonKey : allPersonInXML.keySet()) {
    Map<String, String> aPerson = allPersonInXML.get(mapPersonKey);
    Pair<String, Person> personPair = bh.buildPersonFromMap(aPerson);
    String basecampPersonId = personPair.first;
    Person bsPersonModel = personPair.second;

%>
<tr class="alternate" >
  <td>
    &nbsp; &nbsp; &nbsp; &nbsp;<%=bsPersonModel.getDisplayName()%> (<%=bsCompanyModel.getName()%>)
  </td>
  <td nowrap>
    <%
      // guessing people in tw
      List<Person> perss = ResourceBricks.guessPersonFromName(bsPersonModel.getDisplayName());
      SmartCombo scPeople = ResourceBricks.getPersonCombo("ASSOCIATE_TO_PERSON_" + mapCompanyKey + "_" + mapPersonKey, TeamworkPermissions.resource_canRead, false, null, pageState);
      scPeople.separator = "&nbsp;";
      scPeople.fieldSize = 20;
      scPeople.label = "";
      if (perss.size()>0) {
        Person c = perss.get(0);
        pageState.addClientEntry("ASSOCIATE_TO_PERSON_" + mapCompanyKey + "_" + mapPersonKey, c.getId()); // se esiste un guess in tw lo metto preselezionato
      }
    %>
    <table>
      <tr>
        <td width="50%">
          <%
            RadioButton peopleRadio = new RadioButton(I18n.get("CREATE_NEW"), "OPERATION_PEOPLE_" + mapCompanyKey + "_" + mapPersonKey, "new", "", "", false, "");
            if (perss.size() != 1)
              pageState.addClientEntry("OPERATION_PEOPLE_" + mapCompanyKey + "_" + mapPersonKey, "new");
            peopleRadio.toHtml(pageContext);
          %>
        </td>
        <td width="20%">
          <%
            peopleRadio = new RadioButton(I18n.get("ASSOCIATE_TO"), "OPERATION_PEOPLE_" + mapCompanyKey + "_" + mapPersonKey, "old", "", "", false, "");
            if (perss.size() == 1)
              pageState.addClientEntry("OPERATION_PEOPLE_" + mapCompanyKey + "_" + mapPersonKey, "old");
            peopleRadio.toHtml(pageContext);
          %>
        </td>
        <td width="30%">
          <%
            scPeople.toHtmlI18n(pageContext);
          %>
        </td>
      </tr>
    </table>
  </td>
</tr>
<%
    }
  }
%></table>

<%
  peopleInvolved.end(pageContext);
%>
<br>
<%
  /*
  ________________________________________________________________________________________________________________________________________________________________________


    projects BEGIN

  ________________________________________________________________________________________________________________________________________________________________________

  */
  Container projects = new Container();
  projects.title = I18n.get("TASKS");
  projects.level = 2;
  projects.start(pageContext);
%>
<table class="table"><%
  //TW5 hack as they changed API
  bh.basecampHost =  bh.basecampHost.substring(0,bh.basecampHost.indexOf(".com/")+5);

  Map<String, Map<String, String>> allProjectInXML = bh.getContents("projects.xml", "project");

  //loop on projects

  for (String mapProjectKey : allProjectInXML.keySet()) {
    Map<String, String> aProject = allProjectInXML.get(mapProjectKey);

    // all basic task data

    Pair<String, Task> projectPair = bh.buildTaskFromMap(aProject);
    if (projectPair != null) {
      Task task = projectPair.second;
%>
<tr>
  <th colspan="3" align="left"><%=I18n.get("BS_IMPORTING_PROJECT")%> <%=task.getName()%><%

    RoleTeamwork defRole = TaskBricks.getProjectManagerRole(task.getArea());
    if (defRole == null)
      defRole = TaskBricks.getWorkerRole(task.getArea());

    if (defRole == null) {

      PageSeed addR = pageState.pageFromRoot("security/roleEditor.jsp");
      addR.command = Commands.ADD;
      addR.addClientEntry("AREA", task.getArea().getId());
      addR.addClientEntry(RoleConstants.FLD_ROLE_NAME, ApplicationState.getApplicationSetting("DEFAULT_WORKER_ROLE_NAME", "Worker"));
      addR.addClientEntry("LOCAL_TO_ASS", Fields.TRUE);

      ButtonLink addRole = new ButtonLink(I18n.get("BASECAMP_DEFAULT_ROLE_NOT_FOUND_%%", ApplicationState.getApplicationSetting("DEFAULT_WORKER_ROLE_NAME", "Worker"), task.getArea().getName()), addR);

  %><br><font class="warning" size="3"><%addRole.toHtmlInTextOnlyModality(pageContext);%></font><br><%
    }

  %></th>
</tr>

<tr>
<td valign="top" align="center">
  <%
    CheckField cf = CheckField.getMasterCheckField("TASK_" + mapProjectKey, "TASK_" + mapProjectKey + "_", "SUBTASK_" + mapProjectKey, "ISSUE_TASK" + mapProjectKey, "MESSAGE_" + mapProjectKey);
    cf.selector = StringUtilities.replaceAllNoRegex(cf.selector,":enabled","");
    String script = "{ var selector=$('"+cf.selector+"');";
    cf.additionalOnclickScript = cf.additionalOnclickScript+script+
            " if (this.checked==false) { selector.each(function(){$(this).attr('disabled','disabled')}); };"+
            " if (this.checked==true) { selector.each(function(){$(this).attr('disabled','')}); }; }";
    cf.label = "";
    cf.toHtml(pageContext);
  %>
</td>
<td colspan="2"><!-- all project data -->
<table width="100%">
<tr class="alternate" >
  <td>
    <%=I18n.get("SCRUM_TASK_NAME")%>:
  </td>
  <td>
    <%=task.getAbstractForIndexing()%>
  </td>
</tr>
<tr class="alternate" >
  <td>
    <%=I18n.get("CREATED_ON")%>:
  </td>
  <td>
    <%=JSP.w(task.getCreationDate())%>
  </td>
</tr>
<tr class="alternate" >
  <td>
    <%=I18n.get("LAST_MODIFIED_ON")%>:
  </td>
  <td>
    <%=JSP.w(task.getLastModified())%>
  </td>
</tr>

<%
  /*
  ________________________________________________________________________________________________________________________________________________________________________


    projects assignment

  ________________________________________________________________________________________________________________________________________________________________________

  */


  //people and company on project -> assignments; there is a primary company for the project
  /*People per project
/projects/#{project_id}/contacts/people/#{company_id}
This will return all of the people in the given company that can access the given project.*/

  //this sucking API requires to loop on ALL companies to check whether some people is on the project
  //see http://forum.37signals.com/basecamp/forums/8/topics/3437
  boolean introAssigs = true;
  for (String companyId : companiesIds) {
    Map<String, Map<String, String>> allPeopleOfThisCompanyOnTheTask = bh.getContents("projects/" + projectPair.first + "/contacts/people/" + companyId, "person");
    for (String compId : allPeopleOfThisCompanyOnTheTask.keySet()) {
      Pair<String, Person> idP = bh.buildPersonFromMap(allPeopleOfThisCompanyOnTheTask.get(compId));
      if (idP != null) {
        if (introAssigs) {
%>
<tr>
  <td colspan="2">
    <%=I18n.get("ASSIGNMENTS")%>:
  </td>
</tr>
<%
    introAssigs = false;
  }

%>
<tr class="alternate" >
  <td valign="top" align="right">
    <%
      cf = new CheckField("TASK_" + mapProjectKey + "_ASSIG_" + idP.first, "", true);
      cf.label = "";
      cf.disabled = true;
      cf.toHtml(pageContext);
    %>
  </td>
  <td>
    <%=I18n.get("CREATE_ASSIGNMENT_ON_PROJECT")%> <%=task.getDisplayName()%> <%=I18n.get("TO")%> <b><%=idP.second.getDisplayName()%>
  </b>
  </td>
</tr>
<%
      }
    }
  }
  /*
  ________________________________________________________________________________________________________________________________________________________________________


    projects subtask

  ________________________________________________________________________________________________________________________________________________________________________

  */
  Map<String, Map<String, String>> allMilestonesOfTask = bh.getContents("projects/" + projectPair.first + "/milestones/list", "milestone");
  List<Pair<String, Task>> subtasks = bh.buildSubTasksFromMap(allMilestonesOfTask, task, false, "");
  boolean introSubTasks = true;
  for (Pair<String, Task> subtask : subtasks) {

    if (introSubTasks) {
%>
<tr class="alternate" >
  <td colspan="2"><%=I18n.get("SUBTASKS")%>:</td>
</tr>
<%
    introSubTasks = false;
  }

%>
<tr class="alternate" >
  <td valign="top" align="right"><%
    cf = new CheckField("SUBTASK_" + mapProjectKey + "_" + subtask.first, "", true);
    cf.label = "";
    cf.disabled = true;
    cf.toHtml(pageContext);
  %></td>
  <td><%=I18n.get("CREATE_SUBTAK_WITH_MILESTONE_AT")%> <%=JSP.w(subtask.second.getSchedule().getEndDate())%> <%
    if (subtask.second.getAssignementsSize() > 0) {
  %> <%=I18n.get("WITH_ASSIGNMENT_TO")%> <%=subtask.second.getAssignementsIterator().next().getResource().getDisplayName()%><%
    }
  %>
  </td>
</tr>
<%
  }

  /*
  ________________________________________________________________________________________________________________________________________________________________________


    projects to-do list -----> subtask con assignment e issue

  ________________________________________________________________________________________________________________________________________________________________________

  */
  //to do -> check if someone responsible -> issues
  /*Lists
  /projects/#{project_id}/todos/lists
  This will return the metadata for all of the lists in a given project. */
  Map<String, Map<String, String>> allTodoListsOfTaskInXML = bh.getContents("projects/" + projectPair.first + "/todos/lists", "todo-list");


  Set<String> todoListIdsOnThisTask = new HashSet();
  for (String ss : allTodoListsOfTaskInXML.keySet()) {
    todoListIdsOnThisTask.add(allTodoListsOfTaskInXML.get(ss).get("id"));
  }

  for (String todoListId : todoListIdsOnThisTask) {
    //this case is more refined than previous
    String compCall = bh.basecampHost + "todos/list/" + todoListId;
    String xml = bh.call(bh.client, compCall);
    Map<String, Map<String, String>> todos = new HashTable();
%>
<%
  Node nodeList = bh.getRoot(xml);
  NodeList todoListChildren = nodeList.getChildNodes();
  String listName = "";
  for (int i = 0; i < todoListChildren.getLength(); i++) { //per ogni to_do_list
    Node todoListChild = todoListChildren.item(i);
    if ("name".equals(todoListChild.getNodeName())) {
      //listName = "\"" + todoListChild.getTextContent() + "\"";
      listName = "\"" + todoListChild.getChildNodes().item(0).getNodeValue() + "\"";

    }
    if ("todo-items".equals(todoListChild.getNodeName())) {
%>
<tr class="alternate" >
  <td align="right">
    <%
      cf = new CheckField("ISSUE_TASK" + mapProjectKey + "_" + todoListId, "", true);
      cf.label = "";
      cf.disabled = true;
      cf.toHtml(pageContext);

    %>
  </td>
  <td>
    <%=I18n.get("CREATE_SUBTAK_FROM_TODOLIST")%> <b><%=listName%>
  </b><%=I18n.get("WITH_ISSUE_AND_ASSIGNMENT")%>
  </td>
</tr>
<%

  Map<String, String> contents = new HashTable();
  NodeList todoItems = todoListChild.getChildNodes();
  for (int h = 0; h < todoItems.getLength(); h++) { //per ogni to_do_items
    //todoitem
    Node todoItem = todoItems.item(h);
    if ("todo-item".equals(todoItem.getNodeName())) {
      NodeList todoDatas = todoItem.getChildNodes();
      for (int f = 0; f < todoDatas.getLength(); f++) { // per ogni to_do_item
        Node todoData = todoDatas.item(f);
        /*if (JSP.ex(todoData.getNodeName()) && JSP.ex(todoData.getTextContent())) {
          contents.put(todoData.getNodeName(), todoData.getTextContent());
        }*/
        if (JSP.ex(todoData.getNodeName())) {

          Node node = todoData.getChildNodes().item(0);
          if (node!=null) {
          String value = node.getNodeValue();
          if (JSP.ex(value)) {
            contents.put(todoData.getNodeName(), value);
          }
          }
        }
      }
      todos.put("todoItem" + i + "_" + h, contents);
%>
<tr class="alternate" >
  <td align="right">
  </td>
  <td style="padding-left:10px;">
    - <%=I18n.get("AN_ISSUE_FOR_TODO")%> <b>"<%=JSP.w((todos.get("todoItem" + i + "_" + h)).get("content"))%>"</b> <%=I18n.get("AND_ASS_TO_PERSON_INVOLVED")%>
  </td>
</tr>
<%
          }
        }
      }
    }
  }
  /*
  ________________________________________________________________________________________________________________________________________________________________________


    projects forum

  ________________________________________________________________________________________________________________________________________________________________________

  */

  //message: its posted by someone -> diary
  Map<String, Map<String, String>> allpostsOfTaskInXML = bh.getContents("projects/" + projectPair.first + "/msg/archive", "post");
%>
<tr class="alternate" >
  <td width="30%"><%=I18n.get("MESSAGE_ON_PROJECT")%>  <%=((Task) projectPair.second).getDisplayName()%>:</td>
  <td></td>
</tr>
<%
  for (String mapPostKey : allpostsOfTaskInXML.keySet()) {
    String msgId = allpostsOfTaskInXML.get(mapPostKey).get("id");
%>
<tr>
  <td align="right">
    <%
      cf = new CheckField("MESSAGE_" + mapProjectKey + "_" + msgId, "", true);
      cf.label = "";
      cf.disabled = true;
      cf.toHtml(pageContext);
    %>
  </td>
  <td>
    <%=JSP.w(allpostsOfTaskInXML.get(mapPostKey).get("title"))%> - <%=JSP.w(allpostsOfTaskInXML.get(mapPostKey).get("body"))%>
  </td>
</tr>
<%
  //comments are anwer on the post
  /*
 Comments
 /msg/comments/#{message_id}
 Return the list of comments associated with the specified message.
  */
  Map<String, Map<String, String>> allCommentsOnPostInXML = bh.getContents("msg/comments/" + msgId, "comment");
  for (String mapCommentKey : allCommentsOnPostInXML.keySet()) {
%>
<tr>
  <td>
  </td>
  <td>
    <%=JSP.w(allCommentsOnPostInXML.get(mapCommentKey).get("body"))%>
  </td>
</tr>
<%
    }
  }
%>
</table>
</td>
</tr>
<%
    }
  }
  projects.end(pageContext);
  /*
  ________________________________________________________________________________________________________________________________________________________________________


    projects END

  ________________________________________________________________________________________________________________________________________________________________________

  */


  bb2 = new ButtonBar();

  saveInstance = ButtonSubmit.getSaveInstance(form, I18n.get("GO") + " (" + I18n.get("THIS_WILL_TAKE_FEW_SECONDS") + ")");
  saveInstance.additionalCssClass = "big";
  saveInstance.variationsFromForm.command = "GO";

  /*  PageSeed ps = pageState.thisPage(request);
ps.setCommand("GO");
ButtonLink go = new ButtonLink(ps);
go.additionalOnClickScript = " verifyAssociation();" ;        */

  bb2.addButton(saveInstance);
  bb2.toHtml(pageContext);


%></table>
<%

  }

  /*
  ________________________________________________________________________________________________________________________________________________________________________


    Container risultato

  ________________________________________________________________________________________________________________________________________________________________________

  */

  if (BasecampControllerAction.COMMAND_IMPORT_CORRECTLY.equals(pageState.getCommand())) {
    Container containerResult = new Container();
    containerResult.width = "70%";
    containerResult.draggable = true;
    containerResult.collapsable = true;
    containerResult.centeredOnScreen = true;
    containerResult.closeable = true;
    containerResult.setCssPostfix("warn");
    containerResult.start(pageContext);
%>
<table width="100%">
  <tr>
    <td>
      <%=I18n.get("TASK_CORRECTLY_CREATED")%>
    </td>
  </tr>

  <%
    String createdTaskId = pageState.getEntry("TW_TASK_ID").stringValueNullIfEmpty();

    if (createdTaskId != null) {
      String[] taskIds = createdTaskId.split(",");
     for(String taskid : taskIds){
      Task createdTask = (Task) PersistenceHome.findByPrimaryKey(Task.class, taskid);
  %>
  <tr class="alternate" >
    <td>
      <%=I18n.get("SCRUM_TASK_NAME")%>:
    </td>
    <td>
      <%=createdTask.getAbstractForIndexing()%>
    </td>
  </tr>
  <tr class="alternate" >
    <td>
      <%=I18n.get("CREATED_ON")%>:
    </td>
    <td>
      <%=JSP.w(createdTask.getCreationDate())%>
    </td>
  </tr>
  <tr class="alternate" >
    <td>
      <%=I18n.get("LAST_MODIFIED_ON")%>:
    </td>
    <td>
      <%=JSP.w(createdTask.getLastModified())%>
    </td>
  </tr>
  <tr>
    <td colspan="2">
      <%
        ButtonBar bb3 = new ButtonBar();
        PageSeed taskSeed = pageState.pageFromRoot("task/taskOverview.jsp");
        /*String taskImportedIds = pageState.getEntry("TW_TASK_ID").stringValueNullIfEmpty();
        if (JSP.ex(taskImportedIds)) {
          String[] ids = taskImportedIds.split(",");
          for (String id: ids) {*/
            taskSeed.setMainObjectId(createdTask.getId());
            taskSeed.setCommand(Commands.EDIT);
            ButtonLink taskEditorLink = new ButtonLink(taskSeed);
            taskEditorLink.label = I18n.get("GO_TO_TASK_PAGE");
            bb3.addButton(taskEditorLink);
          bb3.toHtml(pageContext);
        }

      %>
    </td>
  </tr>
  <%
      }
    
  %>
</table>
<%
  containerResult.end(pageContext);
} else if (BasecampControllerAction.COMMAND_IMPORT_FAILED.equals(pageState.getCommand())) {
  Container containerResult = new Container();
  containerResult.width = "70%";
  containerResult.draggable = true;
  containerResult.collapsable = true;
  containerResult.centeredOnScreen = true;
  containerResult.closeable = true;
  containerResult.setCssPostfix("warn");
  containerResult.start(pageContext);
%>
<%=I18n.get("BASECAMP_SOMETHING_WRONG")%>
<%

    containerResult.end(pageContext);
  }

%>
<small>The name and logo for Basecamp and 37signals are registered trademarks of 37signals, LLC. Twproject and Open Lab are in no way affiliated to Basecamp or 37signals, LLC.</small>
<%
  }
%>
