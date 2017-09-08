package com.twproject.exchange;

import com.sun.org.apache.xerces.internal.parsers.DOMParser;
import com.twproject.forum.TeamworkForumEntry;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Company;
import com.twproject.resource.Person;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.*;
import org.apache.commons.httpclient.*;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.GetMethod;
import org.jblooming.ApplicationException;
import org.jblooming.tracer.Tracer;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.anagraphicalData.AnagraphicalData;
import org.jblooming.ontology.Pair;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageState;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.jsp.PageContext;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.*;
import java.text.ParseException;
import java.util.*;

/*
Data reference on 23June2008
  http://developer.37signals.com/basecamp/reference.shtml

   Basecamp API
  Sections

      * Introduction
      * General
      * Messages & Comments
      * To-do Lists
      * Milestones
      * Time Tracking
      * Contact Management
      * Data Reference

  Forum

  Share thoughts, ask questions, and help fellow developers in the Basecamp API forum.
  Data Reference

  The following sections describe the different data types used by the Basecamp API.
  Abbreviated post

  <post>
    <id type="integer">#{id}</id>
    <title>#{title}</title>
    <posted-on type="datetime">#{posted_on}</posted-on>
    <attachments-count type="integer">#{attachments_count}</attachments-count>
    <category>
      <id type="integer">#{id}</id>
      <name>#{name}</name>
    </category>
  </post>

  Comment

  <comment id="#{id}">
    <post_id>#{post_id}</post_id>
    <creator_name>#{creator_name}</creator_name>
    <creator_id>#{creator_id}</creator_id>
    <body>#{body}</body>
    <posted_on>#{posted_on}</posted_on>
  </comment>

  Company

  <company>
    <id type="integer">#{id}</id>
    <name>#{name}</name>
    <address-one>#{address_one}</address-one>
    <address-two>#{address_two}</address-two>
    <city>#{city}</city>
    <state>#{state}</state>
    <zip>#{zip}</zip>
    <country>#{country}</country>
    <web-address>#{web_address}</web-address>
    <phone-number-office>#{phone_number_office></phone-number-office>
    <phone-number-fax>#{phone_number_fax}</phone-number-fax>
    <time-zone-id>#{time_zone_id}</time-zone-id>
    <can-see-private type="boolean">#{can_see_private}</can-see-private>

    <!-- for non-client companies -->
    <url-name>#{url_name}</url-name>
  </company>

  File category

  <attachment-category>
    <id type="integer">#{id}</id>
    <name>#{name}</name>
    <project-id type="integer">#{project_id}</project-id>
    <elements-count type="integer">#{elements_count}</elements-count>
  </attachment-category>

  Message category

  <post-category>
    <id type="integer">#{id}</id>
    <name>#{name}</name>
    <project-id type="integer">#{project_id}</project-id>
    <elements-count type="integer">#{elements_count}</elements-count>
  </post-category>

  Milestone

  <milestone>
    <id type="integer">#{id}</id>
    <title>#{title}</title>
    <deadline type="date">#{deadline}</deadline>
    <completed type="boolean">#{true|false}</completed>
    <project-id type="integer">#{project_id}</project-id>
    <created-on type="datetime">#{created_on}</created-on>
    <creator-id type="integer">#{creator_id}</creator-id>
    <responsible-party-id type="integer">#{responsible_party_id}</responsible-party-id>
    <responsible-party-type>#{responsible_party_type}</responsible-party-type>

    <!-- if the milestone has been completed -->
    <completed-on type="datetime">#{completed_on}</completed-on>
    <completer-id type="integer">#{completer_id}</completer-id>
  </milestone>

  Person

  <person>
    <id type="integer">#{id}</id>
    <first-name>#{first_name}</first-name>
    <last-name>#{last_name}</last-name>
    <title>#{title}</title>
    <email-address>#{email_address}</email-address}
    <im-handle>#{im_handle}</im-handle>
    <im-service>#{im_service}</im-service>
    <phone-number-office>#{phone_number_office}</phone-number-office>
    <phone-number-office-ext>#{phone_number_office_ext}</phone-number-office-ext>
    <phone-number-mobile>#{phone_number_mobile}</phone-number-mobile>
    <phone-number-home>#{phone_number_home}</phone-number-home>
    <phone-number-fax>#{phone_number_fax}</phone-number-fax>
    <last-login type="datetime">#{last_login}</last-login>
    <client-id type="integer">#{client_id}</client-id>

    <!-- if user is an administrator, or is self -->
    <user-name>#{user_name}</user-name>

    <!-- if user is self -->
    <password>#{password}</password>
    <token>#{token}</token>

    <!-- if user is an administrator -->
    <administrator type="boolean">#{administrator}</administrator>
    <deleted type="boolean">#{deleted}</deleted>
    <has-access-to-new-projects type="boolean">#{has_access_to_new_projects}</has-access-to-new-projects>
  </person>

  Post

  <post>
    <id type="integer">#{id}</id>
    <title>#{title}</title>
    <body>#{body}</body>
    <posted-on type="datetime">#{posted_on}</posted-on>
    <project-id type="integer">#{project_id}</project-id>
    <category-id type="integer">#{category_id}</category-id>
    <author-id type="integer">#{author_id}</author-id>
    <milestone-id type="integer">#{milestone_id}</milestone-id>
    <comments-count type="integer">#{comments_count}</comments-count>
    <attachments-count type="integer">#{attachments_count}</attachments-count>
    <use-textile type="boolean">#{use_textile}</use-textile>
    <extended-body>#{extended_body}</extended-body>
    <display-body>#{display_body}</display-body>
    <display-extended-body>#{display_extended_body}</display-extended-body>

    <!-- if user can see private posts -->
    <private type="boolean">#{private}</private>
  </post>

  Project

  <project>
    <id type="integer">#{id}</id>
    <name>#{name}</name>
    <created-on type="datetime">#{created_on}</created-on>
    <status>#{status}</status>
    <last-changed-on type="datetiem">#{last_changed_on}</last-changed-on>
    <company>
      <id type="integer">#{id}</id>
      <name>#{name}</name>
    </company>

    <!-- if user is administrator, or show_announcement is true -->
    <announcement>#{announcement}</announcement>

    <!-- if user is administrator -->
    <start-page>#{start_page}</start-page>
    <show-writeboards type="boolean">#{show_writeboards}</show-writeboards>
    <show-announcement type="boolean">#{show_announcement}</show-announcement>
  </project>

  Time entry

  <time-entry>
    <id type="integer">#{id}</id>
    <project-id type="integer">#{project-id}</project-id>
    <person-id type="integer">#{person-id}</person-id>
    <date type="date">#{date}</date>
    <hours>#{hours}</hours>
    <description>#{description}</description>
    <todo-item-id type="integer">#{todo-item-id}</todo-item-id>
  </time-entry>

  Todo item

  <todo-item>
    <id type="integer">#{id}</id>
    <content>#{content}</content>
    <position type="integer">#{position}</position>
    <created-on type="datetime">#{created_on}</created-on>
    <creator-id type="integer">#{creator_id}</creator-id>
    <completed type="boolean">#{completed}</completed>

    <!-- if the item has a responsible party -->
    <responsible-party-type>#{responsible_party_type}</responsible-party-type>
    <responsible-party-id type="integer">#{responsible_party_id}</responsible-party-id>

    <!-- if the item has been completed -->
    <completed-on type="datetime">#{completed_on}</completed-on>
    <completer-id type="integer">#{completer_id}</completer-id>
  </todo-item>

  Todo list

  <todo-list>
    <id type="integer">#{id}</id>
    <name>#{name}</name>
    <description>#{description}</description>
    <project-id type="integer">#{project_id}</project-id>
    <milestone-id type="integer">#{milestone_id}</milestone-id>
    <position type="integer">#{position}</position>

    <!-- if user can see private lists -->
    <private type="boolean">#{private}</private>

    <!-- if todo-items are included in the response -->
    <todo-items>
      <todo-item>
        ...
      </todo-item>
      <todo-item>
        ...
      </todo-item>
      ...
    </todo-items>
  </todo-list>


    */

public class BasecampControllerAction extends ActionSupport implements ActionController {

  public String basecampHost;
  public HttpClient client;
  public TeamworkOperator logged;
  public boolean connected = false;

  public static String COMMAND_IMPORT_CORRECTLY = "CORRECT";
  public static String COMMAND_IMPORT_FAILED = "FAILED";

  public Map<String, Person> personsByBasecampId = new HashTable();

  public BasecampControllerAction(PageState pageState) {
    super(pageState);
    this.logged = (TeamworkOperator) pageState.getLoggedOperator();
  }

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException, IOException {
    PageState currentPageState = PageState.getCurrentPageState(request);

    String command = restState.getCommand();

    if ("GO".equals(command)) {
      cmdImport();
      String result = restState.getEntry("RESULT").stringValueNullIfEmpty();
      if (result != null) {
        restState.setCommand(COMMAND_IMPORT_CORRECTLY);
        restState.addClientEntry(restState.getEntry("TW_TASK_ID"));
      } else
        restState.setCommand(COMMAND_IMPORT_FAILED);
    } else if ("CONNECT".equals(command)) {
      cmdConnect();
    }
    return currentPageState;
  }

  private void cmdConnect() {

    String host = restState.getEntryAndSetRequired("BASECAMP_HOST_URL").stringValueNullIfEmpty();
    String user = restState.getEntryAndSetRequired("BASECAMP_HOST_USER").stringValueNullIfEmpty();
    String pass = restState.getEntryAndSetRequired("BASECAMP_HOST_PSW").stringValueNullIfEmpty();
    if (JSP.ex(host, user, pass)) {

      basecampHost = host;
      if (!basecampHost.endsWith("/"))
        basecampHost = basecampHost + "/";

      try {
        SimpleHttpConnectionManager hcm = new SimpleHttpConnectionManager();
        client = new HttpClient(hcm);
        Credentials defaultcreds = new UsernamePasswordCredentials(user, pass);
        client.getState().setCredentials(AuthScope.ANY, defaultcreds);

        //test connection
        String compCall = basecampHost + "companies.xml";
        String xml = call(client, compCall);
        if (!JSP.ex(xml)) {
          throw new Exception("");
        }
        if (xml.toLowerCase().startsWith("you must log in")) {
          throw new Exception("login");
        }
        /* test per xml malformed*/
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        DocumentBuilder db = dbf.newDocumentBuilder();
        ByteArrayInputStream b = new ByteArrayInputStream(xml.getBytes());
        db.parse(b);

        connected = true;
      } catch (Throwable e) {
        if ("login".equals(e.getMessage())) {
          restState.getEntry("BASECAMP_HOST_USER").errorCode = restState.getI18n("INVALID_USERNAME_OR_PASSWORD");
          restState.getEntry("BASECAMP_HOST_PSW").errorCode = restState.getI18n("INVALID_USERNAME_OR_PASSWORD");
        } else {
          restState.getEntry("BASECAMP_HOST_URL").errorCode = restState.getI18n("INVALID_DATA");
          restState.getEntry("BASECAMP_HOST_USER").errorCode = restState.getI18n("INVALID_DATA");
          restState.getEntry("BASECAMP_HOST_PSW").errorCode = restState.getI18n("INVALID_DATA");
        }
      }
    }
  }

  /**
   * importazione dei dati da basecamp
   * per tutti gli ioggetti viene assegnato un id significatvo in modo da poterlo recuperare e nel caso non ricrearlo
   *
   * @throws IOException
   */
  private void cmdImport() throws IOException {

    if (restState.getLoggedOperator().hasPermissionFor(TeamworkPermissions.project_canCreate)) {
      try {

        /**
         * Company
         */

        this.cmdConnect();
        String taskIds = "";

        Map<String, Map<String, String>> allCompanyInXML = null;
        allCompanyInXML = this.getContents("contacts/companies", "company");
        boolean companyNew = false;

        for (String mapCompanyKey : allCompanyInXML.keySet()) {
          companyNew = false;
          String operation = restState.getEntry("OPERATION_COMPANY_" + mapCompanyKey).stringValueNullIfEmpty();
          Map<String, String> company = new HashTable();
          company = allCompanyInXML.get(mapCompanyKey);
          String companyId = company.get("id");
          Company twCompany = null;
          if ("new".equals(operation) || ("old".equals(operation)) && restState.getEntry("ASSOCIATE_TO_COMPANY_" + mapCompanyKey).stringValueNullIfEmpty() == null) {

            twCompany = buildCompanyFromMap(company).second;

            String companyNumberFax = company.get("phone-number-fax");
            String companyNumberOffice = company.get("phone-number-office");
            String comapnyCountry = company.get("country");
            String companyCity = company.get("city");
            String companyAddress = company.get("address-one");

            AnagraphicalData companyData = new AnagraphicalData();
            companyData.setIdAsNew();
            companyData.setAddress(companyAddress);
            companyData.setCity(companyCity);
            companyData.setCountry(comapnyCountry);
            companyData.setTelephone(companyNumberOffice);
            companyData.setFax(companyNumberFax);
            companyData.setLocationDescription("-");
            companyData.store();

            twCompany.getAnagraphicalDatas().add(companyData);
            twCompany.store();
            companyNew = true;
          }

          /**
           * Person in Company
           * per ogni compagnia prendo le sue persone coinvolte, controllo quelle ceccate,
           *  dopodiche  controllo se crearle nuove oppure se associarle ad una risorsa gia esistente
           */

          Map<String, Map<String, String>> allPersonsInXML = this.getContents("contacts/people/" + companyId, "person");
          for (String mapPersonKey : allPersonsInXML.keySet()) {
            String operationPers = restState.getEntry("OPERATION_PEOPLE_" + mapCompanyKey + "_" + mapPersonKey).stringValueNullIfEmpty();
            Map<String, String> person = allPersonsInXML.get(mapPersonKey);
            // se la compagnia deve essere associata ad una gia esistente
            if ("new".equals(operationPers) || ("old".equals(operation)) && restState.getEntry("ASSOCIATE_TO_PERSON_" + mapCompanyKey + "_" + mapPersonKey).stringValueNullIfEmpty() == null) {
              /* New person */

              Person twResource = buildPersonFromMap(person).second;

              AnagraphicalData resourceData = new AnagraphicalData();
              resourceData.setIdAsNew();
              resourceData.setEmail(person.get("email-address"));
              resourceData.setTelephone(person.get("phone-number-office"));
              resourceData.setMobile(person.get("phone-number-mobile"));
              resourceData.setFax(person.get("phone-number-fax"));
              resourceData.setLocationDescription("-");
              resourceData.store();
              twResource.getAnagraphicalDatas().add(resourceData);
              twResource.getAnagraphicalDataOrdered().add(resourceData);
              if (companyNew) {
                twCompany.getPersons().add(twResource);
                twCompany.store();
              }
              twResource.store();
            } else {
              String assPersonTo = restState.getEntry("ASSOCIATE_TO_PERSON_" + mapCompanyKey + "_" + mapPersonKey).stringValueNullIfEmpty();
              personsByBasecampId.put(person.get("id"), (Person) PersistenceHome.findByPrimaryKey(Person.class, assPersonTo));
            }

          }
        }

        /**
         * Progetti
         */

        //TW5 hack as they changed API
        basecampHost =  basecampHost.substring(0,basecampHost.indexOf(".com/")+5);
        Map<String, Map<String, String>> allProjectInXML = getContents("projects.xml", "project");

        for (String mapProjectKey : allProjectInXML.keySet()) {
          if (restState.getEntry("TASK_" + mapProjectKey).checkFieldValue()) {

            Map<String, String> theProject = allProjectInXML.get(mapProjectKey);
            String theProjectId = theProject.get("id");
            Task twTask = buildTaskFromMap(theProject).second;
            Period taskPeriod = new Period(new Date(), new Date());
            twTask.setSchedule(taskPeriod);
            TeamworkForumEntry forumRoot = twTask.getForumEntry();
            if (forumRoot==null){
              forumRoot = new TeamworkForumEntry();
              forumRoot.setTask(twTask);
              twTask.setForumEntry(forumRoot);
              forumRoot.store();
            }
            forumRoot.setContent(twTask.getDisplayName());
            forumRoot.setPostedOn(new Date());
            forumRoot.store();
            twTask.store();

            /**
             *  Creo Assignment
             */

            for (String mapCompanyKey : allCompanyInXML.keySet()) {
              Map<String, String> company = allCompanyInXML.get(mapCompanyKey);
              String companyId = company.get("id");

              Map<String, Map<String, String>> allPeopleOfThisCompanyOnTheTask = getContents("projects/" + theProjectId + "/contacts/people/" + companyId, "person");
              for (String resourceOnTask : allPeopleOfThisCompanyOnTheTask.keySet()) {
                Map<String, String> person = allPeopleOfThisCompanyOnTheTask.get(resourceOnTask);
                if (restState.getEntry("TASK_" + mapProjectKey + "_ASSIG_" + person.get("id")).checkFieldValue()) {
                  Person twPerson = personsByBasecampId.get(person.get("id"));
                  if (JSP.ex(twPerson)) {
                    RoleTeamwork defRole = TaskBricks.getProjectManagerRole(twTask.getArea());
                    if (defRole == null)
                      defRole = TaskBricks.getWorkerRole(twTask.getArea());
                    Assignment assig = createAssignment(twTask, twPerson, defRole);
                    assig.store();
                    twTask.getAssignments().add(assig);
                    twTask.store();
                  }
                }
              }
            }

            /**
             *  Creo SubTask
             * il controllo per testare il checkfield l'ho messo nel metodo buildSubTasksFromMap per riusarlo e non ripetere codice
             */

            Map<String, Map<String, String>> allMilestonesOfTaskInXML = getContents("projects/" + theProjectId + "/milestones/list", "milestone");
            List<Pair<String, Task>> subtasks = buildSubTasksFromMap(allMilestonesOfTaskInXML, twTask, true, mapProjectKey);
            for (Pair<String, Task> subtask : subtasks) {
              Task sub = subtask.second;
              sub.store();
              if (sub.getSchedule() != null)
                if (twTask.getSchedule() == null || (sub.getSchedule().getEndDate().after(twTask.getSchedule().getEndDate()))) {
                  Period pr = new Period(sub.getSchedule().getStartDate(), sub.getSchedule().getEndDate());
                  twTask.setSchedule(pr);
                }
            }

            //reset duration
            twTask.setDuration(CompanyCalendar.getDistanceInWorkingDays(twTask.getSchedule().getStartDate(), twTask.getSchedule().getEndDate()));
            twTask.store();
            /**
             *  Creo Issue  ---> le liste di to-do sono per Twproject dei subtask e le persone a cui tali issue sono assegnate saranno gli assignment del subtask
             */

            Map<String, Map<String, String>> allTodoListsOfTaskInXML = getContents("projects/" + theProjectId + "/todos/lists", "todo-list");
            Set<String> todoListIdsOnThisTask = new HashSet();
            for (String mapTodoListKey : allTodoListsOfTaskInXML.keySet()) {
              todoListIdsOnThisTask.add(allTodoListsOfTaskInXML.get(mapTodoListKey).get("id"));
            }

            for (String todoListId : todoListIdsOnThisTask) { // per ogni to_do list

              //this case is more refined than previous
              String compCall = basecampHost + "todos/list/" + todoListId;

              String xml = call(client, compCall);
              Node nodeList = getRoot(xml);
              NodeList todoListChildren = nodeList.getChildNodes();
              String listName = "";
              for (int i = 0; i < todoListChildren.getLength(); i++) { //per ogni to_do items
                Node todoListChild = todoListChildren.item(i);

                if ("name".equals(todoListChild.getNodeName()))
                  listName = todoListChild.getTextContent();

                if ("todo-items".equals(todoListChild.getNodeName())) {
                  if (restState.getEntry("ISSUE_TASK" + mapProjectKey + "_" + todoListId).checkFieldValue()) {

                    Task toDoListSubTask = new Task();
                    toDoListSubTask.setIdAsNew();
                    toDoListSubTask.setCode("-");
                    toDoListSubTask.setName(restState.getI18n("SUBTASK_FROM_TODO_LIST") + "_" + listName);
                    toDoListSubTask.setParentAndStore(twTask);
                    Period pr = new Period(twTask.getSchedule().getEndDate(), twTask.getSchedule().getEndDate());
                    pr.store();
                    toDoListSubTask.setStatus(twTask.getStatus());
                    toDoListSubTask.setSchedule(pr);
                    toDoListSubTask.setDuration(1);
                    toDoListSubTask.store();
                    Map<String, String> contents = new HashTable();
                    NodeList todoItems = todoListChild.getChildNodes();
                    for (int h = 0; h < todoItems.getLength(); h++) { // per ogni to_do item
                      Node todoItem = todoItems.item(h);

                      if ("todo-item".equals(todoItem.getNodeName())) {
                        NodeList todoDatas = todoItem.getChildNodes();
                        Issue twIssue = new Issue();
                        twIssue.setArea(logged);
                        twIssue.setIdAsNew();
                        for (int f = 0; f < todoDatas.getLength(); f++) {
                          Node todoData = todoDatas.item(f);
                          if (JSP.ex(todoData.getNodeName()) && JSP.ex(todoData.getTextContent())) {
                            contents.put(todoData.getNodeName(), todoData.getTextContent());
                          }
                        }
                        twIssue.setDescription(contents.get("content"));
                        String type = contents.get("responsible-party-type");
                        String respId = contents.get("responsible-party-id");
                        if ("person".equalsIgnoreCase(type) && JSP.ex(respId)) {
                          Person p = personsByBasecampId.get(respId);
                          if (p != null) {
                            twIssue.setAssignedTo(p);
                            RoleTeamwork defRole = TaskBricks.getProjectManagerRole(twTask.getArea());
                            if (defRole == null)
                              defRole = TaskBricks.getWorkerRole(twTask.getArea());
                            Set<Assignment> assgs = toDoListSubTask.getAssignments();

                            if (assgs.size() > 0) {
                               for (Assignment assigOld : assgs) {        // controllo che non esista gia un ass per quella persona su quel task
                                if (!assigOld.getResource().getId().equals(p.getId()) || !assigOld.getTask().getId().equals(toDoListSubTask.getId())) {
                                  Assignment assig = createAssignment(toDoListSubTask, p, defRole);
                                  assig.store();
                                  toDoListSubTask.getAssignments().add(assig);
                                  toDoListSubTask.store();
                                }
                              }
                            } else {
                              Assignment assig = createAssignment(toDoListSubTask, p, defRole);
                              assig.store();
                              toDoListSubTask.getAssignments().add(assig);
                              toDoListSubTask.store();
                            }
                          }
                        }
                        String completed = contents.get("completed");
                        if ("false".equals(completed))
                          twIssue.setStatusOpen();
                        else
                          twIssue.setStatusClosed();
                        twIssue.setTask(toDoListSubTask);
                        /**
                         * in basecamp i to-do item non hanno conetto di gravita percui lo metto di default a medium
                         */
                        twIssue.setGravity(Issue.GRAVITY_MEDIUM);
                        twIssue.store();
                      }
                    }
                  }
                }
              }

            }

            /**
             * creo message
             */

            Map<String, Map<String, String>> allpostsOfTaskInXML = getContents("projects/" + theProjectId + "/msg/archive", "post");
            for (String mapPostKey : allpostsOfTaskInXML.keySet()) {
              String msgId = allpostsOfTaskInXML.get(mapPostKey).get("id");
              Map<String, Map<String, String>> allCommentsOnPostInXML = getContents("msg/comments/" + msgId, "comment");
              for (String mapCommentKey : allCommentsOnPostInXML.keySet()) {
                Map<String, String> message = allCommentsOnPostInXML.get(mapCommentKey);
                if (restState.getEntry("ISSUE_" + mapProjectKey + "_" + message.get("id")).checkFieldValue()) {

                  TeamworkForumEntry forumEntry = new TeamworkForumEntry();
                  String content = "";
                  if (JSP.ex(message.get("body")))
                    content = message.get("body");
                  content = content.replaceAll("\r", "").replaceAll("\n", "<br>");
                  forumEntry.setContent(content);
                  TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();
                  forumEntry.setLastPosterOnBranch(logged.getPerson().getDisplayName());
                  forumEntry.setAuthor(restState.getLoggedOperator());
                  Date postedOn = DateUtilities.dateFromString(message.get("posted-on"));
                  forumEntry.setPostedOn(postedOn);
                  twTask.getForumEntry().setLastPostOnBranch(postedOn);
                  forumEntry.setParentAndStore(twTask.getForumEntry());
                  forumEntry.setTask(twTask);
                  forumEntry.store();
                  twTask.getForumEntry().store();
                }
              }
            }

            /**
             * la lavagna non � supportata dalle api di basecamp
             */

            taskIds += twTask.getId() + ",";
          }
        }
        restState.addClientEntry("TW_TASK_ID", taskIds);
        restState.addClientEntry("RESULT", "OK");


      } catch (SAXException e) {
        Tracer.platformLogger.error(e);
      } catch (PersistenceException e) {
        Tracer.platformLogger.error(e);
      } catch (ParseException e) {
        Tracer.platformLogger.error(e);
      }
    }
  }

  private Assignment createAssignment(Task taskParent, Person personAssigned, RoleTeamwork roleInAssignment) throws PersistenceException {
    Assignment assig = new Assignment();
    assig.setIdAsNew();
    assig.setResource(personAssigned);
    assig.setTask(taskParent);
    assig.setOwner(restState.getLoggedOperator());
    assig.setRole(roleInAssignment);
    assig.setDescription(taskParent.getName());
    assig.setEstimatedWorklog(0);
    assig.setAssignmentDate(new Date());
    assig.setActivity(Assignment.ACTIVITY_ALL_IN_ONE);
    assig.setEnabled(true);
    assig.store();
    return assig;
  }


  public String call(HttpClient client, String url) throws IOException {

    HttpMethod get = new GetMethod(url);
    get.setDoAuthentication(true);
    get.setRequestHeader("Content-type", "application/xml");
    get.setRequestHeader("Accept", "application/xml");

    client.executeMethod(get);
    byte[] responseBody = get.getResponseBody();

    return new String(responseBody);
  }

  public Node getRoot(String xml) throws IOException, SAXException {
    DOMParser xmlparser = new DOMParser();
    InputSource is = new InputSource(new StringReader(xml));
    xmlparser.parse(is);
    Document d = xmlparser.getDocument();
    return d.getChildNodes().item(0);
  }

  public Map<String, Map<String, String>> getContents(String url, String rootTag) throws IOException, SAXException {

    String compCall = basecampHost + url;
    String xml = call(client, compCall);
    Map<String, Map<String, String>> allcontents = new HashTable();
    if (xml.contains(rootTag)) {
      Node nodeList = getRoot(xml);
      NodeList chL = nodeList.getChildNodes();
      for (int i = 0; i < chL.getLength(); i++) {
        Node nn = chL.item(i);
        if (rootTag.equals(nn.getNodeName())) {
          Map<String, String> contents = new HashTable();
          NodeList dataOfCompany = nn.getChildNodes();
          for (int f = 0; f < dataOfCompany.getLength(); f++) {
            Node nnn = dataOfCompany.item(f);
            if (JSP.ex(nnn.getNodeName()) && JSP.ex(nnn.getTextContent())) {
              contents.put(nnn.getNodeName(), nnn.getTextContent());
            }
          }
          allcontents.put(rootTag + i, contents);
        }
      }
    }
    return allcontents;
  }

  public Map<String, Map<String, String>> getProjectContents(String url, String rootTag) throws IOException, SAXException {

    String compCall = basecampHost + url;
    String xml = call(client, compCall);
    Map<String, Map<String, String>> allcontents = new HashTable();
    if (xml.contains(rootTag)) {
      Node nodeList = getRoot(xml);
      NodeList chL = nodeList.getChildNodes();
      for (int i = 0; i < chL.getLength(); i++) {
        Node nn = chL.item(i);
        if (rootTag.equals(nn.getNodeName())) {
          Map<String, String> contents = new HashTable();
          NodeList dataOfCompany = nn.getChildNodes();
          for (int f = 0; f < dataOfCompany.getLength(); f++) {
            Node nnn = dataOfCompany.item(f);
            if (JSP.ex(nnn.getNodeName()) && JSP.ex(nnn.getTextContent())) {
              contents.put(nnn.getNodeName(), nnn.getTextContent());
            }
          }
          allcontents.put(rootTag + i, contents);
        }
      }
    }
    return allcontents;
  }


  public void debug(Map<String, Map<String, String>> map, int depth, PageContext pc) throws IOException {
    String sp = "";
    for (int i = 0; i < depth; i++) {
      sp = sp + "&nbsp;&nbsp;&nbsp;&nbsp;";
    }

    for (String s : map.keySet()) {
      pc.getOut().write("<small>");
      pc.getOut().write(sp + s + ":<br>");
      Map<String, String> stringMap = map.get(s);
      for (String ss : stringMap.keySet()) {
        pc.getOut().write(sp + ss + ":" + stringMap.get(ss) + "<br>");
      }
      pc.getOut().write("</small>");
      pc.getOut().write("<hr>");
    }
  }

  public void printPerson(Map<String, String> personData, PageContext pc) throws IOException {
    String fn = personData.get("first-name");
    String ln = personData.get("last-name");
    pc.getOut().write(fn + " " + ln);
  }

  //<created-on type="date">2008-06-09</created-on>
  public Date dateFromBasecampDateString(String bsDate) throws ParseException {
    Date d = null;
    if (JSP.ex(bsDate)) {
      d = DateUtilities.dateFromString(bsDate, "yyyy-MM-dd");
    }
    return d;
  }

  //<last-changed-on type="datetime">2008-06-18T15:00:35Z</last-changed-on>
  public Date dateFromBasecampDateTimeString(String bsDate) throws ParseException {
    Date d = null;
    if (JSP.ex(bsDate)) {
      //remove the ending Z
      if (bsDate.endsWith("Z"))
        bsDate = bsDate.substring(0, bsDate.length() - 1);
      d = DateUtilities.dateFromString(bsDate, "yyyy-MM-dd'T'HH:mm");
    }
    return d;
  }


  public Pair<String, Company> buildCompanyFromMap(Map<String, String> aCompany) throws  PersistenceException {

    Company p = new Company();
    String pId = aCompany.get("id");
    p.setIdAsNew();
    p.setName(aCompany.get("name"));
    p.setArea(logged.getDefaultAreaForPermission(TeamworkPermissions.project_canCreate));
    return new Pair(pId, p);
  }

  public Pair<String, Person> buildPersonFromMap(Map<String, String> aPerson) throws PersistenceException {
    if (JSP.ex(aPerson)) {
      Person p = new Person();
      String pId = aPerson.get("id");
      p.setIdAsNew();
      String fn = aPerson.get("first-name");
      String ln = aPerson.get("last-name");
      p.setPersonName(fn);
      p.setPersonSurname(ln);
      p.setArea(logged.getDefaultAreaForPermission(TeamworkPermissions.project_canCreate));

      personsByBasecampId.put(pId, p);

      return new Pair(pId, p);
    } else
      return null;
  }

  public Pair<String, Task> buildTaskFromMap(Map<String, String> aProject) throws ParseException, PersistenceException {

    Task t = new Task();
    String taskId = aProject.get("id");
    t.setIdAsNew();
    String taskName = aProject.get("name");
    t.setName(taskName);
    t.setCode("-");
    t.setCreationDate(dateFromBasecampDateString(aProject.get("created-on")));
    t.setLastModified(dateFromBasecampDateTimeString(aProject.get("last-changed-on")));
    t.setArea(logged.getDefaultAreaForPermission(TeamworkPermissions.project_canCreate));
    t.setInherit(false);
    t.setPropagate(true);
    String status = aProject.get("status");
    if ("active".equalsIgnoreCase(status))
      t.setStatus(TaskStatus.STATUS_ACTIVE);

    /**
     * NOTA BENE il valore active � verificato
     * archived e onhold invece non lo sono, sono stati trovati su un forum
     * le api di basecamp non specificano i possibili tipi del task in nessun luogo!!!!!!!(i valori codificati)
     */

    if ("archived".equalsIgnoreCase(status))
      t.setStatus(TaskStatus.STATUS_DONE);
    if ("onhold".equalsIgnoreCase(status))
      t.setStatus(TaskStatus.STATUS_SUSPENDED);
    return new Pair(taskId, t);
  }


  public List<Pair<String, Task>> buildSubTasksFromMap(Map<String, Map<String, String>> allMilestonesOfTask, Task task, boolean checkIfChecked, String mapProjectKey) throws ParseException, PersistenceException{

    List<Pair<String, Task>> subs = new ArrayList();

    /**
     * I ruoli in basecamp non esistono, una persona vede o non vede il progetto, non esistono possibilita intermedie.
     * Pertanto vengono creati assignmet in cui la persona ha ruolo di project manager ovvero ha potere sull'intero progetto
     */

    RoleTeamwork defRole = TaskBricks.getProjectManagerRole(task.getArea());
    if (defRole == null)
      defRole = TaskBricks.getWorkerRole(task.getArea());

    for (String s : allMilestonesOfTask.keySet()) {
      Map<String, String> milesProps = allMilestonesOfTask.get(s);
      if (!checkIfChecked || (checkIfChecked && restState.getEntry("SUBTASK_" + mapProjectKey + "_" + s).checkFieldValue())) {
        Task miles = new Task();
        miles.setIdAsNew();
        miles.setArea(task.getArea());
        miles.setCode("-");
        miles.setName("Milestone of " + task.getName());
        miles.setParentAndStore(task);
        miles.setStatus(task.getStatus());

        Date deadline = dateFromBasecampDateString(milesProps.get("deadline"));

        CompanyCalendar cc = new CompanyCalendar(deadline);
        cc.addWorkingDays(-1);
        Period pr = new Period(cc.getTime(), deadline);
        miles.setSchedule(pr);

        if (task.getSchedule() != null) {
          if (task.getSchedule().getEndDate().getTime() < deadline.getTime()) {
            task.getSchedule().setEndDate(deadline);
            task.getSchedule().store();
          } else if (task.getSchedule().getStartDate().getTime() > deadline.getTime()) {
            task.getSchedule().setStartDate(deadline);
            task.getSchedule().store();
          }
        }

        miles.setEndIsMilestone(true);
        miles.setDuration(1);
        if (checkIfChecked)
          miles.store();

        //find responsible/assignee
        String type = milesProps.get("responsible-party-type");
        String respId = milesProps.get("responsible-party-id");
        if ("person".equalsIgnoreCase(type) && JSP.ex(respId)) {
          Person p = personsByBasecampId.get(respId);
          Assignment a = new Assignment();
          a.setIdAsNew();
          a.setResource(p);
          a.setTask(miles);
          a.setOwner(restState.getLoggedOperator());

          a.setRole(defRole);
          a.setDescription(miles.getName());
          a.setEstimatedWorklog(0);
          a.setAssignmentDate(new Date());
          a.setActivity(Assignment.ACTIVITY_ALL_IN_ONE);
          a.setEnabled(true);
          miles.getAssignments().add(a);
          if (checkIfChecked)
            a.store();
        }

        //propagate up


        subs.add(new Pair(s, miles));
      }
    }
    return subs;

  }

}
