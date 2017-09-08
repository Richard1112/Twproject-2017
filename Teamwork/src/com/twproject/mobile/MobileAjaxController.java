package com.twproject.mobile;

import com.opnlb.fulltext.SnowballHackedAnalyzer;
import com.twproject.agenda.Event;
import com.twproject.agenda.PeriodEvent;
import com.twproject.agenda.businessLogic.AgendaAction;
import com.twproject.document.DocumentBricks;
import com.twproject.document.TeamworkDocument;
import com.twproject.document.businessLogic.DocumentAction;
import com.twproject.fulltext.waf.FullTextSearchControllerAction;
import com.twproject.messaging.stickyNote.StickyNote;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Company;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.resource.businessLogic.ResourceAction;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.*;
import com.twproject.task.businessLogic.*;
import com.twproject.task.financial.Cost;
import com.twproject.utilities.TeamworkComparators;
import com.twproject.worklog.Worklog;
import com.twproject.worklog.WorklogPlan;
import com.twproject.worklog.businessLogic.WorklogAction;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import org.apache.lucene.document.Document;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.highlight.Highlighter;
import org.apache.lucene.search.highlight.QueryScorer;
import org.apache.lucene.util.Version;
import org.hibernate.search.engine.ProjectionConstants;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.designer.DesignerField;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.remoteFile.FileStorage;
import org.jblooming.remoteFile.RemoteFile;
import org.jblooming.security.Permission;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.JSONHelper;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.html.display.Explorer;
import org.jblooming.waf.html.input.Uploader;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.ClientEntries;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.util.*;

public class MobileAjaxController {


  public JSONObject perform(HttpServletRequest request, HttpServletResponse response) {
    PageState pageState = PageState.getCurrentPageState(request);


    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    JSONHelper jsonHelper = new JSONHelper();
    JSONObject json = jsonHelper.json;


    try {

      if (logged == null)
        throw new SecurityException("NO_LOGGED_OPERATOR");

      //---------------------------------------------------------------------------------   STICKY ACTIONS  ------------------------------------------------
      Person loggedPerson = logged.getPerson();
      if ("GETSTICKY".equals(pageState.command)) {
        // get loggedUser sticky

        OqlQuery oql = new OqlQuery("from " + StickyNote.class.getName() + " as sticky where sticky.receiver=:rec and sticky.board is null");
        oql.getQuery().setEntity("rec", loggedPerson);
        List<StickyNote> list = oql.list();
        if (JSP.ex(list)) {
          JSONArray jsa = new JSONArray();
          for (StickyNote sn : list) {
            jsa.add(sn.jsonify());
          }

          json.element("stickies", jsa);
        }
      } else if ("RMSTK".equals(pageState.command)) {

        List<String> ids = StringUtilities.splitToList(pageState.getEntry("IDS").stringValueNullIfEmpty(), ",");

        OqlQuery oql = new OqlQuery("delete from " + StickyNote.class.getName() + " where receiver=:rec and id in(:ids)");
        oql.getQuery().setEntity("rec", loggedPerson);
        oql.getQuery().setParameterList("ids", ids);


        oql.getQuery().executeUpdate();


        //---------------------------------------------------------------------------------   NOTIFICATIONS  ------------------------------------------------
      } else if ("CHECKNOTIFICATIONS".equals(pageState.command)) {
        int unreadNotificationsCount = MessagingSystem.getUnreadMessageCount(logged, "LOG");
        json.element("unreadNotifications", unreadNotificationsCount);

        //---------------------------------------------------------------------------------   TASK ACTIONS  ------------------------------------------------
      } else if ("TASKSEARCH".equals(pageState.command)) {
        TaskAction ta = new TaskAction(pageState);
        pageState.setCommand(Commands.FIND);
        ta.cmdFind();
        if (pageState.getPage() != null) {

          JSONArray jsa = new JSONArray();
          List<Object[]> tasks = pageState.getPage().getThisPageElements();
          for (Object[] o : tasks) {
            Task task = (Task) o[0];
            JSONObject jProject = task.jsonify(false, pageState);

            addRequiredJsonData(task, logged, jProject);

            Assignment ass = task.getAssignementForResource(loggedPerson);
            if (JSP.ex(ass))
              jProject.element("myAssId", ass.getIntId());
            else
              jProject.element("myAssId", "");

            jsa.add(jProject);


          }
          json.element("tasks", jsa);
        }
        ReflectionUtilities.invoke(ApplicationState.applicationParameters.get("get"), "doFilter", request, response);


      } else if ("BLOADTASK".equals(pageState.command)) {
        String ids = pageState.getEntry("IDS").stringValueNullIfEmpty();
        if (JSP.ex(ids)) {
          List<String> idsl = StringUtilities.splitToList(ids, ",");
          JSONArray jsa = new JSONArray();
          for (String id : idsl) {
            Task task = Task.load(id);

            if (task.hasPermissionFor(logged, TeamworkPermissions.task_canRead)) {
              JSONObject jProject = task.jsonify(true, pageState);
              addRequiredJsonData(task, logged, jProject);
              jsa.add(jProject);
            }
          }

          json.element("tasks", jsa);
        }

      } else if ("SAVEPROJECT".equals(pageState.command)) {
        pageState.initializeEntries("row");

        if (!JSP.ex(pageState.mainObjectId)) {
          pageState.mainObjectId = PersistenceHome.NEW_EMPTY_ID;
        }

        RestState restState = pageState; //qui si lavora con il pageStatevero
        TaskAction ta = new TaskAction(restState);

        //ce coming from multi editor are incomplete
        // perform a make and override arrived CE
        ClientEntries newCes = new ClientEntries();
        newCes.addEntries(restState.getClientEntriesSet());

        if (!(restState.mainObjectId + "").startsWith("new")) {
          ta.cmdEdit();
        }

        // override ce from edit with saved ones
        restState.addClientEntries(newCes);

        ta.cmdSave();

        Task task = (Task) restState.getMainObject();
        JSONObject jProject = task.jsonify(true, pageState);
        addRequiredJsonData(task, logged, jProject);
        json.element("task", jProject);

      }else if ("SAVETASKSTATUS".equals(pageState.command)) {
        pageState.initializeEntries("row");

        RestState restState = pageState; //qui si lavora con il pageStatevero
        TaskAction ta = new TaskAction(restState);
        ta.cmdChangeStatus();

        Task task = (Task) restState.getMainObject();
        JSONObject jProject = task.jsonify(true, pageState);
        addRequiredJsonData(task, logged, jProject);
        json.element("task", jProject);

      } else if ("LOADTASK".equals(pageState.command)) {
        Task task = null;
        task = Task.load(pageState.getEntry("ID").intValueNoErrorCodeNoExc() + "");
        if (task != null && task.hasPermissionFor(logged, TeamworkPermissions.task_canRead)) {

          JSONObject jProject = task.jsonify(true, pageState);
          addRequiredJsonData(task, logged, jProject);

          List<Issue> issues = task.getOpenIssuesById(pageState);
          JSONArray jsa = new JSONArray();
          for (Issue i : issues) {
            JSONObject jso = i.jsonify();
            jso = addRequiredJsonDataToIssue(i, logged, jso);
            jsa.add(jso);
          }

          jProject.element("issues", jsa);

          json.element("task", jProject);

        }
        //---------------------------------------------------------------------------------------   RESOURCE ACTIONS  ------------------------------------------------
      } else if ("SAVERESOURCE".equals(pageState.command)) {
        pageState.initializeEntries("row");

        if (!JSP.ex(pageState.mainObjectId)) {
          pageState.mainObjectId = PersistenceHome.NEW_EMPTY_ID;
        }

        RestState restState = pageState; //qui si lavora con il pageStatevero
        ResourceAction ta = new ResourceAction(restState);

        //ce coming from multi editor are incomplete
        // perform a make and override arrived CE
        ClientEntries newCes = new ClientEntries();
        newCes.addEntries(restState.getClientEntriesSet());

        // first I have to perform a fake-make or an add
        if ((restState.mainObjectId + "").startsWith("new")) {
          String resource_type = pageState.getEntry("RESOURCE_TYPE").stringValueNullIfEmpty();
          restState.addClientEntry("RESOURCE_TYPE", "PERSON".equals(resource_type) ? Person.class.getName() : Company.class.getName());
          restState.mainObjectId = pageState.getEntry("PARENT_ID").stringValueNullIfEmpty();
          ta.cmdAdd();
          restState.mainObjectId = PersistenceHome.NEW_EMPTY_ID;
        } else {
          ta.cmdEdit();
        }

        // override ce from edit with saved ones
        restState.addClientEntries(newCes);

        ta.cmdSave();

        Resource res = (Resource) restState.getMainObject();
        JSONObject resJ = new JSONObject();

        if (res instanceof Person)
          resJ = ((Person) res).jsonify(true);

        json.element("resource", resJ);

      } else if ("SAVEASSIGNMENT".equals(pageState.command)) {
        pageState.initializeEntries("row");

        AssignmentAction ta = new AssignmentAction(pageState);

        if (!JSP.ex(pageState.mainObjectId)) {
          pageState.mainObjectId = PersistenceHome.NEW_EMPTY_ID;
          ta.cmdAdd();
        }
        ta.cmdSave();
        Assignment assig = (Assignment) pageState.getMainObject();
        Task task = assig.getTask();
        JSONObject jProject = task.jsonify(true, pageState);
        addRequiredJsonData(task, logged, jProject);

        json.element("task", jProject);

      } else if ("RESOURCESEARCH".equals(pageState.command)) {  // this is a sort of find command
        pageState.setCommand(Commands.FIND);
        String search = pageState.getEntry("SEARCH").stringValueNullIfEmpty();

        pageState.setClientEntries(new ClientEntries());

        if (JSP.ex(search)) {
          pageState.addClientEntry("NAME_SURNAME", search);
        } else {
          pageState.addClientEntry(Fields.FLD_FILTER_NAME, "PF_RES_TEAMWORK_USERS");
        }
        ResourceAction ra = new ResourceAction(pageState);
        ra.cmdFind();
        if (pageState.getPage() != null) {

          JSONArray jsa = new JSONArray();
          List<Resource> resources = pageState.getPage().getAllElements();
          for (Resource r : resources) {
            JSONObject resJ = r.jsonify(false);
            resJ.element("canWriteResource", r.hasPermissionFor(logged, TeamworkPermissions.resource_canWrite) || r.equals(logged.getPerson()));
            jsa.add(resJ);


          }
          json.element("resources", jsa);
        }
      } else if ("LOADRESOURCE".equals(pageState.command)) {
        Resource res = null;
        res = Resource.load(pageState.getEntry("ID").intValueNoErrorCodeNoExc() + "");
        if (res != null && res.hasPermissionFor(logged, TeamworkPermissions.resource_canRead)){
          JSONObject resJ = res.jsonify(true);
          resJ.element("canWriteResource", res.hasPermissionFor(logged, TeamworkPermissions.resource_canWrite) || res.equals(logged.getPerson()));

          json.element("resource", resJ);
        }

        //------------------------------------------------------------------------------------------------   ISSUE ACTIONS  ------------------------------------------------
      } else if ("ISSUESEARCH".equals(pageState.command)) {  // this is a sort of find command
        pageState.setCommand(Commands.FIND);
        String search = pageState.getEntry("SEARCH").stringValueNullIfEmpty();
        String searchType = pageState.getEntry("SCHTYPE").stringValueNullIfEmpty();
        String taskId = pageState.getEntry("TASKID").stringValueNullIfEmpty();

        //clear original CEs
        pageState.setClientEntries(new ClientEntries());

        if ("SEARCH".equals(searchType) && JSP.ex(search)) {
          pageState.addClientEntry("FLT_ISSUE_DESCRIPTION", search);

        } else if ("TASK".equals(searchType) && JSP.ex(taskId)) {
          pageState.addClientEntry("FLT_ISSUE_TASK", taskId);
          pageState.addClientEntry("FLT_OPEN_ISSUES", "yes");

        } else {
          pageState.addClientEntry(Fields.FLD_FILTER_NAME, searchType);
        }
        IssueAction ia = new IssueAction(pageState);
        ia.cmdFind();
        if (pageState.getPage() != null) {
          JSONArray jsa = new JSONArray();

          List<Object[]> issues = pageState.getPage().getThisPageElements();
          for (Object[] r : issues) {
            Issue issue = Issue.load((String) r[0]);

            JSONObject jso = issue.jsonify();
            jso = addRequiredJsonDataToIssue(issue, logged, jso);

            jsa.add(jso);
          }
          json.element("issues", jsa);
        }

      } else if ("LOADISSUE".equals(pageState.command)) {
        Issue issue = null;
        issue = Issue.load(pageState.getEntry("ID").intValueNoErrorCodeNoExc() + "");
        if (issue != null && issue.hasPermissionFor(logged, TeamworkPermissions.issue_canRead)) {
          JSONObject jso = issue.jsonify();
          jso = addRequiredJsonDataToIssue(issue, logged, jso);
          json.element("issue", jso);
        }
      } else if ("ADDISSWL".equals(pageState.command)) {
        pageState.initializeEntries("row");
        int issueId = pageState.getEntry("ISSUEID").intValueNoErrorCodeNoExc();
        String wl = pageState.getEntry("WL").stringValueNullIfEmpty();
        String assId = pageState.getEntry("ASSID").stringValueNullIfEmpty();
        String action = pageState.getEntry("ACTION").stringValueNullIfEmpty();
        if (issueId > 0 && JSP.ex(wl)) {

          //remove all CEs
          pageState.setClientEntries(new ClientEntries());
          //set main objectid
          pageState.mainObjectId = issueId + ""; //id is a String

          IssueAction issueAction = new IssueAction(pageState);

          // first I have to perform a fake-make
          issueAction.cmdEdit();

          //set values
          pageState.addClientEntry("ISSUE_WORKLOG_TIME", wl);
          pageState.addClientEntry("ISSUE_WORKLOG_ACTION", action);
          pageState.addClientEntry("ISSUE_WORKLOG_ASSIGNMENT", assId);

          Issue issue = (Issue) pageState.getMainObject();

          // and now perform a real save
          issueAction.cmdSave();
          JSONObject jso = issue.jsonify();
          jso = addRequiredJsonDataToIssue(issue, logged, jso);
          json.element("issue", jso);

        }


      } else if ("CLOSEISSUE".equals(pageState.command)) {
        pageState.initializeEntries("row");
        int issueId = pageState.getEntry("ID").intValueNoErrorCodeNoExc();
        if (issueId > 0) {

          //remove all CEs
          pageState.setClientEntries(new ClientEntries());
          //set main objectid
          pageState.mainObjectId = issueId + ""; //id is a String

          IssueAction issueAction = new IssueAction(pageState);

          // first I have to perform a fake-make
          issueAction.cmdEdit();

          //set status to close
          pageState.addClientEntry("ISSUE_STATUS", IssueStatus.getStatusClose());

          Issue issue = (Issue) pageState.getMainObject();

          IssueStatus oldStatus = issue.getStatus();

          // and now perform a real save
          issueAction.cmdChangeStatus();

          IssueStatus newStatus = issue.getStatus();

          // check if worklog input can be displayed
          if (oldStatus != newStatus && newStatus.isAskForWorklog()) {
            Assignment ass = issue.getAssignmentOnTask(loggedPerson);
            if (ass != null) {
              json.element("askWL", true);
              json.element("assId", ass.getId());
            }
          }
          JSONObject jso = issue.jsonify();
          jso = addRequiredJsonDataToIssue(issue, logged, jso);
          json.element("issue", jso);

        }

      } else if ("SVISSUE".equals(pageState.command)) {
        pageState.initializeEntries("row");
        if (!JSP.ex(pageState.mainObjectId)) {
          pageState.mainObjectId = PersistenceHome.NEW_EMPTY_ID;
        }

        int dateInt = pageState.getEntry("ISSUE_CLOSE_DATEINT").intValueNoErrorCodeNoExc();
        if (dateInt > 0) {
          pageState.addClientEntry("ISSUE_DATE_CLOSE_BY", DateUtilities.intToDate(dateInt));
        }

        IssueAction ia = new IssueAction(pageState);
        ia.cmdSave();
        Issue issue = (Issue) pageState.getMainObject();
        JSONObject jso = issue.jsonify();
        jso = addRequiredJsonDataToIssue(issue, logged, jso);
        json.element("issue", jso);

      } else if ("DLISSUE".equals(pageState.command)) {
        pageState.mainObjectId = pageState.getEntry("ISSUEID").stringValueNullIfEmpty();
        IssueAction ia = new IssueAction(pageState);
        ia.cmdDelete();


        //------------------------------------------------------------------------------------------------   FILE STORAGE  ------------------------------------------------
      } else if ("DLDOC".equals(pageState.command)) {
        pageState.mainObjectId = pageState.getEntry("DOCID").stringValueNullIfEmpty();
        DocumentAction ia = new DocumentAction(pageState);
        ia.cmdDelete();


      } else if ("EXPLOREFS".equals(pageState.command)) {
        FileStorage fileStorage = FileStorage.load(pageState.getEntry("FSID").stringValue());
        String rootPath = null;

        boolean canRead = fileStorage.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canRead);
        boolean canWrite = fileStorage.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canWrite);
        boolean canCreateDirectory = fileStorage.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canCreate);


        // is there is no global permissions check if there is some rights in session
        // in case your are coming from a task and you have global read you can move outside the folder
        if (!canRead || !canWrite || !canCreateDirectory) {
          Explorer.SecurityCarrier esc = (Explorer.SecurityCarrier) pageState.sessionState.getAttribute(Explorer.SecurityCarrier.getKey(pageState.mainObjectId));
          if (esc != null) {
            canRead = canRead || esc.canRead;
            canWrite = canWrite || esc.canWrite;
            canCreateDirectory = canCreateDirectory || esc.canCreateDirectory;
            rootPath = esc.rootPath;
          }
        }

        if (canRead) {
          RemoteFile rfs = RemoteFile.getInstance(fileStorage);
          String path = pageState.getEntry("PATH").stringValueNullIfEmpty();
          rootPath = (rootPath == null ? "" : rootPath);
          if (path == null)
            path = rootPath;
          else if (!path.toLowerCase().replace(File.separatorChar, '/').startsWith(rootPath.toLowerCase().replace(File.separatorChar, '/')))
            path = rootPath;

          rfs.setTarget(path);

          JSONArray dirs = new JSONArray();
          JSONArray files = new JSONArray();

          List<RemoteFile> remoteFileList = rfs.listFiles();

          for (RemoteFile rf : remoteFileList) {
            if (rf.isDirectory())
              dirs.add(rf.jsonify());
            else
              files.add(rf.jsonify());
          }

          json.element("dirs", dirs);
          json.element("files", files);
        }

        //------------------------------------------------------------------------------------------------   DOCUMENTS ACTIONS  ------------------------------------------------

      } else if ("DOCSEARCH".equals(pageState.command)) {

          String search = pageState.getEntry("SEARCH").stringValueNullIfEmpty();
          String searchType = pageState.getEntry("SCHTYPE").stringValueNullIfEmpty();
          String taskId = pageState.getEntry("TASKID").stringValueNullIfEmpty();

          //clear original CEs
          pageState.setClientEntries(new ClientEntries());
          pageState.setCommand(Commands.FIND);

          if ("SEARCH".equals(searchType) && JSP.ex(search)) {  // ----------------------- FULLTEXT SEARCH -----------------------
            FullTextSearchControllerAction ftsca = new FullTextSearchControllerAction(pageState);
            pageState.addClientEntry("TEXT", search);
            pageState.addClientEntry("CLASSNAME", TeamworkDocument.class.getName());
            ftsca.cmdFind();
            if (pageState.getPage() != null) {

              String wt = pageState.getEntry("TEXT").stringValue();

              SnowballHackedAnalyzer hackedAnalyzer = new SnowballHackedAnalyzer();

              QueryParser snowParser = new QueryParser(Version.LUCENE_30, "content", hackedAnalyzer);
              Query searchQuery = snowParser.parse(wt);


              QueryScorer qs = new QueryScorer(searchQuery);
              Highlighter highlighter = new Highlighter(qs);


              JSONArray jsa = new JSONArray();
              List<Object[]> docs = pageState.getPage().getThisPageElements();
              for (Object[] o : docs) {
                Number score = (Number) o[1];

                Document lucDoc = (Document) o[0];
                String id = lucDoc.get("id");
                String clazz = lucDoc.get(ProjectionConstants.OBJECT_CLASS);
                //a check to avoid strange skerz
                if (TeamworkDocument.class.getName().equals(clazz)) {
                  TeamworkDocument doc = TeamworkDocument.load(id);
                  if (doc != null && doc.hasPermissionFor(logged, TeamworkPermissions.document_canRead)) {
                    JSONObject jdoc = doc.jsonify(pageState);
                    jdoc.element("score", (int) (score.doubleValue() * 100)); // add score from lucene

                    String abs = lucDoc.get("abstract");
                    if (JSP.ex(abs)) {
                      String high = highlighter.getBestFragment(hackedAnalyzer, "content", abs); //JSP.cleanHTML(abs)
                      if (JSP.ex(high))
                        jdoc.element("abstract", high);
                    }
                    jsa.add(jdoc);
                  }
                }


              }
              json.element("documents", jsa);
            }

          } else {

            if ("TASK".equals(searchType) && JSP.ex(taskId)) {
              pageState.addClientEntry("task", taskId);
            } else {
              pageState.addClientEntry(Fields.FLD_FILTER_NAME, searchType);
            }

            DocumentAction da = new DocumentAction(pageState);
            da.cmdFind();
            if (pageState.getPage() != null) {
              JSONArray jsa = new JSONArray();

              List<TeamworkDocument> docs = pageState.getPage().getThisPageElements();
              for (TeamworkDocument d : docs) {
                jsa.add(d.jsonify(pageState));
              }
              json.element("documents", jsa);
            }


          }


        }else if ("ISSUEATTACH".equals(pageState.command)) {
        Issue issue = Issue.load(pageState.getEntry("issueId").intValueNoErrorCodeNoExc() + "");
        if (issue != null && issue.hasPermissionFor(logged, TeamworkPermissions.issue_canWrite)) {

          PersistentFile pf = Uploader.save(issue, "attachment", pageState);
          issue.addFile(pf);
          issue.store();

          JSONObject jsonify = pf.jsonify();
          jsonify.element("issueId",issue.getId());
          json.element("attachment", jsonify);

        }

        // ------------------------------------------------------------------------- DELETE SCREENSHOT
      } else if ("DELSCRSHT".equals(pageState.command)) {
        Issue issue = Issue.load(pageState.getEntry("issueId").intValueNoErrorCodeNoExc() + "");
        if (issue != null && issue.hasPermissionFor(logged, TeamworkPermissions.issue_canWrite)) {
          String fileUID= pageState.getEntry("fileUID").stringValueNullIfEmpty();
          if (JSP.ex(fileUID)) {
            PersistentFile pf = PersistentFile.deserialize(fileUID);
            if (pf!=null && pf.checkChecksum(pageState.getEntry("ck").stringValueNullIfEmpty()+"")) { //si controlla anche il checksum
              pf.delete();
              issue.removeFile(pf);
              issue.store();
            }
          }
        }

        //------------------------------------------------ BULK ISSUE ACTIONS -----------------------------------------------------------------------------------------------
      } else if ("TASKATTACH".equals(pageState.command)) {

        Task task = Task.load(pageState.getEntry("taskId").intValueNoErrorCodeNoExc() + "");

        if (task != null && task.hasPermissionFor(logged, TeamworkPermissions.document_canCreate)) {

          PersistentFile pf = Uploader.save(task, "attachment", pageState);


          TeamworkDocument document = new TeamworkDocument();
          document.setIdAsNew();
          document.setName(pf.getOriginalFileName());
          document.setAuthor(logged.getDisplayName());
          document.setType(TeamworkDocument.IS_UPLOAD);
          document.setVersion("01");
          document.setTask(task);
          document.setFile(pf);
          document.setContent("");
          document.store();


          DocumentAction.generateDocumentEvent(document, logged);

          json.element("attachment", pf.jsonify());
          json.element("taskDoc", document.jsonify(pageState));


        }

      } else if ("FILESTORAGES".equals(pageState.command)) {
        List<FileStorage> fsdocs = DocumentBricks.getFileStoragesOrdered(logged, pageState);
        JSONArray jsa = new JSONArray();
        for (FileStorage fs : fsdocs) {
          jsa.add(fs.jsonify());
        }
        json.element("fileStorages", jsa);


      } else if ("LOADDOCUMENT".equals(pageState.command)) {
        TeamworkDocument doc = null;
        doc = TeamworkDocument.load(pageState.getEntry("ID").intValueNoErrorCodeNoExc() + "");
        if (doc != null && doc.hasPermissionFor(logged, TeamworkPermissions.document_canRead)){
          JSONObject d = doc.jsonify(pageState);
          if(doc.getTask() != null)
            d.element("taskId", doc.getTask().getId());
          json.element("document", d);
        }

        //------------------------------------------------------------------------------------------------   CALENDAR ACTIONS  ------------------------------------------------
      } else if ("LOADEVENTS".equals(pageState.command)) {

        int dateInt = pageState.getEntry("DATEINT").intValueNoErrorCodeNoExc();
        boolean loadHoly = pageState.getEntry("LDHOLY").checkFieldValue();

        Date focusedDate = DateUtilities.intToDate(dateInt);
        CompanyCalendar cc = new CompanyCalendar(focusedDate);

        if (loadHoly) {
          JSONObject eventsHolidays = new JSONObject();


          JSONArray jsa = new JSONArray();
          for (String m : cc.getHolyDays())
            jsa.add(Integer.parseInt(m.replace("_", "")));

          eventsHolidays.element("holidays", jsa);

          jsa = new JSONArray();
          if (!CompanyCalendar.FRIDAY_IS_WORKING_DAY)
            jsa.add(CompanyCalendar.FRIDAY - 1);
          if (!CompanyCalendar.SATURDAY_IS_WORKING_DAY)
            jsa.add(CompanyCalendar.SATURDAY - 1);
          if (!CompanyCalendar.SUNDAY_IS_WORKING_DAY)
            jsa.add(CompanyCalendar.SUNDAY - 1);

          eventsHolidays.element("week", jsa);

          json.element("eventsHolidays", eventsHolidays);

        }

        //load events
        List<Resource> resourcesFromFilter = new ArrayList<Resource>();
        resourcesFromFilter.add(loggedPerson);

        cc.setTime(focusedDate);
        cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
        cc.getTime(); // DO NOT REMOVE this forc CC to recompute the fields before setting day of week
        cc.set(CompanyCalendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
        Date startDate = cc.setAndGetTimeToDayStart();

        cc.setTime(focusedDate);

        cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
        cc.add(CompanyCalendar.MONTH, 1);
        cc.add(CompanyCalendar.DAY_OF_MONTH, -1);
        cc.set(CompanyCalendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
        cc.add(CompanyCalendar.DATE, 7);

        Date endDate = cc.setAndGetTimeToDayEnd();

        Period periodMonth = new Period(startDate, endDate);

        boolean onlyMeInIt = false;
        boolean showAuthored = false;
        boolean showTargets = true;
        boolean isPersonal = false;
        boolean showWork = false;
        boolean unavail = false;

        List<Event> eventsForThisMonth = Event.getFilteredEventsInPeriodWithCollisionFor(resourcesFromFilter, periodMonth, 0, showWork, onlyMeInIt, showAuthored, showTargets, isPersonal, unavail);

        JSONArray jsa = new JSONArray();


        cc.setTime(startDate);
        while (cc.getTime().getTime() < endDate.getTime()) {

          Period theWorkingDayPeriod = Period.getDayPeriodInstance(cc.getTime());

          ArrayList<PeriodEvent> oneDayPeriodEvents = new ArrayList();
          for (Event event : eventsForThisMonth) {

            // get working day
            List<Period> periods = event.getSchedule().getPeriods(theWorkingDayPeriod, true, event.getExceptions());
            for (Period period : periods) {
              period.setIdAsNew();
              oneDayPeriodEvents.add(new PeriodEvent(period, event));
            }
          }
          Collections.sort(oneDayPeriodEvents);
          for (PeriodEvent pev : oneDayPeriodEvents) {
            JSONObject jEv = pev.event.jsonify(pageState);
            jEv.element("startMillis", pev.period.getStartDate().getTime());
            jEv.element("endMillis", pev.period.getEndDate().getTime());
            jsa.add(jEv);
          }

          cc.add(CompanyCalendar.DATE, 1);
        }
        json.element("events", jsa);

      } else if ("SAVEEVENT".equals(pageState.command)) {
        pageState.initializeEntries("row");

        //build a fake period in order to create CEs
        pageState.mainObjectId = null;

        Person person = loggedPerson;
        pageState.addClientEntry("WG_IDS", person.getId() + "");

        AgendaAction aa = new AgendaAction(pageState);
        aa.cmdSave();

        Event event = (Event) pageState.getMainObject();
        JSONObject jsEv = event.jsonify(pageState);
        jsEv.element("startMillis", event.getSchedule().getStartDate().getTime());
        jsEv.element("endMillis", event.getSchedule().getEndDate().getTime());
        json.element("event", jsEv);


      } else if ("DLAGEEVENT".equals(pageState.command)) {
        AgendaAction aa = new AgendaAction(pageState);
        pageState.mainObjectId = pageState.getEntry("EVID").stringValueNullIfEmpty();
        aa.cmdDelete();


        //------------------------------------------------------------------------------------------------   COUNTERS ACTIONS  ------------------------------------------------
      } else if ("LOADCOUNTERS".equals(pageState.command)) {
        Person resource = Person.getLoggedPerson(pageState);
        List<Assignment> assigs = resource.getActiveAssignments(Period.getDayPeriodInstance(new Date()), true);
        Collections.sort(assigs, new TeamworkComparators.AssignmentByPriority(new Date()));
        JSONArray jsa = new JSONArray();

        for (Assignment ass : assigs) {
          Task task = ass.getTask();
          if (task.isActive() && ass.isEnabled()) {
            jsa.add(ass.jsonify());
          }
        }
        json.element("counters", jsa);


      } else if ("STARTCOUNTER".equals(pageState.command)) {
        pageState.initializeEntries("row");
        TimeCounterAction tcc = new TimeCounterAction(pageState);
        pageState.mainObjectId = pageState.getEntry("assId").intValueNoErrorCodeNoExc() + "";
        Worklog worklog = tcc.cmdStart(pageState);


      } else if ("STOPCOUNTER".equals(pageState.command)) {
        pageState.initializeEntries("row");
        pageState.mainObjectId = pageState.getEntry("assId").intValueNoErrorCodeNoExc() + "";
        TimeCounterAction tcc = new TimeCounterAction(pageState);
        Worklog worklog = tcc.cmdStop(pageState);
        if (worklog != null)
          json.element("worklog", worklog.jsonify());

        //------------------------------------------------------------------------------------------------   WORKLOG ACTIONS  ------------------------------------------------
      } else if ("CHECKTIMECOUNTER".equals(pageState.command)) {
        Assignment countedAssignment = Assignment.getCountedAssignment(logged);
        if (JSP.ex(countedAssignment)) {
          json.element("assigId", countedAssignment.getIntId());
        }
        //------------------------------------------------------------------------------------------------   WORKLOG ACTIONS  ------------------------------------------------
      } else if ("LOADWORKLOG".equals(pageState.command)) {

        int dateInt = pageState.getEntry("DATEINT").intValueNoErrorCodeNoExc();
        if (dateInt <= 0)
          dateInt = DateUtilities.dateToInt(new Date());

        CompanyCalendar cc = new CompanyCalendar(DateUtilities.intToDate(dateInt));
        cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
        Date start = cc.getTime();
        cc.add(CompanyCalendar.MONTH, 1);
        cc.add(CompanyCalendar.DATE, -1);
        Date end = cc.getTime();

        pageState.addClientEntry("WORKLOG_AT_DAY", DateUtilities.dateToString(start) + ":" + DateUtilities.dateToString(end));
        pageState.addClientEntry("RES_ID", loggedPerson.getId());

        WorklogAction wla = new WorklogAction(pageState);
        wla.cmdFind();


        if (pageState.getPage() != null) {
          Map<String, JSONArray> monthWl = new Hashtable();

          List<Worklog> wls = pageState.getPage().getAllElements();
          for (Worklog wl : wls) {
            int date = DateUtilities.dateToInt(wl.getInserted());
            JSONArray ogg = (JSONArray) monthWl.get(date + "");
            if (ogg == null) {
              ogg = new JSONArray();
              monthWl.put(date + "", ogg);
            }
            ogg.add(wl.jsonify());
          }
          json.element("worklogs", monthWl);
        }

      } else if ("SAVEWORKLOG".equals(pageState.command)) {
        pageState.initializeEntries("row");
        long dur = pageState.getEntry("DURATION").durationInWorkingMillis(false);
        int dateInt = pageState.getEntry("DATEINT").intValueNoErrorCodeNoExc();
        Assignment ass = Assignment.load(pageState.getEntry("ASSID").intValueNoErrorCodeNoExc() + "");
        String action = pageState.getEntry("WORKLOG_ACTION").stringValueNullIfEmpty();

        if (dur > 0 && ass != null && ass.getResource().equals(loggedPerson)) {
          CompanyCalendar cc = new CompanyCalendar(DateUtilities.intToDate(dateInt));
          cc.setMillisFromMidnight(CompanyCalendar.getMillisFromMidnight(new Date()));
          Worklog wl = new Worklog();
          wl.setIdAsNew();
          wl.setAssig(ass);
          wl.setDuration(dur);
          wl.setInserted(cc.getTime());
          wl.bricks.testWritePermission(logged);
          wl.setAction(action);


          int issueId = pageState.getEntry("ISSUEID").intValueNoErrorCodeNoExc();
          if (issueId > 0) {
            wl.setIssue(Issue.load(issueId + ""));
          }

          //Custom fields
          DesignerField.saveCustomFields("WORKLOG_CUSTOM_FIELD_", 4, wl, pageState);

          wl.store();

          WorklogAction wla = new WorklogAction(pageState);
          wla.checkAndGenerateEventForInvalidWorklog(wl);
          json.element("worklog", wl.jsonify());
        }


      } else if ("DLWL".equals(pageState.command)) {
        pageState.initializeEntries("row");
        Worklog wl = Worklog.load(pageState.getEntry("WLID").intValueNoErrorCodeNoExc() + "");
        if (wl != null && wl.getAssig().getResource().equals(loggedPerson)) {
          DeleteHelper.cmdDelete(wl, pageState);
        }


        //------------------------------------------------------------------------------------------------   EXPENSES ACTIONS  ------------------------------------------------
      } else if ("LOADEXPENSES".equals(pageState.command)) {

        int dateInt = pageState.getEntry("DATEINT").intValueNoErrorCodeNoExc();
        if (dateInt <= 0)
          dateInt = DateUtilities.dateToInt(new Date());

        CompanyCalendar cc = new CompanyCalendar(DateUtilities.intToDate(dateInt));
        cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
        Date start = cc.getTime();
        cc.add(CompanyCalendar.MONTH, 1);
        cc.add(CompanyCalendar.DATE, -1);
        Date end = cc.getTime();

        pageState.addClientEntry("COST_AT_DAY", DateUtilities.dateToString(start) + ":" + DateUtilities.dateToString(end));
        pageState.addClientEntry("RES_ID", loggedPerson.getId());

        ExpenseAction expA = new ExpenseAction(pageState);
        expA.cmdFind();


        if (pageState.getPage() != null) {

          Map<String, JSONArray> monthExp = new Hashtable();

          List<Object[]> objs = pageState.getPage().getAllElements();
          for (Object[] obj : objs) {
            Assignment assig = (Assignment) obj[0];
            Cost cost = (Cost) obj[1];

            int date = DateUtilities.dateToInt(cost.getCreationDate());
            JSONArray ogg = (JSONArray) monthExp.get(date + "");
            if (ogg == null) {
              ogg = new JSONArray();
              monthExp.put(date + "", ogg);
            }
            JSONObject costJ = cost.jsonify();
            costJ.element("assId", assig.getId());
            costJ.element("taskId", assig.getTask().getId());
            costJ.element("taskName", assig.getTask().getName());
            costJ.element("taskCode", assig.getTask().getCode());

            ogg.add(costJ);
          }
          json.element("expenses", monthExp);
        }


        //-----------------------------------------   MY ASSIGNMENTS  ------------------------------------------------
      } else if ("GETMYASSIGNMENTS".equals(pageState.command)) {
        boolean showNotActiveTasks = pageState.getEntryOrDefault("ASSIG_SHOW_NOTACTIVETASKS", Fields.FALSE).checkFieldValue();
        boolean showNotActiveAssig = pageState.getEntryOrDefault("ASSIG_SHOW_NOTACTIVE", Fields.FALSE).checkFieldValue();
        int daysOnTheHorizon = pageState.getEntryOrDefault("ASSIG_SHOW_HORIZON", "0").intValue();

        Set<Task> tasks = new HashSet();

        List<Assignment> expiredAssigs = loggedPerson.getExpiredAssignments(new Date(), !showNotActiveAssig, true);
        if (JSP.ex(expiredAssigs)) {
          JSONArray expAssigs = new JSONArray();
          for (Assignment ass : expiredAssigs) {
            if (Assignment.ACTIVITY_REPEATED_IN_TIME.equals(ass.getActivity()))
              continue;
            JSONObject jAss = ass.jsonify();
            tasks.add(ass.getTask());
            expAssigs.add(jAss);
          }
          json.element("expiredAssignments", expAssigs);
        }


        List<Assignment> assigs = loggedPerson.getAssignmentsByPriority(new Period(System.currentTimeMillis(), System.currentTimeMillis() + 10), !showNotActiveAssig, !showNotActiveTasks);
        if (daysOnTheHorizon > 0) {
          CompanyCalendar cc = new CompanyCalendar();
          Date start = cc.setAndGetTimeToDayStart();
          cc.addWorkingDays(daysOnTheHorizon);
          List<Assignment> assignmentList = loggedPerson.getAssignmentsByPriority(new Period(start, cc.setAndGetTimeToDayEnd()), !showNotActiveAssig, !showNotActiveTasks);
          for (Assignment ass : assignmentList) {
            if (!assigs.contains(ass)) {
              assigs.add(ass);
              tasks.add(ass.getTask());
            }
          }
        }

        if (JSP.ex(assigs)) {
          JSONArray jassigs = new JSONArray();

          for (Assignment ass : assigs) {
            if (Assignment.ACTIVITY_REPEATED_IN_TIME.equals(ass.getActivity()))
              continue;
            if (!tasks.contains(ass.getTask())) {
              JSONObject jAss = ass.jsonify();
              jassigs.add(jAss);
              tasks.add(ass.getTask());
            }
          }

          json.element("assignments", jassigs);
        }

        if (JSP.ex(tasks)) {
          JSONArray jtasks = new JSONArray();
          for (Task t : tasks)
            jtasks.add(t.jsonify(true, pageState));

          json.element("tasks", jtasks);

        }


        //-----------------------------------------   HEADLINE  --------------------------------------------   HEADLINE  ------------------------------------------------
      } else {
        if ("GETHEADLINE".equals(pageState.command)) {

          JSONObject headlineData = new JSONObject();
          CompanyCalendar cc = new CompanyCalendar();

          //-------------------------- MILESTONES OVERDUE -------------------------------------
          {
            //ricerca task su cui sei assegnato o tutti se hai i diritti in cui:
            //   lo stato è aperto e end è passata e end è milestone
            //   lo stato è sospeso e start è passato e start è milestone
            cc = new CompanyCalendar();
            String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
            QueryHelper qhelp = new QueryHelper(hql);
            //  security
            TaskBricks.addSecurityReadClauses(qhelp, pageState);
            qhelp.addOQLClause("task.schedule is not null");
            qhelp.addQueryClause("( task.status='" + TaskStatus.STATUS_SUSPENDED + "' and task.startIsMilestone=true and task.schedule.start<=:now");
            qhelp.addOrQueryClause("(task.status='" + TaskStatus.STATUS_SUSPENDED + "' or task.status='" + TaskStatus.STATUS_ACTIVE + "') and task.endIsMilestone=true and task.schedule.end<=:now )");
            qhelp.addParameter("now", cc.setAndGetTimeToDayStart());

            long count = (Long) qhelp.toHql().uniqueResult();
            headlineData.element("wphMilesOver", count);
          }


          //-------------------------- BUDGET OVERFLOW -------------------------------------
          {
            cc = new CompanyCalendar();
            String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
            QueryHelper qhelp = new QueryHelper(hql);

            //security
            Set<Permission> perms = new HashSet();
            perms.add(TeamworkPermissions.task_cost_canRead);
            TaskBricks.addSecurityClauses(qhelp, perms, pageState);

            //qhelp.addOQLClause("task.parent is null");
            qhelp.addOQLClause("task.status='" + TaskStatus.STATUS_ACTIVE + "'");
            qhelp.addOQLClause("task.forecasted>0");
            qhelp.addOQLClause("task.forecasted<task.totalCostsDone");

            long count = (Long) qhelp.toHql().uniqueResult();
            headlineData.element("wphProjectBudgetOverflow", count);
          }

          //-------------------------- TASK OVER -------------------------------------
          {
            cc = new CompanyCalendar();
            String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
            QueryHelper qhelp = new QueryHelper(hql);
            //  security
            TaskBricks.addSecurityReadClauses(qhelp, pageState);
            qhelp.addOQLClause("task.schedule is not null");
            qhelp.addOQLClause("task.status='" + TaskStatus.STATUS_ACTIVE + "'");
            qhelp.addQueryClause("task.schedule.end<:now");
            qhelp.addParameter("now", cc.setAndGetTimeToDayStart());
            long count = (Long) qhelp.toHql().uniqueResult();
            headlineData.element("wphTasksOver", count);
          }

          //-------------------------- ISSUES OVER -------------------------------------
          {
            cc = new CompanyCalendar();
            String hql = "select count(distinct issue.id) from " + Issue.class.getName() + " as issue";
            QueryHelper qhelp = new QueryHelper(hql);
            //  security
            IssueBricks.addSecurityClauses(qhelp, pageState);
            qhelp.addJoinAlias("join issue.task as task");
            qhelp.addJoinAlias("left outer join issue.assignedTo as resource");

            qhelp.addOQLClause("task.status='" + TaskStatus.STATUS_ACTIVE + "'");
            qhelp.addQueryClause("issue.status.behavesAsOpen=true");
            qhelp.addQueryClause("issue.shouldCloseBy<:now");
            qhelp.addParameter("now", cc.setAndGetTimeToDayStart());

            long count = (Long) qhelp.toHql().uniqueResult();
            headlineData.element("wphIssuesOver", count);
          }


          //-------------------------- FORTHCOMING MILESTONES -------------------------------------
          {
            cc = new CompanyCalendar();
            String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
            QueryHelper qhelp = new QueryHelper(hql);
            //  security
            TaskBricks.addSecurityReadClauses(qhelp, pageState);
            qhelp.addOQLClause("task.status in ('" + TaskStatus.STATUS_ACTIVE + "','" + TaskStatus.STATUS_SUSPENDED + "')");
            qhelp.addOQLClause("task.schedule is not null");
            qhelp.addOQLClause("( task.startIsMilestone=true and task.schedule.start between :now and :nextWeek");
            qhelp.addOrQueryClause("task.endIsMilestone=true and task.schedule.end between :now and :nextWeek )");
            qhelp.addParameter("now", cc.setAndGetTimeToDayStart());
            cc.add(CompanyCalendar.DATE, 7);
            qhelp.addParameter("nextWeek", cc.setAndGetTimeToDayEnd());

            long count = (Long) qhelp.toHql().uniqueResult();
            headlineData.element("wphMilesForthcoming", count);
          }


          //-------------------------- FORTHCOMING END -------------------------------------
          {
            cc = new CompanyCalendar();
            String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
            QueryHelper qhelp = new QueryHelper(hql);
            //  security
            TaskBricks.addSecurityReadClauses(qhelp, pageState);
            qhelp.addOQLClause("task.schedule is not null");
            qhelp.addOQLClause("task.schedule.end between :now and :nextWeek");
            qhelp.addOQLClause("task.status='" + TaskStatus.STATUS_ACTIVE + "'");

            qhelp.addParameter("now", cc.setAndGetTimeToDayStart());
            cc.add(CompanyCalendar.DATE, 7);
            qhelp.addParameter("nextWeek", cc.setAndGetTimeToDayEnd());

            PageSeed taskList = pageState.pageFromRoot("task/taskList.jsp");
            taskList.command = Commands.FIND;
            taskList.addClientEntry("END", "T:1w");
            taskList.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);

            long count = (Long) qhelp.toHql().uniqueResult();
            headlineData.element("wphTasksForthcoming", count);

          }
          //-------------------------- ISSUES ABOUT TO CLOSE -------------------------------------
          {
            cc = new CompanyCalendar();
            String hql = "select count(distinct issue.id) from " + Issue.class.getName() + " as issue";

            QueryHelper qhelp = new QueryHelper(hql);
            //  security
            IssueBricks.addSecurityClauses(qhelp, pageState);
            qhelp.addJoinAlias("join issue.task as task");
            qhelp.addJoinAlias("left outer join issue.assignedTo as resource");


            qhelp.addOQLClause("task.status='" + TaskStatus.STATUS_ACTIVE + "'");
            qhelp.addQueryClause("issue.status.behavesAsOpen=true");
            qhelp.addQueryClause("issue.shouldCloseBy between :now and :nextWeek");

            qhelp.addParameter("now", cc.setAndGetTimeToDayStart());
            cc.add(CompanyCalendar.DATE, 7);
            qhelp.addParameter("nextWeek", cc.setAndGetTimeToDayEnd());


            long count = (Long) qhelp.toHql().uniqueResult();
            headlineData.element("wphIssuesForthcoming", count);
          }

          //-------------------------- OPEN PROJECT -------------------------------------
          {
            cc = new CompanyCalendar();
            String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
            QueryHelper qhelp = new QueryHelper(hql);
            //  security
            TaskBricks.addSecurityReadClauses(qhelp, pageState);
            qhelp.addOQLClause("task.parent is null");
            qhelp.addOQLClause("task.status='" + TaskStatus.STATUS_ACTIVE + "'");

            PageSeed taskList = pageState.pageFromRoot("task/taskList.jsp");
            taskList.command = Commands.FIND;
            taskList.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
            taskList.addClientEntry("ROOT_OR_STANDALONE", Fields.TRUE);

            long count = (Long) qhelp.toHql().uniqueResult();
            headlineData.element("wphOpenProjects", count);
          }


          //-------------------------- PROJECT CREATED IN LAST 7 DAYS -------------------------------------
          {
            cc = new CompanyCalendar();
            String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
            QueryHelper qhelp = new QueryHelper(hql);
            //  security
            TaskBricks.addSecurityReadClauses(qhelp, pageState);
            qhelp.addOQLClause("task.parent is null");
            qhelp.addOQLClause("task.creationDate>:lastWeek");

            cc.add(CompanyCalendar.DATE, -7);
            qhelp.addParameter("lastWeek", cc.setAndGetTimeToDayEnd());


            PageSeed taskList = pageState.pageFromRoot("task/taskList.jsp");
            taskList.command = Commands.FIND;
            taskList.addClientEntry("CREATED_ON", ">-1w");
            taskList.addClientEntry("ROOT_OR_STANDALONE", Fields.TRUE);

            long count = (Long) qhelp.toHql().uniqueResult();
            headlineData.element("wphProjectCreated", count);
          }


          //-------------------------- PROJECT CLOSED IN LAST 7 DAYS -------------------------------------
          {
            cc = new CompanyCalendar();
            String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
            QueryHelper qhelp = new QueryHelper(hql);
            qhelp.addJoinAlias("join task.statusHistory as hist");

            //  security
            TaskBricks.addSecurityReadClauses(qhelp, pageState);
            qhelp.addOQLClause("task.parent is null");
            qhelp.addOQLClause("hist.creationDate>:lastWeek");
            qhelp.addOQLClause("task.status='" + TaskStatus.STATUS_DONE + "' or task.status='" + TaskStatus.STATUS_FAILED + "'");


            cc.add(CompanyCalendar.DATE, -7);
            qhelp.addParameter("lastWeek", cc.setAndGetTimeToDayStart());

            PageSeed taskList = pageState.pageFromRoot("task/taskList.jsp");
            taskList.command = Commands.FIND;
            taskList.addClientEntry("STATUS_CHANGE_DATE", ">-1w");
            taskList.addClientEntry("ROOT_OR_STANDALONE", Fields.TRUE);
            taskList.addClientEntry("STATUS", TaskStatus.STATUS_DONE + "," + TaskStatus.STATUS_FAILED);

            long count = (Long) qhelp.toHql().uniqueResult();
            headlineData.element("wphProjectClosed", count);
          }

          //-------------------------- ASSIGNMENT CREATED IN LAST 7 DAYS -------------------------------------
          {
            cc = new CompanyCalendar();
            String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
            QueryHelper qhelp = new QueryHelper(hql);

            //  security
            TaskBricks.addSecurityReadClauses(qhelp, pageState);
            //qhelp.addOQLClause("task.parent is null");
            qhelp.addOQLClause("assignment.creationDate>:lastWeek");

            cc.add(CompanyCalendar.DATE, -7);
            qhelp.addParameter("lastWeek", cc.setAndGetTimeToDayStart());

            PageSeed taskList = pageState.pageFromRoot("task/taskList.jsp");
            taskList.command = Commands.FIND;
            taskList.addClientEntry("ASSIG_CREATED_ON", ">-1w");
            //taskList.addClientEntry("ROOT_OR_STANDALONE", Fields.TRUE);
            //taskList.addClientEntry("STATUS", TaskStatus.STATUS_DONE+","+TaskStatus.STATUS_FAILED);

            long count = (Long) qhelp.toHql().uniqueResult();
            headlineData.element("wphAssigCreated", count);
          }


          //-------------------------- MY ASSIGNMENTS -------------------------------------
          {
            boolean showNotActiveTasks = pageState.getEntryOrDefault("ASSIG_SHOW_NOTACTIVETASKS", Fields.FALSE).checkFieldValue();
            boolean showNotActiveAssig = pageState.getEntryOrDefault("ASSIG_SHOW_NOTACTIVE", Fields.FALSE).checkFieldValue();
            int daysOnTheHorizon = pageState.getEntryOrDefault("ASSIG_SHOW_HORIZON", "0").intValue();

            Set<Assignment> assigs = new HashSet();
            assigs.addAll(loggedPerson.getExpiredAssignments(new Date(), !showNotActiveAssig, true));
            assigs.addAll(loggedPerson.getAssignmentsByPriority(new Period(System.currentTimeMillis(), System.currentTimeMillis() + 10), !showNotActiveAssig, !showNotActiveTasks));
            if (daysOnTheHorizon > 0) {
              cc = new CompanyCalendar();
              Date start = cc.setAndGetTimeToDayStart();
              cc.addWorkingDays(daysOnTheHorizon);
              assigs.addAll(loggedPerson.getAssignmentsByPriority(new Period(start, cc.setAndGetTimeToDayEnd()), !showNotActiveAssig, !showNotActiveTasks));
            }
            int count = assigs.size();
            headlineData.element("wphMyAssignments", count);
          }

          //-------------------------- MY ISSUES -------------------------------------
          {
            boolean showDep = pageState.getEntryOrDefault("SHOW_ALSO_DEP", "yes").checkFieldValue();
            List<Resource> myDeps = new ArrayList();
            if (showDep) {
              OqlQuery query = new OqlQuery("select c from " + Company.class.getName() + " as c where c.myManager=:me");
              query.getQuery().setEntity("me", loggedPerson);
              myDeps = query.list();
              myDeps.add(loggedPerson);
            }
            String hql = "select count(issue.id) from " + Issue.class.getName() + " as issue";
            QueryHelper qhelp = new QueryHelper(hql);
            qhelp.addOQLClause("issue.status.behavesAsOpen = true");
            if (showDep) {
              qhelp.addOQLInClause("issue.assignedTo", "depsandme", myDeps);
            } else {
              qhelp.addOQLClause("issue.assignedTo = :myself", "myself", loggedPerson);
            }
            qhelp.addOQLClause("issue.task != null");
            qhelp.addOQLClause("issue.task.status = :taskOpen", "taskOpen", TaskStatus.STATUS_ACTIVE);
            long count = (Long) qhelp.toHql().uniqueResult();
            headlineData.element("wphMyIssues", count);
          }

          //-------------------------- MY PLAN -------------------------------------
          {
            cc = new CompanyCalendar(new Date());
            Date start = cc.setAndGetTimeToDayStart();
            cc.add(CompanyCalendar.DAY_OF_MONTH, 1);
            Date end = cc.setAndGetTimeToDayEnd();
            String hql = "select count(wlp.id) from " + WorklogPlan.class.getName() + " as wlp where wlp.assig.resource = :res and wlp.inserted>=:pst and wlp.inserted<=:pen and wlp.duration>0";
            OqlQuery oql = new OqlQuery(hql);
            oql.getQuery().setEntity("res", loggedPerson);
            oql.getQuery().setTimestamp("pst", start);
            oql.getQuery().setTimestamp("pen", end);
            long count = (Long) oql.uniqueResult();
            headlineData.element("wphMyPlan", count);
          }

          //-------------------------- MY MY APPOINTMENTS -------------------------------------
          {
            // get candidate events
            TreeMap<Period, Event> result = Event.getPeriodsInPeriodFor(loggedPerson, Period.getDayPeriodInstance(new Date()), false);
            int count = result.size();
            headlineData.element("wphMyAppointments", count);
          }

          json.element("headlineData", headlineData);

        } else if ("STOPCOUNTER".equals(pageState.command)) {
          pageState.mainObjectId = pageState.getEntry("assId").intValueNoErrorCodeNoExc() + "";
          TimeCounterAction tcc = new TimeCounterAction(pageState);
          Worklog worklog = tcc.cmdStop(pageState);
          if (worklog != null) {
            json.element("worklog", worklog.jsonify());
            pageState.getEntryOrDefault("wpTCSAssignment", worklog.getAssig().getId() + "");
          }

        } else if ("STARTCOUNTER".equals(pageState.command)) {
          pageState.mainObjectId = pageState.getEntry("assId").intValueNoErrorCodeNoExc() + "";
          TimeCounterAction tcc = new TimeCounterAction(pageState);
          Worklog worklog = tcc.cmdStart(pageState);
          if (worklog != null) {
            json.element("worklog", worklog.jsonify());
          }

          Assignment a = Assignment.load(pageState.mainObjectId);
          if (a != null && a.getResource().equals(logged.getPerson())) {
            json.element("assignment", a.jsonify());
            pageState.getEntryOrDefault("wpTCSAssignment", a.getId() + "");
          }


        } else if ("GETASSIGNMENT".equals(pageState.command)) {
          pageState.mainObjectId = pageState.getEntry("assId").intValueNoErrorCodeNoExc() + "";
          Assignment a = Assignment.load(pageState.mainObjectId);
          if (a != null && a.getResource().equals(logged.getPerson())) {
            json.element("assignment", a.jsonify());
          }

        } else {
          ReflectionUtilities.invoke(ApplicationState.applicationParameters.get("get"), "doFilter", request, response);
        }
      }


    } catch (Throwable t) {
      jsonHelper.error(t);
    }

    return jsonHelper.json;
  }

  private JSONObject addRequiredJsonData(Task task, TeamworkOperator logged, JSONObject jProject) {

    jProject.element("cannotCloseTaskIfIssueOpen", I18n.isActive("CUSTOM_FEATURE_DO_NOT_CLOSE_TASK_IF_OPEN_ISSUES"));
    jProject.element("canWriteTask", task.hasPermissionFor(logged, TeamworkPermissions.task_canWrite));
    jProject.element("canDeleteTask", task.hasPermissionFor(logged, TeamworkPermissions.task_canDelete));
    jProject.element("canAddIssue", task.hasPermissionFor(logged, TeamworkPermissions.issue_canCreate));
    jProject.element("canDeleteIssue", task.hasPermissionFor(logged, TeamworkPermissions.issue_canDelete));
    jProject.element("canAddDocument", task.hasPermissionFor(logged, TeamworkPermissions.document_canCreate));
    jProject.element("canDeleteDocument", task.hasPermissionFor(logged, TeamworkPermissions.document_canDelete));
    jProject.element("canAssign", task.hasPermissionFor(logged, TeamworkPermissions.assignment_canCRW));
    jProject.element("canChangeTaskStatus", task.hasPermissionFor(logged, TeamworkPermissions.task_canChangeStatus));
    jProject.element("canAddTask", task.hasPermissionFor(logged, TeamworkPermissions.task_canCreate));

    jProject.element("path", task.getPath("/", false));

    return jProject;
  }


  private JSONObject addRequiredJsonDataToIssue(Issue issue, TeamworkOperator logged, JSONObject jso) {

    Resource resource = issue.getAssignedTo();
    if (resource != null) {
      Assignment ass = issue.getAssignmentOnTask(resource);
      if (ass != null) {
        jso.element("assId", ass.getIntId());
        jso.element("isMine", issue.getAssignedTo().equals(logged.getPerson()));
      }
    }
    //permessi
    jso.element("canWriteIssue", issue.hasPermissionFor(logged, TeamworkPermissions.issue_canWrite));
    jso.element("canCreateIssue", issue.hasPermissionFor(logged, TeamworkPermissions.issue_canCreate));
    jso.element("canDeleteIssue", issue.hasPermissionFor(logged, TeamworkPermissions.issue_canDelete));
    jso.element("askForWorklog", issue.getStatus().isAskForWorklog());


    //todo il change status non è contemplato. essendo un caso remoto aspettiamo a farlo
    //jso.element("canChangeStatus", issue.hasPermissionFor(logged, TeamworkPermissions.issue_canChangeStatus));


    return jso;
  }
}
