<%@ page import="org.jblooming.waf.constants.OperatorConstants, java.util.Properties, org.jblooming.utilities.HttpUtilities, java.io.File, java.io.FileInputStream, org.jblooming.waf.constants.Fields, com.twproject.task.TaskStatus, org.jblooming.security.License" %>

<div id="__FEEDBACKMESSAGEPLACE" style="display:none;"></div>
<div id="__COUNTER" style="display:none;" class="twTimeCounter" onclick="showTimeCounterEditor();"><span class="teamworkIcon">A</span></div>
<%--&copy;--%>

<%----------------------------------------------------------  PAGES DEFINITION  ---------------------------------------------------------%>


<%-----------------------------------  default page -------------------------------------- --%>

<div data-role="defaultPage" id="_defaultPage">

  <div data-role="header" data-position="fixed">
    <div data-role="button" onmouseup="manageMainMenuView();" class="mainMenuOpener" ></div>
    <div data-role="title"></div>
  </div>

  <div data-role="footer" data-position="fixed" class="groupRow hiddenElement">
    <div class="groupCell col6"></div>
    <div class="groupCell col6"></div>
  </div>

</div>

<%------------------------------------  home page -------------------------------------- --%>

<div data-role="page" id="home" class="" title="Twproject">

  <div data-role="content" class="scroll">

    <div class="homeListPlace">
      <%---  roba grave inizio --%>
      <div id="wphMilesOver" data-role="list_line" class="wpHLBox wpHLGrave wpHLBoxClosed" onclick="taskSearch('PF_MILESTONES_OVERDUE',$(this).find('h2').html());">
        <h2><%=I18n.get("MILESTONES_OVERDUE")%></h2>
        <span class="wphlNumber"></span>
        <p class="wpDesc"><%=I18n.get("MILESTONES_OVERDUE_DESCR")%></p>
      </div>
      <div id="wphProjectBudgetOverflow" data-role="list_line" class="wpHLBox wpHLGrave wpHLBoxClosed" onclick="taskSearch('PF_BUDGET_OVERFLOW',$(this).find('h2').html());">
        <h2><%=I18n.get("BUDGET_OVERFLOW")%></h2>
        <span class="wphlNumber"></span>
        <p class="wpDesc"><%=I18n.get("BUDGET_OVERFLOW_DESCR")%></p>
      </div>
      <div id="wphTasksOver" data-role="list_line" class="wpHLBox wpHLGrave wpHLBoxClosed" onclick="taskSearch('PF_OVERDUE_TASK',$(this).find('h2').html());">
        <h2><%=I18n.get("OVERDUE_TASK")%></h2>
        <span class="wphlNumber"></span>
        <p class="wpDesc"><%=I18n.get("OVERDUE_TASK_DESCR")%></p>
      </div>
      <div id="wphIssuesOver" data-role="list_line" class="wpHLBox wpHLGrave wpHLBoxClosed" onclick="issueSearch('PF_EXPIRED_ISSUES',$(this).find('h2').html());" >
        <h2><%=I18n.get("EXPIRED_ISSUES")%></h2>
        <span class="wphlNumber"></span>
        <p class="wpDesc"><%=I18n.get("EXPIRED_ISSUES_DESCR")%>
      </div>
      <%---  roba grave fine --%>

      <%------------  my roba inizio ----------%>
      <div id="wphMyAssignments" data-role="list_line" class="wpHLBox wpHLBoxClosed wpHLBoxMy"  onclick="taskSearch('PF_MY_OPEN_TASK',$(this).find('h2').html());">
        <h2><%=I18n.get("MY_ASSIGNMENTS")%></h2>
        <span class="wphlNumber"></span>
        <p class="wpDesc"><%=I18n.get("MY_ASSIGNMENTS_DESCR")%></p>
      </div>

      <div id="wphMyIssues" data-role="list_line" class="wpHLBox wpHLBoxClosed wpHLBoxMy"  onclick="issueSearch('PF_MY_OPEN_ISSUES',$(this).find('h2').html());" >
        <h2><%=I18n.get("MYISSUES")%></h2>
        <span class="wphlNumber"></span>
        <p class="wpDesc"><%=I18n.get("MYISSUES_DESCR")%></p>
      </div>

      <%--<div id="wphMyAppointments" data-role="list_line" class="wpHLBox wpHLBoxClosed wpHLBoxMy" >--%>
      <%--<h2><%=I18n.get("MY_APPOINTMENTS")%></h2>--%>
      <%--<span class="wphlNumber"></span>--%>
      <%--<p class="wpDesc"><%=I18n.get("MY_APPOINTMENTS_DESCR")%></p>--%>
      <%--</div>--%>

      <div id="wphMyPlan" data-role="list_line" class="wpHLBox wpHLBoxClosed wpHLBoxMy">
        <h2><%=I18n.get("PLANNED_WORK")%></h2>
        <span class="wphlNumber"></span>
        <p class="wpDesc"><%=I18n.get("PLANNED_WORK_DESCR")%></p>
      </div>
      <%------------  my roba fine ----------%>

      <%---  roba minore inizio --%>
      <div id="wphMilesForthcoming" data-role="list_line" class="wpHLBox wpHLBoxClosed" onclick="taskSearch('PF_FORTHCOMING_MILESTONES',$(this).find('h2').html());">
        <h2><%=I18n.get("FORTHCOMING_MILESTONES")%></h2>
        <span class="wphlNumber"></span>
        <p class="wpDesc"><%=I18n.get("FORTHCOMING_MILESTONES_DESCR")%></p>
      </div>
      <div id="wphTasksForthcoming" data-role="list_line" class="wpHLBox wpHLBoxClosed"  onclick="taskFreeSearch({END:'T:1w',STATUS:'<%=TaskStatus.STATUS_ACTIVE%>'},$(this).find('h2').html());">
        <h2><%=I18n.get("FORTHCOMING_ENDS")%></h2>
        <span class="wphlNumber"></span>
        <p class="wpDesc"><%=I18n.get("FORTHCOMING_ENDS_DESCR")%></p>
      </div>
      <div id="wphIssuesForthcoming" data-role="list_line" class="wpHLBox wpHLBoxClosed">
        <h2><%=I18n.get("FORTHCOMING_ISSUES")%></h2>
        <span class="wphlNumber"></span>
        <p class="wpDesc"><%=I18n.get("FORTHCOMING_ISSUES_DESCR")%></p>
      </div>
      <div id="wphOpenProjects" data-role="list_line" class="wpHLBox wpHLBoxClosed" onclick="taskSearch('PF_OPEN_PROJECT',$(this).find('h2').html());">
        <h2><%=I18n.get("OPEN_PROJECTS")%></h2>
        <span class="wphlNumber"></span>
        <p class="wpDesc"><%=I18n.get("OPEN_PROJECTS_DESCR")%></p>
      </div>
      <%if (logged.hasPermissionFor(TeamworkPermissions.task_canRead)) {%>
      <div id="wphProjectCreated" data-role="list_line" class="wpHLBox wpHLBoxClosed" onclick="taskFreeSearch({'CREATED_ON':'>-1w','ROOT_OR_STANDALONE':'yes'},$(this).find('h2').html());">
        <h2><%=I18n.get("PROJECTS_RECENTLY_CREATED")%></h2>
        <span class="wphlNumber"></span>
        <p class="wpDesc"><%=I18n.get("PROJECTS_RECENTLY_CREATED_DESCR")%></p>
      </div>
      <div id="wphProjectClosed" data-role="list_line" class="wpHLBox wpHLBoxClosed"onclick="taskFreeSearch({'STATUS_CHANGE_DATE':'>-1w','ROOT_OR_STANDALONE':'yes',STATUS:'<%=TaskStatus.STATUS_DONE%>,<%=TaskStatus.STATUS_FAILED%>'},$(this).find('h2').html());">
        <h2><%=I18n.get("PROJECTS_RECENTLY_CLOSED")%></h2>
        <span class="wphlNumber"></span>
        <p class="wpDesc"><%=I18n.get("PROJECTS_RECENTLY_CLOSED_DESCR")%></p>
      </div>
      <%--<div id="wphAssigCreated" data-role="list_line" class="wpHLBox wpHLBoxClosed"   onclick="taskFreeSearch({'ASSIG_CREATED_ON':'>-1w'},$(this).find('h2').html());">--%>
      <%--<h2><%=I18n.get("ASSIGMENT_RECENTLY_CREATED")%></h2>--%>
      <%--<span class="wphlNumber"></span>--%>
      <%--<p class="wpDesc"><%=I18n.get("ASSIGMENT_RECENTLY_CREATED_DESCR")%></p>--%>
      <%--</div>--%>
      <%}%>
      <%---  roba minore fine --%>
    </div>
  </div>

</div>

<%-------------------------------------  time counter editor  --------------------------------------%>

<div data-role="page" class="editor" id="timeCounterEditor" title="<%=I18n.get("TIMECOUNTER_MENU")%>">
  <div data-role="header" data-position="fixed">
    <div data-role="button" onmousedown="backPage();" class="close" ></div>
    <div data-role="title"></div>
  </div>
  <div data-role="content" class="scroll">
    <div id="timeCounterEditorPlace"></div>
  </div>
  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell inputBox col6">
      <button onmousedown="stopTimeCounter();" class="stop" ><%=I18n.get("STOP")%></button>
    </div>
  </div>
</div>
<%------------------------------------  info page -------------------------------------- --%>

<div data-role="page" id="info" class="editor info" title="Twproject">
  <div data-role="content">
    <div class="groupRow">
      <div class="groupCell  col12">
        <p align="center"><%=I18n.get("LOGGED_IN_AS")%></p>
        <p align="center"><img id="" class="face big" src=""></p>
        <h2 align="center"><%=logged.getDisplayName()%></h2>
      </div>
    </div>
    <p align="center">
      <b><%=I18n.get("APP_VERSION")%>
      </b>
      <%=ApplicationState.getApplicationVersion()%><br><%

      if (!Fields.TRUE.equals(ApplicationState.applicationParameters.get("TEAMWORK_ASP_INSTANCE"))) {
    %><i>license:</i><br>number of clients:<%=License.getLicense().licenses%>
      <br>expires:<%=JSP.w(License.getLicense().expires)%><%
      }
    %><br><br><br>
      <a href="javascript:void(0);" class="button" onclick="  createCookie('browseTwAsDefault',1);self.location.href= contextPath + '/'"><%=I18n.get("DESKTOP_BROWSER")%></a>
    </p>
  </div>

  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell  col4 left">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell col4">
      <button onmousedown="window.location.reload()" class="teamworkIcon big" title="<%=I18n.get("REFRESH")%>" >&#x2039;</button>
    </div>
    <div class="groupCell col4 right">
      <%--<div data-role="button" onclick="window.location.reload()"  class="teamworkIcon big" title="<%=I18n.get("REFRESH")%>">&#x203b;</div>--%>
      <button onmousedown="changeUser();" class="edit"  title="<%=I18n.get("LOGOUT")%>"><%=I18n.get("LOGOUT")%></button>
    </div>
  </div>
</div>

<%-- -----------------------------------  no connection page -------------------------------------- --%>

<div data-role="page" id="noConnectionPage">

  <div data-role="content">
    <img src="images/splash-mobile-hand.png" style="width:80%; margin: 50px auto; display: block ">

    <h2>No connection available</h2>

    <p>You must be online to use Twproject mobile.</p>
    <br>
    <button onclick="checkLogin();" class="full first">Retry</button>
  </div>
</div>

<%-- -----------------------------------  main menu -------------------------------------- --%>

<div data-role="mainMenu" class="mainMenuBox">

  <div data-role="user-box">
    <img id="userAvatar" class="face" src="" onmouseup="goToPage('info');">
    <div id="userDisplayName" onmouseup="goToPage('info');"></div>
    <button id="notificationCounterPlace" onmouseup="goToPage('notifications');"></button>
  </div>

  <div onmouseup="manageMainMenuView();" class="mainMenuCloser"></div>

  <div data-role=content class="scroll">

    <button data-role="home" onmouseup="backHome();">
      <span><%=I18n.get("DASHBOARD")%></span>
    </button>

    <button data-role="taskFilter,taskList,taskView" onmouseup="goToPage('taskFilter');">
      <span><%=I18n.get("TASKS")%></span>
    </button>

    <button data-role="issueFilter,issueList,issueView" onmouseup="goToPage('issueFilter');">
      <span><%=I18n.get("ISSUES")%></span>
    </button>

    <button data-role="resourceList,resourceView"  onmouseup="goToPage('resourceList');">
      <span><%=I18n.get("CONTACTS")%></span>
    </button>

    <button data-role="calendarPage,calendarDayList" onmouseup="goToPage('calendarPage');">
      <span><%=I18n.get("AGENDA")%></span>
    </button>

    <button data-role="worklogList" onmouseup="goToPage('worklogList')">
      <span><%=I18n.get("WORKLOGS")%></span>
    </button>

    <button data-role="documentFilter,explorer,fileStorages" onmouseup="goToPage('documentFilter');">
      <span><%=I18n.get("DOCUMENTS")%></span>
    </button>

    <button data-role="expenseList" onmouseup="goToPage('expenseList');">
      <span><%=I18n.get("ASSIG_EXPENSES")%></span>
    </button>

    <%--<button  data-role="timeCounters"  onmouseup="goToPage('timeCounters');">
            <span><%=I18n.get("TIMECOUNTER_MENU")%></span>
          </button>--%>

    <button data-role="stickyList" onmouseup="goToPage('stickyList');">
      <span><%=I18n.get("MESSAGES")%> (<span id="stickyCount"></span>)</span>
    </button>

    <%--<button data-role="info" onmouseup="goToPage('info');">
      <span class="icon">g</span>
      <span><%=I18n.get("SETTINGS")%></span>
    </button>--%>

  </div>
</div>

<%-- loader --%>
<div id="loader"></div>


<%----------------------------------------------------------  TEMPLATES  ---------------------------------------------------------%>

<div class="_mobileTemplates">
  <%-- ---------------------------------  NO_ELEMENT FOUND ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("NO_ELEMENT_FOUND")%>
  <div class="noElementFound">
    <%=I18n.get("NO_ELEMENTS_FOUND")%>
  </div>
  <%=JST.end()%>

  <%-- ---------------------------------  ERROR TEMPLATE  ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("ERROR_FEEDBACK")%>
  <div class="FFC_(#=type#) FFC_Global" type="(#=type#)">
    <table cellpadding="0" cellspacing="0" align="center" style="width:100%;">
      <tr>
        <td valign="middle" style="padding: 20px" class="_errorTemplateMessage">(#if(obj.title){#)
          <b>(#=title#)</b><br>
          (#}#)(#=message#)<br>
          <span class="teamworkIcon FMClose" style="cursor:pointer" onclick="hideFeedbackMessages();">x</span>
        </td>
      </tr>
    </table>
  </div>
  <%=JST.end()%>

  <%-- ---------------------------------  PERCENTILE TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("PERCENTILE")%>
  <%-- <div class="percentile" style="width:(#=width#)px;">
     <div class="bar" style="width:(#=width*(perc<0?0:(perc>100?100:perc))/100#)px;background-color:(#=(perc>100?'rgb(201, 3, 0)':'rgb(255, 233, 0)')#)"></div>
     <div class="overlay" style="width:(#=width#)px;">(#=perc.toFixed(0)#)%</div>
   </div>--%>
  <div class="progressNumber">(#=perc.toFixed(0)#)%</div>
  <%=JST.end()%>


  <%-- ---------------------------------  STYCK_COUNT template ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("STICKY_COUNT")%>
  <span class="msgCount" onclick="goToPage('notifications');"> (#=count#)</span>
  <%=JST.end()%>

</div>

<%----------------------------------------------------------  DECORATORS  ---------------------------------------------------------%>
<script></script>


<%----------------------------------------------------------  JAVASCRIPT  ---------------------------------------------------------%>

<script type="application/javascript">

var teamworkUrl = "..";
var controller = "ajax/mobileAjaxController.jsp";

var CHECK_LOGIN_EVERY=10; //in secondi dall'ultima chiamata ajax al server

var currentFilter = new Object();
var allMyTags;
var applicationCache;
var userLogged;

var currentDate=new Date();
var lastServerCallMillis=new Date().getTime();

var mobileFullDateFormat="EEEE " + Date.defaultFormat;

function startup(oldHash) {
  //console.debug("startup",location.href, location.hash);

  if($("body.level5,body.level10").size()>0){
    var msg=$("<div>").addClass("upgradeMsg").css({position:"fixed",display:"none","z-index":10000}).html("<div><p><%=I18n.get("PURCHASE_MESSAGE_MOBILE")%></p></div>")
    $("body").append(msg);
    setTimeout(function(){
      msg.fadeIn(500);;
    },1200);



  }

  //bind ajax events
  $(document).ajaxSend(function( event, jqxhr, settings ) {
    //console.debug("ajaxSend");
    lastServerCallMillis = new Date().getTime();
  });

  setupLocalVariables();
  checkLogin(function () {
    checkStickyNotes();
    checkNotifications();

    //bind keepAlive
    $("body").everyTime(10*1000,"keepAlive",keepAlive);

    //oldHash=oldHash||"#home";
    //location.hash=oldHash;
    location.hash="#home"
  });
}

if (!window.navigator.standalone && !(/Android/i).test(navigator.userAgent)) {
  $("#bookmarkme").show();
}

function setupLocalVariables() {
  applicationCache = new Object();
  applicationCache.user = new Object();
  applicationCache.tasks = new Object();
  applicationCache.assignments = new Object();
  applicationCache.issues = new Object();
  applicationCache.resources = new Object(); //e.g. {id1:{res1},...,idn:{resn}}
  applicationCache.documents = new Object();
  applicationCache.events = new Object();//e.g. {yyyymm:[{event1},...,{eventn}], ...}
  applicationCache.eventsDays = [];  //e.g. [yyyymmdd,20110111,20101215]
  applicationCache.eventsHolidays = undefined;//new Object(); //e.g. {week:[5,6], holidays:[yyyymmdd,mmdd]}
  applicationCache.worklogs = new Object();//new Object(); //e.g. {yyyymm:{yyyymmdd:[{wl1},...,{wln}],...}, ... }
  applicationCache.expenses = new Object();//new Object(); //e.g. {yyyymm:{yyyymmdd:[{cost1},...,{costn}],...}, ... }
  applicationCache.stickies=[];
  applicationCache.countedAssignment = undefined;
}

function updateApplicationCacheElement(elements, newElement) {
  var oldElement = elements[newElement.id];

  if (!oldElement || newElement.lastModified > oldElement.lastModified || newElement.loadComplete == true)
    elements[newElement.id] = newElement;
}


function updateIssueListinTaskCache(newElement){
  var task = getTaskById(newElement.taskId)
  if(task && task.issues){
    var oldElement;

    for (var i = 0; i < task.issues.length; i++)  {
      if (task.issues[i].id == newElement.id){
        oldElement =  task.issues[i];
        break;
      }
    }


    if(!oldElement){
      task.issues.push(newElement);
    }else if(newElement.lastModified > oldElement.lastModified || newElement.loadComplete == true){
      task.issues[i] = newElement;
    }

    if((oldElement && oldElement.statusName != "open" && newElement.statusName == "open")|| !oldElement && newElement.statusName== "open")
      task.openIssues ++;
    else if(oldElement && oldElement.statusName == "open" && newElement.statusName != "open")
      task.openIssues --

    updateApplicationCacheElement(applicationCache.tasks, task);
  }
}


function updateDocumentListinTaskCache(newElement){
  var task = getTaskById(newElement.taskId)
  if(task && task.documents){
    var oldElement;

    for (var i = 0; i < task.documents.length; i++)  {
      if (task.documents[i].id == newElement.id){
        oldElement =  task.documents[i];
        break;
      }
    }
    if(!oldElement){
      task.documents.push(newElement);
    }else if(newElement.lastModified > oldElement.lastModified){
      task.documents[i] = newElement;
    }

    updateApplicationCacheElement(applicationCache.tasks, task);
  }
}

function updateDocumentListinIssueCache(newElement){
  var issue = getIssueById(newElement.issueId)
  if(issue && issue.documents){
    var oldElement;

    for (var i = 0; i < issue.documents.length; i++)  {
      if (issue.documents[i].id == newElement.id){
        oldElement =  issue.documents[i];
        break;
      }
    }
    if(!oldElement){
      issue.documents.push(newElement);
    }else if(newElement.lastModified > oldElement.lastModified){
      issue.documents[i] = newElement;
    }

    updateApplicationCacheElement(applicationCache.issues, issue);
  }
}

function updateSubTaskListinTaskCache(newElement){
  var task = getTaskById(newElement.parentId)
  if(task && task.children){
    var oldElement;
    for (var i = 0; i < task.children.length; i++)  {
      if (task.children[i].id == newElement.id){
        oldElement =  task.children[i];
        break;
      }
    }
    if(!oldElement){
      task.children.push(newElement);
    }else if(newElement.lastModified > oldElement.lastModified || newElement.loadComplete == true){
      task.children[i] = newElement;
    }

    updateApplicationCacheElement(applicationCache.tasks, task);
  }
}

function updateListWithCache(templateName, divToReload, list){
  if (list && list.length > 0) {
    for (var i in list) {
      var obj = list[i];
      divId.append($.JST.createFromTemplate(obj, template, true));
    }
  }
}


function resetEnvironment() {
  setupLocalVariables();
}

/* -------------------------------------------------------------------------------------  GENERAL FUNCTIONS -------------------------------------------------*/

function callController(filter, callback) {
  showSavingMessage();
  $.getJSON(controller, filter, function(response) {
    jsonResponseHandling(response);
    if (response.ok) {
      if (typeof(callback) == "function") {
        callback(response);

        if (currentPage)
          refreshIscroll();

      }
    }
    hideSavingMessage();
  });
}

function backHome() {
  goToPage("home");
}

function checkLogin(callBackAfterLogin) {
  //console.debug("checkLogin",callBackAfterLogin);
  showSavingMessage();
  //rise a timer in order to test connection if no connection go to appiccia the net page
  $("body").stopTime("testOnLine");
  $("body").oneTime(4000, "testOnLine", function() {
    goToPage("noConnectionPage");
    hideSavingMessage();
  });

  //provo a fare un login con i cookies
  $.getJSON(teamworkUrl + "/applications/teamwork/security/loginClient.jsp", {CM:"LI"}, function(response) {
    $("body").stopTime("testOnLine");
    jsonResponseHandling(response);
    hideSavingMessage();
    if (response.ok) {
      if (response.loginOk) {
        applicationCache.user = response.user;
        userLogged = true;
        //console.debug("sessionRefresh");
        $.get("sessionRefresh.jsp");

        if (typeof(callBackAfterLogin) == "function")
          callBackAfterLogin();

      } else {
        location.href=teamworkUrl+ "/mobile/mobile.jsp";
      }
    } else {
      location.href=teamworkUrl+ "/mobile/mobile.jsp";
    }
  });
}

function changeUser() {
  showSavingMessage();
  // clear client
  resetEnvironment();

  //location.href=teamworkUrl+"/applications/teamwork/security/login.jsp?CM=LO";
  //perform a logout
  $.getJSON(teamworkUrl + '/applications/teamwork/security/loginClient.jsp', {CM:"LO"}, function(response) {
    userLogged = false;
    hideSavingMessage();
    location.href=teamworkUrl+"/mobile";
  });
}

function keepAlive(){
  //console.debug("keepAlive:"+ parseInt((new Date().getTime()-lastServerCallMillis)/1000)) ;
  if (new Date().getTime()-lastServerCallMillis> CHECK_LOGIN_EVERY *1000){ //ogni x secondi controlla se siamo ancora loggati
    $.getJSON(teamworkUrl + "/applications/teamwork/security/loginClient.jsp", {CM:"TST"}, function(response) {
      var ok=false;
      if (response.ok) {
        if (response.loginOk) {
          ok=true;
          if (response.newSession) {
            //console.debug("sessionRefresh");
            $.get("sessionRefresh.jsp");
          }
        }
      }

      //se qualcosa è andato storto si fa un login e si va alla pagina iniziale
      if (!ok){
        $.getJSON(teamworkUrl + '/applications/teamwork/security/loginClient.jsp', {CM:"LO"}, function(response) {
          location.href = "mobile.jsp"
        });

      }

    });
  }
  lastServerCallMillis=new Date().getTime();
}

function showFeedbackMessage(typeOrObject, message, title, autoCloseTime) {
  //console.debug("showFeedbackMessage",typeOrObject, message, title);

  if(!autoCloseTime)
    autoCloseTime = 2000;

  var place = $("#__FEEDBACKMESSAGEPLACE");
  var mess;
  if (typeof(typeOrObject)=="object" )
    mess=typeOrObject;
  else
    mess = {type:typeOrObject, message:message,title:title};
  //if exists append error message
  var etm = $(".FFC_"+mess.type+":visible ._errorTemplateMessage");
  if(etm.length>0){
    etm.append("<hr>"+(mess.title?"<b>"+mess.title+"</b><br>":"")+mess.message+"<br>");
  } else {
    etm = $.JST.createFromTemplate(mess, "ERROR_FEEDBACK");
    place.append(etm);
    place.addClass("msg-on");
    place.fadeIn();
  }

  if(autoCloseTime >0)
    setTimeout(function(){
      $("#__FEEDBACKMESSAGEPLACE").fadeOut().empty();
      place.removeClass("msg-on");
    },autoCloseTime);

  $("body").oneTime(1200, function () {
    $(".FFC_OK").fadeOut();
  });
}

function hideFeedbackMessages() {
  var place = $("#__FEEDBACKMESSAGEPLACE");
  $("#__FEEDBACKMESSAGEPLACE").fadeOut().empty();
  place.removeClass("msg-on");

}

<%---------------------------------------------------------------------------  HOME PAGE ----------------------------------------------------------------------%>
function homeEnter(event, data, fromPage, isBack){
//  console.debug("homePageEnter",event, data, fromPage, isBack);
  //var page = $(this);

  //si resetta la data corrente in modo da rientrare sempre ad oggi nelle altre pagine
  currentDate=new Date();

  //si carica headline
  var request  = {"CM": "GETHEADLINE"};

  callController(request,function(response){
    for (var headName in response.headlineData) {
      var count = parseInt(response.headlineData[headName]);
      var box = $("#" + headName);
      if (box.size() > 0) {
        if (count > 0) {
          box.find(".wphlNumber").html(count);
          box.removeClass("wpHLBoxClosed");
        } else {
          box.addClass("wpHLBoxClosed");
        }
      }
    }

    $(".mainMenuBox .face, .info .face.big").attr("src", applicationCache.user.person.avatarUrl);
    $("#userDisplayName").html(applicationCache.user.person.displayName);

    var request  = {"CM": "CHECKTIMECOUNTER"};

    callController(request,function(response){
      if(response.assigId){
        showTimeCounterIcon();
        applicationCache.countedAssignment = response.assigId;
      }
    });


  });

}


</script>
