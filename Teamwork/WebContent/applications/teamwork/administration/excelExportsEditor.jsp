<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.waf.TeamworkHBFScreen, org.jblooming.waf.ScreenArea, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Tab, org.jblooming.waf.html.container.TabSet, org.jblooming.waf.html.input.TextArea, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    logged.testIsAdministrator();

    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

  } else {
    %><%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
%>
<%adminLink.toHtmlInTextOnlyModality(pageContext);%><h1>Excel exports definition</h1><%

  Form form= new Form(pageState.thisPage(request));
  form.alertOnChange=true;
  form.start(pageContext);

  TabSet tabset= new TabSet("CFTS",pageState);

  Tab help = new Tab("HELP", "Help & Examples");
  tabset.addTab(help);
  Tab tab = new Tab("TASK", "Task/project");
  tab.additionalCssClass="lreq30 lreqLabel";
  tabset.addTab(tab);
  tab = new Tab("ISSUE", "Issue/todo");
  tab.additionalCssClass="lreq30 lreqLabel";
  tabset.addTab(tab);
  tab = new Tab("RESOURCE", "Resource");
  tab.additionalCssClass="lreq30 lreqLabel";
  tabset.addTab(tab);
  tab = new Tab("WORKLOG", "Worklog");
  tab.additionalCssClass="lreq30 lreqLabel";
  tabset.addTab(tab);
  tabset.tabs.get(0).focused=true;

  tabset.drawBar(pageContext);


  //----------------------------------------------- HELP TAB -------------------------------------------------------------------
  help.start(pageContext);

%>
<br>
<h2>How to define Excel exports</h2>

Excel export is defined as json object in the form of <br>
<code>{"<i>label1</i>":"obj.<i>property1</i>","<i>label2</i>":"obj.<i>method1()</i>",...,"<i>labeln</i>":"obj.<i>propertyn</i>"}</code><br>
every Twproject object is slightly different, and you need to take care to the methods called. <br><br>
Be <b>VERY CAREFUL</b> on what your are writing; runtime errors can be thrown, both for invalid JSON format or for invalid method calls.
<br>
Use <%ButtonLink.getBlackInstance("Twproject's object reference",pageState.pageFromRoot("administration/classInspection.jsp")).toHtmlInTextOnlyModality(pageContext);%>
to see methods and properties that you can use on definitions
<br><br>

<h3>Some examples</h3>
<ul>
  <li><b>Issue</b>:<code>{"id":"obj.id","code":"obj.name"}</code> </li> <br>
  <li><b>Resource</b>:<code>{"id":"obj.id","code":"obj.code","name":"obj.getDisplayName()"}</code> </li> <br>
  <li><b>Task</b>:<code>{"id":"obj.id","code":"obj.code","start":"obj.getSchedule().getStartDate()","status":"obj.status"}</code> </li><br>
  <li><b>Worklog</b>:<code>{"id":"obj.id","task name":"obj.assig.task.name","resource name":"obj.assig.resource.name","worklog in millis":"obj.getDuration()","worklog in hours":"DateUtilities.getMillisInHoursMinutes(new Long(obj.getDuration()))"}</code> </li>
</ul>
<%
  help.end(pageContext);

  //-------------------------------------------------------------------------------- CUSTOM FIELDS TABS --------------------------------------------------------------------------------

  for (int i=1; i<tabset.tabs.size();i++) {
    tab=tabset.tabs.get(i);
    tab.start(pageContext);
    int numOfFields= "TASK ISSUE RESOURCE ASSIGNMENT".contains(tab.id)?6:4;

    %>
<table class="table dataTable">
  <thead>
    <th class="tableHead">Definition</th>
  </thead>
<%
      String fieldName="CUSTOM_EXPORT_EXCEL_"+tab.id;

      pageState.addClientEntry(fieldName,I18n.getLabel(fieldName, "Teamwork", "EN"));
      TextArea ta= new TextArea(fieldName,"",30,10,"cfta");
      ta.label="";
      ta.preserveOldValue=true;
      ta.setAutosize(300,800,60);

%>
  <tr class="alternate">
    <td><%ta.toHtml(pageContext);%></td>
  </tr>
</table><%


    ButtonBar bb = new ButtonBar();
    ButtonJS bjs= new ButtonJS("Save","saveCustomFields();");
    bjs.additionalCssClass="first big";
    bb.addButton(bjs);
    bb.toHtml(pageContext);


    tab.end(pageContext);
  }


  tabset.end(pageContext);

%>
<style type="text/css">
  .cfta{
    width: 100%;
  }
</style>
<script>
  function saveCustomFields(){
    //console.debug("saveCustomFields");
    var request={CM:"SVCST"};
    var cfs={};
    $("#TABSETPART_CFTS textarea").each(function(){
      var ta=$(this);
      cfs[ta.attr("name")]=ta.val();
    });
    request.cfs=JSON.stringify(cfs);

    showSavingMessage();
    $.getJSON("adminAjaxController.jsp",request,function(response){
      jsonResponseHandling(response);
      hideSavingMessage();
      if (response.ok==true){
        $("#TABSETPART_CFTS textarea").updateOldValue();
        location.reload();
      }
    })

  }
</script>
<%
    form.end(pageContext);

  }
%>