<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.waf.TeamworkHBFScreen, org.jblooming.waf.ScreenArea, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Tab, org.jblooming.waf.html.container.TabSet, org.jblooming.waf.html.input.TextArea, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    logged.testIsAdministrator();

    final ScreenArea body = new ScreenArea(request);
    //body.areaHtmlClass="lreq30 lreqPage";
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

  } else {
    %><%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
%>
<%adminLink.toHtmlInTextOnlyModality(pageContext);%><h1>Custom fields</h1><%

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

  tab =new Tab("ASSIGNMENT", "Assignment");
  tab.additionalCssClass="lreq30 lreqLabel";
  tabset.addTab(tab);

  tab = new Tab("WORKLOG", "Worklog");
  tab.additionalCssClass="lreq30 lreqLabel";
  tabset.addTab(tab);

  tab = new Tab("COST", "Expenses");
  tab.additionalCssClass="lreq30 lreqLabel";
  tabset.addTab(tab);

  tab =new Tab("COST_ADD", "Project costs");
  tab.additionalCssClass="lreq30 lreqLabel";
  tabset.addTab(tab);

  tab =new Tab("BUDGET", "Project budget");
  tab.additionalCssClass="lreq30 lreqLabel";
  tabset.addTab(tab);

  tabset.tabs.get(0).focused=true;

  tabset.drawBar(pageContext);


  //----------------------------------------------- HELP TAB -------------------------------------------------------------------
  help.start(pageContext);

%>
<br>
<h2>How to define a custom field</h2>
<h3>Simple cases</h3>
<ul>
  <li>Unused field: <code>no</code> -> the field is inactive</li>
  <li>Simplest case: <code>my code</code> -> a string</li>
  <li>Lenght defined case: <code>my code,20</code> -> a string 20 chars length</li>
  <li>Double case: <code>cost,15,java.lang.Double</code> (also Integer or Long)</li>
  <li>Date case: <code>when,10,java.util.Date</code></li>
  <li>Boolean case: <code>approved,1,java.lang.Boolean</code> (a checkbox will be used for data entry)</li>
  <li>Twproject object case: <code>customer referral,25,com.twproject.resource.Person</code> -> a Person</li>
  <li><div style="display:inline-block;width:145px;">&nbsp;</div> <code>see this task,25,com.twproject.task.Task</code> -> a Task</li>
  <li><div style="display:inline-block;width:145px;">&nbsp;</div> <code>attachment,15,org.jblooming.ontology.PersistentFile</code> -> an uploaded file</li>
  <%ButtonLink.getBlackInstance("see Twproject's object reference",pageState.pageFromRoot("administration/classInspection.jsp")).toHtmlInTextOnlyModality(pageContext);%>
</ul>

<h3>Fixed values</h3>
<ul>
  <li>A fixed code-value list displayed as radios:  <code>genre,6,{values:{"m":"male","f":"female"}}</code></li>
  <li>A fixed code-value list displayed as combo:  <code>genre,6,{values:{"m":"male","f":"female"} ,displayAsCombo:true}</code></li>
</ul>

<h3>Database lookup</h3>
Using DB table (both internal to Twproject or external)<br>
<code>lookup on field,25,{ query:{ ... },<i>connection:{...}</i> }</code>
<ul>
  <li>simple case:  <code>query:{ tableName:"olpl_operator", idColumnName:"id", descriptionColumnName:"loginname" }</code></li>
  <li>complete case (more flexible)
    <code><pre>query: {
  select:"select id,name,surname from olpl_operator",
  whereForFiltering:"where name like ? order by name,surname",
  whereForId:"where id=?"
  }
</pre>
    </code>
  </li>
  <li>specify an external connection (if not specified Twproject db will be used):
    <code><pre>
connection:{
  driver:"com.mysql.jdbc.Driver",
  url:"jdbc:mysql://[YOUR SERVER]/[YOUR DB]",
  user:"[YOUR USER]",
  password:"[YOUR PASSWORD]"
}
</pre>
    </code>
  </li>
</ul>
<h3>Required fields</h3>
  just add ",required" at the end of the field definition
<ul>
  <li>A generic text field: <code>my code<b>,required</b></code></li>
  <li>Date required case: <code>when,10,java.util.Date<b>,required</b></code></li>
  <li>A lookup: <code>lookup on field,25,{ query:{ ... },<i>connection:{...}</i> }<b>,required</b></code></li>
</ul>

<h3>Hierarchic object</h3>
For tree structured objects (e.g.: Task or Resource) is possible to enable fields for roots or children
<ul>
  <li>Add a field on root level only: <code>my code,20,...<b>,rootOnly</b></code></li>
  <li>Add a field on child level only: <code>my code,20,...<b>,childOnly</b></code></li>
</ul>

<h3>Conditional fields</h3>
will be visible if a condition is satisfied <code>{<b>visibleIf</b>:<i>[boolean condition]</i>}</code>
<ul>
  <li>Visible if a Task has a progress >70% <code>test,20,{visibleIf:'obj.progress>70'}</code></li>
  <li>Visible if a Resource has a code staring with 'A' <code>test,20,{visibleIf:'obj.code.startsWith("A")'}</code></li>
  <li>Visible if a Issue is in a defined status <code>test,20,{visibleIf:'obj.status.id==123'}</code></li>
</ul>

<h3>Field labels translation</h3>
If you want to internationalize the field, just create a label with the name of the field. <br>
<b>E.G.</b> if you need a "street" field, insert on the custom field "street,25" and the go to <br>
admin->labels and create a new label called "street" and insert "street" for English, "rue" for French, "via" for Italian and so on.

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
    <th class="tableHead">Field</th>
    <th class="tableHead">Definition</th>
  </thead>
<%
    for (int j=1;j<=numOfFields;j++){
      String fieldName=tab.id+"_CUSTOM_FIELD_"+j;

      pageState.addClientEntry(fieldName,I18n.getLabel(fieldName, "Teamwork", "EN"));
      TextArea ta= new TextArea(fieldName,"",30,3,"cfta");
      ta.label="";
      ta.preserveOldValue=true;
      ta.setAutosize(50,500,30);

%>
  <tr class="alternate">
    <td>Field <%=j%></td>
    <td><%ta.toHtml(pageContext);%></td>
  </tr>
  <%
    }

  %></table><%


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