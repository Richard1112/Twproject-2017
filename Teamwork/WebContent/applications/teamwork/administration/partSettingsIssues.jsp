<%@ page import="com.twproject.security.TeamworkPermissions, com.twproject.task.TaskBricks, org.jblooming.system.SystemConstants, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!Commands.SAVE.equals(pageState.command)) {
    pageState.addClientEntry("USETYPEONISSUES", ApplicationState.getApplicationSetting("USETYPEONISSUES"));
    pageState.addClientEntry("USECODEONISSUES", ApplicationState.getApplicationSetting("USECODEONISSUES"));
    pageState.addClientEntry("USEIMPACTONISSUES", ApplicationState.getApplicationSetting("USEIMPACTONISSUES"));
    pageState.addClientEntry("EMAIL_TASK_ASSOCIATIONS", ApplicationState.getApplicationSetting("EMAIL_TASK_ASSOCIATIONS"));
  }
%>
<h2>Issues configuration</h2>
<table cellpadding="3" id="help" class="table edged">
  <tr>
    <th class="tableHead large">Parameter</th>
    <th width="1%" class="tableHead" nowrap>Current value</th>
    <th width="1%" class="tableHead" nowrap>Sample value</th>
    <th class="tableHead">Description</th>
  </tr>

  <tr class="issueDefaults table">
    <td class="lreq30"><%
      CheckField utoi = new CheckField("USETYPEONISSUES", "</td><td align=\"center\" class=\"lreq30\">", true);
      utoi.toHtmlI18n(pageContext);%>
    </td>
    <td class="lreq30" align="center">no</td>
    <td class="lreq30 lreqLabel">When true the "type" field will be available on issues.</td>
  </tr>

  <tr class="issueDefaults ">
    <td class="lreq30"><%
      CheckField ucoi = new CheckField("USECODEONISSUES", "</td><td align=\"center\" class=\"lreq30\">", true);
      ucoi.toHtmlI18n(pageContext);%>
    </td>
    <td class="lreq30" align="center">no</td>
    <td class="lreq30 lreqLabel">When true the "external code" field will be available on issues.<br>Mainly used to refer external issues tracking systems.</td>
  </tr>
  <tr class="issueDefaults">
    <td class="lreq30"><%
      CheckField uioi = new CheckField("USEIMPACTONISSUES", "</td><td align=\"center\" class=\"lreq30\">", true);
      uioi.toHtmlI18n(pageContext);%>
    </td>
    <td class="lreq30" align="center">no</td>
    <td class="lreq30 lreqLabel">When true the "impact" field will be available on issues lists and fast editor.</td>
  </tr>
</table>

<%
  String twEmail = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_FROM);


  //horrible trick to setup right values in combo template. Works thanks to changes on smartCombo to preserve filled values
  pageState.addClientEntry("emailTask", "##emailTask##");
  pageState.addClientEntry("emailTask_txt", "##emailTask_txt##");

  //initialize smartcombo
  SmartCombo taskCombo = TaskBricks.getTaskCombo("emailTask", false, TeamworkPermissions.task_canRead, pageState);
  taskCombo.label = "";
  taskCombo.script = "style=\"width:95%\" ";

  new TextField("hidden","","EMAIL_TASK_ASSOCIATIONS","",10,false,false,1,"").toHtml(pageContext);

%>
<table width="100%" class="mailboxes table lreq30 lreqLabel">
  <tr>
    <th colspan="99" align="left"><h2>You can send issues to a Teamwork' task by sending email to a reserved address.<br>
      Activate here the service by associating an email to each task. You cannot use the same email address for multiple tasks.<br></h2><br>
      Remember that you can always add issues to a task by sending an e-mail to: <%=JSP.ex(twEmail) ? twEmail : "<b>not cofigured</b>"%> with subject "TASK #<i>taskcode/id</i># ISSUE"

      Some hints:
      <ul>
        <li>"port number", leave -1 for default values: e.g: pop3=110, pop3s=995, imap=143</li>
        <li>"public" means that everyone can send issue (via e-mail) to the task, regardless the sender is a teamwork user.<br> Otherwise only users with issue write-permission on that task will be able to add issues.</li>
        <li>"active" means that issues will be imported (alias: mail will be downloaded) only while the task is open, and we are in the time scope.</li>
      </ul>
    </th>
  </tr>
</table>
<table width="100%" id="emailTaskAssoc" class="mailboxes table lreq30">
  <tr>
    <th width="20%" class="tableHead">email server</th>
    <th width="20%" class="tableHead">user</th>
    <th width="15%" class="tableHead">password</th>
    <th width="40" class="tableHead">port</th>
    <th width="60" class="tableHead">protocol</th>
    <th width="80%" colspan="2" class="tableHead">task</th>
    <th width="15" class="tableHead">public</th>
    <th width="15" class="tableHead" nowrap>when active</th>
    <th width="1%" align="center" class="tableHead"><span class="teamworkIcon edit" onclick="createRow({port:-1});" title="add association" style="cursor: pointer">P</span></th>
  </tr>
</table>


<div id="template">
  <%=JST.start("emailTask")%>
  <tr class="emailTaskRow">
    <td align="center">
      <input type="text" value="##host##" name="host" style="width:95%" class="host formElements">
    </td>
    <td align="center">
      <input type="text" value="##user##" name="user" style="width:95%" class="user formElements">
    </td>
    <td align="center">
      <input type="password" value="##password##" autocomplete="off" name="password" style="width:95%" class="password formElements">
    </td>
    <td align="center">
      <input type="text" value="##port##" name="port" style="width:40px;" class="port formElements validated integer">
    </td>
    <td><select style="width:60px;" name="protocol" class="formElements">
      <option value="pop3">pop3</option>
      <option value="pop3s">pop3s</option>
      <option value="imap">imap</option>
      <option value="imap">imaps</option>
    </select></td>
    <td>
      <%
        taskCombo.toHtml(pageContext);
      %>
    </td>
    <td><input type="checkbox" name="public" ></td>
    <td><input type="checkbox" name="active"></td>
    <td align="center"><span onclick="$(this).parents('tr:first').fadeOut(500,function(){$(this).remove();generateEmaiTaskInput()});"><span class="teamworkIcon" style="cursor: pointer">d</span></span></td>

  </tr>
  <%=JST.end()%>
</div>


<script type="text/javascript">
  $().ready(function () {
    $("#template").loadTemplates().remove();

     $.JST.loadDecorator("emailTask", function (strip, jsonData) {
      if(jsonData.public=="yes")
        strip.find("[name=public]").prop("checked",true);

      if(jsonData.active=="yes")
        strip.find("[name=active]").prop("checked",true);

       if(jsonData.protocol)
        strip.find("option[value="+jsonData.protocol+"]").attr("selected",true); 
    });

    var burpString=$("#EMAIL_TASK_ASSOCIATIONS").val();
    if (burpString && burpString!=""){
      var burp=JSON.parse(burpString);
      for (var i=0;i<burp.length;i++){
         createRow(burp[i]);
      }
    }
  });

  function createRow(data) {
    var row = $.JST.createFromTemplate(data, "emailTask");
    row.find(":input").blur(generateEmaiTaskInput);
    row.find(":checkbox").click(generateEmaiTaskInput);
    $("#emailTaskAssoc").append(row);
  }


  function generateEmaiTaskInput() {
    var burp=[];
    $("tr.emailTaskRow").each(function(){
      burp.push(getDataFromEditor($(this)));
    });
    $("#EMAIL_TASK_ASSOCIATIONS").val(JSON.stringify(burp));

  }


function  getDataFromEditor(editor){
  var data = {};

  // then copy all inputs
  editor.find(":input[name]").each(function() {
    var el = $(this);
    if (el.is(":checkbox")){
      data[el.prop("name")]=el.get(0).checked?"yes":"no";
    }else if (el.is("radio")){
      if (el.get(0).checked)
        data[el.prop("name")]=el.val();
    } else{
      data[el.prop("name")] = el.val();
    }
  });

  // copy all select
  editor.find("select option:selected").each(function() {
    var el = $(this);
      data[el.parents("select:first").prop("name")] = el.val();
  });

  return data;
}


</script>
