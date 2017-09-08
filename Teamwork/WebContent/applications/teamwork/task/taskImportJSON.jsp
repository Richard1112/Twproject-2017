<%@ page
        import="com.twproject.security.TeamworkPermissions, com.twproject.task.Task,
        com.twproject.waf.TeamworkPopUpScreen, net.sf.json.JSONObject,
        org.jblooming.operator.Operator, org.jblooming.persistence.PersistenceHome,
        org.jblooming.utilities.file.FileUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.html.button.ButtonSubmit,
        org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.html.input.Uploader.UploadHelper,
        org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed,
        org.jblooming.waf.view.PageState, java.io.File" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    Operator logged= pageState.getLoggedOperator();
    boolean canWrite=false;

    Task task = null;
    if (pageState.mainObjectId != null) {
      task = (Task) PersistenceHome.findByPrimaryKey(Task.class, pageState.mainObjectId);
      canWrite=task.hasPermissionFor(logged, TeamworkPermissions.task_canCreate);
    } else {
      canWrite= logged.hasPermissionFor(TeamworkPermissions.project_canCreate);
    }


    %>
    <div style="padding: 30px 0; ">
      <h1><%=I18n.get("IMPORT_FROM_JSON")%></h1>
      <br>
  <%


  boolean uploadOk=false;
  JSONObject uploadedJson=new JSONObject();
  String origFileName ="";
  if ("UPLJSON".equals(pageState.command)) {
    UploadHelper instance = Uploader.getHelper("JSON_FILE_TO_IMPORT", pageState);
    origFileName = instance.originalFileName;
    if (instance != null) {
      File temporaryFile = instance.temporaryFile;
      if (temporaryFile != null && temporaryFile.exists()) {

        String jsonData = FileUtilities.readTextFile(temporaryFile.getAbsolutePath());

        try {
          uploadedJson = JSONObject.fromObject(jsonData);
          uploadOk=true;
          } catch (Throwable t) {
           pageState.addMessageError(I18n.get(t.getMessage()), I18n.get("INVALID_JSON_FORMAT"));
         }

         pageState.removeEntry("JSON_FILE_TO_IMPORT");


       }
     }
  }

  if (!uploadOk) {
    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = pageState.mainObjectId;
    self.setCommand("UPLJSON");
    Form f = new Form(self);
    f.encType = Form.MULTIPART_FORM_DATA;
    pageState.setForm(f);
    f.start(pageContext);

    Uploader u = new Uploader("JSON_FILE_TO_IMPORT", pageState);

    u.label = I18n.get("JSON_FILE_TO_IMPORT");
    u.separator = "<br><br>";
    u.required = true;
    u.toHtml(pageContext);

    %><br><%

    ButtonBar bb2 = new ButtonBar();

    ButtonSubmit saveInstance = new ButtonSubmit(f);
    saveInstance.label = I18n.get("UPLOAD");
    saveInstance.enabled = canWrite;
    saveInstance.additionalCssClass = "first big";
    bb2.addButton(saveInstance);

    bb2.toHtml(pageContext);

    f.end(pageContext);


  }
    
%>
<div id="importResults" style="display: none; text-align: center;">
  <h2><%=I18n.get("ABOUT_TO_IMPORT_%%_TASKS","<span id='_taskcnt'></span>")%></h2>
  <span class="button big first" onclick="startImport($(this));"><%=I18n.get("PROCEED_WITH_THE_IMPORT")%></span>
</div>
  
<script type="text/javascript">

  var uploadedJson= <%=uploadedJson.toString()%>;
  $(function(){
    //console.debug("uploadedJson",uploadedJson);

    if (uploadedJson && Object.size(uploadedJson)>0 ) {
      //import singolo
      if (uploadedJson.project) {
        $("#_taskcnt").html("1");

        //import progetti multipli
      } else if (uploadedJson.projects) {
        $("#_taskcnt").html(uploadedJson.projects.length);

        //import vecchio todo ????? cosa vuol dire?
      } else if (uploadedJson.tasks) {
        $("#_taskcnt").html(uploadedJson.tasks.length);

      }
      $("#importResults").show();

    } else {
      $("#importResults").hide();
    }

    });

function startImport(el){

  var request = {taskId:"<%=task==null?"":task.getId()%>",filename:"<%=origFileName%>"};

  //import singolo: da export task editor
  if (uploadedJson.project) {
    request.CM="IMPORTPROJECT";
    request.prj=JSON.stringify(uploadedJson.project);

  //import multiplo: da export task list
  } else if (uploadedJson.tasks) {
    request.CM="IMPORTPROJECTLIST";
    request.prj=JSON.stringify(uploadedJson);

    //import progetti multipli: tipo gantt service
  } else if (uploadedJson.projects) {
    request.CM="IMPORTMULTIPROJECTS";
    request.prj=JSON.stringify(uploadedJson);
  }




  if (uploadedJson.deletedTaskIds && uploadedJson.deletedTaskIds.length>0){
    if (!confirm("<%=I18n.get("TASK_THAT_WILL_BE_REMOVED")%>\n"+uploadedJson.deletedTaskIds.length))
      return;
  }

  el.fadeOut();
  showSavingMessage();
  $.ajax("gantt/ganttAjaxController.jsp", {
    dataType:"json",
    data: request,
    type:"POST",

    success: function(response) {
      hideSavingMessage();
      //console.debug(response);
      var ndo=$("#importResults");
      ndo.empty();

      jsonResponseHandling(response);
      if (response.ok) {

        if(response.project && response.project.tasks.length>0){
          // procede to the editor: create a button
          ndo.append("<h1><%=I18n.get("PROJECT_CORRECTLY_IMPORTED")%></h1>");
          var task=response.project.tasks[0];
          ndo.append("<br>");
          ndo.append("<a href='taskEditor.jsp?CM=ED&TASK_ID="+task.id+"' class='button big first' target='_top'>"+task.name+"</a>");
          ndo.append("<hr>");

        } else {
          ndo.append("<h1><%=I18n.get("PROJECT_SEEMS_EMPTY")%></h1>");          
        }
      }
    }

  });
}

</script>

    </div>
<%
}
%>