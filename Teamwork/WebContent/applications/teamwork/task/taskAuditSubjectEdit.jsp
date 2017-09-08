<%@ page import="com.twproject.operator.TeamworkOperator,org.jblooming.waf.html.display.Img, com.twproject.security.SecurityBricks, com.twproject.security.TeamworkPermissions,
com.twproject.task.TaskAuditSubject,com.twproject.task.TaskBricks, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.ontology.businessLogic.DeleteHelper, org.jblooming.oql.OqlQuery,
 org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.exceptions.PersistenceException, org.jblooming.security.Area, org.jblooming.waf.ActionUtilities,
  org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.SecurityConstants, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit,
   org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.HeadBar, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.Combo,
    org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed,
     org.jblooming.waf.view.PageState, java.util.ArrayList,java.util.List,org.jblooming.waf.html.input.InputElement.EntryType, java.util.Map, java.util.Set, org.jblooming.waf.html.button.ButtonJS" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    if (Commands.SAVE.equals(pageState.command)) {

      Map<String, ClientEntry> map = pageState.getClientEntries().getEntriesStartingWithStripped("DESC_");
      for (String id : map.keySet()) {

    	TaskAuditSubject t = (TaskAuditSubject) PersistenceHome.findByPrimaryKey(TaskAuditSubject.class, id);
        //t.setAuditLevel(pageState.getEntry("ALEVEL_" + id).intValue());
        t.setDescription(pageState.getEntry("DESC_" + id).stringValue());
        //t.setPicture(null);
        t.store();
      }

      String newDesc = pageState.getEntry("DESC").stringValueNullIfEmpty();
      if (newDesc != null) {
    	TaskAuditSubject t = new TaskAuditSubject();
        //t.setAuditLevel(pageState.getEntry("ALEVEL").intValue());
        t.setDescription(pageState.getEntry("DESC").stringValue());
        t.setPicture(null);
        t.setIdAsNew();
        t.store();
        pageState.removeEntry("ALEVEL");
        pageState.removeEntry("DESC");
        pageState.removeEntry("PIC");
      }
    }

    pageState.toHtml(pageContext);

  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Set<Area> areas = null;

    if (!logged.hasPermissionAsAdmin()) {
      areas = logged.getAreasForPermission(TeamworkPermissions.classificationTree_canManage);
      if (areas.size() == 0)
        throw new org.jblooming.security.SecurityException(SecurityConstants.I18N_PERMISSION_LACKING, TeamworkPermissions.classificationTree_canManage);
      
    }


    String hql = "from " + TaskAuditSubject.class.getName() + " as tt ";
    
    OqlQuery oql = new OqlQuery(hql);
    List<TaskAuditSubject> tts =new ArrayList<TaskAuditSubject>();
    try{
    	tts = oql.list();
    }catch(Exception e){
    	e.printStackTrace();
    }
    


    PageSeed ps = pageState.thisPage(request);
    ps.mainObjectId = pageState.mainObjectId;
    Form form = new Form(ps);
    pageState.setForm(form);
    form.start(pageContext);

%><h1>Audit Subject Edit</h1>

<table class="table">
  <tr>
    <th class="tableHead">id</th>
    <th class="tableHead">Description</th>
    <!-- <th class="tableHead">AuditLevel</th> -->
    <th class="tableHead">Picture</th>
    <th class="tableHead"><%=I18n.get("DELETE_SHORT")%></th>
  </tr><%
    for (TaskAuditSubject tt : tts) {
     
        %> <tr class="alternate" objid="<%=tt.getId()%>">
       <td><%=tt.getId()%></td><%

      pageState.addClientEntry("DESC_"+tt.getId(),tt.getDescription());
      TextField tf = new TextField("TEXT","","DESC_"+tt.getId(),"",30,false);
      tf.label="";
      tf.separator="";  
      %><td><%tf.toHtml(pageContext);%></td><%

    /*   pageState.addClientEntry("ALEVEL_"+tt.getId(),tt.getAuditLevel());
      tf = new TextField("TEXT","","ALEVEL_"+tt.getId(),"",15,false);
      tf.label="";
      tf.entryType=EntryType.INTEGER;
      tf.separator=""; */

      %><td>
        <div class="profileImage canWrite" onclick="openProfileImageEditor('<%=tt.getId()%>')" style="cursor: pointer"><%
        	Img img = tt.bricks.getAvatarImage("");
        	if(tt.getPicture()!=null){
        		img = new Img(tt.getPicture(), tt.getDescription());
			}
			img.script = "style='width:30px;height:auto;'";
			img.id = "PIC_"+tt.getId();
			img.toHtml(pageContext);
		%>
		</div>
      </td>

  <td align="center"><%
    ButtonJS delLink = new ButtonJS("delRow($(this))");
    delLink.iconChar = "d";
    delLink.label = "";
    delLink.additionalCssClass = "delete";
    delLink.toHtmlInTextOnlyModality(pageContext);
  %></td></tr><%
    }

    %><tr class="alternate highlight"><td><span class="sectionTitle"><%=I18n.get("NEW")%></span></td><%

    TextField tf = new TextField("TEXT","","DESC","",30,false);
    tf.label="";
    tf.separator="";
    %><td><%tf.toHtml(pageContext);%></td><%

   /*  tf = new TextField("TEXT","","ALEVEL","",15,false);
    tf.label="";
    tf.entryType=EntryType.INTEGER;
    tf.separator=""; */
    %><td>
    	<div class="profileImage canWrite"  onclick="openProfileImageEditor('')" style="cursor: pointer"><%
			Img img = new TaskAuditSubject().bricks.getAvatarImage("");
			//img.script = "class='face " + (JSP.ex(size) ? size : "") + "'";
			img.script = "style='width:30px;height:auto;'";
			img.id = "PIC";
			//img.toHtml(pageContext);
		%>
		</div>
    </td>
    <td></td>
    </tr> <%
    %></table><%
    ButtonBar bb = new ButtonBar();

    ButtonSubmit save = ButtonSubmit.getSaveInstance(form, I18n.get("SAVE"));
    save.additionalCssClass="first";
    bb.addButton(save);

    bb.toHtml(pageContext);

    form.end(pageContext);
    new DeletePreviewer("OBJ_DEL",TaskAuditSubject.class.getName(), pageState);
%>
<script>

  function delRow(el) {
    var issueRow = $(el).closest("[objid]");
    var issueId = issueRow.attr("objid");
    deletePreview("OBJ_DEL", issueId, function(response) {  // callback function
      if (response && response.ok){
        issueRow.fadeOut(500, function () {$(this).remove();});
      }
    });
  }
  
  function openProfileImageEditor(resId,callback){
	    $(".uploadizeDrop").attr("disabled", "true");
	    openBlackPopup(contextPath+"/applications/teamwork/task/auditImageUploader.jsp?SUB_ID="+resId,'500px','550px', function (response) {
	      //console.debug("imageEditorCallback",response);
	      if (response && response.imageUrl) {
	        $("#PIC_"+resId).prop("src", response.imageUrl);
	        //$(".menuTools .avatarImage img").prop("src", response.imageUrl)
	      }
	    });
	  }

</script>
<%
  }
%>