<%@ page import="com.twproject.agenda.DiscussionPointStatus, com.twproject.meeting.DiscussionPoint, com.twproject.meeting.DiscussionPointType, com.twproject.meeting.Meeting,
                 com.twproject.messaging.board.BoardBricks, com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.resource.ResourceBricks,
                 com.twproject.security.TeamworkPermissions, com.twproject.task.TaskBricks, com.twproject.waf.html.MeetingDrawer, net.sf.json.JSONArray,
                 net.sf.json.JSONObject, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.core.JST,
                 org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.input.ComboBox, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.io.Serializable, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.button.ButtonLink" %><%


    PageState pageState = PageState.getCurrentPageState(request);

    MeetingDrawer meetingDrawer = (MeetingDrawer) JspIncluderSupport.getCurrentInstance(request);
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Person loggedPerson = logged.getPerson();
    Meeting meeting = meetingDrawer.meeting;

    boolean isAuthor = true;
    if (meeting.getEvent().getAuthor() != loggedPerson)
        isAuthor = false;

    PageSeed ps = pageState.thisPage(request);
    Serializable pageMeetingId = meeting.getId();
    ps.mainObjectId = pageMeetingId;
%>

<div style="margin-top: 15px"> <%

  SmartCombo combo = BoardBricks.getVisibleBoards("MEETING_BOARD", pageState);
  combo.separator = "<br>";
  combo.label = I18n.get("MEETING_BOARD");
  combo.fieldSize = 35;
  combo.readOnly = !isAuthor;
  if (meeting.getBoard()!=null && meeting.getBoard().hasPermissionFor(logged,TeamworkPermissions.board_canRead)) {
    combo.linkToEntity = new ButtonJS("", "openBoardInBlack(" + pageState.getEntry("MEETING_BOARD").stringValueNullIfEmpty() + ");");
    combo.linkToEntity.iconChar = "8";
    combo.linkToEntity.style = "padding:0 10px";
  }

  if (logged.hasPermissionFor(TeamworkPermissions.board_canCreate)) {
    PageSeed editor = pageState.pageFromRoot("board/boardEditor.jsp");
    editor.command=Commands.ADD;
    ButtonSupport addBl = new ButtonJS("createBoard('"+editor.toLinkToHref()+"',$(this));");
    addBl.enabled = logged.hasPermissionFor(TeamworkPermissions.board_canCreate);
    addBl.label = I18n.get("CREATE_NEW_BOARD");
    addBl.additionalCssClass = "small";
    combo.addEntityButton = addBl;
  }
  combo.toHtml(pageContext);

%> </div>
<%------------------------------------------------------------------------ DISCUSSION POINT TEMPLATES ---------------------------------------------------------------------------------------%>
<div class="dipTemplates" style="display:none;">

    <%=JST.start("DIP_EDITOR")%>
    <table class="table DIPEditor new" border="0" orderBy="(#=orderBy#)" discId="(#=obj.id#)" cellpadding="4" cellspacing="0">
        <tr>
            <td align="center" class="dragHandler" nowrap valign="middle" width="20" rowspan="3">&nbsp;</td>
            <td align="right" colspan="99"><span onclick="closeDIPEditor($(this));" class="teamworkIcon" style="cursor:pointer" title="<%=I18n.get("CLOSE")%>">x</span></td>
        </tr>
        <tr>

            <td valign="top" style="padding-right: 30px" valign="top">
                <table width="100%" border="0">
                    <tr>
                        <td nowrap ><%
                            pageState.addClientEntry("title","(#=obj.title#)");
                            TextField tf = new TextField("title", "");
                            tf.innerLabel = I18n.get("DISCUSSION_POINT_TITLE");
                            tf.label = "";
                            tf.fieldClass=tf.fieldClass+" dpTitle";
                            tf.toHtmlI18n(pageContext);
                        %>
                        </td>
                    </tr>
                    <tr>
                        <td valign="top" ><label><%=I18n.get("MEETING_MINUTE_VIEW")%></label>
                            <textarea oldvalue="1" rows="1" cols="1" name="minute" id="min(#=obj.id#)" style="overflow:auto;background-color:white;height:150px;width:100%;" class="minute linkEnabled">(#=obj.minute#)</textarea>
                        </td>
                    </tr>

                </table>
              <div class="dipDocs"></div>
            </td>
            <td width="300" valign="top" style="border-left: 1px solid #91B4B7;padding-left:20px">
                <table>

                    <tr>
                        <td nowrap  colspan="2">
                            <%
                                pageState.addClientEntry("lead","(#=obj.lead#)");
                                pageState.addClientEntry("lead_txt","(#=obj.leadName#)");
                                SmartCombo cb = ResourceBricks.getPersonCombo("lead", TeamworkPermissions.resource_canRead, false, "", pageState);
                                cb.label = I18n.get("DISCUSSION_POINT_LEAD");
                                cb.separator="<br>";
                                cb.fieldSize =30;
                                cb.addAllowed=true;
                                cb.toHtml(pageContext);
                            %>
                        </td></tr><tr>
                    <td nowrap   colspan="2">
                        <%
                            pageState.addClientEntry("task","(#=obj.task#)");
                            pageState.addClientEntry("task_txt","(#=obj.taskName#)");
                            PageSeed psET = pageState.pageFromRoot("task/taskOverview.jsp");
                            psET.setCommand(Commands.EDIT);
                            SmartCombo taskCombo = TaskBricks.getTaskCombo("task", false, TeamworkPermissions.task_canRead, pageState);
                            taskCombo.separator = "<br>";
                            taskCombo.label = I18n.get("DISCUSSION_POINT_TASK");
                            taskCombo.onValueSelectedScript = "reloadDocuments($(this));";
                            taskCombo.fieldSize =30;
                            taskCombo.addAllowed=true;
                            //taskCombo.addLinkToEntity(psET, I18n.get("HIS_TASKEDITOR"), pageState);
                            taskCombo.toHtml(pageContext);

                        %>
                    </td>
                </tr>
                    <tr >
                        <td nowrap >
                            <%
                                pageState.addClientEntry("timeScheduled","(#=obj.timeScheduled#)");
                                ComboBox durationField = ComboBox.getTimeInstance("timeScheduled", "AGE_DURATION", pageState);
                                durationField.separator = "<br>";
                                durationField.label = I18n.get("DURATION");
                                durationField.toHtml(pageContext);

                            %>
                        </td>
                        <td><%
                            pageState.addClientEntry("type","(#=obj.type#)");
                            pageState.addClientEntry("type_txt","(#=obj.typeDescription#)");
                            SmartCombo types = new SmartCombo("type", DiscussionPointType.class);
                            types.separator = "<br>";
                            types.label =I18n.get("DISCUSSION_POINT_TYPE");
                            types.fieldSize = 10;

                            if (pageState.getLoggedOperator().hasPermissionFor(TeamworkPermissions.classificationTree_canManage)) {
                              ButtonSupport addTT = ButtonLink.getBlackInstance(I18n.get("ADD"), pageState.pageFromRoot("meeting/discussionPointType.jsp"));
                              addTT.additionalCssClass = "small";
                              types.addEntityButton = addTT;
                            }
                            types.toHtmlI18n(pageContext);
                        %>
                        </td>
                    </tr><tr>
                    <td nowrap  colspan="2">
                        <%
                            pageState.addClientEntry("status","(#=obj.status#)");
                            pageState.addClientEntry("status_txt","(#=obj.statusDescription#)");
                            SmartCombo status = new SmartCombo("status", DiscussionPointStatus.class);
                            status.separator = "<br>";
                            status.label = I18n.get("STATUS");

                          if (pageState.getLoggedOperator().hasPermissionFor(TeamworkPermissions.classificationTree_canManage)) {
                            ButtonSupport addTT = ButtonLink.getBlackInstance(I18n.get("ADD"), pageState.pageFromRoot("meeting/discussionPointStatus.jsp"));
                            addTT.additionalCssClass = "small";
                            status.addEntityButton = addTT;
                          }

                          status.toHtml(pageContext);
                        %></td></tr>
                </table>
            </td>

        </tr>

        <tr>
            <td colspan="99" align="right">
                <span onclick="$(this).confirm(delRow);" class="button textual icon" style="cursor:pointer" title="<%=I18n.get("DELETE")%>"><span class="teamworkIcon">d</span> </span>
            </td>
        </tr>
    </table>
    <%=JST.end()%>

    <%=JST.start("DIP_VIEW")%>
    <table class="table DIPView discussionPointRow" border="0"  discId="(#=obj.id#)" orderBy="(#=orderBy#)" cellpadding="4" cellspacing="0">
        <tr>
            <td align="center" class="dragHandler" nowrap valign="middle" width="20" rowspan="2">&nbsp;</td>
            <td valign="top" style="padding-right: 30px">
                <table width="100%" border="0">
                    <tr>
                        <td nowrap title="<%=I18n.get("DISCUSSION_POINT_TITLE")%>"><h4>(#=obj.title#)</h4></td>
                    </tr>
                    <tr>
                        <td valign="top" onclick="editDIP($(this));" title="<%=I18n.get("MEETING_MINUTE_VIEW")%>">(#=obj.minute#)</td>
                    </tr>

                </table>
              <div class="dipDocs"></div>

            </td>
            <td  class="dipMeta" valign="top">
                <table>(#if (obj.leadName){#)
                    <tr>
                        <td nowrap  colspan="2">
                            <label><%=I18n.get("DISCUSSION_POINT_LEAD")%>:</label>(#=obj.leadName#)
                        </td>
                    </tr>(#}if(obj.taskName){#)<tr>
                    <td   colspan="2">
                        <label><%= I18n.get("DISCUSSION_POINT_TASK")%>:</label>(#=obj.taskName#)
                    </td>
                </tr>(#}if(obj.timeScheduled){#)<tr >
                        <td nowrap colspan="2">
                            <label><%= I18n.get("DURATION")%>:</label>(#=obj.timeScheduled#)
                        </td>
                    </tr>(#}if(obj.typeDescription){#)<tr>
                    <td><label><%=I18n.get("DISCUSSION_POINT_TYPE") %>:</label>(#=obj.typeDescription#)
                    </td>
                </tr>(#}if(obj.statusDescription){#)<tr>
                    <td nowrap  colspan="2">
                        <label><%= I18n.get("STATUS")%>:</label>(#=obj.statusDescription#)
                    </td></tr>(#}#)
                </table>
            </td>
          <td align="right" valign="top" width="20" style="background-color: #f0f0f0">(#if (obj.canWrite){#)
            <span onclick="editDIP($(this));" class="teamworkIcon" style="cursor:pointer" title="<%=I18n.get("EDIT")%>">e</span>
            (#}#)
          </td>

        </tr>
    </table>
    <%=JST.end()%>

    <%=JST.start("DIP_OMISSIS")%>
    <div  discId="(#=obj.id#)" orderBy="(#=obj.orderBy#)" style="background-color:#e0e0e0;padding:8px;"><i><%=I18n.get("DISCUSSION_POINT_OMISSIS")%></i></div>
    <%=JST.end()%>

</div>


<%
    /**************************************************** Discussion point ****************************************************************************************/
%><br>


<div id="dipRows"></div>

<div class="tdButtons" style="text-align: center">
  <%if (isAuthor ) {%>
  <span class="button small" onclick="addPoint();"><span class="teamworkIcon withLabel" title="<%=I18n.get("ADD_POINT")%>">P</span>&nbsp;<%=I18n.get("ADD_POINT")%></span>
  <%}%>
</div>

<%
    JSONArray dips= new JSONArray();
    for (DiscussionPoint d : meeting.getDiscussionPoints()) {
        JSONObject jd=d.jsonify(pageState);
        jd.element("canWrite", isAuthor || logged.hasPermissionAsAdmin() );
        dips.add(jd);
    }
%>
<%--<script language="javascript" type="text/javascript" src="<%=request.getContextPath()%>/commons/layout/tinymce/jscripts/tiny_mce/jquery.tinymce.js"></script>--%>
<script language="javascript" type="text/javascript" src="<%=request.getContextPath()%>/commons/layout/tinymce/tiny_mce.js"></script>
<script>

var dips=<%=dips%>;

$(document).ready(function () {
  $("#AGENDA_MENU").addClass('selected');

  //templates
  $(".dipTemplates").loadTemplates().remove();
  $.JST.loadDecorator("DIP_EDITOR", function (domElement, jsonData) {
    domElement.oneTime(10, "tmce", function () {
      var ta = $(this).find("textarea").prop("id");
      tinyMCE.execCommand("mceAddControl", true, ta);
    });

    //manage documents
    appendDocumentsCheckboxes(domElement.find(".dipDocs"),jsonData.documents,jsonData.id);

  });


  $.JST.loadDecorator("DIP_VIEW", function (domElement, jsonData) {
    //manage documents
    //console.debug("DIP_VIEW",jsonData);
    var ndo=domElement.find(".dipDocs");
    for (var i=0;jsonData.documents && i<jsonData.documents.length;i++){
      var doc=$("<a>").prop("href",jsonData.documents[i].contentLink).append(jsonData.documents[i].name).attr("docId",jsonData.documents[i].id);
      ndo.append(doc).append("&nbsp;&nbsp;");
    }
  });


  drawDips(dips);

  //hide event details
  <% if(!isAuthor && !pageState.getLoggedOperator().hasPermissionAsAdmin()){
     %>$("#slideDivUp .button").click();
  <%
    }%>
  //add a discussion point if none
  if (dips.length <= 0) {
    addPoint();
  }

  bindDrag();

  //init MCE
  tinyMCE.init({
    mode:                             "none",
    setup:                            tinyCustomSetup,
    theme_advanced_resize_horizontal: false,
    element_format:                   "html",
    cleanup:                          true,
    relative_urls:                    true,
    theme_advanced_toolbar_location:  "top",
    plugins:                          "paste,fullscreen",
    theme:                            "advanced",
    theme_advanced_resizing:          true,
    valid_elements:                   "*[*]",
    theme_advanced_toolbar_align:     "left",
    force_br_newlines:                true,
    forced_root_block:                false,
    remove_script_host:               true,

    theme_advanced_buttons1:           "bold,italic,underline,strikethrough,|,justifyleft,justifycenter,justifyright,justifyfull,|,forecolor,backcolor,fontsizeselect,cut,copy,paste,pastetext,pasteword,|,bullist,numlist,|,undo,redo,|,link,unlink,anchor,code,fullscreen,|,addIssueButton",
    theme_advanced_buttons2:           "",
    theme_advanced_buttons3:           "",
    theme_advanced_buttons4:           "",
    theme_advanced_statusbar_location: "bottom"
  });

});


function tinyCustomSetup(ed) {
  ed.onSaveContent.add(function (ed, o) {
    //console.debug("onSaveContent");
    ed.startContent = tinymce.trim(ed.getContent({format: 'raw', no_events: 1}));
  });

  ed.addButton('addIssueButton', {
    title:   "<%=I18n.get("CREATE_ISSUE_FROM_SELECTION")%>",
    image:   contextPath + '/img/issuesIcon.png',
    onclick: function () {
      createIssue(ed);
    }
  });


  ed.onNodeChange.add(function (ed, cm, e) {
    // Activates the link button when the caret is placed in a anchor element
    if (ed.selection.getContent().trim() != "")
      cm.setDisabled('addIssueButton', false);
    else
      cm.setDisabled('addIssueButton', true);
  });
}


function managePageUnload() {
  //save all tinymce open editor in the underneath textarea
  for (var i in tinymce.editors) {
    tinymce.editors[i].save();
  }
}

function createIssue(tinyEd) {
  var description = tinyEd.selection.getContent();

  if (description.trim() != "") {
    var params = "ISSUE_DESCRIPTION=" + encodeURIComponent(description) + "&ASSIGNEE=<%=loggedPerson.getId()%>";

    var ta = $(tinyEd.getElement());
    var ed = ta.closest("[discId]");
    var taskId = ed.find(":input[name=task]").val();

    if (taskId)
      params += "&ISSUE_TASK=" + taskId;


    openIssueEditorInBlack(0, "<%=Commands.ADD%>", params);
  }
}


function drawDips(dips) {
  //console.debug("drawDips", dips);

  //sort dips
  dips.sort(function (a, b) {return a.orberBy - b.orberBy;});

  var ndo = $("#dipRows");
  for (var i = 0; i < dips.length; i++) {
    var dip;
    if (!dips[i].canRead)
      dip = $.JST.createFromTemplate(dips[i], "DIP_OMISSIS");
    else
      dip = $.JST.createFromTemplate(dips[i], "DIP_VIEW");
    ndo.append(dip);
  }
}


function bindDrag() {
  $("#dipRows").sortable({
    handle:      ".dragHandler",
    axis:        "y",
    containment: "parent",

    stop: function (ev, ui) {
      var order = 0;
      $(this).children().each(function () {
        $(this).attr("orderBy", order++);
      });
    }
  });
}


function addPoint() {
  var ndo = $("#dipRows");
  var dip = $.JST.createFromTemplate({id: "new_" + new Date().getTime(), orderBy: ndo.find("[discid]").size()}, "DIP_EDITOR");
  ndo.append(dip);
  dip.find(":input[name=title]").focus();
  $(window).resize();
}


function editDIP(el) {
  var dipView = el.closest("[discid]");
  var idDip = dipView.attr("discid");
  dipView.replaceWith($.JST.createFromTemplate(getDipFromId(idDip), "DIP_EDITOR"));
}


function closeDIPEditor(el) {
  var dipEdit = el.closest("[discid]");
  var idDip = dipEdit.attr("discid");

  //maleficotiny
  managePageUnload();

  var close = function () {
    if (idDip.startsWith("new")) {
      dipEdit.fadeOut(500, function () {
        $(this).remove();
        $(window).resize();
      });
    } else
      dipEdit.replaceWith($.JST.createFromTemplate(getDipFromId(idDip), "DIP_VIEW"));

    //remove tiny
    var ta = dipEdit.find("textarea").prop("id");
    //console.debug("removing tiny from",ta);
    tinyMCE.execCommand("mceRemoveControl", true, ta);

  };

  if (dipEdit.find("[oldvalue]:input").isValueChanged())
    el.confirm(close);
  else
    close();
}


function delRow() {
  var button = $(this);
  showSavingMessage();
  var dipRow = $(button).closest("[discId]");
  var request = {
    CM: "DELDIP",
    id: dipRow.attr("discId")
  };
  $.getJSON("agendaAjaxController.jsp", request, function (response) {
    jsonResponseHandling(response);
    if (response.ok) {
      dipRow.remove();
    }
    hideSavingMessage();
  });
}


function getDipFromId(id) {
  for (var i = 0; i < dips.length; i++) {
    if (dips[i].id == id) {
      return dips[i];
    }
  }
  return false;
}


function reloadDocuments(el) {
  //console.debug("reloadDocuments", el);
  showSavingMessage();
  var taskId = el.val();
  var row=el.closest("[discId]");
  $.getJSON("agendaAjaxController.jsp",{CM:"REFRESHDOC",taskId:taskId},function(response){
    hideSavingMessage();
    jsonResponseHandling(response);
    if (response.ok) {
      appendDocumentsCheckboxes(row.find(".dipDocs"),response.documents,row.attr("discId"))
    }
  });
}

function appendDocumentsCheckboxes(ndo,docs,discId){
  ndo.empty();
  var dip=getDipFromId(discId);
  for (var i=0; docs && i<docs.length;i++){
    var doc = docs[i];
    var ck =$("<input type='checkbox'>").prop("name","ckDoc_"+doc.id).attr("docId",doc.id);

    for (j=0; dip && dip.documents && j<dip.documents.length ;j++){
      if(dip.documents[j].id==doc.id){
        ck.prop("checked",true);
        break;
      }
    }

    var wrp= $("<div>").addClass("docWrapper");
    wrp.append(ck).append(doc.name+"   ");
    ndo.append(wrp);
  }

}

function editMinute(theEl) {
  var tr = $(theEl).parents("[discId]");
  var url = contextPath + "/applications/teamwork/meeting/partMinuteEditor.jsp?DISC_POINT_ID=" + tr.attr("discId");
  openBlackPopup(url, $(window).width() - 100, $(window).height() - 50, function () {
    alert("callback per copiare il contenuto di sotto")
  });

}

<%------------------------------------------- ADD BOARD MANAGEMENT ---------------------------------------------------%>
function createBoard(url, el) {
  //var row = el.closest(".assigRow");
  var name = $("[name=MEETING_BOARD_txt]").val();
  url = url + "&NAME=" + encodeURI(name);

  openBlackPopup(url, 800, 600, function (response) {
    //fillare lo smart combo
    if (response && response.boardId && response.boardName) {
      $("[name=MEETING_BOARD]").val(response.boardId);
      $("[name=MEETING_BOARD_txt]").val(response.boardName).focus().blur();
    }
  })
}





</script>





