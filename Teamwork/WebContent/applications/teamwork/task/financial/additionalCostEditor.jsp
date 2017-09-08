<%@ page import="com.twproject.task.financial.Cost,
                 com.twproject.task.financial.CostClassification,
                 org.jblooming.designer.DesignerField,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.ClientEntries,
                 org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, com.twproject.task.financial.FinancialBricks, com.twproject.task.Assignment, org.jblooming.utilities.StringUtilities, org.jblooming.waf.view.ClientEntry, org.jblooming.agenda.CompanyCalendar, org.jblooming.waf.ScreenArea, com.twproject.waf.TeamworkPopUpScreen, com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, org.jblooming.waf.constants.SecurityConstants, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.input.*, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.html.core.JST, com.twproject.task.Task" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    body.areaHtmlClass="lreq20 lreqPage";
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
    TeamworkOperator logged= (TeamworkOperator) pageState.getLoggedOperator();
    Task task = Task.load(pageState.getEntry("taskId").stringValueNullIfEmpty() + "");
    Cost cost = Cost.load(pageState.getEntry("costId").stringValueNullIfEmpty() + "");

    if (task == null ||
      cost!=null && !task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canRead) ||
      cost==null && !task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canCreate)) {
      throw new org.jblooming.security.SecurityException(SecurityConstants.I18N_PERMISSION_LACKING, TeamworkPermissions.task_cost_canRead);
    }

    boolean canManageCosts = task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canWrite);
    boolean readOnly = !canManageCosts;


  //make
  if (cost != null) {
    pageState.setClientEntries(new ClientEntries());
    pageState.addClientEntry("classification", cost.getClassification());
    pageState.addClientEntry("creationDate", cost.getCreationDate());
    pageState.addClientEntry("description", cost.getDescription());
    pageState.addClientEntry("estimatedCost", cost.getEstimatedCost());
    pageState.addClientEntry("realCost", cost.getRealCost());
  } else {
    cost = new Cost();
    cost.setIdAsNew();
  }


  %><h3><%=cost.isNew()?I18n.get("ADD_COST"):I18n.get("EDIT_COST")%><br>
<span class="textSmall"><%=task.getDisplayName()%></span></h3>
<%

  readOnly=readOnly || cost.getStatus()!=null;

  PageSeed ps = pageState.thisPage(request);

  ps.command = "";
  Form f = new Form(ps);
  f.alertOnChange=true;
  f.id="CSTED";
  f.start(pageContext);


  TextField tf = new TextField(I18n.get("DESCRIPTION"), "description", "", 40, false);
  tf.required=true;
  tf.readOnly=readOnly ;
  tf.separator="<br>";
  tf.script="style=width:100%";
  tf.toHtml(pageContext);

%>

<table class="table" >
  <tr>
    <td nowrap>
  <%
  DateField df = new DateField("creationDate");
  df.labelstr = "DATE";
  df.separator = "<br>";
  df.required=false;
  df.readOnly=readOnly;
  df.toHtmlI18n(pageContext);

  %></td><td nowrap><%
    SmartCombo costClass = FinancialBricks.getCostClassificationCombo("classification", task.getArea(),pageState );
    costClass.separator = "<br>";
    costClass.id = "";
    costClass.label=I18n.get("COST_CLASSIFICATION");
    costClass.disabled=readOnly;
    //costClass.script="style=width:100%";
    costClass.iframe_width=300;
    costClass.toHtml(pageContext);
  %></td>
  </tr>  <tr>
    <td nowrap>
  <%
    TextField cs = TextField.getCurrencyInstance("estimatedCost");
    cs.label = I18n.get("COST_ESTIMATED");
    cs.fieldSize=10;
    cs.readOnly=readOnly ;
    cs.separator="<br>";
    cs.toHtml(pageContext);


  %></td><td nowrap><%
  cs = TextField.getCurrencyInstance("realCost");
  cs.label = I18n.get("COST_DONE");
  cs.fieldSize=10;
  cs.readOnly=readOnly ;
  cs.separator="<br>";
  cs.toHtml(pageContext);

%></td>
  </tr></table><%




  if ( DesignerField.hasCustomField("COST_ADD_CUSTOM_FIELD_", 4)) {
  %><div class="clearfix"><%

    for (int i = 1; i < 5; i++) {
     pageState.removeEntry("COST_ADD_CUSTOM_FIELD_" +i);
      DesignerField dfStr = DesignerField.getCustomFieldInstance("COST_ADD_CUSTOM_FIELD_", i, cost, false, false, false, pageState);
      if (dfStr != null) {
        dfStr.separator = "<br>";
        dfStr.readOnly=readOnly;
        %><div style="float:left;margin-right: 10px"><%dfStr.toHtml(pageContext);%></div><%
      }
    }
%></div><%
  }
  %>

<br>
<label><%=I18n.get("ATTACHMENT")%> </label><br>
<div id="holder"  <%=readOnly?"disabled":""%>  class="uploadizeDrop visible" ></div>
<input type="hidden" name="PENDING_PF" id="PENDING_PF">
<%


  ButtonBar buttonBar = new ButtonBar();

  ButtonJS save = new ButtonJS(I18n.get("SAVE"),"saveCost($(this));");
  save.additionalCssClass="first big";
  save.enabled = !readOnly;
  buttonBar.addButton(save);

  if (!cost.isNew()) {
    ButtonJS del = new ButtonJS(I18n.get("DELETE"), "deleteCost($(this));");
    del.enabled = !readOnly;
    del.confirmRequire=true;
    buttonBar.addButton(del);
  }

  buttonBar.loggableIdentifiableSupport = cost;
  buttonBar.toHtml(pageContext);

 f.end(pageContext);

%>


<div id="asctmpl">
  <%=JST.start("FILEBOX")%>
  <div class="repoFileBox" >
    <img src="(#=contextPath+'/img/mime/'+obj.img#)" style="height:20px;">&nbsp;<span class="button textual" title="<%=I18n.get("PREVIEW_DOWNLOAD")%>">(#=obj.name#)</span><span class="del teamworkIcon" title="<%=I18n.get("DELETE")%>">d</span>
  </div>
  <%=JST.end()%>
</div>
<script>
  $(function () {

    var cost=<%=cost.jsonify()%>;
    $("#asctmpl").loadTemplates().remove();
    enableUpload();
    decorators();

    //add filebox if any
    if (cost.attachment){
      $("#holder").append($.JST.createFromTemplate(cost.attachment,"FILEBOX")).removeClass("uploadizeDrop").attr("disabled",true);
      //$("#holder").hide();
    }

  });

  function enableUpload() {
    //console.debug("enableUpload",view)
    $("#holder").uploadize({
      fieldName:"attachment",
      url:"costAjaxController.jsp",
      maxSize:<%=Uploader.getMaxUploadSizeInByte()%>,
      multi:  false,
      showPlaceHolder:true,
      //activeElement:"#holder",

      additionalRequestParameters: {CM: "COSTATTACH", costId: "<%=cost.getId()%>",taskId:"<%=task.getId()%>"},

      onLoadCallback:function(response){
        if (response.ok){
          //in caso di nuovo costo mette da una parte i pf temporanei
          if (<%=cost.isNew()? "true" : "false" %>) {
            $("#PENDING_PF").val(response.attachment.uid);
          }
          $("#holder").append($.JST.createFromTemplate(response.attachment,"FILEBOX")).removeClass("uploadizeDrop").attr("disabled",true);
        }
      },
      fileAreaSelector:"#holder"
    });

  }

function saveCost(button){
  //console.debug("saveCost")
  if ($("#CSTED :input[oldValue]").isValueChanged()) {
    if ($("#CSTED :input").isFullfilled()) {
      showSavingMessage();
      button.hide();
      var request = {};
      $("#CSTED").fillJsonWithInputValues(request);

      request.CM="SVADDCOST";
      request.costId= "<%=cost.getId()%>";
      request.taskId="<%=task.getId()%>";
      $.getJSON('costAjaxController.jsp', request, function (response) {
        jsonResponseHandling(response);
        if (response.ok) {
          $("#CSTED :input").updateOldValue();
          closeBlackPopup(response.cost);
        } else {
          button.show();
        }
        hideSavingMessage();
      })
    }
  } else {
    <%if (cost.isNew()){%>
      closeBlackPopup();
    <%}else{%>
      closeBlackPopup({id:"<%=cost.getId()%>",taskId:"<%=task.getIntId()%>"});
    <%}%>
  }
}


  function decorators(){
    $.JST.loadDecorator("FILEBOX", function (box, pf) {
      //console.debug(box,pf)
      box.data("pf",pf);
      //bind open image/download file
      box.click(function (ev) {
        ev.stopPropagation();
        openPersistentFile($(this).data("pf"));
      });

      //console.debug("box.data",box.data("pf"))
      //delete file
      box.find(".del").click(function(ev){
        ev.stopPropagation();
        var el = $(this);
        var fb = el.closest(".repoFileBox");

        var editor=$("#CSTED");
        fb.confirm(function(){
          var box=$(this);
          showSavingMessage();
          var request = {CM: "DELCOSTATTACH", costId: "<%=cost.getId()%>",taskId:"<%=task.getId()%>", fileUID: box.data("pf").uid};
          $.getJSON("costAjaxController.jsp", request, function (response) {
            jsonResponseHandling(response);
            if (response.ok) {
              $("#PENDING_PF").val("");
              box.fadeOut(200, function () {$(this).remove();$("#holder").addClass("uploadizeDrop").removeAttr("disabled");});
            }
            hideSavingMessage();
          });
        },"<%=I18n.get("FLD_CONFIRM_DELETE")%>");

      });
    });
  }

  function deleteCost(){
    var request = {
      CM:   "DLADDCOST",
      costId:<%=cost.getId()%>,
      taskId:<%=task.getId()%>
    };
    $.getJSON('costAjaxController.jsp', request, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        getBlackPopupOpener().$("tr[costId=<%=cost.getId()%>]").remove();
        getBlackPopupOpener().recomputeAddCosts();
        closeBlackPopup();
      }
    });
  }

</script>

<%
  }
%>
