<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Assignment,
                 com.twproject.task.financial.Cost,
                 com.twproject.task.financial.FinancialBricks,
                 com.twproject.waf.TeamworkPopUpScreen,
                 org.jblooming.agenda.CompanyCalendar,
                 org.jblooming.designer.DesignerField, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.SecurityConstants, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.input.DateField, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntries, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState" %>
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


    Assignment assig = Assignment.load(pageState.getEntry("assId").stringValueNullIfEmpty() + "");
    if (assig == null)
      throw new org.jblooming.security.SecurityException(SecurityConstants.I18N_PERMISSION_LACKING);

    boolean isYours = logged.getPerson().equals(assig.getResource());
    boolean canReadExp = assig.hasPermissionFor(logged, TeamworkPermissions.task_cost_canRead);
    boolean canManageExp = assig.hasPermissionFor(logged, TeamworkPermissions.expense_manage);
    if (!isYours && !canManageExp &&!canReadExp) {
      throw new org.jblooming.security.SecurityException(SecurityConstants.I18N_PERMISSION_LACKING, TeamworkPermissions.task_cost_canRead);
    }

    Cost cost = Cost.load(pageState.getEntry("costId").stringValueNullIfEmpty() + "");


  //make
  if (cost != null) {
    pageState.setClientEntries(new ClientEntries());
    pageState.addClientEntry("classification", cost.getClassification());
    pageState.addClientEntry("creationDate", cost.getCreationDate());
    pageState.addClientEntry("description", cost.getDescription());
    pageState.addClientEntry("realCost", cost.getRealCost());
  } else {
    cost = new Cost();
    cost.setIdAsNew();
  }


  boolean canManageCosts = isYours &&! cost.isLockedByDateOrStatus() || canManageExp;
  boolean readOnly=!canManageCosts;

  PageSeed ps = pageState.thisPage(request);

  ps.command = "";
  Form f = new Form(ps);
  f.alertOnChange=true;
  f.id="CSTED";
  f.start(pageContext);

%>

<table class="table" >
  <tr>
    <td nowrap>
  <%
  DateField df = new DateField("creationDate");
  df.labelstr = "DATE";
  df.separator = "<br>";
  df.required=true;
  df.readOnly=readOnly;
  df.toHtmlI18n(pageContext);

  %></td><td nowrap><%

    TextField cs = TextField.getCurrencyInstance("realCost");
    cs.label = I18n.get("COST_DONE");
    cs.required=true;
    cs.readOnly=readOnly ;
    cs.separator="<br>";
    cs.toHtml(pageContext);

  %></td><td nowrap>
    </td></tr></table><%

  TextField tf = new TextField(I18n.get("DESCRIPTION"), "description", "", 40, false);
  tf.required=true;
  tf.readOnly=readOnly ;
  tf.separator="<br>";
  tf.script="style=width:100%";
  tf.toHtml(pageContext);

  SmartCombo costClass = FinancialBricks.getCostClassificationCombo("classification", assig.getTask().getArea(),pageState );
  costClass.separator = "<br>";
  costClass.id = "";
  costClass.label=I18n.get("COST_CLASSIFICATION");
  costClass.disabled=readOnly;
  //costClass.script="style=width:100%";
  costClass.toHtml(pageContext);


  if (Cost.hasCustomField()) {
  %><div class="clearfix"><%

    for (int i = 1; i < 5; i++) {
     pageState.removeEntry("COST_CUSTOM_FIELD_" +i);
      DesignerField dfStr = DesignerField.getCustomFieldInstance("COST_CUSTOM_FIELD_", i, cost, false, false, false, pageState);
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

      additionalRequestParameters: {CM: "COSTATTACH", costId: "<%=cost.getId()%>",assId:"<%=assig.getId()%>"},

      onLoadCallback:function(response){
        if (response.ok){
          //in caso di nuova issue mette da una parte i pf temporanei
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

      request.CM="SVASSCOST";
      request.costId= "<%=cost.getId()%>";
      request.assId="<%=assig.getId()%>";
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
      closeBlackPopup({id:"<%=cost.getId()%>"});
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
          var request = {CM: "DELCOSTATTACH", costId: "<%=cost.getId()%>",assId:"<%=assig.getId()%>", fileUID: box.data("pf").uid};
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
      CM:   "DLASSCOST",
      costId:<%=cost.getId()%>,
      assId:<%=assig.getId()%>
    };
    $.getJSON('costAjaxController.jsp', request, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        var openerWindow = getBlackPopupOpener();
        openerWindow.$("tr[costId=<%=cost.getId()%>]").remove();
        if (typeof (openerWindow.refreshCostsTotal)=="function")
          openerWindow.refreshCostsTotal();
        closeBlackPopup();
      }
    });
  }


</script>

<%
  }
%>
