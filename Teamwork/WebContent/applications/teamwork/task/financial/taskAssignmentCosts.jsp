<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment, com.twproject.task.financial.Cost, com.twproject.task.financial.CostClassification,
                 com.twproject.waf.TeamworkPopUpScreen, org.jblooming.designer.DesignerField, org.jblooming.oql.QueryHelper, org.jblooming.waf.ActionUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.SecurityConstants, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.DateField, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, org.jblooming.waf.html.container.ButtonBar, com.twproject.task.financial.FinancialBricks, org.jblooming.waf.html.core.JST, com.twproject.worklog.businessLogic.WorklogBricks, net.sf.json.JSONArray" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    pageState.setPopup(true);
    final ScreenArea body = new ScreenArea(request);
    body.areaHtmlClass="lreq20 lreqPage";
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

    TeamworkOperator logged= (TeamworkOperator) pageState.getLoggedOperator();

    Assignment assig = Assignment.load(pageState.mainObjectId);
    boolean canManageExpense = assig.hasPermissionFor(logged, TeamworkPermissions.expense_manage);
    boolean isYours = logged.getPerson().equals(assig.getResource());
    if (!isYours && !assig.hasPermissionFor(logged, TeamworkPermissions.task_cost_canRead) && !canManageExpense){
      throw new org.jblooming.security.SecurityException(SecurityConstants.I18N_PERMISSION_LACKING,TeamworkPermissions.task_cost_canRead);
    }



    boolean canReadBudget= assig.hasPermissionFor(logged, TeamworkPermissions.task_cost_canRead);
    boolean canWriteBudget= assig.hasPermissionFor(logged, TeamworkPermissions.task_cost_canWrite);
    canManageExpense= canManageExpense || isYours ||  canWriteBudget;


    /* ---------------------------------  CONTROLLER START -------------------------------------------------*/

    QueryHelper qh = new QueryHelper("select distinct cost from " + Assignment.class.getName() + " as ass inner join ass.costs as cost where ass.id=:id and cost.estimatedCost=0 order by cost.creationDate desc");
    qh.addParameter("id",assig.getId());

    boolean filterOn=false;
    filterOn=filterOn||ActionUtilities.addQBEClause("DATE", "cost.creationDate", "creationDate", qh, QueryHelper.TYPE_DATE, pageState);
    filterOn=filterOn||ActionUtilities.addQBEClause("DESCRIPTION", "cost.description", "descr", qh, QueryHelper.TYPE_CHAR, pageState);
    filterOn=filterOn||ActionUtilities.addQBEClause("COST", "cost.realCost", "rc", qh, QueryHelper.TYPE_DOUBLE, pageState);
    filterOn=filterOn||ActionUtilities.addOQLClause("CLASSIFIC", "cost.classification.id", "cid", qh, QueryHelper.TYPE_INT, pageState);

    filterOn=filterOn | DesignerField.queryCustomFields("COST_CUSTOM_FIELD_",4,"cost",qh, pageState);

    List<Cost> costs= (List<Cost>) qh.toHql().list();

    /* ---------------------------------  CONTROLLER END -------------------------------------------------*/



%><div id="taskAssignmentCost">
<div style="float:right;margin: 10px 20px;"><%
  //PageSeed print = pageState.getNewInstance();
  //print.href="taskAssignmentCostsPrint.jsp";
  ButtonJS printB = new ButtonJS(I18n.get("PRINT"), "print();");
  //printB.target="_blank";
  printB.iconChar="p";
  printB.label="";
  printB.toolTip=I18n.get("PRINT");
  printB.toHtmlInTextOnlyModality(pageContext);
%></div>

<div class="taskAssignmentCosts">
  <h1><%=I18n.get("ASSIG_EXPENSES")%></h1>

  <div style="float: right;display: inline-block;">
    <%
      ButtonJS addcb = new ButtonJS("openExpenseEditorPopup($(this),{assId:"+assig.getId()+"},saveCostCallback);");
      addcb.iconChar = "P";
      addcb.label = I18n.get("ADD_EXPENSE");
      addcb.additionalCssClass ="first";
      addcb.enabled=canManageExpense;
      addcb.toHtml(pageContext);
    %></div>
  <h2><%=assig.getDisplayNameFull()%></h2>
  <%

  PageSeed pageSeed = pageState.thisPage(request);
  pageSeed.mainObjectId=pageState.mainObjectId;
  Form f = new Form(pageSeed);
  f.alertOnChange=true;
  f.start(pageContext);

    if (canReadBudget){
  %>
  <table class="table">
    <tr>
      <td style="font-size: 14px;height: 40px;"><%

        pageState.addClientEntryCurrency("budget",assig.getBudget());
        TextField  cs = TextField.getCurrencyInstance("budget");
        cs.label = I18n.get("PERSONAL_BUDGET");
        //cs.required=true;
        cs.readOnly=!canWriteBudget;
        cs.separator="&nbsp;";
        cs.script=" onblur='refreshCostsTotal()'; assid=\""+assig.getId()+"\"";
        cs.toHtml(pageContext);
        %>&nbsp;<%

        ButtonJS sab = new ButtonJS(I18n.get("SAVE"), "saveBudget($(this));");
        sab.id="SVBTN";
        sab.enabled=canWriteBudget;
        sab.additionalCssClass="first small";
        sab.toHtml(pageContext);

      %></td>
    </tr>
  </table><br>

  <%
    }

    //----------------------------------------------------------------------------  START FILTER ----------------------------------------------------------------------------

  %> <div class="filterBar clearfix">
  <div class="filterElement"><%
    DateField df = new DateField("DATE");
    df.separator = "<br>";
    df.size=20;
    df.preserveOldValue=false;
    df.setSearchField(true);
    df.toHtmlI18n(pageContext);
  %></div><div class="filterElement"><%
  TextField tfName = new TextField("TEXT", I18n.get("DESCRIPTION"), "DESCRIPTION", "<br>", 40, false);
  tfName.searchField = true;
  tfName.preserveOldValue=false;
  tfName.toHtml(pageContext);
%></div><div class="filterElement"><%
  TextField cs = null;
  cs = TextField.getCurrencyInstance("COST");
  cs.label = I18n.get("COST_DONE");
  cs.searchField=true;
  cs.separator="<br>";
  cs.preserveOldValue=false;
  cs.toHtml(pageContext);
%></div><div class="filterElement"><%
  Combo costClass = Combo.getLookupInstance("CLASSIFIC", I18n.get("COST_CLASSIFICATION"), CostClassification.class);
  costClass.separator = "<br>";
  costClass.id = "";
  costClass.cvl.addAsFirst("0", "- " + I18n.get("EDITOR_CHOOSE") + " -");
  costClass.preserveOldValue=false;
  costClass.toHtml(pageContext);
%></div><%

  if (Cost.hasCustomField()) {
    for (int i = 1; i < 5; i++) {
      DesignerField dfStr = DesignerField.getCustomFieldInstance("COST_CUSTOM_FIELD_", i, null, false, false, true, pageState);
      if (dfStr != null) {
        dfStr.separator = "<br>";
        dfStr.preserveOldValue=false;
%><div class="filterElement"><%dfStr.toHtml(pageContext);%></div><%
      }
    }
  }



%><div class="filterButtons"><div class="filterButtonsElement filterSearch"><%
  new ButtonSubmit("SEARCH", Commands.FIND,f).toHtmlI18n(pageContext);
%></div></div></div><%


  f.end(pageContext);

  //----------------------------------------------------------------------------  END FILTER ----------------------------------------------------------------------------


%>



    <table assId="<%=assig.getId()%>" class="table dataTable fixHead fixFoot">
      <thead><tr>
        <th nowrap class="tableHead lreq30 lreqHide" width="40px"><%=I18n.get("STATUS")%></th>
        <th nowrap class="tableHead"><%=I18n.get("DATE")%> </th>
        <th nowrap class="tableHead" align="right" width="100px"><%=I18n.get("COST_DONE")%> </th>
        <th nowrap class="tableHead"><%=I18n.get("DESCRIPTION")%> </th>
        <th nowrap class="tableHead"><%=I18n.get("COST_CLASSIFICATION")%> </th>
        <th nowrap class="tableHead" align="center"><%=I18n.get("ATTACHMENT")%> </th>

        <%
          if (Cost.hasCustomField()) {
            for (int i = 1; i < 5; i++) {
              if (I18n.isActive("COST_CUSTOM_FIELD_" + i)) {
        %><th nowrap class="tableHead"><%=I18n.get(I18n.get("COST_CUSTOM_FIELD_" + i).split(",")[0])%></th><%
            }
          }
        }
      %>
        <th nowrap style="text-align: center; width:60px;" class="tableHead noprint">&nbsp;</th>
      </tr>
      </thead>
      <tbody id="costRows">
      <%
        for (Cost cost: costs){
          JspHelper costrow = new JspHelper("/applications/teamwork/task/financial/partTaskAssignmentCostRow.jsp");
          costrow.parameters.put("cost", cost);
          costrow.parameters.put("assig", assig);
          costrow.toHtml(pageContext);
        }

      %></tbody>
      <tfoot>
      <tr class="data totals">
        <td colspan="2" align="right"><%=I18n.get("TOTAL")%></td>
        <td colspan="1" id="totals" align="right"><span class="total"></span></td>
        <td>
          <%if (canReadBudget && !filterOn){%>
          (<%=I18n.get("BUDGET_DELTA")%>:&nbsp;<span class="delta"></span>)
          <%}%>
        </td>
        <td colspan="99"></td>
      </tr>
      </tfoot>
    </table>


</div>

<%
  f.end(pageContext);

%></div>

<script type="text/javascript">
$(":input").updateOldValue();

$(function(){
  refreshCostsTotal();
});


function editCost(el){
  var row=el.closest("[costId]");
  var request={costId:row.attr("costId"),assId:"<%=assig.getId()%>"};
  openExpenseEditorPopup(el,request,saveCostCallback);
}


function saveCostCallback(cost){
  //console.debug("saveCostCallback",cost);
  if (cost){
    $.get("partTaskAssignmentCostRow.jsp?costId="+cost.id+"&assId=<%=assig.getId()%>",function(row){
      row=$(row);
      var ndo= $("#costRows");
      var tr=ndo.find("tr[costId="+cost.id+"]");
      if (tr.size()>0)
        tr.replaceWith(row);
      else
        ndo.prepend(row);
      refreshCostsTotal();
      row.effect("highlight", { color: "#F9EFC5" }, 1500);
    });
  }
}

function deleteCost(el) {
  var tr = el.closest("tr");
  var costId = parseInt(tr.attr("costId"));
  var request = {
    CM:   "DLASSCOST",
    costId:tr.attr("costId"),
    assId:"<%=assig.getId()%>"
  };
  $.getJSON('costAjaxController.jsp', request, function (response) {
    jsonResponseHandling(response);
    if (response.ok) {
      tr.fadeOut(500, function () {
        $(this).remove();
        refreshCostsTotal();
      });
    }

  });
}

function saveBudget() {
  //console.debug("saveBudget");
  //budget is changed?
  var bdg = $("#budget");
  if (bdg.isValueChanged() && bdg.isFullfilled()) {
    showSavingMessage();
    var request = {
      CM:     "SVBDG",
      assId:  bdg.attr("assid"),
      budget: bdg.val()
    };
    $.getJSON('costAjaxController.jsp', request, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        bdg.updateOldValue();
        refreshCostsTotal();
      }
      hideSavingMessage();
    });
  }
}


function closeWin(){
  getBlackPopupOpener().showFeedbackMessage("OK", "<%=I18n.get("COSTS_CORRECTLY_SAVED")%>");
  closeBlackPopup();

}

function refreshCostsTotal(){
  var tot=0;
  $(".realCost").each(function(){
    tot+=getCurrencyValue($(this).text());
  });

  $("#totals .total").html(formatCurrency(tot));

  var bdg= $("#budget");
  if (bdg.size()>0){
    var delta=getCurrencyValue(bdg.val())-tot;
    $(".delta").html(formatCurrency(delta)).css("color",delta>0?"black":"red");
  }
}

function rowChangeStatus(el){
  showSavingMessage();
  var theRow = el.closest("tr[costId]");
  var costId = theRow.attr("costId");
  var assId=theRow.closest("[assId]").attr("assId");
  //console.debug("rowChangeStatus",wlId,el.val());
  var req = {"CM":"COSTCHSTS",assId:assId,costId:costId,csSts:el.val()};

  $.getJSON("costAjaxController.jsp", req, function(response) {
    jsonResponseHandling(response);
    if (response.ok) {
      //do nothing

    }
    hideSavingMessage();
  });

}



</script>
<%
  }
%>
