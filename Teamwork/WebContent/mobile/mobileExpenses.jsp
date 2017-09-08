<%@ page import="org.jblooming.waf.settings.I18n, org.jblooming.waf.html.input.DateField, org.jblooming.waf.html.input.TextField, com.twproject.task.financial.FinancialBricks, org.jblooming.waf.html.input.SmartCombo, com.twproject.task.financial.Cost, org.jblooming.designer.DesignerField, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.html.input.Uploader, com.twproject.task.TaskBricks, java.util.Date, org.jblooming.agenda.Period" %>
<%----------------------------------------------------------  PAGES DEFINITION  ---------------------------------------------------------%>

<%-------------------------------------  expense List page ---------------------------------------%>
<div data-role="page" id="expenseList" title="<%=I18n.get("ASSIG_EXPENSES")%>">

  <table width="100%" cellspacing="0" cellpadding="0" class="worklogNav" data-position="fixed">
    <tr>
      <td width="10%"><div data-role="button" onclick="expenseMove(false);" class="prev full" title="<%=I18n.get("PREVIOUS")%>"></div></td>
      <td><div data-role="date"></div></td>
      <td width="10%"><div data-role="button" onclick="expenseMove(true);" class="next full" title="<%=I18n.get("NEXT")%>"></div></td>
    </tr>
  </table>
  <div data-role="content" class="scroll">
    <div id="expenseListPlace" width="100%"></div>
  </div>
  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell right inputBox col6">
      <button onmousedown="expenseEditor();" class="first"><span class="teamworkIcon big">P</span></button>
    </div>
  </div>
</div>

<%-------------------------------------  expense editor page --------------------------------------%>
<div data-role="page" id="expenseEditor" class="editor" title="<%=I18n.get("ADD_COST")%>">

  <div data-role="header" data-position="fixed">
    <div data-role="button" onmousedown="backPage();" class="close" ></div>
    <div data-role="title"></div>
  </div>

  <div data-role="content" class="scroll">
    <div id="expenseEditorPlace"></div>
  </div>


  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell right inputBox col6">
      <button onmousedown="saveExpense($(this));" class="save" id="saveExpButton"><%=I18n.get("SAVE")%></button>
    </div>
  </div>

</div>
<%----------------------------------------------------------  TEMPLATES  ---------------------------------------------------------%>
<div class="_mobileTemplates">

  <%-- ---------------------------------  EXPENSE_ROW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("EXPENSE_ROW")%>
  <div data-role="swiper" class="listRow" expId="(#=id#)" assId="(#=assId#)" taskId="(#=taskId#)">
    <%--<td width="5px"><span class="icon" style="color:(#if(obj.status){#)(#=obj.status.color#)(#}else{#)#FFCC33;(#}#)">&copy;</span></td>--%>
    <div taskId="(#=taskId#)">
      <h2>(#=obj.taskName#)</h2>
      (#=obj.description#)
    </div>
    <div attachment style="float: right">
      (#if(obj.attachment){#)
      <a href="(#=obj.attachment.url#)" target="_blank"><span class="teamworkIcon filesIcon" style="font-size: 150%">n</span></a>
      (#}#)
    </div>

    <div style="width:80px; font-size:1.5em;" align="right">(#=formatCurrency(obj.realCost)#)</div>
    <%-- SWIPE BOX --%>
    <div class="swipeBox">
      (#if(!obj.readOnly){#)
      <%--<button class="teamworkIcon medium delete" (#=obj.readOnly?'disabled':''#) msg="<%=I18n.get("FLD_CONFIRM_DELETE")%>" onclick="deleteExpense($(this));" >d</button>--%>

      <%--todo: add actions--%>
      <div class="swipeButton"><span class="teamworkIcon delete" msg="<%=I18n.get("FLD_CONFIRM_DELETE")%>" onclick="deleteExpense($(this));">d</span></div>

      (#}#)

    </div>
  </div>
  <%=JST.end()%>
  <%-- ---------------------------------  EXPENSE_TOTAL TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("EXPENSE_TOTAL")%>
  <div class="listRow">
    <div style="font-size:0.9em;"><i><%=I18n.get("TOTAL")%></i></div>
    <div style="width:80px; font-size:1.5em;font-weight: 600">(#=formatCurrency(expTot)#)</div>
  </div>
  <%=JST.end()%>
  <%-- ---------------------------------  EXPENSE_EDITOR TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("EXPENSE_EDITOR")%>
  <div class="editor" costId="(#=obj.id#)" assId="(#=obj.assId#)">

    <div class="groupRow">
      <div class="groupCell inputBox touchEl col12">
        <%

          pageState.addClientEntry("expAssId", "(#=obj.assId#)");
          pageState.addClientEntry("expAssId_txt", "(#=obj.taskName#)");
          SmartCombo expAssigs = TaskBricks.getAllAssignmentsOfCombo("expAssId", logged.getPerson(), Period.getWeekPeriodInstance(new Date(),logged.getLocale()), false, false, false, false, null);
          expAssigs.label = I18n.get("TASK");
          expAssigs.separator = "";
          expAssigs.fieldSize = 45;
          expAssigs.required=true;
          expAssigs.toHtml(pageContext);
        %>
      </div>
    </div>
    <div class="groupRow">
      <div class="groupCell inputBox touchEl col12">
        <label for="doneOn"><%=I18n.get("DATE")%> </label>
        <input type="text" id="doneOn" name="doneOn" value="(#=currentDate.format()#)" readonly class="noFocus" onmousedown="this.blur();expOpenCalendar($(this))">
        <span class="teamworkIcon calendarOpener" onmousedown="expOpenCalendar($(this))">m</span>
      </div>
    </div>
    <div class="groupRow">
      <div class="groupCell inputBox touchEl col12">
        <%
          TextField tf = new TextField(I18n.get("DESCRIPTION"), "description", "", 40, false);
          tf.required=true;
          tf.separator="";
          tf.script="style=width:100%";
          tf.toHtml(pageContext);
        %>

      </div>
    </div>

    <div class="groupRow">
      <div class="groupCell inputBox touchEl col6">
        <%
          SmartCombo costClass = FinancialBricks.getCostClassificationCombo("expClassification", null, pageState);
          costClass.separator = "";
          costClass.id = "";
          costClass.label=I18n.get("COST_CLASSIFICATION");
          costClass.iframe_width=0;
          costClass.addEntityButton=null;
          costClass.toHtml(pageContext);


          Cost cost=null;
          if (Cost.hasCustomField()) {
            for (int i = 1; i < 5; i++) {
              pageState.removeEntry("COST_CUSTOM_FIELD_" +i);
              DesignerField dfStr = DesignerField.getCustomFieldInstance("COST_CUSTOM_FIELD_", i, cost, false, false, false, pageState);
              if (dfStr != null) {
                dfStr.separator = "";
                dfStr.toHtml(pageContext);
              }
            }
          }
        %>

      </div>
      <div class="groupCell inputBox touchEl col6">
        <%
          TextField cs = TextField.getCurrencyInstance("realCost");
          cs.type="NUMBER";
          cs.label = I18n.get("COST_DONE");
          cs.required=true;
          cs.separator="";
          cs.toHtml(pageContext);
        %>
      </div>
    </div>

    <div class="groupRow">
      <div class="groupCell inputBox col12">
        <label><%=I18n.get("ATTACHMENT")%> </label>
        <div style="padding: 15px">
          <div id="holder" (#=obj.readOnly?"disabled":""#)  class="uploadizeDrop picture" ></div>
        </div>
      <input type="hidden" name="PENDING_PF" id="PENDING_PF">
    </div>
  </div>


</div>
<%=JST.end()%>


<%-- ---------------------------------  FILEBOX template ------------------------------------------------------------------------------------------------------------------- --%>
<%=JST.start("FILEBOX")%>
<div class="repoFileBox" >
  <img src="(#=contextPath+'/img/mime/'+obj.img#)" style="height:20px;">&nbsp;<span title="<%=I18n.get("PREVIEW_DOWNLOAD")%>">(#=obj.name#)</span>
</div>
<%=JST.end()%>


<%-- ---------------------------------  DOCUMENT_EXPENSES_ROW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
<%=JST.start("DOCUMENT_EXPENSES_ROW")%>
<div class="documentRow" docId="(#=obj.id#)" taskId="(#=obj.taskId#)">
  <a onclick="viewDocument($(this))"   docId="(#=obj.id#)" target="_blank"><img src="(#=contextPath+'/img/mime/'+obj.img#)" style="height:20px;">&nbsp;<span title="<%=I18n.get("PREVIEW_DOWNLOAD")%>">(#=obj.name#)</span></a>
  <div class="delete"><span class="teamworkIcon delete" onclick="deleteDocument($(this));">d</span></div>
</div>
</div>
<%=JST.end()%>


<%-- ---------------------------------  NO_EXPENSES FOUND ------------------------------------------------------------------------------------------------------------------- --%>
<%=JST.start("NO_EXPENSES")%>
<div class="noElementFound">
  <%=I18n.get("NO_EXPENSES")%>
</div>
<%=JST.end()%>

</div>

<%----------------------------------------------------------  DECORATORS  ---------------------------------------------------------%>
<script>
  $.JST.loadDecorator("EXPENSE_ROW", function(row, cost) {
    //se c'è un allegato si fa il bind del click all'open aleggato
    if(cost.attachment){
      row.find("attachment").data("attachment",cost.attachment).click(function(){

      })
    }
  });


  $.JST.loadDecorator("EXPENSE_EDITOR", function(editor, cost) {

    editor.find("#holder").uploadize({
      fieldName:"attachment",
      url:"../applications/teamwork/task/financial/costAjaxController.jsp",
      maxSize:<%=Uploader.getMaxUploadSizeInByte()%>,
      multi:  false,
      showPlaceHolder:true,
      additionalRequestParameters: {CM: "COSTATTACH", costId: "-1"},
      onStartProcess:function(){$("#saveExpButton").prop("disabled",true)},
      onLoadCallback:function(response){
        if (response.ok){
          //in caso di nuovo costo mette da una parte i pf temporanei
          if (true) {
            $("#PENDING_PF").val(response.attachment.uid);
          }
         // currentPage.find(".documentsPlace").append($.JST.createFromTemplate(response.attachment, "DOCUMENT_EXPENSES_ROW"));

          $("#holder").append($.JST.createFromTemplate(response.attachment,"FILEBOX")).removeClass("uploadizeDrop").attr("disabled",true);
          $("#saveExpButton").prop("disabled",false)
        }
      },
      fileAreaSelector:"#holder"
    });
  });

  $.JST.loadDecorator("FILEBOX", function (box, pf) {
    //console.debug(box,pf)
    box.data("pf",pf);

    /*
     //bind open image/download file
     box.click(function (ev) {
     ev.stopPropagation();
     openPersistentFile($(this).data("pf"));
     });*/

    //console.debug("box.data",box.data("pf"))
    //delete file
    box.find(".del").click(function(ev){
      ev.stopPropagation();
      var el = $(this);
      var fb = el.closest(".repoFileBox");

      var editor=el.closest(".editor");
      fb.confirm(function(){
        var box=$(this);
        showSavingMessage();
        var request = {CM: "DELCOSTATTACH", costId: editor.attr("costId"),assId:$("#expAssId").val()||editor.attr("assId"), fileUID: box.data("pf").uid};
        $.getJSON("../applications/teamwork/task/financial/costAjaxController.jsp", request, function (response) {
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
</script>


<%----------------------------------------------------------  EXPENSE PAGES FUNCTIONS  ---------------------------------------------------------%>
<script>
  var maxCostAgeInDays=<%=new ClientEntry("dummy",I18n.get("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_COSTS")).durationInWorkingDaysNoErrorNoCatchedExc(false)%>;

  function expenseEditorEnter(event, data, fromPage, isBack){
    if(!data){
      data ={};
    }
    var editor = $.JST.createFromTemplate(data, "EXPENSE_EDITOR");
    $(this).find("#expenseEditorPlace").empty().append(editor);
    if(data.assId){
      wlAssigSelected(currentPage.find("#wlAssId"))
    }
  }
  function expenseEditorLeave(event, data, newPage, isBack){
    //console.debug("expenseEditorLeave",data);
    $(this).find("#expenseEditorPlace").empty();
  }

  function expenseListEnter(event, data, fromPage, isBack){
    //console.debug("expenseListEnter",data);
    fillExpenseDay();
  }


  function fillExpenseDay(){
    //console.debug("fillExpenseDay",currentDate);

    var page=$("#expenseList");
    currentDate=currentDate||new Date();

    var dateInt=currentDate.toInt();
    var monthExpenses = applicationCache.expenses[parseInt(dateInt / 100)];

    //console.debug("fillExpenseDay",currentDate,monthExpenses)

    if (!monthExpenses) {
      var filter = {"CM": "LOADEXPENSES", "DATEINT": dateInt};
      callController(filter, function(response) {

        //update cache
        var month = new Object();
        for (var day in response.expenses) {
          var wls = response.expenses[day];
          month[day] = wls;
        }
        applicationCache.expenses[parseInt(dateInt / 100)] = month;

        //call itself
        fillExpenseDay();
      });

    } else {
      //set title
      page.find("[data-role=date]").html(currentDate.format(mobileFullDateFormat));

      //draw rows
      var dayExpenses = monthExpenses[dateInt];
      var ndo = $("#expenseList #expenseListPlace");
      ndo.empty();

      var notOlderThan=0;
      var readOnly=false;
      if (maxCostAgeInDays>0){
        var nb=new Date();
        nb.setDate(nb.getDate()-maxCostAgeInDays);
        notOlderThan=nb.getTime();
        readOnly=currentDate.getTime()<notOlderThan;
      }

      // page.find("[addButton]").prop("disabled",readOnly);

      if (dayExpenses && dayExpenses.length > 0) {
        var expTot = 0;
        for (var i in dayExpenses) {
          var exp = dayExpenses[i];
          expTot += exp.realCost;
          exp.readOnly=readOnly || exp.status;

          ndo.append($.JST.createFromTemplate(exp, "EXPENSE_ROW"));
        }

        ndo.append($.JST.createFromTemplate({expTot:expTot}, "EXPENSE_TOTAL"));


        //enable swipe action on list row
        enableSwipe();



      } else {
        ndo.append($.JST.createFromTemplate({}, "NO_EXPENSES"));
      }
    }
  }


  function expenseMove(forward) {
    currentDate.setDate(currentDate.getDate() +(forward ? 1 : -1));
    fillExpenseDay();
  }


  function expenseEditor(el) {
    goToPage("expenseEditor");
  }


  function saveExpense(el) {
    var ed = currentPage.find("#expenseEditorPlace");

    if (ed.find(":input").isFullfilled()) {
      showSavingMessage();

      var request = {"CM": "SVASSCOST"};
      ed.fillJsonWithInputValues(request);

      //si copiano le CE locali su quelli che servono per il controller
      request.assId=request.expAssId;
      request.classification=request.expClassification;
      request.creationDate=request.doneOn;

      request.costId = "-1";

      showSavingMessage();
      $.getJSON("../applications/teamwork/task/financial/costAjaxController.jsp", request, function(response) {
        jsonResponseHandling(response);
        if (response.ok) {

          //update cache
          var cost = response.cost;

          currentDate = new Date(cost.creationMillis);
          var dateInt = currentDate.toInt();
          var monthExpenses = applicationCache.expenses[parseInt(dateInt / 100)];
          if (monthExpenses) {
            var expDay = monthExpenses[dateInt];
            if (!expDay) {
              expDay = [];
              monthExpenses[dateInt] = expDay;
            }
            expDay.push(cost);
          }

          backPage();
        }
        //hideSavingMessage();
      });


    }
  }


  function deleteExpense(el){
    el.confirm(function(){
      var row = el.closest("[expId][assId]");
      var expId = row.attr("expId");
      var assId = row.attr("assId");
      var request = { "CM": "DLASSCOST", costId:expId,assId:assId};
      //console.debug("deleteExpense",request);

      $.getJSON("../applications/teamwork/task/financial/costAjaxController.jsp", request, function(response) {
        jsonResponseHandling(response);
        if (response.ok) {
          //update cache
          var dateInt = currentDate.toInt();
          var dayExpenses = applicationCache.expenses[parseInt(dateInt / 100)][dateInt];
          for  (var i in dayExpenses) {
            if (dayExpenses[i].id==expId){
              dayExpenses.splice(i,1);
              break;
            }
          }
          fillExpenseDay();
        }
        hideSavingMessage();
      });
    },"<%=I18n.get("FLD_CONFIRM_DELETE")%>");
  }


  function expOpenCalendar(el){
    var inp = el.closest("[data-role=page]").find("#doneOn");
    var config = {
      inputField:     inp,
      centerOnScreen:true,
      useYears:       1,
      useMonths:      3,
      showToday:      true,
      notAfterMillis: new Date().getTime() + 3600000 * 24,
      width:          "100%"
    };
    if (maxCostAgeInDays>0)
      config.notBeforeMillis=new Date()-3600000*24*maxCostAgeInDays;
    inp.dateField(config);
  }

</script>



