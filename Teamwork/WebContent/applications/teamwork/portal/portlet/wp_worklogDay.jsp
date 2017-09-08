<%@ page import="com.twproject.resource.Person, com.twproject.task.TaskBricks, com.twproject.worklog.Worklog,com.twproject.worklog.businessLogic.WorklogBricks, net.sf.json.JSONArray, org.jblooming.agenda.CompanyCalendar, org.jblooming.oql.QueryHelper,
org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields,org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.table.ListHeader, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date, java.util.List" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  long focusMillis = pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
  focusMillis = focusMillis ==0?System.currentTimeMillis(): focusMillis;
  pageState.addClientEntry("FOCUS_MILLIS", focusMillis);

  CompanyCalendar cc = new CompanyCalendar(new Date(focusMillis));

  Person focusedResource = Person.getLoggedPerson(pageState);

  QueryHelper qhelp = WorklogBricks.getWorklogForDay(focusedResource,new Date(focusMillis));
  ListHeader.orderAction(qhelp, "WORKLH", pageState, "worklog.inserted");
  List<Worklog> wklg = qhelp.toHql().list();

  JSONArray worklogs=new JSONArray();
  if (wklg.size() > 0) {
    for (Worklog workLog : wklg) {
      worklogs.add(workLog.jsonify());
    }
  }



%><div id="wp_worklogDay" class="portletBox"><div style="float:right"><%

  ButtonJS bs = new ButtonJS();
  bs.onClickScript = "$('#configWorklogDay').toggle()";
  bs.iconChar="g";
  bs.label="";
  bs.toolTip=I18n.get("FILTER");
  bs.toHtmlInTextOnlyModality(pageContext);

  %></div><h1><%=I18n.get("MENU_WORKLOG_DAY")%></h1>


  <%-------------------------------------------------------  PARAMETERS  ----------------------------------------------------------------------------%>

  <div id="configWorklogDay" class="portletParams" style="display:none;">
    <table class="table"><tr><td><%
      boolean showTodays = pageState.getEntryOrDefault("WORKLOG_SHOW_TODAY", Fields.TRUE).checkFieldValue();
      CheckField cfTask = new CheckField("WORKLOG_SHOW_TODAY","&nbsp;",false);
      cfTask.additionalOnclickScript="refreshPortlet($(this),{'WORKLOG_SHOW_TODAY':$('#WORKLOG_SHOW_TODAY').val()});";
      cfTask.toHtmlI18n(pageContext);
    %><td></tr></table>
  </div>


  <table class="table">
    <tr>
      <td class="calHeader left">
        <span class="button textual today noprint" onclick="wpWLDGoToMillis(new Date().getTime())"><%=I18n.get("TODAY")%></span>
        <span class="button textual teamworkIcon" onclick="wpWLDGoToMillis(new Date(focusMillis).add('d',-1).getTime())" title="<%=I18n.get("PREVIOUS_DAY")%>">{</span>
        <span class="button textual teamworkIcon" onclick="wpWLDGoToMillis(new Date(focusMillis).add('d',1).getTime())" title="<%=I18n.get("NEXT_DAY")%>">}</span>
      </td>
      <td class="calHeader"><%
        cc.setTimeInMillis(focusMillis);
        boolean isToday = cc.isToday();
      %><h2 style="margin: 0" id="wpWLTIT"></h2>
      </td>

    </tr>
  </table>

  <%-------------------------------------------------------  TABLE  ----------------------------------------------------------------------------%>

  <table class="table" cellpadding="2" cellspacing="0" border="0">
    <tbody id="wlDayRowPlace">

    <%-------------------------------------------------------  TEMPLATES  ----------------------------------------------------------------------------%>

    <div id="wp_WLDayTemplates">
      <%=JST.start("WLDAYROW")%>
      <tr wlid="(#=obj.id#)" assId="(#=obj.assId#)" class="alternate">
        <td><%
          PageSeed assList = pageState.pageFromRoot("/task/worklog/worklogAssignmentList.jsp");
          //assList.addClientEntry("ASS_ID", "(#=obj.assId#)");
          assList.command= Commands.FIND;
          bs = new ButtonJS("openBlackPopup('"+assList.toLinkToHref()+"&ASS_ID=(#=obj.assId#)')");
          bs.label="";
          bs.iconChar="A";
          bs.toHtmlInTextOnlyModality(pageContext);

        %><td class="columnTaskName"><span>(#=obj.taskName#)</span></td><td class="textSmall columnTaskCode"><span>(#=obj.taskCode#)</span></td><td width="55%" align="center"><%

        TextField tf = new TextField("TEXT", "WLACT" , "",40);
        tf.label = "";
        tf.fieldClass="formElements";
        tf.addKeyPressControl(13,"$(this).blur();","onkeydown");
        tf.script = "style=\"width:100%;overflow:hidden;\" ";

        tf.toHtml(pageContext);

      %></td><td width="1%" align="center"><%
        pageState.addClientEntry("WLDUR", "(#=getMillisInHoursMinutes(obj.duration)#)");
        TextField wd = TextField.getDurationInMillisInstance("WLDUR");
        wd.separator="";
        wd.label = "";
        wd.fieldSize = 5;
        wd.fieldClass="formElements";
        wd.required=true;
        wd.addKeyPressControl(13,"$(this).blur();","onkeydown");
        wd.toHtml(pageContext);
      %></td><td><%
        ButtonJS delData = new ButtonJS("wpWLDDeleteWL($(this))");
        delData.confirmRequire = true;
        delData.confirmQuestion = I18n.get("CONFIRM_DELETE_WKL");
        delData.label="";
        delData.iconChar="d";
        delData.additionalCssClass="delete";
        delData.toHtmlInTextOnlyModality(pageContext);
      %></td>
      </tr>
      <%=JST.end()%>
    </div>

    </tbody>


    <%----------------------------------------------------------------------------------------- ADD  -------------------------------------------------------------------------%>
    <tr class="alternate highlight" id="wlDayLineEdit">
      <td width="45%" nowrap align="left" colspan="3"><%
        SmartCombo assignmentsCombo = TaskBricks.getAllAssignmentsOfCombo("ASSIG", focusedResource, true, null);
        assignmentsCombo.fieldSize = 30;
        assignmentsCombo.maxRowToFetch=40;
        assignmentsCombo.iframe_height=200;
        //assignmentsCombo.iframe_width=800;
        assignmentsCombo.innerLabel = I18n.get("WHICH_ASSIG");
        assignmentsCombo.label = "";
        assignmentsCombo.script = "style=\"width:98%;\" ";
        assignmentsCombo.required=true;
        assignmentsCombo.toHtml(pageContext);

      %></td><td width="55%"><%
        pageState.removeEntry("WLACT");
        TextField action = new TextField("WLACT", "");
        action.fieldSize = 40;
        action.label = "";
        action.innerLabel= I18n.get("WORKLOG_ACTION_WRITE_HERE");
        action.script = "style=\"width:100%;overflow:hidden;\" ";
        action.addKeyPressControl(13,"$(this).closest('tr').find('[name=WLDUR]').focus();","onkeydown");
        action.fieldClass="formElements";
        action.toHtmlI18n(pageContext);

      %></td><td align="left" nowrap><%

        pageState.removeEntry("WLDUR");
        TextField hid = TextField.getDurationInMillisInstance("WLDUR");
        hid.separator = "";
        hid.fieldSize = 5;
        hid.preserveOldValue = false;
        hid.label = "";
        hid.required=true;
        hid.minValue="0:01";
        //hid.maxValue="24:00";
        hid.addKeyPressControl(13,"wpWDAddWl($(this));","onkeydown");
        hid.fieldClass="formElements";
        hid.toHtml(pageContext);

      %></td><td align="center"><%
        ButtonJS svi = new ButtonJS ("wpWDAddWl($(this));");
        svi.iconChar="w";
        svi.label="";
        svi.toHtmlInTextOnlyModality(pageContext);
      %></td>
    </tr>

    <%-------------------------------------------------------  TOTAL  ----------------------------------------------------------------------------%>

    <tr class="totals">
      <td colspan="4" align="right"><%=I18n.get("WORKLOG_TOTAL_DAILY")%></td>
      <td id="wpWLDtotWorklog"  colspan="2"></td>
    </tr>
  </table>

</div>

<%
  // se la custom feature è abilitata non puoi mettere wl nel passato
  long notBeyond=Long.MIN_VALUE;
  if (I18n.isActive("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_WORLOG")) {
    int days = new ClientEntry("dummy", I18n.get("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_WORLOG")).durationInWorkingDaysNoErrorNoCatchedExc(false);
    notBeyond = days > 0 ? System.currentTimeMillis() - days * CompanyCalendar.MILLIS_IN_DAY : notBeyond;
  }

%>


<%-----------------------------------------------------------------------------------  JS  ----------------------------------------------------------------------------%>

<script type="text/javascript">
  var focusMillis=<%=focusMillis%>;
  var workable=0;
  var notBeyond=<%=notBeyond%>;

  (function startup(){

    //listen for worklogEvent
    registerEvent("worklogEvent.wpWoklogDay",function(e,data){
      wpWLDGoToMillis(focusMillis);
    });


    $("#wp_WLDayTemplates").loadTemplates().remove();
    $.JST.loadDecorator("WLDAYROW", function (row, wl) {
      row.find("#WLACT").val(wl.action);

      if (wl.insertedMillis && wl.insertedMillis >= notBeyond && wl.insertedMillis < new Date().getTime()+<%=CompanyCalendar.MILLIS_IN_WEEK%>) {
        row.find(":input").blur(function () {
          wlDSaveWl($(this));
        });
      } else {
        row.find(":input").prop("disabled", true); //disabilita le righe nel passato
        //row.find("span.delete").remove();
      }
    });
    wpWLDGoToMillis(focusMillis);

  })();


  function wpWLDDrawRows(response){
    //console.debug("wpWLDDrawRows",response);
    var focusDate = new Date(focusMillis);

    $("#wpWLTIT").html(focusDate.format(Date.masks.fullDate) + (focusDate.isToday()?" (<%=I18n.get("TODAY")%>)" : ""));

    var tbodyRows=$("#wlDayRowPlace").empty();

    for (var i=0;i<response.worklogs.length;i++){
      tbodyRows.append($.JST.createFromTemplate(response.worklogs[i], "WLDAYROW"));
    }
    workable=response.workable;
    wpWLDUpdateTotals();
  }

  function wpWLDGoToMillis(millis){
    focusMillis=millis;
    showSavingMessage();
    $.getJSON(contextPath + "/applications/teamwork/task/worklog/worklogAjaxController.jsp", {CM: "LISTWL1DAY", "FOCUS_MILLIS":focusMillis},function(response){
      jsonResponseHandling(response);
      if (response.ok) {
        wpWLDDrawRows(response);
      }
      hideSavingMessage();
    });

    //si disabilita la riga di input se troppo nel passato
    $("#wlDayLineEdit :input").attr("disabled",focusMillis<notBeyond)
  }

  function wpWLDUpdateTotals(){
    var tot=0;
    $("#wlDayRowPlace [name=WLDUR]").each(function(){
      var el=$(this);
      tot+=millisFromString(el.val(),true);
    });
    var diff=tot-workable;
    var diffStr = getMillisInHoursMinutes(tot - workable);
    $("#wpWLDtotWorklog").html(getMillisInHoursMinutes(tot) +((Math.abs(diff)<5?"":" ("+(diff>0?"+":"")+diffStr+")")));

    $(window).resize();

  }


  function wlDSaveWl(el){
    //console.debug("wlDSaveWl")
    var row=el.closest("[wlId]");
    var durField = row.find("[name=WLDUR]");
    //console.debug(durField.validateField())
    if (el.isValueChanged() && durField.validateField()) {
      showSavingMessage();
      var request = {CM: "SV",
        wlId: row.attr("wlId"),
        assId: row.attr("assId"),
        WORKLOG_DURATION: durField.val(),
        WORKLOG_ACTION: row.find("[name=WLACT]").val(),
        WORKLOG_INSERTIONDATE:new Date(focusMillis).format()
      };
      $.getJSON(contextPath + "/applications/teamwork/task/worklog/worklogAjaxController.jsp", request, function (response) {
        jsonResponseHandling(response);
        if (response.ok) {
          if (response.duration==0) {
            row.fadeOut(200, function () {
              $(this).remove();
              wpWLDUpdateTotals(workable);
              $("body").trigger("worklogEvent",[{type:"delete",response:response}])
            })
          } else {
            wpWLDUpdateTotals(workable);
            row.find(":input").updateOldValue();
            $("body").trigger("worklogEvent",[{type:"save",response:response}])

          }

        }
        hideSavingMessage();
      });
    }
  }

  function wpWDAddWl(el){
    //console.debug("wpWDAddWl")
    var row=el.closest("tr");
    if (row.find(":input").isFullfilled()) {
      showSavingMessage();
      var request = {CM: "SV",
        assId: row.find("[name=ASSIG]").val(),
        WORKLOG_DURATION: row.find("[name=WLDUR]").val(),
        WORKLOG_ACTION: row.find("[name=WLACT]").val(),
        WORKLOG_INSERTIONDATE:new Date(focusMillis).format()
      };

      //console.debug(request);
      $.getJSON(contextPath + "/applications/teamwork/task/worklog/worklogAjaxController.jsp", request, function (response) {
        jsonResponseHandling(response);
        if (response.ok) {
          row.find("[name=WLDUR]").val("");
          row.find("[name=WLACT]").val("");
          row.find(":input").updateOldValue();

          $("#wlDayRowPlace").append($.JST.createFromTemplate(response.worklog, "WLDAYROW"));

          wpWLDUpdateTotals(workable);

          $("body").trigger("worklogEvent",[{type:"save",response:response}])
        }
        hideSavingMessage();
      });
    }
  }



  function wpWLDDeleteWL(el){
    var row=el.closest("[wlId]");
    showSavingMessage();
    var request = {CM: "REMOVEWL", wlId: row.attr("wlId")};
    $.getJSON(contextPath + "/applications/teamwork/task/worklog/worklogAjaxController.jsp", request,function(response){
      jsonResponseHandling(response);
      if (response.ok) {
        row.fadeOut(200,function(){
          $(this).remove();
          wpWLDUpdateTotals(workable);
          $("body").trigger("worklogEvent",[{type:"delete",response:response}])

        })
      }
      hideSavingMessage();
    });
  }


  </script>
