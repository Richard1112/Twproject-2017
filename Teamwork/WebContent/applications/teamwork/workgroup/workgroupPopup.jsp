<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.waf.TeamworkPopUpScreen,
                 net.sf.json.JSONArray, org.jblooming.ontology.IdentifiableSupport, org.jblooming.oql.QueryHelper, org.jblooming.utilities.JSP,
                 org.jblooming.utilities.StringUtilities,org.jblooming.waf.Bricks,org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.core.JST,
  org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.LoadSaveFilter, org.jblooming.waf.html.input.TextField,
   org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.state.PersistentSearch,
    org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList,
     java.util.List, org.jblooming.waf.html.input.TagBox"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    // here because should be used in the default find
    pageState.getEntryOrDefault("HAVE_LOGIN",Fields.TRUE);

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    TeamworkOperator logged= (TeamworkOperator) pageState.getLoggedOperator();

    //-------------------------------- CONTROLLER START -------------------------------------------

    JSONArray resources = new JSONArray();
    String selectedIds = "";
    String candidateIds = "";


    // si carica il filtro di default
    /*if ( !JSP.ex(pageState.getCommand()))
      PersistentSearch.feedFromDefaultSearch("WORKGROUP", pageState);

    */


    //se viene passata una callbackFunction invece di fare il submit form del parent chiama la callback alla chiusura -> più JS oriented
    boolean useCallbackFunction=pageState.getEntry("useCallbackFunction").checkFieldValue();
    boolean enableTags=pageState.getEntry("enableTags").checkFieldValue();
    String title=pageState.getEntry("title").stringValueNullIfEmpty();

    title= JSP.ex(title)?title:I18n.get("WORK_GROUP");
    if (PersistentSearch.feedFromSavedSearch(pageState))
      title+=": "+pageState.getEntryOrDefault(Fields.FLD_FILTER_NAME).stringValue();

    List<String> ids = new ArrayList<String>();

    // rebuid selected ids
    String selIdsString = pageState.getEntry("WG_IDS").stringValueNullIfEmpty();
    if (JSP.ex(selIdsString)) {
      selectedIds = selIdsString;
      ids.addAll(StringUtilities.splitToList(selIdsString, ","));
    }


    //rebuild candidates
    String candIdsString = pageState.getEntry("WG_CAND_IDS").stringValueNullIfEmpty();
    if (JSP.ex(candIdsString)) {
      candidateIds = candIdsString;
      ids.addAll(StringUtilities.splitToList(candIdsString, ","));
    }

    //build resources json objects from ids
    if (JSP.ex(ids)) {
      String hql = "select distinct resource from " + Person.class.getName() + " as resource";
      QueryHelper qhelp = new QueryHelper(hql);
      qhelp.addOQLInClause("id", "ids", ids);
      List<Resource> ress = qhelp.toHql().list();
      for (Resource res : ress) {
        resources.add(res.jsonify(false));
      }


      // put yourself in selected
    } else {
      Person person = logged.getPerson();
      selectedIds = person.getId() + "";
      resources.add(person.jsonify(false));
    }

    //-------------------------------- CONTROLLER END -------------------------------------------


    PageSeed seed = pageState.thisPage(request);
    seed.setPopup(true);
    seed.setCommand(Commands.FIND);
    seed.addClientEntry(pageState.getEntry("PERM_REQUIRED"));
    seed.addClientEntry(pageState.getEntry("useCallbackFunction"));
    seed.addClientEntry(pageState.getEntry("enableTags"));
    seed.addClientEntry(pageState.getEntry("title"));

    Form form = new Form(seed);
    pageState.setForm(form);
    form.start(pageContext);


    pageState.getEntryOrDefault("HAVE_LOGIN",Fields.TRUE);
    CheckField cf = new CheckField("HAVE_LOGIN", "", false);

%>
<h2><%=title%></h2>

<table border="0" cellpadding="0" cellspacing="5">
<tr>
<td><%=I18n.get("PEOPLE")%>&nbsp;&nbsp;&nbsp;<%cf.toHtmlI18n(pageContext);%><%

  TextField tf = new TextField("","PEOPLE","<br>",30,false);
  tf.addKeyPressControl(13,"searchResources();","onkeydown");
  pageState.setFocusedObjectDomId(tf.id);
  tf.toHtmlI18n(pageContext);

  if (enableTags){
%><td>
  <%
    TagBox tags= new TagBox("RESOURCE_TAGS",Resource.class,logged.getPerson().getArea());
    tags.label=I18n.get("TAGS");
    tags.separator= "<br>";
    tags.fieldSize=25;
    tags.preserveOldValue=false;
    tags.toHtml(pageContext);
  %>
</td>
<%
  }
%>


  <td valign="bottom"><%


  ButtonJS search = new ButtonJS("searchResources();");
  search.label = I18n.get("FLD_SEARCH");
  search.additionalCssClass ="small";

%><%search.toHtml(pageContext);%></td></tr><%

     LoadSaveFilter lsf = new LoadSaveFilter(I18n.get("WANT_TO_SAVE_FILTER"), "WORKGROUP", pageState.getForm());

     %><tr><td style="font-size: 12px; padding-top: 10px"><%lsf.drawEditor(pageContext);%></td><td style="text-align: right; padding-top: 10px" class="filtersInline"><%lsf.drawButtons(pageContext);%></td>
</tr>
</table>

  <div style="margin-top: 20px">
    <%TextField.hiddenInstanceToHtml("WG_CAND_IDS",pageContext);%>
    <%TextField.hiddenInstanceToHtml("WG_IDS",pageContext);%>


  <table id="wgBoxExt" class="table dataTable edged" cellpadding="0" cellspacing="0">
    <thead class="wgBoxTitle">
    <tr>
      <th width="40%" class="tableHead"><%=I18n.get("WORKGROUP_CANDIDATES")%></th>
      <th width="10%" style="text-align: center;" class="tableHead"><span class="teamworkIcon textual" style="cursor:pointer;" onclick="$('#wgCandBox .wgResEl').appendTo('#wgSelBox');updateFields();return false;" title="<%=I18n.get("MOVE_ALL_TO_SELECTED_TITLE")%>">}</span> </th>
      <th width="10%" style="text-align: center;" class="tableHead"><span class="teamworkIcon textual" style="cursor:pointer;" onclick="$('#wgSelBox .wgResEl').appendTo('#wgCandBox');updateFields();return false;" title="<%=I18n.get("MOVE_ALL_TO_UNSELECTED_TITLE")%>">{</span> </th>
      <th width="40%" class="tableHead"><%=I18n.get("WORKGROUP_CHOSEN")%></th>
    </tr>
    </thead>
    <tr>
      <td colspan="2" width="50%"><div id="wgCandBox" class="wgBox"></div></td>
      <td colspan="2" width="50%"><div id="wgSelBox" class="wgBox"></div></td>
    </tr>
  </table>


  </div>

<table width="100%">
  <tr>
    <td align="left">
        <%
            ButtonSupport ins = new ButtonJS(useCallbackFunction?"useCallBack();":"submitParent();");
            ins.label = I18n.get("INSERT");
            ins.additionalCssClass = "first";
            ins.toHtml(pageContext);

      %>
    </td>
  </tr>
</table>
<%
      form.end(pageContext);

%>


<div id="wgTemplates" style="display:none">
  <%=JST.start("WG_RESOURCE")%>
  <div resId="##id##" class="wgResEl" ><img class="face" src="##avatarUrl##" align="middle"/> ##displayName##</div>
  <%=JST.end()%>
</div>


<script type="text/javascript" >

  var resources =<%=resources%>;
  var selectedIds = "<%=selectedIds%>"; //comma separated
  var candidateIds = "<%=candidateIds%>"; //comma separated

  $(function() {
    $("#wgTemplates").loadTemplates().remove();

    var candPlace = $("#wgCandBox");
    var selPlace = $("#wgSelBox");

    $.JST.loadDecorator("WG_RESOURCE",function(domEl,json){
      //draggable
      domEl.draggable({
        revert:"invalid",
        containment:"#wgBoxExt",
        helper: 'clone'
      });


      domEl.dblclick(function() {
        var el = $(this);
        var mb = el.closest(".wgBox");
        var ob=$("#"+(mb.prop("id")=="wgSelBox"?"wgCandBox":"wgSelBox"));
        ob.append(el);
        updateFields();
      });
    });

    function fillBox(box, idsString) {
      var idsArr = idsString.split(",");
      for (var i=0;i<idsArr.length;i++) {
        var id = idsArr[i];
        for (var j=0;j<resources.length;j++) {
          if (resources[j].id == id) {
            box.append($.JST.createFromTemplate(resources[j], "WG_RESOURCE"));
            break;
          }
        }
      }
    }
    fillBox(candPlace, candidateIds);
    fillBox(selPlace, selectedIds);

    //droppable
    $("#wgCandBox,#wgSelBox").droppable({
      accept:function(ui){ return !ui.parent().is($(this));},
      tolerance:"pointer",
      drop:function(ev, ui) {
        $(this).append(ui.draggable);
        ui.draggable.css({position:"relative",left:0,top:0});
        $("body").oneTime(50, "wgupdfld", updateFields); //this because during drop the element is stille there
      }
    });

  });


  function updateFields() {
    var ids = "";
    $("#wgCandBox .wgResEl").each(function() {
      ids += (ids.length > 0 ? "," : "") + $(this).attr("resId");
    });
    $("#WG_CAND_IDS").val(ids);

    ids = "";
    $("#wgSelBox .wgResEl").each(function() {
      ids += (ids.length > 0 ? "," : "") + $(this).attr("resId");
    });
    $("#WG_IDS").val(ids);
  }


  function searchResources() {
    //console.debug("searchResources");
    var search=$("#PEOPLE").val();
    var data={
      CM:"WGSRCPEOPLE",
      PEOPLE:search,
      RESOURCE_TAGS:$("#RESOURCE_TAGS").val(),
      PERM_REQUIRED:$("[name=PERM_REQUIRED]").val(),
      HAVE_LOGIN:$("#HAVE_LOGIN").val()
    };

    $.getJSON("<%=pageState.pageFromRoot("workgroup/wGAjaxController.jsp")%>", data, function(response) {
      jsonResponseHandling(response);
      if (response.ok) {
        var candBox = $("#wgCandBox");
        var selBox = $("#wgSelBox");
        candBox.empty();
        if ( response.resources.length==1){
          addResInMem(response.resources[0]);
          if ($("[resId="+response.resources[0].id+"]").size()==0 ){
            selBox.append($.JST.createFromTemplate(response.resources[0], "WG_RESOURCE"));
          }
          
        } else {
          for (var i=0;i<response.resources.length;i++){
            //if not already in selected
            var res = response.resources[i];
            addResInMem(res);

            if ($("[resId="+res.id+"]").size()==0 ){
              candBox.append($.JST.createFromTemplate(res, "WG_RESOURCE"));
            }
          }
        }
        updateFields();
      }
    });
  }


  function submitParent() {
    //console.debug("submitParent.");
    var targetWin = getBlackPopupOpener();

    //console.debug("form",targetWin,targetWin.$(targetWin).data("openerForm"));
    var selIdsStr = $("#WG_IDS").val();
    var parForm= targetWin.$(targetWin).data("openerForm");
    parForm.attr('alertOnChange', 'false');
    parForm.find("[name=WG_IDS]").val(selIdsStr);

    //get names
    var namesString="";
    var idsArr = selIdsStr.split(",");

    if(!idsArr.length || (idsArr.length == 1 && !getResourceById(idsArr[0])) ){
      showFeedbackMessage("ERROR", "<%=I18n.get("AT_LEAST_ONE_RESOURCE_REQUIRED")%>",null,3000);
      return;
    }
    for (var i=0;i<idsArr.length;i++) {
      var id = idsArr[i];
      namesString+= (namesString.length==0?"":";") + getResourceById(id).displayName;
    }

    parForm.find("[name=WG_NAMES]").val(namesString);
    //console.debug(selIdsStr,namesString);
    parForm.submit();
    closeBlackPopup()
  }


  function useCallBack(){
    var selIdsStr = $("#WG_IDS").val();
    var namesString="";
    var idsArr = selIdsStr.split(",");

    if(!idsArr.length || (idsArr.length == 1 && !getResourceById(idsArr[0])) ){
      showFeedbackMessage("ERROR", "<%=I18n.get("AT_LEAST_ONE_RESOURCE_REQUIRED")%>",null,3000);
      return;
    }

    var callbackData={};

    for (var i=0;i<idsArr.length;i++) {
      var id = idsArr[i];
      callbackData[id]=getResourceById(id).displayName;
    }
    closeBlackPopup(callbackData);
  }

  function getResourceById(id){
    var ret = null;
    for (var j=0;j<resources.length;j++) {
      if (resources[j].id == id) {
        ret=resources[j];
        break;
      }
    }
    return ret;
  }


  function addResInMem(res){
    if (!getResourceById(res.id))
      resources.push(res);
  }

</script>

<%
  }
%>
