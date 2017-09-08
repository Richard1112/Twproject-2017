<%@ page import="org.jblooming.waf.settings.I18n, com.twproject.resource.Person" %>
<%----------------------------------------------------------  PAGES DEFINITION  ---------------------------------------------------------%>


<!-- -----------------------------------  resource list page -------------------------------------- -->
<div data-role="page" id="resourceList" title="<%=I18n.get("CONTACTS")%>">

  <div data-position="fixed" class="searchBox">
    <input type="text" placeHolder="<%=I18n.get("SEARCH")%> ..." onblur="resourceSearch($(this));" onkeypress="if(event.keyCode==13)this.blur();" class="search">
  </div>
  <div data-role="content" class="scroll">
    <div id="resourceListPlace"></div>

    <%if (pop3isConf){%>
    <div class="small entityLog"><%=I18n.get("CREATE_CONTACT_BY_SENDING_VCARD")%> <a href="mailto://<%=twEmail%>"><%=twEmail%></a></div>
    <%} %>

  </div>



  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell right inputBox col6">
      <button onmousedown="resourceEditor();" class="first addRootResource"><span class="teamworkIcon big">P</span></button>
    </div>
  </div>



</div>

<%-------------------------------------  resource editor page --------------------------------------%>
<div data-role="page" class="editor" id="resourceEditor" title="<%=I18n.get("RESOURCE")%>">
  <div data-role="header" data-position="fixed">
    <div data-role="button" onmousedown="backPage();" class="close" ></div>
    <div data-role="title"></div>
  </div>

  <div data-role="content" class="scroll">
    <div id="resourceEditorPlace"></div>
  </div>


  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell right inputBox col6">
      <button onmousedown="saveResource($(this));" class="save" ><%=I18n.get("SAVE")%></button>
    </div>
  </div>

</div>


<!-- -----------------------------------  resource view page -------------------------------------- -->
<div data-role="page" id="resourceView" title="<%=I18n.get("CONTACTS")%>">
  <div data-role="content" class="scroll">
    <div id="resourceViewPlace"></div>
  </div>

  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell right inputBox col6">
      <button onmousedown="resourceEditor();" class="edit" ><%=I18n.get("EDIT")%></button>
    </div>
  </div>

</div>


<%----------------------------------------------------------  TEMPLATES  ---------------------------------------------------------%>
<div class="_mobileTemplates">
  <%-- ---------------------------------  RESOURCE_ROW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("RESOURCE_ROW")%>
  <div data-role="swiper" class="listRow resources" resourceId="(#=id#)" onclick="viewResource($(this));">

    <table width="100%">
      <tr>
        <td width="45"><img id="userAvatar" title="TODO" class="face" src="(#=obj.avatarUrl#)">
        </td>
        <td>
          <h2>(#=displayName#)</h2>
        </td>

      </tr>
    </table>

<%-- Todo: add personal contacts data on the swiper
    <div class="swipeBox">
      <div class="swipeButton phone" style="(#=telephone ? '':'display:none'#)"><span class="teamworkIcon" onclick="self.location.href='tel:(#=telephone#)'">&</span></div>
      <div class="swipeButton mobile" style="(#=mobile ? '':'display:none'#)"><span class="teamworkIcon" onclick="self.location.href='tel:(#=mobile#)'">U</span></div>
      <div class="swipeButton mail" style="(#=email ? '':'display:none'#)"><span class="teamworkIcon" onclick="self.location.href='mailto:(#=email#)'">S</span></div>
      <div class="no-content-swiper" style="(#=telephone&&mobile&&email ?'display:none' : ''#)">No actions</div>
    </div>
--%>

  </div>
  <%=JST.end()%>


  <%-- ---------------------------------  RESOURCE_VIEW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("RESOURCE_VIEW")%>
  <div class="editor resource" resourceId="(#=id#)">

    <div class="resourceHeader">

    <table width="100%" cellpadding="0" cellspacing="0">
      <tr>
        <td width="65"><img src="(#=obj.avatarUrl#)" class="face big"></td>
        <td><h1>(#=displayName#)</h1></td>
      </tr>
    </table>
      <div class="personalDataPlace"></div>

      </div>





    <%-- ACCORDION --%>

    <div class="accordion">
      <h3>Assignments <span class="small" id="assigSize" style="(#=obj.assignments.length && obj.assignments.length>0?'':'display:none'#);">
            ((#=assignments.length#))</span>
      </h3>
      <div class="assigPlace"></div>
    </div>


  </div>
  <%=JST.end()%>

  <%-- ---------------------------------  ASSIG_RES_ROW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("ASSIG_RES_ROW")%>
  <div class="assigRow" assId="(#=id#)" taskId="(#=taskId#)" resourceId="(#=resId#)" onclick="viewTask($(this))">
    <table class="" width="100%">
      <tr>
        <td>
          <div style="width:190px" class="ellipsis">(#=taskName#)<br><span class="small">(#=roleCode#)</span></div>
        </td>
        <td align="right" valign="top" class="small (#=done>estimated?'warning':''#)"><%=I18n.get("WORKLOG_SHORT")%> (#=getMillisInHoursMinutes(done)#) / (#=getMillisInHoursMinutes(estimated)#)</td>
      </tr>
    </table>
  </div>
  <%=JST.end()%>

  <%-- ---------------------------------  PERSONAL_DATA_ROW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("PERSONAL_DATA_ROW")%>
  <div class="persDataRow" anagrId="(#=id#)" resourceId="(#=resId#)">
    <label>(#=location&&location!="-"?location:""#)</label>
    <blockquote>
      <a href="tel:(#=telephone#)" style="(#=obj.telephone?'':'display:none'#)"><span class="teamworkIcon">&amp;</span> (#=telephone#)</a>
      <a href="mailto:(#=email#)" style="(#=obj.email?'':'display:none'#)"><span class="teamworkIcon">S</span> (#=email#)</a>
      <a href="tel:(#=mobile#)" style="(#=obj.mobile?'':'display:none'#)"><span class="teamworkIcon">U</span> (#=mobile#)</a>
<%--
      <a href="fax:(#=fax#)" style="(#=obj.fax?'':'display:none'#)">(#=fax#)</a>
--%>
      (#=obj.address#) (#=obj.city#)
      (#=obj.state#) (#=obj.zip#) (#=obj.country#)
    </blockquote>
  </div>
  <%=JST.end()%>

    <%-- ---------------------------------  TASK_EDITOR TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
    <%=JST.start("RESOURCE_EDITOR")%>
    <div class="editor" resourceId="(#=obj.id#)">
      <div class="generalData">
    <div class="groupRow">
      <div class="groupCell inputBox touchEl col12"><%

        pageState.addClientEntry(OperatorConstants.FLD_NAME, "(#=obj.name#)");
        TextField resourceName = new TextField(OperatorConstants.FLD_NAME,"");
        resourceName.label="NAME";
        resourceName.fieldSize=30;
        resourceName.toHtmlI18n(pageContext);
%></div>
    </div>
        <div class="groupRow">
          <div class="groupCell inputBox touchEl col12"><%
        pageState.addClientEntry(OperatorConstants.FLD_SURNAME, "(#=obj.surname#)");
        TextField resourceSurname = new TextField(OperatorConstants.FLD_SURNAME,"");
        resourceSurname.label="FLD_SURNAME";
        resourceSurname.fieldSize=30;
        resourceSurname.required=true;
        resourceSurname.toHtmlI18n(pageContext);

        %></div></div>
      </div>
      <div class="ADEditor" idAd="(#=obj.ads_id#)">
    <div class="groupRow">
      <div class="groupCell inputBox touchEl col12"><%

        pageState.addClientEntry("email", "(#=obj.email#)");
        TextField resourceEmail = new TextField("email","");
        resourceEmail.label="EMAIL";
        resourceEmail.fieldSize=30;
        resourceEmail.toHtmlI18n(pageContext);
      %></div>
    </div>
        <div class="groupRow">
          <div class="groupCell inputBox touchEl col12"><%
        pageState.addClientEntry("mobile", "(#=obj.mobile#)");
        TextField resourceMob = new TextField("mobile","");
        resourceMob.label="mobile";
        resourceMob.fieldSize=30;
        resourceMob.toHtmlI18n(pageContext);
          %></div>
        </div>
        <div class="groupRow">
          <div class="groupCell inputBox touchEl col12"><%
        pageState.addClientEntry("telephone", "(#=obj.telephone#)");
        TextField resourceTel = new TextField("telephone","");
        resourceTel.label="telephone";
        resourceTel.fieldSize=30;
        resourceTel.toHtmlI18n(pageContext);

//        pageState.addClientEntry("PARENT_ID", "(#=obj.parent#)");
//        TextField resourceAddr = new TextField("PARENT_ID","");
//        resourceAddr.label="PARENT_ID";
//        resourceAddr.fieldSize=30;
//        resourceAddr.toHtmlI18n(pageContext);


          %></div>
        </div>
        <div class="groupRow">
          <div class="groupCell inputBox touchEl col12"><%

      pageState.addClientEntry("address", "(#=obj.address#)");
      resourceTel = new TextField("address","");
      resourceTel.label="address";
      resourceTel.fieldSize=30;
      resourceTel.toHtmlI18n(pageContext);
          %></div>
        </div>
        <div class="groupRow">
          <div class="groupCell inputBox touchEl col12"><%
      pageState.addClientEntry("location", "(#= obj.personalData[0].location#)");
      resourceTel = new TextField("location","");
      resourceTel.label="location";
      resourceTel.fieldSize=30;
      resourceTel.toHtmlI18n(pageContext);
          %></div>
        </div>
        <div class="groupRow">
          <div class="groupCell inputBox touchEl col12"><%
      pageState.addClientEntry("city", "(#=obj.personalData[0].city#)");
      resourceTel = new TextField("city","");
      resourceTel.label="city";
      resourceTel.fieldSize=30;
      resourceTel.toHtmlI18n(pageContext);
          %></div>
        </div>
        <div class="groupRow">
          <div class="groupCell inputBox touchEl col12"><%
      pageState.addClientEntry("province", "(#=obj.personalData[0].province#)");
      resourceTel = new TextField("province","");
      resourceTel.label="province";
      resourceTel.fieldSize=30;
      resourceTel.toHtmlI18n(pageContext);
          %></div>
        </div>
        <div class="groupRow">
          <div class="groupCell inputBox touchEl col12"><%
      pageState.addClientEntry("zip", "(#=obj.personalData[0].zip#)");
      resourceTel = new TextField("zip","");
      resourceTel.label="zip";
      resourceTel.fieldSize=30;
      resourceTel.toHtmlI18n(pageContext);
          %></div>
        </div>
        <div class="groupRow">
          <div class="groupCell inputBox touchEl col12"><%
      pageState.addClientEntry("country", "(#=obj.personalData[0].country#)");
      resourceTel = new TextField("country","");
      resourceTel.label="country";
      resourceTel.fieldSize=30;
      resourceTel.toHtmlI18n(pageContext);
          %></div>
        </div>
        <div class="groupRow">
          <div class="groupCell inputBox touchEl col12"><%
      pageState.addClientEntry("fax", "(#=obj.personalData[0].fax#)");
      resourceTel = new TextField("fax","");
      resourceTel.label="fax";
      resourceTel.fieldSize=30;
      resourceTel.toHtmlI18n(pageContext);
          %></div>
        </div>
        <div class="groupRow">
          <div class="groupCell inputBox touchEl col12"><%
      pageState.addClientEntry("url", "(#=obj.personalData[0].url#)");
      resourceTel = new TextField("url","");
      resourceTel.label="url";
      resourceTel.fieldSize=30;
      resourceTel.toHtmlI18n(pageContext);
          %></div>
        </div>
</div>
    </div>
    <%=JST.end()%>

</div>

<%----------------------------------------------------------  DECORATORS  ---------------------------------------------------------%>
<script>
  $.JST.loadDecorator("RESOURCE_VIEW", function (domEl, resource) {
    //add personal data

    var ndo = domEl.find(".personalDataPlace");
    for (var i in resource.personalData) {
      var pda = resource.personalData[i];
      pda.resId = resource.id;
      ndo.append($.JST.createFromTemplate(pda, "PERSONAL_DATA_ROW"));
    }
    //add assigs
    ndo = domEl.find(".assigPlace");
    for (var i in resource.assignments) {
      ndo.append($.JST.createFromTemplate(resource.assignments[i], "ASSIG_RES_ROW"));
    }
    if (!resource.canWrite) {
      $("#resourceView").find("button.edit").hide()
    } else {
      $("#resourceView").find("button.edit").show()
    }

  });
</script>


<%----------------------------------------------------------  RESOURCE PAGES FUNCTIONS  ---------------------------------------------------------%>
<script>

  function resourceListEnter(event, data, fromPage, isBack){
    var filter = {"CM": "RESOURCESEARCH"};

    var page = $(this);
    if (data && data.search)
      filter.SEARCH=data.search;

    callController(filter, function(response) {
      var resources=response.resources;
      var ndo = $("#resourceListPlace");
      ndo.empty();
      if (resources && resources.length > 0) {
        for (var i in resources) {
          var resource = resources[i];
          ndo.append($.JST.createFromTemplate(resource, "RESOURCE_ROW"));
          updateApplicationCacheElement(applicationCache.resources, resource);
        }

        //enable swipe action on list row
        enableSwipe();

      } else {
        ndo.append($.JST.createFromTemplate({}, "NO_ELEMENT_FOUND"));
      }
    });

    if(!applicationCache.user.canCreateResource){
      page.find(".addRootResource").remove();
    }
  }

  function resourceViewEnter(event, data, fromPage, isBack){
    var resource = getResourceById(data.resourceId);
    if (resource && resource.loadComplete) { // load from cache
      var ndo = $("#resourceViewPlace").empty().append($.JST.createFromTemplate(resource, "RESOURCE_VIEW"));

      $( ".accordion" ).accordion({
        heightStyle: "content",
        animate: 200,
        collapsible: true,
        active: false,
        activate: function (event, ui) {
          enableSwipe();
          refreshIscroll();
        }

      });

    } else {
      var filter = {"CM": "LOADRESOURCE", "ID": data.resourceId};
      callController(filter, function(response) {
        var ndo = $("#resourceViewPlace").empty().append($.JST.createFromTemplate(response.resource, "RESOURCE_VIEW"));

        $( ".accordion" ).accordion({
          heightStyle: "content",
          animate: 200,
          collapsible: true,
          active: false,
          activate: function (event, ui) {
            enableSwipe();
            refreshIscroll();
          }

        });

        //update cache
        updateApplicationCacheElement(applicationCache.resources, response.resource);

      });
    }
  }

  function resourceEditorEnter(event, data, fromPage, isBack){



    if(!data.personalData) {
      data.personalData = [{id:"new"+new Date().getTime(), order:0}];
    }
    $(this).find("#resourceEditorPlace").empty().append($.JST.createFromTemplate(data, "RESOURCE_EDITOR"));
  }


  function resourceEditorLeave(event, data, fromPage, isBack){
    $(this).find("#resourceEditorPlace").empty();
  }


  function resourceEditor(el) {
    var id = currentPage.find("div.resource").attr("resourceid");
    var res = {};

    if (id) {
      res = getResourceById(id);
    } else {
      res.id = -1;
    }
    goToPage("resourceEditor", res);
  }


  function resourceSearch(el) {
    goToPage("resourceList",{search:el.val()});
  }


  function viewResource(el) {
    goToPage("resourceView",{resourceId:el.attr("resourceId")});
  }

  function saveResource(el) {

    var ed = currentPage.find("#resourceEditorPlace");
    ed.find(":input").clearErrorAlert();

    var resourceId = ed.find("div.editor").attr("resourceId");

    var request = {"CM": "SAVERESOURCE"};

    if (canSubmitForm("resourceEditorPlace")) {

        var ads=[];
        currentPage.find(".ADEditor").each(function(order){
        var editor=$(this);
        var idAD=editor.attr("idAD");
        var fromEdi={};
        if(idAD){
          fromEdi.id=idAD;
        }else{
          fromEdi.id="new"+new Date().getTime();
        }
          fromEdi.order=0;
          editor.fillJsonWithInputValues(fromEdi);
          ads.push(fromEdi);

      });

      if (ads.length>0){
        request["ads"] = (JSON.stringify(ads));
      }
      currentPage.find(".generalData").fillJsonWithInputValues(request);


      if (resourceId != -1){
        request["OBJID"] = resourceId;
      }

      request["RESOURCE_TYPE"] = "<%=Person.class.getName()%>";

      callController(request, function (response) {
        if (response.ok) {

          updateApplicationCacheElement(applicationCache.resources, response.resource);
          backPage();
        }
      });
    }


  }



  function getResourceById(id) {
    for (i in applicationCache.resources) {
      if (applicationCache.resources[i].id == id)
        return applicationCache.resources[i];
    }
    return false;
  }


</script>



