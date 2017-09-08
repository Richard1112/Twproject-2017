<%@ page import="org.jblooming.waf.settings.I18n" %>
<%----------------------------------------------------------  PAGES DEFINITION  ---------------------------------------------------------%>



<%-- -----------------------------------  document filter page ---------------------------------------%>
<div data-role="page" id="documentFilter" title="<%=I18n.get("DOCUMENTS")%>">
  <div data-role="content">

    <div class="searchBox"><input type="text" placeHolder="<%=I18n.get("SEARCH")%> ..." onblur="documentSearch('SEARCH',$(this).val());" onkeypress="if(event.keyCode==13)this.blur();" class="search"></div>

    <div class="buttonList">
      <button onclick="fillFileStorageList();" class="full first"><%=I18n.get("FILESTORAGE_MENU")%></button>
      <button onclick='documentSearch("PF_DOCUMENTS_RECENTLY_CHANGED",$(this).html());' class="full"><%=I18n.get("PF_DOCUMENTS_RECENTLY_CHANGED")%></button>
    </div>
  </div>
  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col12">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
  </div>
</div>

<%-- -----------------------------------  document list page ---------------------------------------%>
<div data-role="page" id="documentList" title="<%=I18n.get("DOCUMENT_LIST")%>">
  <div data-role="content">
    <div id="documentsPlace"></div>
  </div>
  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col12">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
  </div>
</div>

<%-- -----------------------------------  document view page ---------------------------------------%>
<div data-role="page" id="documentView">
  <div data-role="content">
    <div id="documentViewPlace"></div>
  </div>
  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col12">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
  </div>
</div>

<%-- -----------------------------------  file storages page ---------------------------------------%>
<div data-role="page" id="fileStorages" title="<%=I18n.get("FILESTORAGE_MENU")%>">
  <div data-role="content" class="scroll">
    <div id="fileStoragesPlace"></div>
  </div>
  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col12">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
  </div>
</div>


<%-- -----------------------------------  explorer page ---------------------------------------%>
<div data-role="page" id="explorer" title="<%=I18n.get("FILESTORAGE_MENU")%>">
  <div data-role="content" class="scroll">
    <div id="explorerPlace"></div>
  </div>
  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col12">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
  </div>
</div>


<%----------------------------------------------------------  TEMPLATES  ---------------------------------------------------------%>
<div class="_mobileTemplates">

  <%-- ---------------------------------  FS_ROW template ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("FS_ROW")%>
  <div>

  <div class="listRow fileStorage" FSId="(#=id#)" onclick="navigateFileStorage('(#=id#)','')">
    <h2>(#=obj.code#) (#=obj.name#)</h2>
  </div>
  </div>
  <%=JST.end()%>

  <%=JST.start("FS_DIR")%>
  <div class="listRow fileStorage" path="(#=obj.path#)" style="font-weight:300;color:#666;" onclick="navigateFileStorage('(#=fsId#)',$(this).attr('path'))">
    <div class="docMime"><img src="../img/mime/(#=img#)" width="20"></div>
    <div>(#=obj.name#)</div>
  </div>
  <%=JST.end()%>

  <%=JST.start("FS_FILE")%>
  <a href="(#=obj.downloadUrl#)">
    <div class="listRow fileStorage">
      <div class="docMime"><img src="../img/mime/(#=img#)" width="20"></div>
      <div>(#=obj.name#) <small>((#=objectSize(obj.length)#))</small></div>
    </div>
  </a>
  <%=JST.end()%>

  <%-- ---------------------------------  PERSISTENT FILE template ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("PERS_FILE")%>
  <a href="(#=url#)" target="_blank">
    <div class="listRow fileStorage">
      <div class="docMime"><img src="../img/mime/(#=img#)" width="20"></div>
       <div>(#=obj.name#)</div>
    </div>
  </a>
  <%=JST.end()%>

  <%-- ---------------------------------  DOCUMENT_ROW template ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("DOCUMENT_ROW")%>
  <div class="listRow document" docId="(#=id#)" onclick="viewDocument($(this))" style="padding: 10px">
    <div>
      <h2>(#=obj.name#) </h2>
      <span class="entityLog"><%=I18n.get("LAST_MODIFIED_BY")%> (#=obj.lastModifier#) - (#=dateToRelative(obj.lastModified)#)</span>
    </div>
    <div class="score"><%=I18n.get("SCORE")%>:(#=obj.score#)</div>
  </div>
  <%=JST.end()%>

  <%-- ---------------------------------  DOCUMENT_VIEW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("DOCUMENT_VIEW")%>
  <div class="editor document" docId="(#=id#)" docCode="(#=obj.code#)">

    <div class="docDataRow" style="(#=obj.taskId?'':'display:none'#)" taskId="(#=obj.taskId#)" onclick="viewTask($(this))">
      <div><label><%=I18n.get("TASK")%>:</label></div>
      <div>(#=obj.taskName#)</div>
    </div>

    <div class="docDataRow" style="(#=obj.resId?'':'display:none'#)" resourceId="(#=obj.resId#)" onclick="viewResource($(this))">
      <div><label><%=I18n.get("RESOURCE")%>:</label></div>
      <div>(#=obj.resName#)</div>
    </div>

    <div class="docDataRow" style="(#=obj.date?'':'display:none'#)">
      <div><label><%=I18n.get("DOCUMENT_AUTHORED")%>:</label></div>
      <div>(#=obj.date#)</div>
    </div>

    <div class="docDataRow" style="(#=obj.author?'':'display:none'#)">
      <div><label><%=I18n.get("DOCUMENT_AUTHOR")%>:</label></div>
      <div>(#=obj.author#)</div>
    </div>

   <div class="docDataRow" style="(#=obj.version?'':'display:none'#)">
     <div><label><%=I18n.get("DOCUMENT_VERSION")%>:</label></div>
     <div>(#=obj.version#) (#=obj.versionLbl#)</div>
   </div>

    <div class="docDataRow" style="(#=obj.url?'':'display:none'#);" >
      <a href="(#=obj.url#)" target="_blank">(#=obj.url#)</a>
    </div>


    <div class="docDataRow" style="(#=obj.content?'':'display:none'#)">
      <div><label><%=I18n.get("SUMMA")%>:</label></div>
      <div>(#=obj.content#)</div>
    </div>

    <div id="documentLinkPlace"></div>

    <br>

    <div style="padding: 10px"><span class="entityLog"><%=I18n.get("LAST_MODIFIED_BY")%> (#=obj.lastModifier#) - (#=dateToRelative(obj.lastModified)#)</span></div>
  </div>
  <%=JST.end()%>
</div>
<%----------------------------------------------------------  DECORATORS  ---------------------------------------------------------%>
<script>
  $.JST.loadDecorator("DOCUMENT_VIEW", function (domEl, document) {
    if (document.remoteFile) {
      domEl.find("#documentLinkPlace").append(getFileStorageElement(document.remoteFile));
    }

    if (document.persFile) {
      domEl.find("#documentLinkPlace").append($.JST.createFromTemplate(document.persFile, "PERS_FILE"));
    }

  });
</script>


<%----------------------------------------------------------  DOCUMENT PAGES FUNCTIONS  ---------------------------------------------------------%>
<script>
  function fileStoragesEnter(event, data, fromPage, isBack) {
    var ndo = $("#fileStoragesPlace");
    ndo.empty();
    if (applicationCache.fileStorages.length > 0) {
      for (var i in applicationCache.fileStorages) {
        var fs = applicationCache.fileStorages[i];
        ndo.append($.JST.createFromTemplate(fs, "FS_ROW"));
      }
    } else {
      ndo.append($.JST.createFromTemplate({}, "NO_ELEMENT_FOUND"));
    }
  }


  function explorerEnter(event, data, fromPage, isBack){
    var fs=getFSById(data.fsId);
    $("#explorer [data-role=title]").html(fs.name);

    var filter = {"CM": "EXPLOREFS","FSID":data.fsId,"PATH":data.path};
    callController(filter, function(response) {

      var ndo = $("#explorerPlace");
      ndo.empty();

      if (response.dirs && response.dirs.length > 0) {
        for (var i in response.dirs) {
          ndo.append(getFileStorageElement(response.dirs[i]));
        }
      }

      if (response.files && response.files.length > 0) {
        for (var i in response.files) {
          ndo.append(getFileStorageElement(response.files[i]));
        }
      }
    });
  }


  function documentListEnter(event, data, fromPage, isBack) {
    //console.debug("documentListEnter", data,isBack);
    if (isBack)
      return;
    var page = $(this);
    if ("SEARCH" == data.searchType)
      page.find("[data-role=title]").html("\"" + data.label + "\"");
    else
      page.find("[data-role=title]").html(data.label);

    var filter = {"CM": "DOCSEARCH", "SCHTYPE": data.searchType, "SEARCH": data.label,"TASKID":data.taskId};
    callController(filter, function (response) {
      var ndo = $("#documentsPlace");
      ndo.empty();

      if (response.documents && response.documents.length > 0) {
        for (var i in response.documents) {
          var doc = response.documents[i];
          ndo.append($.JST.createFromTemplate(doc, "DOCUMENT_ROW"));
          updateApplicationCacheElement(applicationCache.documents, doc);
        }
      } else {
        ndo.append($.JST.createFromTemplate({}, "NO_ELEMENT_FOUND"));
      }
    });
  }


  function documentViewEnter(event, data, fromPage, isBack) {
    var doc = getDocumentById(data.docId);
    $("#documentView [data-role=title]").html(doc.name);

    if (doc && doc.loadComplete) { // load from cache
      ndo = $("#documentViewPlace").empty().append($.JST.createFromTemplate(doc, "DOCUMENT_VIEW"));
    } else {
      var filter = {"CM": "LOADDOCUMENT", "ID": data.docId};
      callController(filter, function(response) {
        $("#documentViewPlace").empty().append($.JST.createFromTemplate(response.document, "DOCUMENT_VIEW", true));

        //update cache
        updateApplicationCacheElement(applicationCache.documents, response.document);
      });
    }
  }


  function documentSearch(searchType, label) {
    //console.debug("documentSearch: "+searchType+" "+label);

    if(!label || label.length == 0)
      return false;

    goToPage("documentList",{"searchType":searchType,"label":label});
  }



  function fillFileStorageList() {
    if (!applicationCache.fileStorages) {
      var filter = {"CM": "FILESTORAGES"};
      callController(filter, function (response) {
        applicationCache.fileStorages = response.fileStorages;
        fillFileStorageList();
      });
    } else {
      goToPage("fileStorages");
    }
  }


  function navigateFileStorage(fsId, path) {
    goToPage("explorer",{fsId:fsId,path:path});
  }

  function viewDocumentsForTask(el) {
    var taskId = el.closest("[taskId]").attr("taskId");
    var label="";
    goToPage("documentList",{"searchType":"TASK","label":label,"taskId":taskId});
  }


  function viewDocument(el) {
    var id = el.attr("docId");
    goToPage("documentView",{docId:id});
  }

  function getFSById(FSid) {
    for (i in applicationCache.fileStorages) {
      if (applicationCache.fileStorages[i].id == FSid)
        return applicationCache.fileStorages[i];
    }
    return false;
  }

  function getFileStorageElement(dirOrFile) {
    if (dirOrFile.isDirectory)
      return $.JST.createFromTemplate(dirOrFile, "FS_DIR");
    else
      return $.JST.createFromTemplate(dirOrFile, "FS_FILE");
  }


  function getDocumentById(docId) {
    for (i in applicationCache.documents) {
      if (applicationCache.documents[i].id == docId)
        return applicationCache.documents[i];
    }
    return false;
  }


  function deleteDocument(el) {
    el.confirm(function () {
      var ed = el.closest("[docId]");
      var docId = ed.attr("docId");
      var taskId = ed.attr("taskId");
      var task;
      if (taskId) {
        var task = getTaskById(taskId);
      }


      var filter = { "CM": "DLDOC", "DOCID": docId};

      callController(filter, function (response) {

        delete applicationCache.documents[docId];
        if (task) {
          for (i in task.documents) {
            if (task.documents[i].id == docId) {
              task.documents.splice(i, 1);
              updateApplicationCacheElement(applicationCache.tasks, task);
              break;
            }
          }
        }

        //remove issue row
        currentPage.find(".documentRow[docId=" + docId + "]").remove();
      });
    });
  }


</script>



