<%@ page import="net.sf.json.JSONObject,
                 org.jblooming.remoteFile.BasicDocumentBricks,
                 org.jblooming.remoteFile.Document,
                 org.jblooming.remoteFile.RemoteFile,
                 org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.file.FileUtilities,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.AHref,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.core.JST,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.display.Explorer,
                 org.jblooming.waf.html.display.Img,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.input.Uploader,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 java.io.File,
                 java.io.IOException,
                 java.util.ArrayList,
                 java.util.Date, java.util.List" %>
<%@ page pageEncoding="UTF-8" %>
<%!
  private void drawParent(RemoteFile parentFile, PageSeed ps, PageContext pc, PageState pst, String rootPath) throws IOException {
    if (!parentFile.isRoot())
      drawParent(parentFile.getParentFile(), ps, pc, pst,rootPath);
    pc.getOut().write("/");
    ps.setCommand(null);
    ps.addClientEntry("PATH", parentFile.getRelativePath());
    ps.setPopup(pst.isPopup());
    ButtonLink up = new ButtonLink("PARENT_FOLDER", ps);
    up.enabled=parentFile.getRelativePath().toLowerCase().replace(File.separatorChar, '/').startsWith(rootPath.toLowerCase().replace(File.separatorChar, '/'));
    up.label = parentFile.isRoot()?parentFile.document.getName() : parentFile.getDisplayName();
    up.toHtmlInTextOnlyModality(pc);
  }
%>
<style type="text/css">
  .checkDiv {
    position: absolute;
    left:0;
    bottom:0;
    width: 100%;
    text-align: right;
    background-color: #FCF4CF;
    border: 1px solid #FED331;
    padding: 2px 10px
  }

  .checkDiv table {
    border-collapse: collapse;
  }

  .divScroll {overflow:auto;height: 10px;}

  .colheaders{
    position: relative;
    font-size: 14px;
  }

</style>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  Explorer exp = (Explorer) JspIncluderSupport.getCurrentInstance(request);
  Document document = exp.document;
  RemoteFile rfs = exp.rfs;
  PageSeed psParent = pageState.thisPage(request);
  psParent.mainObjectId = pageState.mainObjectId;
  try{

  if (rfs.isDirectory()) {
    List<RemoteFile> files = rfs.listFiles();

%>
<h2><%=I18n.get("REMOTE_FILE")%>: <%drawParent(rfs, psParent, pageContext, pageState,exp.rootpath);%></h2>
<table id="headerTable" cellpadding="2" cellspacing="1" class="table" border="0">
<tr>
  <th class="tableHead"><%=I18n.get("FOLDERS")%></th>
  <th class="tableHead"><%=I18n.get("FILES")%></th>
</tr>
<tr>
<td valign="middle" width="30%"  style="background-color: #EEEEEE; padding: 0 10px">
  <div class="colheaders">


    <%if (exp.canCreateDirectory){%>
    <span class="button small" onclick="toggleAddFolder($(this));"><%=I18n.get("ADD_FOLDER")%></span>
    <%}%>

      <span id="mkdirArea" style="display: none;">
  <%
    if (rfs.canWrite()) {
      if (exp.canCreateDirectory) {
        TextField mdf = new TextField("DIR_NAME", "&nbsp;");
        //mdf.label = I18n.get("NAME");
        mdf.label = "";
        mdf.fieldSize = 15;
        mdf.script="style=' margin: 0 5px'";
        mdf.toHtml(pageContext);
        ButtonSubmit md = new ButtonSubmit(pageState.getForm());
        md.variationsFromForm.setCommand("MKDIR");
        md.label = I18n.get("CREATE");
        md.additionalCssClass = "small";
        //md.toHtmlInTextOnlyModality(pageContext);
        md.toHtml(pageContext);
      }
    }
  %></span>
  </div>

</td>
<td valign="middle" width="70%" style="background-color: #EEEEEE; padding: 0">
  <div style="position: relative; font-size: 14px;">

    <div id="dropFilesHere" style="float: right; width: 100%;height: 30px;" class="uploadizeDrop visible"></div>

    <div id="oldUpload" style="display: none;"><%
      TextField upl = new TextField("FILE", "UPLOAD_HERE", "&nbsp;", 20);
      upl.label = "";

      if (exp.canWrite)
        upl.toHtml(pageContext);

      ButtonSubmit upload = new ButtonSubmit(pageState.getForm());
      upload.variationsFromForm.setCommand("UPLOAD");
      upload.label = I18n.get("UPLOAD");
      upload.additionalCssClass="first";
      if (exp.canWrite)
        upload.toHtml(pageContext);
    %>

    </div>
  </div>
</td>
</tr>
</table>

<div id="multiSelActions" style="display: none; padding: 10px; background-color: #CBEAED; width: 100%; font-weight: bold">
  <%

    ButtonSubmit zip = new ButtonSubmit(pageState.getForm());
    zip.variationsFromForm.setHref(pageState.pageInThisFolder("partZip.jsp", request).getHref());
    zip.preserveFormStatus = true;
    zip.target = "_blank";
    zip.label = I18n.get("ZIP_SELECTED");
    zip.variationsFromForm.setCommand("ZIP");
    zip.toHtmlInTextOnlyModality(pageContext);

  %>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<%

  ButtonSubmit razz = new ButtonSubmit(pageState.getForm());
  razz.label = I18n.get("DEL_SELECTED");
  razz.variationsFromForm.setCommand(Commands.DELETE);
  razz.confirmRequire = true;
  razz.confirmQuestion = I18n.get("FS_CONFIRM_DELETE");

  if (exp.canWrite)
    razz.toHtmlInTextOnlyModality(pageContext);

%></div>

<table id="bodyTable" cellpadding="0" cellspacing="0" class="table" border="0">
<tr>
<td valign="top" width="30%" style="border-right:1px solid #999999;">

  <div class="divScroll" id="boxFolders">

    <table class="table">
      <%
        for (RemoteFile remoteFile : files) {
          if (remoteFile.isDirectory()) {
      %>
      <tr class="alternate" >
        <td>
          <table>
            <tr><%

            %> <td><%
              CheckField sel = new CheckField("FILE_" + remoteFile.getName(), "", true);
                sel.label = "";
                sel.id = JSP.normalize("FILE_" + remoteFile.getName());
                sel.toHtml(pageContext);
              %></td><td><%
                PageSeed p = BasicDocumentBricks.getPageSeedForExplorer(request, pageState, remoteFile, exp.document);
                AHref ahref=new AHref(remoteFile.getDisplayName(),p);

              %></td>
              <td>&nbsp;<%
                ahref.toHtml(pageContext);
              %>&nbsp;
                <small>(<b><%=DateUtilities.dateToString(new Date(remoteFile.lastModified()))%>
                </b>)
                </small>
              </td>
            </tr>
          </table>
        </td>
      </tr>
      <%
          }
        }
      %></table>
  </div>
</td>
<td valign="top" width="70%">


  <div class="divScroll" id="boxFiles">
  <table class="table" border="0"><%

    // files part
    int howManyFiles = 0;
    long totSize = 0;

    List<JSONObject> jsonFiles= new ArrayList<JSONObject>();

    for (RemoteFile remoteFile : files) {
      if (!remoteFile.isDirectory()) {
        jsonFiles.add(remoteFile.jsonify());
        howManyFiles++;
        totSize+=remoteFile.length();
      }
    }
  %></table>
  <p align="right">
    <i><%=I18n.get("TOTAL_FILES")%> <b><%=howManyFiles%>
    </b>&nbsp; - <%=I18n.get("TOTAL_SIZE")%> <b><%=FileUtilities.convertFileSize(totSize)%>
    </b></i>
  </p>
</div>
</td>
</tr>
</table>

<div id="rowTemplates" style="display: none;">
  <%=JST.start("FILEROW")%>
  <tr class="alternate"><td>
  <table border=0>
    <tr><%

      if (rfs.canWrite()) {

    %><td><%
      CheckField sel = new CheckField("FILE_" + "(#=obj.name#)", "", true);
      sel.label = "";
      sel.id = JSP.normalize("FILE_" + "(#=obj.name.asId()#)");
      sel.toHtml(pageContext);
    %></td><%
      }

    %><td><%
      new Img("mime/" + "(#=img#)", "").toHtml(pageContext);
    %></td><td>
      <a href="(#=obj.downloadUrl#)">(#=name#)</a>
      </td>
      <td>&nbsp;(<small>(#=objectSize(obj.length)#)</small></td>
      <td>&nbsp;<small>(#=new Date(obj.lastModified).format()#)</small>)</td>
    </tr>
  </table></td></tr>
  <%=JST.end()%>
</div>


  <script type="text/javascript">

    $(function () {
      $("#rowTemplates").loadTemplates().remove();
      $(".divScroll").height($(window).height() - $("#headerTable").height() - 120);

      $("#boxFolders :checkbox").click(multiselect);

      var files=<%=jsonFiles%>;
      //load files
      var ndo=$("#boxFiles table:first");
      for (var i=0;i<files.length;i++){
        addFileRow(files[i],ndo);
      }

      //activate uploader
      if(<%=exp.canWrite?"true":"false"%>){
        $("#dropFilesHere").uploadize({
          url:"<%=ApplicationState.contextPath%>/commons/layout/explorerAjaxController.jsp",
          fieldName:"UPLOAD_HERE",
          preview:false,
          fileAreaSelector:"#boxFiles",
          additionalRequestParameters:{CM:"UPLOAD",OBJID:"<%=pageState.mainObjectId%>",PATH:"<%=rfs.getRelativePath()%>"},
          maxSize:<%=Uploader.getMaxUploadSizeInByte()%>,
          onLoadCallback:function(response){
            var row = $.JST.createFromTemplate(response.remoteFile,"FILEROW");
            row.find(":checkbox").click(multiselect);
            $("#boxFiles table:first").prepend(row);
            row.find("td").effect("highlight", { color: "#F9EFC5" }, 1500);
          }
        });
      } else {
        $("#dropFilesHere").hide();
      }

      if ( !$("#dropFilesHere").is(":visible"))
        $("#oldUpload").show();
    });


    function addFileRow(jsonData,ndo){
      var row = $.JST.createFromTemplate(jsonData,"FILEROW");
      row.find(":checkbox").click(multiselect);
      ndo.append(row);
    }


    function multiselect(){
      if ($("#boxFolders :checkbox:checked,#boxFiles :checkbox:checked").size() > 0) {
        $("#multiSelActions").fadeIn();
      } else {
        $("#multiSelActions").fadeOut();
      }
    }

    function displayDivForUnzip(el,isDropBox) {
      //console.debug("displayDivForUnzip")

      var fileToCheck = el.val();
      var extFile = fileToCheck.substring(fileToCheck.length - 3, fileToCheck.length);

      if (extFile.toUpperCase() == 'ZIP')
        $("#checkUnzip").show();
      else
        $("#checkUnzip").hide();

      if ($(":input[name="+fileToCheck+"]").size()>0)
        $("#checkOverwrite").show();
      else
        $("#checkOverwrite").hide();

    }

    function objectSize(size) {
      var divisor = 1;
      var unit = "bytes";
      if (size >= 1024 * 1024) {
        divisor = 1024 * 1024;
        unit = "MB";
      } else if (size >= 1024) {
        divisor = 1024;
        unit = "KB";
      }
      if (divisor == 1)
        return size + " " + unit;

      return (size / divisor).toFixed(2) + ' ' + unit;
    }


    function toggleAddFolder (el){
      var mkda=$('#mkdirArea');

      if (mkda.is(":visible")){
        mkda.hide();
        el.html("<%=I18n.get("ADD_FOLDER")%>");
      } else {
        mkda.show();
        mkda.find(":input").focus();
        el.html("<%=I18n.get("RESET")%>");
      }

    }

  </script>


<%
  }

}catch (Throwable e){
%><table width="100%"style="padding-top:50px"><tr><td align="center"><h1><%=I18n.get("ERROR_CONTACTING_THE_REPOSITORY")%></h1></td></tr></table><%
  }


%>
