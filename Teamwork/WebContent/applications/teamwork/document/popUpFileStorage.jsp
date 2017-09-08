<%@ page import=" com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.oql.QueryHelper, org.jblooming.remoteFile.BasicDocumentBricks, org.jblooming.remoteFile.FileStorage, org.jblooming.remoteFile.RemoteFile,
                 org.jblooming.security.Area, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.utilities.file.FileUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.display.Img, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, javax.servlet.jsp.PageContext,
                 java.io.IOException, java.util.ArrayList, java.util.Date, java.util.List, java.util.Set" %>
<%@ page pageEncoding="UTF-8" %>
<%!
  private void drawParent(RemoteFile parentFile, PageState pageState, PageContext pc) throws IOException {
    boolean isRoot = parentFile.isRoot();
    if (!isRoot)
      drawParent(parentFile.getParentFile(), pageState, pc);
    pc.getOut().write(" / ");
    PageSeed self = pageState.thisPage((HttpServletRequest) pc.getRequest());
    self.command = "EXPLORER";
    self.addClientEntry("PATH", parentFile.getRelativePath());
    self.mainObjectId = pageState.mainObjectId;
    self.addClientEntry(pageState.getEntry("OPENER_FIELD_ID"));

    pc.getOut().print("<a class='button textual' href=\"" + self.toLinkToHref() + "\">" + (isRoot?parentFile.document.getName():parentFile.getDisplayName()) + "</a>");
  }
%>

<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    body.areaHtmlClass="lreq30 lreqPage";
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

  } else {
    String fieldId=pageState.getEntry("OPENER_FIELD_ID").stringValueNullIfEmpty();

    %>

<style>
  .dirFileLink{
    word-break: break-all;
    white-space: pre-line;
  }
  .dirFileLink small{
    font-size: 70%;
    font-weight: bold;
  }
  .divScroll{
    overflow: auto;
    height: calc(100vh - 230px);
  }
</style>

  <%
    String currentlySelectedFile="__none__";

    //  ----------------------------------------- se non arriva un mainObject siamo nel caso della selezione del file storage
    if (!JSP.ex(pageState.mainObjectId)){
      // file storages lists
      String hql = "from " + FileStorage.class.getName() + " as document";
      QueryHelper qh = new QueryHelper(hql);
      TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
      List<Area> alt = new ArrayList<Area>();
      Set<Area> al = logged.getAreasForPermission(TeamworkPermissions.fileStorage_canRead);
      if (al != null)
        alt.addAll(al);
      al = logged.getAreasForPermission(TeamworkPermissions.fileStorage_canWrite);
      if (al != null)
        alt.addAll(al);
      if (alt.size() > 0)
        qh.addOQLClause("document.area in (:al)", "al", alt);
      List<FileStorage> fsdocs = qh.toHql().list();
%>

<h2 id="rfHead">    <%=pageState.getI18n("SELECT_FILE_STORAGE")%></h2>
<div class="divScroll">

<table border="0" class="table filebox" >
  <tr>
    <th class="tableHead"><%=I18n.get("NAME")%></th>
    <th class="tableHead"><%=I18n.get("CODE")%></th>
    <th class="tableHead"><%=I18n.get("TYPE")%></th></tr>
  <%

    for (FileStorage docu : fsdocs) {

      RemoteFile docRF = RemoteFile.getInstance(docu);
      if (docu.hasPermissionFor(logged, TeamworkPermissions.fileStorage_canRead) && docRF.exists()) {
  %>
  <tr  class="alternate" data-filename="<%=docRF.getName()%>">
    <td><b>
      <%
        PageSeed explorer = new PageSeed(ApplicationState.contextPath + "/applications/teamwork/document/popUpFileStorage.jsp");
        explorer.mainObjectId = docu.getId();
        explorer.addClientEntry("OPENER_FIELD_ID",fieldId);
        ButtonLink bl= new ButtonLink(docu.getName(),explorer);
        bl.toHtmlInTextOnlyModality(pageContext);
      %>
    </b>
    </td>
    <td nowrap><%=JSP.w(docu.getCode())%></td>
    <td nowrap><%=I18n.get(docu.getConnType().toString())%></td>
  </tr>
  <%
      }
    }
    if (!JSP.ex(fsdocs)){
      %><tr><td colspan="3"><%=pageState.getI18n("FILESTORAGE_SEES_NONE")%></td></tr><%
    }

%></table></div><%


  // ----------------------------------------- abbiamo scelto un FS quindi si listano directories e files
} else {

  FileStorage document = FileStorage.load(pageState.mainObjectId + "");
  RemoteFile rfsROOT = RemoteFile.getInstance(document);

  RemoteFile rfs = RemoteFile.getInstance(document);
  String path = pageState.getEntry("PATH").stringValueNullIfEmpty();

  if (path != null)
    rfs.setTarget(path);

  currentlySelectedFile="RF" + document.getId() + ":" +JSP.javascriptEncode(rfs.getRelativePath());


  //se partimo puntando su un file deve fare vedere il folder non il file
  if (rfs.exists() && !rfs.isDirectory()) {
    rfs=rfs.getParentFile();
  }


  PageSeed self = pageState.thisPage(request);
  self.command = "EXPLORER";
  self.mainObjectId = pageState.mainObjectId;
  self.addClientEntry(pageState.getEntry("OPENER_FIELD_ID"));

  self.mainObjectId = document.getId();


  if (rfs.exists() && rfs.isDirectory()) {
    List<RemoteFile> files = rfs.listFiles();


    PageSeed selFS = pageState.thisPage(request);
    selFS.command = "EXPLORER";
    selFS.addClientEntry(pageState.getEntry("OPENER_FIELD_ID"));
    ButtonLink selBL = new ButtonLink(I18n.get("FILESTORAGE_LIST"), selFS);
%>
<h2 id="rfHead"><%selBL.toHtmlInTextOnlyModality(pageContext);%></a>: <%drawParent(rfs, pageState, pageContext);%></h2>

<table cellpadding="2" cellspacing="0" class="table" id="rfSelector">
<tr>
  <tr>
    <th class="tableHead" id="folderHead">Folders</th>
    <th class="tableHead" id="fileHead">Files</th>
  </tr>

<td valign="top" width="30%" style="border-right:1px solid #999999;">

  <div class="divScroll">
    <table class="table filebox">

      <%
      String rootRelPath = rfsROOT.getRelativePath();
      for (RemoteFile remoteFile : files) {
        if (remoteFile.exists() && remoteFile.isDirectory() ) {

          self.addClientEntry("PATH", remoteFile.getRelativePath());
          //String relativePath = StringUtilities.replaceAllNoRegex(remoteFile.getRelativePath().substring(rootRelPath.length()), File.separator, "/");
          String rowId = "RF" + document.getId() + ":" +JSP.javascriptEncode(remoteFile.getRelativePath());
    %>
      <tr class="alternate folderRow" data-filename="<%=rowId%>" >
        <td><input type="radio" name="fssel"></td>
        <td class="dirFileLink"><a href="<%=self.toLinkToHref()%>"><%=remoteFile.getDisplayName()%></a>&nbsp;<small>(<%=DateUtilities.dateToString(new Date(remoteFile.lastModified()))%>)</small> </td>

      </tr>
      <%
          }
        }
      %>
    </table>
  </div>
</td>
<td valign="top" width="70%">
  <div class="divScroll" >

    <table class="table" >

        <%
          // files part
          int howManyFiles = 0;
          long totSize = 0;
          for (RemoteFile remoteFile : files) {
            if (!remoteFile.isDirectory()) {
              totSize = totSize + remoteFile.length();
              howManyFiles++;

              PageSeed down = BasicDocumentBricks.getPageSeedForExplorer(request, pageState, remoteFile, document);
              //String relativePath = StringUtilities.replaceAllNoRegex(remoteFile.getRelativePath().substring(rootRelPath.length()), File.separator, "/");
              String rowId = "RF" + document.getId() + ":" +JSP.javascriptEncode(remoteFile.getRelativePath());

              %>
              <tr class="alternate fileRow" data-filename="<%=rowId%>">
              <td width="1%"><input type="radio" name="fssel"></td>
              <td width="1%"><%new Img ("mime/" + remoteFile.getImageName(), "").toHtml(pageContext);%></td>
              <td class="dirFileLink"><a href="<%=down.toLinkToHref()%>"><%=remoteFile.getDisplayName()%></a> &nbsp;&nbsp;<small>(<%=FileUtilities.convertFileSize(remoteFile.length())%>&nbsp; <%=DateUtilities.dateToString(new Date(remoteFile.lastModified()))%>)</small></td>
            </tr>
            <%
          }
        }
      %>
    </table>
    <p align="right">
      <i><%=pageState.getI18n("TOTAL_FILES")%> <b><%=howManyFiles%>
      </b>&nbsp; - <%=pageState.getI18n("TOTAL_SIZE")%> <b><%=FileUtilities.convertFileSize(totSize)%>
      </b></i>
    </p>
  </div>
</td>
</tr>
</table>
<%
    ButtonBar bb = new ButtonBar();
    ButtonJS select= new ButtonJS(I18n.get("EDITOR_CHOOSE"),"selectAndClose();");
      select.additionalCssClass="first";
    bb.addButton(select);
    bb.toHtml(pageContext);

  }
  }
    %>

<script>
  $(window).bind("resize", function() {
    resize();
  });

  $(function(){
    resize();
    $("[data-filename='<%=currentlySelectedFile%>']").find(":input").focus().prop("checked",true);
  });


  //inject the table search
  createTableFilterElement($("#folderHead"),"tr.folderRow",".dirFileLink");
  createTableFilterElement($("#fileHead"),"tr.fileRow",".dirFileLink");


  function resize(){
    //console.debug("resize",$(window).height());
    $(".divScroll").height($(window).height() - $("#rfHead").height()-190);
  }

  function selectAndClose(){
    var selectedRadio=$(":radio:checked");
    if (selectedRadio.size()==1) {
      var openerWin = getBlackPopupOpener();
      var topInput=openerWin.$("#<%=fieldId%>");
      var topDocNameField=openerWin.$("#DOCUMENT_NAME");

      var selectedRow = selectedRadio.closest("[data-filename]");
      topInput.val(selectedRow.attr("data-filename"));
      if(!topDocNameField.val())
        topDocNameField.val(selectedRow.find(".dirFileLink a").text());
      closeBlackPopup();
    }
  }

</script>


<%
  }
%>