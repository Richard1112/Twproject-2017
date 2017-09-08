<%@ page import="com.twproject.waf.TeamworkPopUpScreen,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.html.display.Img,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.view.PageState, org.jblooming.waf.settings.I18n, org.jblooming.oql.OqlQuery, java.util.List, java.util.ArrayList, org.jblooming.waf.html.container.TabSet, org.jblooming.waf.html.container.Tab, org.jblooming.waf.html.input.ColorValueChooser, java.util.Map, java.util.Hashtable, org.jblooming.waf.constants.Fields, com.twproject.waf.TeamworkHBFScreen, com.twproject.task.*, org.jblooming.agenda.CompanyCalendar, org.jblooming.waf.html.button.ButtonJS, net.sf.json.JSONArray, org.jblooming.utilities.DateUtilities, org.jblooming.ontology.PersistentFile, org.jblooming.waf.html.core.JST" %>
<%@page pageEncoding="UTF-8" %><%
  PageState pageState = PageState.getCurrentPageState(request);


  //--------------------------------------  CONTROLLER --------------------------------




  pageState.setPopup(true);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    lw.menu = null;
    pageState.toHtml(pageContext);
  } else {

    //si controlla subito se la chiave è buona se non lo è si esce subito zitti-zitti
    String key=pageState.getEntry("key").stringValue();
    String requesterEmail=StringUtilities.generateEmailFromKey(key);
    if (!JSP.ex(requesterEmail)){
      return;
    }



    Img logo = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");

%>

<div style="display: none;" id="iltmplt">
  <%=JST.start("FILEBOX")%>
  <div class="repoFileBox" ><span class="button textual" title="<%=I18n.get("PREVIEW_DOWNLOAD")%>">(#=obj.name#)</span></div>
  <%=JST.end()%>
</div>

<div id="twMainContainer">
<div id="twInnerContainer">

  <table border="0" cellpadding="5" cellspacing="0" class="table noprint">
    <tr>
      <td align="left" width="180"><%logo.toHtml(pageContext);%></td>
      <td align="right" valign="bottom"><h1><%=requesterEmail%></h1></td>
    </tr>
  </table>

  <%

    // si prendono tutte le issue manadate dal requester sui task attivi
    String oql="select issue from "+ Issue.class.getName()+" as issue where issue.task.status='STATUS_ACTIVE' and issue.extRequesterEmail=:extEmail order by issue.lastStatusChangeDate desc";
    OqlQuery query = new OqlQuery(oql);
    query.setParameter("extEmail",requesterEmail);
    List<Issue> allIss= query.list();

    if (!JSP.ex(allIss)){
      %><h1><%=I18n.get("NO_REQUESTS")%></h1><%
      return;
    }


     %><h1><%=I18n.get("YOUR_TICKETS")%>: <%=allIss.size()%></h1><%


  //si smistano
    List<Issue> openIssues=new ArrayList();
    List<Issue> closedIssues=new ArrayList();

    for (Issue iss:allIss){
      if (iss.getStatus().isBehavesAsOpen())
        openIssues.add(iss);
      else // in effetti potrebbero non essere ne aperte ne chiuse ed in tal caso dove si mettono? tra le chiuse
        closedIssues.add(iss);

    }


  //si prepara il tabset
  Map<Tab,List<Issue>> pageData=new Hashtable();
  TabSet tabSet = new TabSet("ticketsTS", pageState);

  if (openIssues.size()>0) {
    Tab openTAB = new Tab("openTAB", IssueStatus.getStatusOpen().getDescription());
    tabSet.addTab(openTAB);
    pageData.put(openTAB,openIssues);
  }

  if (closedIssues.size()>0) {
    Tab closedTAB = new Tab("closedTAB", IssueStatus.getStatusClose().getDescription());
    tabSet.addTab(closedTAB);
    pageData.put(closedTAB,closedIssues);
  }

  //si da il focus al primo tab
  tabSet.tabs.get(0).focused=true;

  tabSet.drawBar(pageContext);

  for (Tab tab:tabSet.tabs) {
    tab.start(pageContext);
    List<Issue> issues = pageData.get(tab);
    %>
      <table border="0" cellpadding="5" cellspacing="3" class="table">
        <tr>
          <th class="tableHead" colspan="2"><%=I18n.get("STATUS")%></th>
          <th class="tableHead" >&nbsp;</th>
          <th class="tableHead" ><%=I18n.get("DESCRIPTION")%></th>
          <th class="tableHead" style=""><%=I18n.get("ISSUE_COMMENTS")%></th>
          <th class="tableHead" style="min-width:150px; "><%=I18n.get("ISSUE_TASK")%></th>
          <th class="tableHead" width="180"><%=I18n.get("RESOURCE")%></th>
          <th class="tableHead" style="width:120px;"><%=I18n.get("ISSUE_CLOSE_BY_SHORT")%></th>
          <th class="tableHead" style="width:120px;"><%=I18n.get("CREATED_ON")%></th>
        </tr>
        <%for (Issue issue:issues) {
          ColorValueChooser cvc = issue.bricks.getStatusChooser("dummy", "is", false, false, pageState);
          cvc.readOnly=true;
          cvc.height=20;
          cvc.width=20;
          cvc.displayValue=false;
          cvc.showOpener = false;
          Task task = issue.getTask();
          boolean isPublic= task.getJsonData().has("publicPage")&& Fields.TRUE.equals(task.getJsonData().getJSONObject("publicPage").get("MASTER_PUBLIC_TASK"));
        %>
        <tr issueId="<%=issue.getId()%>">
          <td valign="top" width="10" nowrap><span class="teamworkIcon" style="color: <%=issue.getStatus().getColor()%>">©</span></td>
          <td valign="top" nowrap><%=issue.getStatus().getDescription()%></td>
          <td valign="top"  class="tikDescr"><small>I#<%=issue.getMnemonicCode()%>#</small></td>
          <td valign="top"  class="tikDescr">
            <%=JSP.encode(issue.getDescription())%>


          <div class="filesBox textSmall"><span class="teamworkIcon filesIcon" style="display:none;">n</span></div><%
            //------------------------------------------------------- FILES ---------------------------------------------------------------------
            JSONArray files= new JSONArray();
            for (PersistentFile pf:issue.getFiles())
              files.add(pf.jsonify());
          %>

            <script>$(function(){drawFiles("<%=issue.getId()%>",<%=files%>)});</script>


          </td>
          <td>
            <% // -----------------------  COMMENTS --------------------------
              List<IssueHistory> issueComments = issue.getComments();

              ButtonJS comments = new ButtonJS("showComments($(this));");
              comments.toolTip = I18n.get("ISSUE_COMMENTS") + ": " + (JSP.ex(issueComments) ? issueComments.size() : 0);
              comments.additionalCssClass = "small";
              comments.label = issueComments.size() + "";
              comments.iconChar="Q";
            %><span class="textSmall"><%comments.toHtmlInTextOnlyModality(pageContext);%></span><%

            //mostra l'ultimo commento se non è del logged ed è più recente di 2 settimane
            //mostra l'ultimo commento se  è più recente di 2 settimane
            if (JSP.ex(issueComments)) {
              IssueHistory issueHistory = issueComments.get(issueComments.size() - 1);
              if (JSP.ex(issueHistory.getComment()) &&
                !requesterEmail.equals(issueHistory.getExtRequesterEmail())  &&
                (issueHistory.getCreationDate().getTime() > (System.currentTimeMillis()- CompanyCalendar.MILLIS_IN_WEEK * 2 ))) {
                  ButtonJS bjs = new ButtonJS(I18n.get("REPLY"), "addComment($(this));");
                  bjs.additionalCssClass = "small";
                  %><span class="textSmall" style="color: #999"><br>
                  <%=issueHistory.getCreator()%><br><%=DateUtilities.dateToRelative(issueHistory.getCreationDate())%>
                  </span>
                  <%bjs.toHtmlInTextOnlyModality(pageContext);%><%
              }
            }
          %>

          </td>
          <td valign="top" class="textSmall tikTask">
            <%=task.getName()%><br>
            <%if (isPublic){%>
              <a target="PPP" href="<%=ApplicationState.serverURL + "/project/" + task.getId()%>">T#<%=task.getMnemonicCode()%>#</a>
            <%}else {%>
              T#<%=task.getMnemonicCode()%>#
            <%}%>
          </td>
          <td nowrap valign="top"  class="tikRes"><%=issue.getAssignedTo() != null ? issue.getAssignedTo().getDisplayName() : ""%>&nbsp;  </td>
          <td nowrap valign="top"><%=JSP.w(issue.getShouldCloseBy())%>&nbsp;</td>
          <td nowrap valign="top"><%=JSP.w(issue.getCreationDate())%>&nbsp;</td>
        </tr>
        <% }%>
      </table>
    <%
    tab.end(pageContext);
  }

  tabSet.end(pageContext);

%>


<script>

  $(function(){
    createTableFilterElement($(".tabSetHeader"),"[issueId]",".tikCode,.tikDescr,.tikTask,.tikRes");
  })

</script>


  </div>
  </div>

<script>

  //WARINING template must be loaded before rows
  $("#iltmplt").loadTemplates().remove();
  $.JST.loadDecorator("FILEBOX", function (box, pf) {
    //console.debug(box,pf)
    box.data("pf", pf);
    //bind open image/download file
    box.click(function (ev) {
      ev.stopPropagation();
      openPersistentFile($(this).data("pf"));
    });

  });

  function drawFiles(issueId,files){
    //console.debug("drawFiles",issueId,files);
    var ndo=$("[issueId="+issueId+"] .filesBox");
    if (files.length>0) {
      ndo.find(".filesIcon").show();
    }
    //ndo.append(icon);
    for (var i=0; i<files.length;i++) {
      ndo.append($.JST.createFromTemplate(files[i], "FILEBOX"));
    }
  }


  function addComment(el){
    showComments(el,true);
  }

  function showComments(el,add){
    var row=el.closest("[issueId]");
    var issueId = row.attr("issueId");
    openBlackPopup(contextPath+"/applications/teamwork/publicPage/publicIssueComments.jsp?key=<%=key%>&issueId="+ issueId+  (add?"&CM=ADD":"")   +"&_="+(new Date().getTime()),800,600,function(){
      location.reload();
    });
  }


</script>

<%
  }
%>
