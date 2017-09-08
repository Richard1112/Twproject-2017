<%@ page import="com.twproject.document.TeamworkDocument,com.twproject.resource.Person,org.hibernate.Query,org.jblooming.agenda.CompanyCalendar,
                 org.jblooming.oql.QueryHelper,org.jblooming.utilities.JSP, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.button.AHref, com.twproject.security.TeamworkPermissions, org.jblooming.remoteFile.RemoteFile, org.jblooming.remoteFile.Document, com.twproject.operator.TeamworkOperator"%>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  Person loggedPerson = logged.getPerson();

  int maxAge = pageState.getEntryOrDefault("MAX_DOCUMENT_AGE_IN_DAYS", "0").intValue();
  boolean showInboxOnly =pageState.getEntryOrDefault("SHOW_ONLY_INBOX", "no").checkFieldValue();

  //first get issues expired
  String hql = "from "+TeamworkDocument.class.getName()+" as doc";
  QueryHelper qh = new QueryHelper(hql);

  if (maxAge>0){
    CompanyCalendar cc = new CompanyCalendar();
    cc.add(CompanyCalendar.DATE,-maxAge);
    qh.addOQLClause("doc.creationDate>=:maxAge","maxAge",cc.getTime());
  }

  if (showInboxOnly){
    qh.addOQLClause("doc.kind='inbox'");
  }

  qh.addOQLClause("doc.resource=:res","res", loggedPerson);


  qh.addToHqlString("order by doc.creationDate desc");
  Query query = qh.toHql().getQuery();
  List<TeamworkDocument> docs = query.list();

  //se non c'è niente bye
  if (!JSP.ex(docs))
    return;



%><div class="portletBox myDocuments"><%
  PageSeed ps = new PageSeed(request.getContextPath() + "/applications/teamwork/resource/resourceDocumentList.jsp");
  ps.command= "LIST_DOCS";
  ps.addClientEntry("RES_ID", loggedPerson.getId());

  ButtonLink sub = new ButtonLink(ps);
  sub.iconChar="i";
  sub.label="";
  sub.label=I18n.get("MYDOCUMENTS");
  sub.iconChar="";


  ButtonJS bs = new ButtonJS();
  bs.onClickScript = "$('#myDocuments').toggle()";
  bs.iconChar="g";
  bs.label="";
  bs.additionalCssClass="ruzzol";
  bs.toolTip=I18n.get("FILTER");


%><div style="float:right;padding-top: 5px">
  <%bs.toHtmlInTextOnlyModality(pageContext);%>
</div>

  <h1><%sub.toHtmlInTextOnlyModality(pageContext);%></h1>

  <div id="myDocuments" class="portletParams" style="display:none"><%


    TextField tf = new TextField("MAX_DOCUMENT_AGE_IN_DAYS","&nbsp;");
    tf.label=I18n.get("MAX_DOCUMENT_AGE_IN_DAYS");
    tf.fieldSize=2;
    tf.script=" onBlur=\"refreshPortlet($(this),{'MAX_DOCUMENT_AGE_IN_DAYS':$(this).val()});\"";
    tf.toHtmlI18n(pageContext);

  %>&nbsp;&nbsp;<%
    CheckField showInboxCK = new CheckField("SHOW_ONLY_INBOX","",true);
    showInboxCK.additionalOnclickScript="refreshPortlet($(this),{'SHOW_ONLY_INBOX':$('#SHOW_ONLY_INBOX').val()});";
    showInboxCK.toHtmlI18n(pageContext);
  %></div><%


  %><table id="docsTable" class="table dataTable" cellpadding="0" cellspacing="2" border="0"><thead>
  <tr>
    <th nowrap class="tableHead" colspan="2"><span class="tableHeadEl"><%=I18n.get("NAME")%></span></th>
  </tr>

    <%
    for (TeamworkDocument document:docs){

      boolean docCanRead = document.hasPermissionFor(logged, TeamworkPermissions.document_canRead);
      boolean docCanWrite = document.hasPermissionFor(logged, TeamworkPermissions.document_canWrite);
      boolean exists = document.existsFile();
  boolean validUrlToContent = true;
  if (Document.IS_FILE_STORAGE == document.getType()) {
    RemoteFile rf = document.getRemoteFile();
    if (exists && rf != null && rf.isDirectory()) {
      docCanRead = docCanRead && document.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canRead);
    } else if (!exists) {
      docCanRead = docCanRead && document.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canRead);
    }

  }


    PageSeed editDoc = pageState.pageFromRoot(document.getTask()!=null ? "task/taskDocumentEditor.jsp" : "resource/resourceDocumentEditor.jsp");
    editDoc.mainObjectId = document.getId();
    editDoc.command= Commands.EDIT;
    editDoc.addClientEntry(document.getTask()!=null?"TASK_ID":"RES_ID",document.getReferral().getId());
    ButtonSupport editLink = ButtonLink.getBlackInstance("",700,1000,editDoc);
    editLink.additionalCssClass="textual";
    editLink.iconChar="e";
    editLink.enabled = docCanWrite;

    String version="";
    if (TeamworkDocument.IS_UPLOAD==document.getType()){
      version=I18n.get("VERSION")+" "+I18n.get(document.getVersion());
    }


    AHref bl = document.bricks.getContentLink(pageState);



  %><tr class="alternate" docId="<%=document.getId()%>" title="<%=version%> <%=I18n.get("CREATED_ON")+" "+JSP.timeStamp(document.getCreationDate())%>">

    <td style="text-align: center"><div class="docLabelWrapper"><a href="<%=bl.href%>" target="<%=JSP.w(bl.target)%>"><%document.bricks.getMimeImage().toHtml(pageContext);%></a></div></td>
    <td><a href="<%=bl.href%>" target="<%=JSP.w(bl.target)%>"><%=document.getDisplayName()%></a><%
      if (!validUrlToContent || !exists){
    %><span class="teamworkIcon alert" style="cursor: default" title="<%=I18n.get("DOCUMENT_DATA_INVALID")%>">!</span><%
      }
    %>
    </td>
  </tr>
    <%


    }

  %>
</div>



