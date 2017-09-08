<%@ page import="com.twproject.document.TeamworkDocument, com.twproject.security.TeamworkPermissions, com.twproject.task.Task, com.twproject.waf.html.DocumentDrawer,
org.jblooming.ontology.Documentable, org.jblooming.ontology.Identifiable, org.jblooming.ontology.PerformantNodeSupport, org.jblooming.operator.Operator, org.jblooming.remoteFile.Document,
org.jblooming.remoteFile.RemoteFile, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.AHref, org.jblooming.waf.html.button.ButtonJS,
org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.settings.I18n,
org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Collections, java.util.Comparator, java.util.List, org.jblooming.utilities.StringUtilities, org.jblooming.ontology.IdentifiableSupport, org.jblooming.persistence.PersistenceHome, org.jblooming.waf.html.display.Img, com.twproject.resource.Resource, org.jblooming.designer.DesignerData"%><%


  DocumentDrawer documentDrawer = (DocumentDrawer) JspIncluderSupport.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);


  if ("OBJECTPART".equals(request.getAttribute(DocumentDrawer.ACTION))) {

    Documentable documentable = documentDrawer.documentable;
    Documentable startingPoint = documentDrawer.startingPoint;
    //IdentifiableSupport documentableObject= (IdentifiableSupport) documentDrawer.documentable;

    Class docClass=null;
    String editor ="";
    String documentableParamName="";

    if(documentable.getClass().getName().toLowerCase().contains("task")) {
      docClass=Task.class;
      editor ="task/taskDocumentEditor.jsp";
      documentableParamName ="TASK_ID";
    } else if(documentable.getClass().getName().toLowerCase().contains("resource")) {
      docClass= Resource.class;
      editor ="resource/resourceDocumentEditor.jsp";
      documentableParamName ="RES_ID";
    }

    if (docClass==null)
      return;


    //impostazioni di visualizzazione della pagina
    boolean docShowChildren = pageState.getEntry("DOC_SHOW_CHILDREN").checkFieldValue();
    boolean docShowEmpty=pageState.getEntryOrDefault("DOC_SHOW_EMPTY_CHILD").checkFieldValue();


    //a documenti come siamo messi?
    boolean printingMainObject = documentable.equals(startingPoint);
    boolean hasDocuments=JSP.ex(documentable.getDocuments()) ;
    boolean hasFormFilled=DesignerData.hasFormFilled(documentable,null);


    boolean skip=!hasDocuments && !hasFormFilled && !docShowEmpty;


    // si stampa l'header dell'object se non è quello principale
    PageSeed listDocs =  pageState.thisPage(request);
    listDocs.command="LIST_DOCS";

    String styleColor="";
    if (Task.class.equals(docClass)){
      Task task= (Task) documentable;
      String color = task.getTaskColor();
      if (I18n.isActive("CUSTOM_FEATURE_USE_PROJECT_COLOR") && JSP.ex(color)) {
        styleColor = " style=\"border-left: 5px solid " + color + ";\"";
      }
    }

    //si disegna se non è il primo e se non va saltato
    if (!printingMainObject && !skip) {
      listDocs.addClientEntry(documentableParamName,documentable.getId());
      ButtonLink edit = ButtonLink.getTextualInstance(I18n.get(documentable.getName()), listDocs);
      %><tr><td colspan="99">&nbsp;</td></tr>
        <tr class="tableSection"><td colspan="10" <%=styleColor%> ><div class="childNode"><h2><span class="pathSmall inline"><%=((PerformantNodeSupport)documentable).getPath(" / ", false)%> /</span><%edit.toHtml(pageContext); %></h2></div></td></tr>
      <%
    }



    List<Document> documents = new ArrayList<Document>();
    documents.addAll(documentable.getDocuments());

    Collections.sort(documents, new Comparator<Document>() {
      public int compare(Document o1, Document o2) {
        return (o1.getName().toUpperCase()+o1.getVersion()).compareTo(o2.getName().toUpperCase()+o2.getVersion());
      }
    });

    if (documents.size() > 0) {
      for (Document document : documents) {
        documentDrawer.currentDocument = (TeamworkDocument) document;
        documentDrawer.drawDocument((TeamworkDocument) document, pageContext);
      }

    }



  //si aggiungono i custom forms  added R 23/9/2016
  //dato che il mainObject del pageState è usato per valutare il contesto di visualizzazione dei form occorre salvare quello vecchio e sostituirci il task del loop
  Identifiable oldMainObject = pageState.getMainObject();
  pageState.setMainObject(documentable);

  int formsDisplayed=0;
  if (printingMainObject || !skip ) { // si stampano solo se stiamo disegnando il mainObject altrimenti solo se riempiti
    JspHelper cufd = new JspHelper("/applications/teamwork/document/partCustomForm.jsp");
    cufd.parameters.put("documentable", documentable);
    cufd.toHtml(pageContext);
    formsDisplayed = pageState.getEntry("formsDisplayed").intValueNoErrorCodeNoExc();
    pageState.removeEntry("formsDisplayed");
    pageState.setMainObject(oldMainObject);
  }
  boolean isTask= pageState.href.indexOf("task")>=0;

  if(formsDisplayed==0 && documents.size()==0 && !docShowChildren) {
    %><tr class="alternate">
  <td colspan="99" ><h2 class="hint" style="text-align: center"><%=I18n.get("NO_DOCUMENTS_FOR_TASK")%></h2><%
    if (documentable.hasPermissionFor(pageState.getLoggedOperator(),TeamworkPermissions.document_canCreate)) {
      %>
        <div id="no_docs_add_box" class="footerWarningWrapper">

          <div class="footerWarning uploadizeDrop visible fake" style="width: 25%">
            <div><%new Img("dropHere.png","","","40").toHtml(pageContext);%></div>
            <p><%=I18n.get("ADD_DOCUMENT_UPLOAD_TEXT")%></p>
          </div>

          <div class="footerWarning" style="width: 25%">
            <div><%
              new Img("mime/fileStorage.png", "", "30","").toHtml(pageContext);
            %></div>
            <div><%
              PageSeed ps = pageState.pageFromRoot(editor);
              ps.setCommand(Commands.ADD);
              ps.addClientEntry(documentableParamName, documentable.getId());
              ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_FILE_STORAGE);
              ButtonSupport bl = ButtonLink.getBlackInstance(I18n.get("ADD_DOCUMENT_REMOTE"),620,1000, ps);
              bl.iconChar="P";
              bl.toHtml(pageContext);
            %></div>
            <p><%=I18n.get("ADD_DOCUMENT_REMOTE_TEXT")%></p>
          </div>


          <div class="footerWarning" style="width: 25%">
            <div><%new Img("external-storages.png","","","35").toHtml(pageContext);%></div>
            <div><%
              ps = pageState.pageFromRoot(editor);
              ps.setCommand(Commands.ADD);
              ps.addClientEntry(documentableParamName, documentable.getId());
              ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_URL);
              bl = ButtonLink.getBlackInstance(I18n.get("ADD_DOCUMENT_URL"),620,700, ps);
              bl.iconChar="P";
              bl.toHtml(pageContext);
            %></div>
            <p><%=I18n.get("ADD_DOCUMENT_URL_TEXT")%></p>
          </div>

          <div class="footerWarning" style="width: 25%">

            <div><span class="teamworkIcon" style="font-size:35px">&#233;</span></div>
            <div><%
              ps = pageState.pageFromRoot(editor);
              ps.setCommand(Commands.ADD);
              ps.addClientEntry(documentableParamName, documentable.getId());
              ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_CONTENT);
              bl = ButtonLink.getBlackInstance(I18n.get("ADD_DOCUMENT_CONTENT"),700,700, ps);
              bl.iconChar="P";
              bl.toHtml(pageContext);
              %>
            </div><p><%=I18n.get("ADD_DOCUMENT_CONTENT_TEXT")%></p></div>
          </div>
      <%
    }
    %></td> </tr><%
  }


  /* ---------------------------------------- CHILDREN ---------------------------------------- */
  if (docShowChildren) {

    List children= new ArrayList();

    if (isTask) {
      children.addAll(((Task)documentable).getChildrenSorted());
    } else {
      children.addAll(documentable.getChildren());
    }

    for (Object pns:children) {
      Documentable child = (Documentable) pns;
      child=PersistenceHome.initializeAndUnproxy(child);
      DocumentDrawer recurseAssigOnChildren = new DocumentDrawer(child,startingPoint);
      recurseAssigOnChildren.drawingChild=true;
      recurseAssigOnChildren.drawDocumentable(pageContext);
    }
  }


  /* ---------------------------------------- DOCUMENT PART ----------------------------------------*/
} else {

  TeamworkDocument document = documentDrawer.currentDocument;

  Operator logged = pageState.getLoggedOperator();
  boolean docCanRead = document.hasPermissionFor(logged, TeamworkPermissions.document_canRead);
  boolean docCanWrite = document.hasPermissionFor(logged, TeamworkPermissions.document_canWrite);

  boolean exists = document.existsFile();


  //in case of IS_FILE_STORAGE check even for fileStorage_explorer_canRead in case of "directory" otherwise is a doc_read standard
  boolean validUrlToContent = true;
  if (Document.IS_FILE_STORAGE == document.getType()) {
    RemoteFile rf = document.getRemoteFile();
    if (exists && rf != null && rf.isDirectory()) {
      docCanRead = docCanRead && document.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canRead);
    } else if (!exists) {
      docCanRead = docCanRead && document.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canRead);
    }

  }
  if (docCanRead) {

%><tr class="alternate" docId="<%=document.getId()%>"><%

  PageSeed editDoc = pageState.pageFromRoot(document.getTask()!=null ? "task/taskDocumentEditor.jsp" : "resource/resourceDocumentEditor.jsp");
  editDoc.mainObjectId = document.getId();
  editDoc.command=Commands.EDIT;
  editDoc.addClientEntry(document.getTask()!=null?"TASK_ID":"RES_ID",document.getReferral().getId());

  ButtonSupport editLink = ButtonLink.getBlackInstance("",700,1000,editDoc);
  editLink.additionalCssClass="textual edit";
  editLink.iconChar="e";
  editLink.enabled = docCanWrite;


  AHref bl = document.bricks.getContentLink(pageState);


%><td style="text-align: center" width="10" >  <input type="checkbox" class="selector" onclick="refreshBulk($(this));"></td>
  <td style="text-align: center"><div class="docLabelWrapper"><a href="<%=bl.href%>" target="<%=JSP.w(bl.target)%>"><%document.bricks.getMimeImage().toHtml(pageContext);%></a></div></td>
  <td><a href="<%=bl.href%>" target="<%=JSP.w(bl.target)%>"><%=document.getDisplayName()%></a><%
    if (!validUrlToContent || !exists){
  %><span class="teamworkIcon alert" style="cursor: default" title="<%=I18n.get("DOCUMENT_DATA_INVALID")%>">!</span><%
    }
  %>
  </td>

  <td class="textSmall"><%
    if (TeamworkDocument.IS_UPLOAD==document.getType()){
      %><%=I18n.get("VERSION")+" "+I18n.get(document.getVersion())%><%
      if ((document.getLockedBy()!=null)) {
        %> (<span class="teamworkIcon" style="font-size: 100%">o</span> <%=document.getLockedBy().getDisplayName()%>)<%
      }
    }

  %></td>
  <td class="textSmall"><span class="period"><%=JSP.w(document.getLastModified())%></span></td>
  <td class="textSmall" width="20%">

  <%
    if (JSP.ex(document.getTags())) {
      PageSeed search = pageState.pageFromRoot("task/taskDocumentList.jsp");
      search.command = Commands.FIND;

      for (String tag : StringUtilities.splitToList(document.getTags(), ",")) {
        %><span class="tag button textual" style="cursor:auto;text-decoration: none"><%=tag%></span> <%
      }
    }

  %></td>
  <td align="left" width="50"><%
    editLink.toHtml(pageContext);

    if (document.hasPermissionFor(logged, TeamworkPermissions.document_canDelete)) {
      ButtonJS delete = new ButtonJS("delRow($(this));");
      delete.iconChar = "d";
      delete.toolTip = I18n.get("DELETE");
      delete.additionalCssClass = "delete";
      delete.toHtmlInTextOnlyModality(pageContext);
    }
  %>
  </td>
</tr>
<%
    }
  }
%>

