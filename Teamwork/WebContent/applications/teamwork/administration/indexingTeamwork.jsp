<%@ page import="com.opnlb.fulltext.Indexable,
                 com.opnlb.fulltext.IndexingConstants,
                 com.opnlb.fulltext.IndexingMachine,
                 com.opnlb.fulltext.SnowballHackedAnalyzer,
                 com.opnlb.website.forum.ForumEntry,
                 org.apache.lucene.index.IndexReader,
                 org.hibernate.*,
                 org.hibernate.search.FullTextSession,
                 org.hibernate.search.Search,
                 org.hibernate.search.SearchFactory,
                 org.hibernate.search.indexes.IndexReaderAccessor,
                 org.jblooming.persistence.hibernate.HibernateFactory,
                 org.jblooming.persistence.hibernate.PersistenceContext,
                 org.jblooming.tracer.Tracer,
                 org.jblooming.utilities.CollectionUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.ReflectionUtilities,
                 org.jblooming.utilities.file.FileUtilities,
                 org.jblooming.waf.ScreenBasic,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.container.TabSet,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 java.io.File,
                 java.util.Iterator,
                 java.util.Map,
                 java.util.Set, org.jblooming.waf.settings.I18n" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    ScreenBasic.preparePage(null, pageContext);
    pageState.perform(request, response);

    if (Commands.START.equals(pageState.command)) {
      IndexingMachine.start();
    } else if (Commands.STOP.equals(pageState.command)) {
      IndexingMachine.stop();

    } if ("RANCID".equals(pageState.command)) {
      int batchSize = 500;
      String result="";

      PersistenceContext pc = PersistenceContext.getDefaultPersistenceContext();
      FullTextSession fullTextSession = Search.getFullTextSession(pc.session);
      SearchFactory searchFactory = fullTextSession.getSearchFactory();

      fullTextSession.setFlushMode(FlushMode.MANUAL);
      fullTextSession.setCacheMode(CacheMode.IGNORE);


      SessionFactory sf = HibernateFactory.getSessionFactory();
      Map acm = sf.getAllClassMetadata();
      Set keysAcm = acm.keySet();

      for (Iterator iterator = keysAcm.iterator(); iterator.hasNext();) {
        String className = (String) iterator.next();
        //it may be an entity name, and not a class
        Class persClass = null;
        try {
          persClass = Class.forName(className);
        } catch (ClassNotFoundException e) { }

        //if (!exceptions.contains(persClass) && ReflectionUtilities.directlyImplements(persClass,Indexable.class)) {
        if (persClass != null && pageState.getEntry(persClass.getName()).checkFieldValue()) {

          //Transaction t = fullTextSession.beginTransaction();

          Tracer.platformLogger.debug("trying to reindex: "+persClass.getName());
          ScrollableResults results = fullTextSession.createCriteria(persClass).scroll(ScrollMode.FORWARD_ONLY);

          int index = 0;
          int i=0;
          while (results.next()) {
            index++;
            Object o = results.get(0);
            fullTextSession.index(o); //index each element

            if (i == batchSize) {
              Tracer.platformLogger.debug("trying to reindex: "+persClass.getName()+" "+index);
              pc.checkPoint();
              i = 0;
            }
            i++;

          }
          pc.checkPoint();

          result+="OK: indexed "+index+" "+persClass.getSimpleName()+".<br>";
          Tracer.platformLogger.debug("OK: indexed "+index);
        }

      }
      searchFactory.optimize();
      //fullTextSession.close();

      pageState.addMessageOK("Indexing complete.<br>"+result);
    }


    pageState.toHtml(pageContext);
  } else {

    %><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>

<%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
%>
<%adminLink.toHtmlInTextOnlyModality(pageContext);%>
<h1>Index management</h1><%
/*
________________________________________________________________________________________________________________________________________________________________________


  INFO

________________________________________________________________________________________________________________________________________________________________________

*/
  String setting = ApplicationState.getApplicationSetting(IndexingConstants.INDEX_PATH);
  if (!JSP.ex(setting)) {
    %><p align="center"><big class="warning"><%=IndexingConstants.INDEX_PATH%> is not set in global settings!</big></p><%
  } else if (!new File(setting).exists()){
    %><p align="center"><big class="warning">Folder at <%=setting%> does no longer exist - cannot index.</big></p><%
  } else {

    Container indexInfo = new Container();
    indexInfo.title = "Twproject and Lucene - index and indexing information";
    indexInfo.start(pageContext);

    %><table width="100%"><tr><td><b>Index info</b><br><%
    %>Default analyzer language: <%=SnowballHackedAnalyzer.language%><br><%
    
    %>Index location: <%=setting%><br><%
    %>Index files total size: <%=FileUtilities.convertFileSize(
        FileUtilities.getFileSize(new File(setting)))%><br><%


  FullTextSession fullTextSession = Search.getFullTextSession(PersistenceContext.getDefaultPersistenceContext().session);
  SearchFactory searchFactory = fullTextSession.getSearchFactory();

  IndexReaderAccessor readerAccessor = searchFactory.getIndexReaderAccessor();

  IndexReader reader = null;
    try {
      reader = readerAccessor.open("fulltext");
        %>numDocs: <%=reader.numDocs()%><br><%
    } catch (Throwable e) {
        Tracer.platformLogger.error(e);
        %>Impossible to open Lucene index: <%=e.getMessage()%><br><%
    } finally {
        if (reader!=null)
          readerAccessor.close(reader);
    }


    %></td><td><b>Indexing Machine info</b><br><%
    if (IndexingMachine.isRunning()) {
      %>Indexing machine is running.<br><%
    } else {
      %>Indexing machine isn't running.<br><%
    }

  if (IndexingMachine.isIndexing()) {
    %>Indexing machine has documents in queue and is indexing them.<br><%
  } else {
    %>Indexing machine queue is empty.<br><%
  }



  %>toBeExecuted.size: <%=IndexingMachine.getQueueSize()%>

  </td></tr></table><%
  indexInfo.end(pageContext);

   ButtonBar bbInfo = new ButtonBar();

  PageSeed configPage = pageState.pageFromRoot("administration/teamworkGlobalSettings.jsp");
  TabSet.pointToTab("genTabSet", "indexingTS", configPage);  
  bbInfo.addButton(new ButtonLink("index location and language configuration", configPage));

  PageSeed reind = pageState.thisPage(request);
  reind.command = "RANCID";

  PageSeed ps = pageState.thisPage(request);
  ButtonLink stopStart = null;

  if (IndexingMachine.isRunning()) {
    ps.command=Commands.STOP;
    stopStart = new ButtonLink("stop indexing job",ps);
  } else {
    ps.command=Commands.START;
    stopStart = new ButtonLink("start indexing job",ps);
  }
  bbInfo.addButton(stopStart);

  bbInfo.toHtml(pageContext);


  PageSeed self = pageState.thisPage(request);
  self.setCommand(Commands.FIND);
  Form f = new Form(self);
  pageState.setForm(f);
  f.start(pageContext);


  Container reindexC = new Container();
  reindexC.title = "Reindex entities";
  reindexC.level=2;
  reindexC.start(pageContext);


  SessionFactory sf = HibernateFactory.getSessionFactory();
  Map acm = sf.getAllClassMetadata();
  Set keysAcm = acm.keySet();
  



%><table><%


  Set exceptions = CollectionUtilities.toSet(ForumEntry.class);

  if (keysAcm != null && keysAcm.size() > 0) {



    for (Iterator iterator = keysAcm.iterator(); iterator.hasNext();) {
      String className = (String) iterator.next();
      //it may be an entity name, and not a class
      Class persClass = null;
      try {
        persClass = Class.forName(className);
      } catch (ClassNotFoundException e) { }


      if (!exceptions.contains(persClass) && ReflectionUtilities.directlyImplements(persClass,Indexable.class)) {


        %> <tr class="alternate" ><td><%

        CheckField field = new CheckField(persClass.getName(), "</td><td>", true);
        field.label = persClass.getSimpleName();
        field.toHtml(pageContext); %></td></tr><%
      }
    }
  }
  %> </table> <%
  ButtonBar bb = new ButtonBar();
  ButtonSubmit sb = new ButtonSubmit(f);
  sb.label="reindex";
  sb.variationsFromForm.command = "RANCID";
  bb.addButton(sb);
  bb.toHtml(pageContext);

  reindexC.end(pageContext);    

    f.end(pageContext);





    }

  }
%>
