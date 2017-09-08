<%@ page import="com.opnlb.fulltext.Indexable, com.opnlb.fulltext.SnowballHackedAnalyzer, com.twproject.operator.TeamworkOperator,
org.apache.lucene.document.Document, org.apache.lucene.search.Query,
 org.apache.lucene.util.Version,
 org.jblooming.ontology.Identifiable, org.jblooming.ontology.IdentifiableSupport, org.jblooming.ontology.PersistentFile, org.jblooming.page.Page, org.jblooming.persistence.PersistenceHome, org.jblooming.security.Securable, org.jblooming.tracer.Tracer, org.jblooming.utilities.HtmlSanitizer, org.jblooming.utilities.JSP, org.jblooming.waf.Bricks, org.jblooming.waf.EntityViewerBricks, org.jblooming.waf.SessionState, org.jblooming.waf.constants.OperatorConstants, org.jblooming.waf.html.button.ButtonImg, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.display.Img, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Date, java.util.List, org.apache.lucene.queryparser.classic.QueryParser, org.hibernate.search.engine.ProjectionConstants, org.apache.lucene.analysis.TokenStream, org.hibernate.search.FullTextSession, org.hibernate.search.Search, org.jblooming.persistence.hibernate.PersistenceContext, org.apache.lucene.search.IndexSearcher, java.io.IOException, org.apache.lucene.index.*, org.apache.lucene.search.highlight.*" %>

<style>
  .ftResultRow {
    border-bottom:1px solid #e0e0e0;
    width: 100%;
  }
  .highlight{
    background-color: #ffff00;
    font-weight: bold;
  }

  .bestMatch{
    font-style: italic;
    font-size: 105%;
    margin-bottom: 10px;
  }

  .bestFragments{
    font-size: 85%;
  }

</style>

<%

   PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

Page results = pageState.getPage();
boolean foundSomething = false;
if (results != null) {
  int ps =  pageState.getEntry(OperatorConstants.OP_PAGE_SIZE).intValueNoErrorCodeNoExc();
  if (ps==0)
    ps = 10;
  boolean thereAreMoreResults = false;

  String wt = pageState.getEntry("TEXT").stringValue();
  List<Object[]> sortedByLuceneRank = new ArrayList();

  SnowballHackedAnalyzer hackedAnalyzer = new SnowballHackedAnalyzer();

  QueryParser snowParser = new QueryParser(Version.LUCENE_30,"content", hackedAnalyzer);
  Query searchQuery = snowParser.parse(wt);


  QueryScorer qs = new QueryScorer(searchQuery);
  //Highlighter highlighter = new Highlighter(new SimpleHTMLFormatter(), new SimpleHTMLEncoder(), qs);
  Highlighter highlighter = new Highlighter(new SimpleHTMLFormatter("<span class=\"highlight\">", "</span>"),qs);

  List<Object[]> tmpList = (List<Object[]>) results.getAllElements();
  for (Object[] o : tmpList) {
    //IdentifiableSupport is = (IdentifiableSupport) ReflectionUtilities.getUnderlyingObject(o[0]);
    Document lucDoc = (Document) o[0];
    String id = lucDoc.get("id");
    String clazz = lucDoc.get(ProjectionConstants.OBJECT_CLASS);
    Class<? extends Identifiable> aClass = (Class<? extends Identifiable>) Class.forName((String) clazz);

    IdentifiableSupport is = (IdentifiableSupport) PersistenceHome.findByPrimaryKey(aClass, id);
    if (is != null) {
      EntityViewerBricks.EntityLinkSupport edi = Bricks.getLinkSupportForEntity(is, pageState);
      if (edi==null)
        Tracer.platformLogger.error("Entity Link support not found for object class: " +aClass.getName()+ " id:"+id);
      if (edi!=null && ((Securable) is).hasPermissionFor(logged, edi.readPermission)) {
        foundSomething = true;
        if (sortedByLuceneRank.size() <= ps)
          sortedByLuceneRank.add(o);
        else {
          thereAreMoreResults = true;
          break;
        }
      }
    }
  }


  Date now = new Date();

  for (Object[] o : sortedByLuceneRank) {

    Document lucDoc = (Document) o[0];
    String id = lucDoc.get("id");
    String clazz = lucDoc.get(ProjectionConstants.OBJECT_CLASS);
    Class<? extends Identifiable> aClass = (Class<? extends Identifiable>) Class.forName((String) clazz);
    Indexable is = (Indexable) PersistenceHome.findByPrimaryKey(aClass, id);
    IdentifiableSupport ourFriend = (IdentifiableSupport)is;

    Number score = (Number) o[1];
    String area = lucDoc.get("area.id");

    boolean contentIsAbstract = true;
    String abs = lucDoc.get("abstract");
    if (!JSP.ex(abs)) {
      abs = is.getAbstractForIndexing();
      contentIsAbstract = false;
    }

    String pfS = lucDoc.get("persistentFile");
    PersistentFile pf = null;
    if (JSP.ex(pfS))
      pf = PersistentFile.deserialize(pfS);


%><table cellpadding="1" cellspacing="2"  class="ftResultRow">
 <tr><%

   String absSanitized = HtmlSanitizer.sanitizer(abs).text;
   String high = highlighter.getBestFragment(hackedAnalyzer, "content", absSanitized);

   String[] bestFragments = highlighter.getBestFragments(hackedAnalyzer, "content",absSanitized, 5);

   if (contentIsAbstract && !JSP.ex(high)) {
    high = "[content found but not in abstract]";
  }
%><td><div style="float: right"><span style="font-size:12px;"><%=I18n.get("SCORE")%>: <%=(int)(score.doubleValue()*100)%></span></div>
   <big>
    <%
      EntityViewerBricks.EntityLinkSupport edi = Bricks.getLinkSupportForEntity(ourFriend, pageState);

      if (edi != null && edi.bs!=null) {
        edi.bs.toHtmlInTextOnlyModality(pageContext);
      }

    %></big><div style="padding-left:30px"><%

        if (JSP.ex(high)) {
            %><div class="bestMatch"><%=high%></div><%
        }

        if (false && !(abs.equals(high))) {
          %><i><%=I18n.get("SEARCH_DOC")%>:</i> <%=JSP.limWr(absSanitized, 1000)%><%
        }


     for (String bf:bestFragments) {
       if (bf.equals(high))
         continue;
       %><div class="bestFragments"><%=JSP.w(bf)%></div><%
     }


     if (pf != null) {
       Img img = new Img("mime/" + pf.getMimeImageName() , "","18","");
       PageSeed view = pf.getPageSeed(false);
       view.setPopup(true);
       ButtonLink popup = new ButtonLink(view);
       popup.target = "_blank";
       popup.popup_toolbar = "no";
       popup.label=pf.getOriginalFileName();
       ButtonImg buttonImg = new ButtonImg(popup, img);
       buttonImg.toHtml(pageContext);
     }

      %></div></td>
</tr></table><br>
<%
  }

  if (thereAreMoreResults) {
%><div style="text-align: right"><%ButtonSubmit more = new ButtonSubmit(pageState.getForm());
    more.variationsFromForm.addClientEntry(OperatorConstants.OP_PAGE_SIZE,ps+10);
    more.label = I18n.get("MORE");
    more.additionalCssClass = "big";
    more.toHtmlInTextOnlyModality(pageContext);%></div> <%

  } 
  }

  if (!foundSomething) {

    String text = pageState.getEntry("TEXT").stringValueNullIfEmpty();
    if (JSP.ex(text)) {
      %><div style="font-size: 16px;padding-left: 10px"><%=I18n.get("SEARCH_NO_RESULTS_FOUND_SORRY")%></div><%
    }


  }
%>