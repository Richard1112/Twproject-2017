<%@ page
        import="org.hibernate.SessionFactory, org.jblooming.logging.AuditLogRecord, org.jblooming.logging.Auditable, org.jblooming.operator.Operator, org.jblooming.oql.QueryHelper, org.jblooming.page.HibernatePage, org.jblooming.page.Page, org.jblooming.utilities.CodeValueList, org.jblooming.utilities.JSP, org.jblooming.utilities.ReflectionUtilities, org.jblooming.waf.ScreenBasic, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.display.Paginator, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.DateField, org.jblooming.waf.html.input.LoadSaveFilter, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.html.table.ListHeader, org.jblooming.waf.settings.PersistenceConfiguration, org.jblooming.waf.state.PersistentSearch, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Map, java.util.Set" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  Operator logged = pageState.getLoggedOperator();
  if (!logged.hasPermissionAsAdmin())
    throw new SecurityException("Hahahahahahahaha!!!!");

  if (!pageState.screenRunning) {

    ScreenBasic.preparePage(pageContext);

    pageState.perform(request, response);

    if (Commands.FIND.equals(pageState.command)) {

      String hql = "from " + AuditLogRecord.class.getName() + " as au order by au.created desc";

      QueryHelper qhelp = new QueryHelper(hql);

      boolean recoveredFromSavedFilter = PersistentSearch.feedFromSavedSearch(pageState);

      String FLD_des = pageState.getEntry("message").stringValueNullIfEmpty();
      if (FLD_des != null) {
        qhelp.addQBEClause("au.message", "message", FLD_des, QueryHelper.TYPE_CHAR);
      }

      FLD_des = pageState.getEntry("entityClass").stringValueNullIfEmpty();
      if (FLD_des != null) {
        qhelp.addQBEClause("au.entityClass", "entityClass", FLD_des, QueryHelper.TYPE_CHAR);
      }

      FLD_des = pageState.getEntry("entityId").stringValueNullIfEmpty();
      if (FLD_des != null) {
        qhelp.addQBEClause("au.entityId", "entityId", FLD_des, QueryHelper.TYPE_CHAR);
      }

      FLD_des = pageState.getEntry("created").stringValueNullIfEmpty();
      if (FLD_des != null) {
        qhelp.addQBEClause("au.created", "created", FLD_des, QueryHelper.TYPE_DATE);
      }

      FLD_des = pageState.getEntry("data").stringValueNullIfEmpty();
      if (FLD_des != null) {
        qhelp.addQBEClause("au.data", "data", FLD_des, QueryHelper.TYPE_CHAR);
      }

      DataTable.orderAction(qhelp, "AUDITLH", pageState);
      pageState.setPage(HibernatePage.getHibernatePageInstance(qhelp.toHql().getQuery(), Paginator.getWantedPageNumber(pageState), Paginator.getWantedPageSize(pageState)));
    }

    pageState.toHtml(pageContext);

  } else {
    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);
    Form f = new Form(self);
    pageState.setForm(f);
    f.addKeyPressControl(13, "this.submit();", "onkeyup");
    f.start(pageContext);
    Container filter = new Container();
    filter.title = "Audit Trail";

    filter.start(pageContext);
%>
<style type="text/css">
  .audit {
    padding-left:10px;
    background-color:bisque;
  }
</style>
<table cellspacing="5" >
  <tr><td><%
    TextField tf = new TextField("TEXT",  "AUDIT_MESSAGE", "message", "</td><td>", 15, false);
    pageState.setFocusedObjectDomId(tf.id);
    tf.toHtmlI18n(pageContext);
  %>
    </td><td><%
      CodeValueList cvl = new CodeValueList();
      SessionFactory sf = PersistenceConfiguration.getDefaultPersistenceConfiguration().getSessionFactory();
      Map acm = sf.getAllClassMetadata();
      Set keysAcm = acm.keySet();

      if (keysAcm != null && keysAcm.size() > 0) {

        for (Object iterator : keysAcm) {
          String className = (String) iterator;
          //it may be an entity name, and not a class
          Class persClass = null;
          try {
            persClass = Class.forName(className);
          } catch (ClassNotFoundException e) {
          }

          if (ReflectionUtilities.extendsOrImplements(persClass, Auditable.class)) {
            cvl.add(persClass.getName(), persClass.getSimpleName());
          }

        }
        cvl.sort();
        cvl.addChoose(pageState);
        Combo cbb = new Combo("entityClass", "</td><td>", null, 25, cvl, "");
        cbb.label = "AUDIT_ENTITY";
        cbb.toHtmlI18n(pageContext);
      }
    %></td><td><%
     tf = new TextField("TEXT", "id", "entityId", "</td><td>", 15, false);
     tf.toHtml(pageContext);
     %>
    <td></tr><tr><td>
    <%
      DateField start_date_audit = new DateField("created");
      start_date_audit.separator="</td><td>";
      start_date_audit.labelstr = pageState.getI18n("AUDIT_CREATION_DATE");
      start_date_audit.setSearchField(true);
      start_date_audit.toHtml(pageContext);
    %></td><td><%

      tf = new TextField("TEXT", "changes", "data", "</td><td colspan='3'>", 35, false);
      tf.toHtml(pageContext);
      %>
    </td>
    </tr>
</table>
<%

  filter.end(pageContext);

  ButtonBar bb = new ButtonBar();

  LoadSaveFilter lsf = new LoadSaveFilter("AUDIT", f);
  bb.addButton(lsf);

  ButtonSubmit bs = new ButtonSubmit(f);
  bs.label = "<b>" + pageState.getI18n("SEARCH") + "</b>";
  bb.addButton(bs);
  bb.toHtml(pageContext);


  Page logs = pageState.getPage();

  Paginator p = new Paginator("AUDITPG", f);
%>
<table border="0" width="100%">
  <tr>
    <td><%p.toHtml(pageContext);%></td>
  </tr>
</table>

<table width="100%">
  <tr><%
    ListHeader lh = new ListHeader("AUDITLH", f);
    lh.addHeader("id");
    lh.addHeader("message");
    lh.addHeader("entity id");
    lh.addHeader("entity class");
    lh.addHeader("operator");
    lh.addHeader("when");
    lh.addHeader("changes");
    lh.toHtml(pageContext);
  %></tr>
  <%
    if (logs != null && logs.getThisPageElements().size() > 0) {



      for (Object alr : logs.getThisPageElements()) {
        AuditLogRecord log = (AuditLogRecord) alr;

  %>
  <tr class="alternate" >
    <td><%=log.getId()%></td>
    <td><%=JSP.w(log.getMessage())%> </td>
    <td><%=JSP.w(log.getEntityId())%> </td>
    <td><%

      String cl = JSP.w(log.getEntityClass());
      cl = cl.substring(cl.lastIndexOf(".")+1);%><%=cl%> </td>
    <td><%=JSP.w(log.getFullName())%> </td>
    <td><%=JSP.timeStamp(log.getCreated())%> </td>
    <td><%=JSP.convertLineFeedToBR(JSP.w(log.getData()))%> </td>

  </tr>
  <%
      }
    }

  %></table>
<%

    f.end(pageContext);

  }
%>