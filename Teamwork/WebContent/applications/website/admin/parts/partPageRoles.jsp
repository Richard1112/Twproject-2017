<%@ page import="
                org.jblooming.operator.Operator,
                org.jblooming.oql.OqlQuery,
                org.jblooming.security.Area,
                org.jblooming.security.PlatformPermissions,
                org.jblooming.utilities.CodeValueList,
                org.jblooming.waf.html.button.ButtonJS,
                org.jblooming.waf.html.container.Container,
                org.jblooming.waf.html.input.Collector,
                org.jblooming.waf.html.input.Combo,
                org.jblooming.waf.settings.I18n,
                org.jblooming.waf.view.PageState"%><%

    PageState pageState = PageState.getCurrentPageState(request);
    Operator logged = pageState.getLoggedOperator();



   // inherit e propagate
      String hql = "select area.id, area.name from " + Area.class.getName() + " as area";
      OqlQuery qhelp = new OqlQuery(hql);
      CodeValueList cvl = new CodeValueList();
      for (Object o : qhelp.list()) {
        Object[] idN = (Object[]) o;
        cvl.add(idN[0].toString(),idN[1].toString());
        }
      cvl.addChoose(pageState);
      boolean applicationWithOutAreas = cvl.size()<=1;
      Combo cbb = new Combo("AREA", "&nbsp;", null, 15, null, cvl, "");
      //cbb.required = !applicationWithOutAreas;
      // area CAN'T be mandatory
      cbb.required = false;
      boolean readOnly = applicationWithOutAreas || !logged.hasPermissionFor(PlatformPermissions.area_canManage);
      cbb.readOnly = readOnly;
      cbb.label=I18n.get("AREA");

      if (!applicationWithOutAreas) {
        cbb.toHtml(pageContext);
      }

  %>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<%
  ButtonJS bjs= new ButtonJS(I18n.get("PERMISSIONS"),"showPerm($(this));");
  bjs.additionalCssClass="opener";
  bjs.iconChar="g&ugrave;";
  bjs.toHtmlInTextOnlyModality(pageContext);
%><div id="page_perm" style="display:<%=Collector.isCollectorCommand("pagePermColl", pageState.command)?"block":"none"%>;margin: 10px 0"><%


    /**
     * permissions
     */
    Container contPerms = new Container("pageprm",1);
    contPerms.title = I18n.get("PERMISSIONS");
    contPerms.collapsable = false;
    contPerms.status = Container.MAXIMIZED;
    contPerms.start(pageContext);
    Collector perms = new Collector("pagePermColl",200,pageState.getForm());
    perms.setDefaultLabels(pageState);
    perms.toHtml(pageContext);
    contPerms.end(pageContext);
    // end permissions
  %></div>
<script type="text/javascript">

  function showPerm(el){
    $('#page_perm').fadeIn(300,function(){el.remove()});
  }

</script>