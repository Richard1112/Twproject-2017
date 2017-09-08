<%@ page import=" org.jblooming.designer.DesignerField,
                  org.jblooming.designer.Detail,
                  org.jblooming.ontology.Identifiable,
                  org.jblooming.ontology.LookupSupport,
                  org.jblooming.tracer.Tracer,
                  org.jblooming.utilities.JSP,
                  org.jblooming.utilities.ReflectionUtilities,
                  org.jblooming.waf.html.button.ButtonJS,
                  org.jblooming.waf.html.core.JspHelper,
                  org.jblooming.waf.html.core.JspIncluderSupport,
                  org.jblooming.waf.html.input.TextField,
                  org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry,org.jblooming.waf.view.PageSeed,
                  org.jblooming.waf.view.PageState, java.util.Date, java.util.List, java.util.Map,
                  java.util.TreeSet" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  Detail.Drawer detailDrawer = (Detail.Drawer) JspIncluderSupport.getCurrentInstance(request);
  Detail detail = detailDrawer.detail;

  if (detail.detailDesignerFields == null || detail.detailDesignerFields.size() == 0)
    return;

  String detaiLinePart;

  if (detail.showAsTable)
    detaiLinePart = "/commons/layout/designer/partDesignerDetailLine.jsp";
  else
    detaiLinePart = "/commons/layout/designer/partDesignerDetailBlock.jsp";

  TreeSet<Integer> ids = new TreeSet<Integer>();
  Map<String, ClientEntry> mces = pageState.getClientEntries().getEntriesStartingWithStripped( detail.name + "_");
  for (String key : mces.keySet()) {
    String id = key.substring(key.lastIndexOf("_") + 1, key.length());
    if (JSP.ex(id)) {
      try {
        ids.add(Integer.parseInt(id));
      } catch (NumberFormatException e) {
        Tracer.platformLogger.debug(e);
      }
    }
  }
  int max = 0;
  if (ids != null && ids.size() > 0) {
    for (int id : ids) {
      max = Math.max(max, id);
    }
  } else if (detail.required || detail.firstLineVisible) {
    max = 1;
  }
  detail.maxLine = max;
  pageState.getSessionState().getAttributes().put(detail.name, detail);


  // inizializzazione componenti
  for (DesignerField df : detail.detailDesignerFields.values()) {
    Class type = Class.forName(df.kind);
    List classes = ReflectionUtilities.getInheritedClasses(type);
    if (classes.contains(Date.class) || classes.contains(LookupSupport.class) || classes.contains(Identifiable.class) || df.smartCombo != null) {
      %> <div style="visibility:hidden; height:1px; position:absolute;"> <%
      String origLabel = df.label;
      df.label="";
      df.separator = "";
      df.toHtml(pageContext);
      df.label = origLabel;
      %></div><%
    }
  }

  TextField.hiddenInstanceToHtml("DETAIL_IS_EMPTY_", pageContext);
  String error = pageState.getEntry("DETAIL_IS_EMPTY_" + detail.name).errorCode;

  // Draw add rows button
  PageSeed ps = pageState.pageFromWebApp(detaiLinePart);
  ps.setCommand("ADD_LINE");
  ButtonJS bs2 = new ButtonJS();
  bs2.onClickScript = "addDetailLine('" + ps.toLinkToHref() + "','TABLE_"+detail.name+"','"+detail.name+"');";
  bs2.label="";
  bs2.toolTip=I18n.get("ADD_LINE");
  bs2.iconChar="P";
  bs2.additionalCssClass="edit";


%>
<div id="DETAIL_<%=detail.name%>" >
  <table id="TABLE_<%=detail.name%>" width="100%" maxRow="<%=max%>" border="0" class="table">
    <thead>
<%

  if (detail.showAsTable){
    %><tr><th class="tableHead <%=JSP.ex(error)?"warning":""%>" colspan="99"><label><%=detail.label + (detail.required ? "*" : "")%></label></th></tr><tr><%
    //si disegna l'header
    for (DesignerField df : detail.detailDesignerFields.values()) {
      %><th align="center" width="<%=(df.fieldSize*10)%>px" class="tableHead"><%=df.label%><%=df.required ? "*" : ""%></th><%
    }
  } else {
    %><tr><th class="tableHead <%=JSP.ex(error)?"warning":""%>"><label><%=detail.label + (detail.required ? "*" : "")%></label></th><%

  }

  if(!detail.readOnly && !detail.correctionOnly && !detail.exportable) {
    %><th align="center" width="20" class="tableHead" style="text-align: center"><%bs2.toHtmlInTextOnlyModality(pageContext);%></th><%
  }

%>
    </tr>
    </thead>
<%
  int lineNum = 1;
  if(ids != null && ids.size()>0) {
    for (int id : ids) {
      pageState.addClientEntry("rowLine",id);
      pageState.addClientEntry("detailName",detail.name);
      new JspHelper(detaiLinePart).toHtml(pageContext);
      lineNum++;
    }
  } else if(detail.required || detail.firstLineVisible) {
    pageState.addClientEntry("rowLine",lineNum);
    pageState.addClientEntry("detailName",detail.name);
    new JspHelper(detaiLinePart).toHtml(pageContext);
  }

%>
  </table>
</div>
