<%@ page import="org.jblooming.designer.DesignerField,
                 org.jblooming.designer.Detail,
                 org.jblooming.ontology.Identifiable,
                 org.jblooming.ontology.LookupSupport,
                 org.jblooming.utilities.ReflectionUtilities,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.input.DateField,
                 org.jblooming.waf.html.input.SmartCombo,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageState,
                 java.util.Date,
                 java.util.List"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  String detailName = pageState.getEntry("detailName").stringValue();
  String rowLine = pageState.getEntry("rowLine").stringValue();
  Detail detail = (Detail) pageState.getSessionState().getAttributes().get(detailName);
  int id;
  if ("ADD_LINE".equals(pageState.getCommand())) {
    id = detail.maxLine + 1;
    detail.maxLine = id;
  } else
    id = Integer.parseInt(rowLine);
  if (detail.detailDesignerFields != null && detail.detailDesignerFields.size() > 0) {
    %><tr id="TR_<%=detailName+'_'+id%>" class="alternate"><%
    for (DesignerField df : detail.detailDesignerFields.values()) {
      Class type = Class.forName(df.kind);
      List classes = ReflectionUtilities.getInheritedClasses(type);
      if (classes.contains(Date.class)) {
        pageState.initedElements.add(DateField.init);
      } else      if (classes.contains(LookupSupport.class) || classes.contains(Identifiable.class) || df.smartCombo != null) {
        pageState.initedElements.add(SmartCombo.class.getName());
      }
      df.label = "";
      df.separator = "";
      String origName = df.name;
      df.name = detail.name + "_" + df.name + "_" + id;
      %><td><%df.toHtml(pageContext);%></td><%
      df.name = origName;


    }
  }
  ButtonJS bs = new ButtonJS();
  bs.confirmQuestion = I18n.get("PROCEED_LINE_DELETION");
  bs.onClickScript = "$('#TR_"+detailName+"_"+rowLine+"').remove();";
  bs.confirmRequire = detail.confirmRequire;
  bs.iconChar="d";
  bs.additionalCssClass = "delete";
  bs.toolTip=""+id;
  bs.label="";
%><td style="text-align: center"><%bs.toHtmlInTextOnlyModality(pageContext);%></td>